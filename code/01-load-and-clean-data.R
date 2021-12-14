library(tidyverse)
library(tidycensus)
library(srvyr)
library(sf)


# Download 2020 Experimental PUMA data ------------------------------------

# Download household-level data
# Modify "dc" in file with 2 letter state FIPS codes, or "us" for all states
download.file(url = "https://www2.census.gov/programs-surveys/acs/experimental/2020/data/pums/1-Year/csv_hdc.zip",
              destfile = "data/csv_hdc.zip")

# Unzip and read CSV
unzip(zipfile = "data/csv_hdc.zip", exdir = "data/csv_hdc")
hdc <- read_csv("data/csv_hdc/psam_h11.csv")

# Download person-level data
# Modify "dc" in file with 2 letter state FIPS codes, or "us" for all states
download.file(url = "https://www2.census.gov/programs-surveys/acs/experimental/2020/data/pums/1-Year/csv_pdc.zip",
              destfile = "data/csv_pdc.zip")

# Unzip and read CSV
unzip(zipfile = "data/csv_pdc.zip", exdir = "data/csv_pdc")
pdc <- read_csv("data/csv_pdc/psam_p11.csv")


# Get PUMA names and shapefiles -------------------------------------------

# Download PUMA shapefiles
download.file(url = "https://usa.ipums.org/usa/resources/volii/shapefiles/ipums_puma_2010.zip",
              destfile = "data/ipums_puma_2010.zip")
# Unzip
unzip(zipfile = "data/ipums_puma_2010.zip", 
      exdir = "data/ipums_puma_2010")

# Read in shapefiles
us_pumas <- st_read(dsn = "data/ipums_puma_2010",
                    layer = "ipums_puma_2010")

# Clean shape data
us_pumas <- us_pumas %>% mutate(STATEFIP = as.numeric(STATEFIP))

# View PUMAs
head(us_pumas)

# Merge with PUMA Name ----------------------------------------------------
# Household
hdc_geo <- hdc %>% left_join(us_pumas, by = c("ST" = "STATEFIP", "PUMA" = "PUMA"))
# Person
pdc_geo <- pdc %>% left_join(us_pumas, by = c("ST" = "STATEFIP", "PUMA" = "PUMA"))


# (Optional) Recode Categorical Var Labels --------------------------------

# Download PUMA data dictionary

download.file("https://www2.census.gov/programs-surveys/acs/experimental/2020/documentation/pums/PUMS_Data_Dictionary_2020.csv",
              destfile = "data/PUMS_Data_Dictionary_2020.csv")

data_dictionary <- read_csv("data/PUMS_Data_Dictionary_2020.csv",
                            col_names = c("record_type","var_code","var_type","val_length","var_min","var_max","label"))

# All variable names except weights
var_names <- data_dictionary %>% 
  select(record_type, 
         variable_code = var_code, 
         variable_label = var_min) %>% 
  filter(record_type == "NAME") %>%
  distinct(variable_code, variable_label) %>%
  filter(!grepl("WGTP",variable_code))

# Select only categorical variables
# Convert to named list
var_labels_list <- data_dictionary %>%
  filter(record_type == "VAL" & var_type == "C") %>%
  distinct(var_code, var_min, label) %>%
  filter(!var_code %in% c("SERIALNO","PUMA","ADJHSG","ADJINC"))%>%
  split(f = factor(.$var_code, ordered = T)) %>%
  map(~.x %>% select(-var_code) %>% deframe())

# Matching variables in dataset
matching_h <- na.omit(names(var_labels_list[names(hdc_geo)]))
matching_p <- na.omit(names(var_labels_list[names(pdc_geo)]))

# Recode matching columns for household
hdc_geo_recoded <- map2_dfr(hdc_geo %>% select(matching_h), 
                            var_labels_list[matching_h], 
                            ~recode(.x, !!!.y)) %>%
  bind_cols(hdc_geo %>% select(-matching_h)) %>%
  select(names(hdc_geo))

# Recode matching columns for person
pdc_geo_recoded <- map2_dfr(pdc_geo %>% select(matching_p), 
                            var_labels_list[matching_p], 
                            ~recode(.x, !!!.y)) %>%
  bind_cols(pdc_geo %>% select(-matching_p)) %>%
  select(names(pdc_geo))

# Convert to survey design with tidycensus package -------------------------

hdc_srvy <- tidycensus::to_survey(hdc_geo_recoded, type = "housing")
pdc_srvy <- tidycensus::to_survey(pdc_geo_recoded, type = "person")


# Analyze variables with srvyr --------------------------------------------

# Example: population and median income
pdc_srvy %>% 
  mutate(PINCP_ADJ = PINCP * (ADJINC/1000000)) %>%
  group_by(Name) %>%
  summarize(population = survey_total(vartype = "ci"),
            median_income = survey_median(PINCP_ADJ, vartype = "ci",na.rm = T)) 
