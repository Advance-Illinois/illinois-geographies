# Building geographic files that are useful for analysis

library(tidyverse)
library(sf)
library(readxl)
library(openxlsx)

#######################################
### BEFORE UPDATING, DOWNLOAD DATA! ###
#######################################

# I know it's not the most elegant way, but start with manually downloading data:

## 1 ## Download the most recent district shapefile: https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries
## 2 ## Download the most recent district AND school locale assignments (they are separate files): https://nces.ed.gov/programs/edge/Geographic/SchoolLocations
## 3 ## Then unzip the folders and put the unzipped versions under "raw/" in this code's parent folder
## 4 ## Download the most recent directory of entities and store under "raw/": https://www.isbe.net/Pages/Data-Analysis-Directories.aspx

## _ ## You don't need to update this regularly, but as an FYI, this is where the county files are from: https://www.arcgis.com/home/item.html?id=49aa3f4f9f754c78a1876fe355d63620

####################
### READ IN DATA ###
####################

# set this variable with the school year you want (year of spring semester)
schlyr = 2023
schlyr_short = paste0(schlyr %% 100 - 1, schlyr %% 100)

# This file is for getting geographies (shapes)
districts_geo <- read_sf(paste0("raw/EDGE_SCHOOLDISTRICT_TL", schlyr - 2000, "_SY", schlyr_short, "/EDGE_SCHOOLDISTRICT_TL_", schlyr - 2000, "_SY", schlyr_short,  ".shp")) %>% 
  filter(STATEFP == "17")

# These files are for getting locale assignments AND geographies
schools_locale <- read_sf(paste0("raw/EDGE_GEOCODE_PUBLICSCH_", schlyr_short, "/Shapefiles_SCH/EDGE_GEOCODE_PUBLICSCH_", schlyr_short, ".shp")) %>%
  filter(STFIP == "17")

districts_locale <- read_sf(paste0("raw/EDGE_GEOCODE_PUBLICLEA_", schlyr_short, "/Shapefiles_LEA/EDGE_GEOCODE_PUBLICLEA_", schlyr_short, ".shp")) %>%
  filter(STFIP == "17")

counties <- read_sf("raw/IL_BNDY_County/IL_BNDY_County_Py.shp")

crosswalk <- read_excel("raw/dir_ed_entities.xls", sheet = 2) %>%
  mutate(RCDTS = paste0(`Region-2\nCounty-3\nDistrict-4`, Type, School)) %>%
  select("NCES ID", RCDTS, "CountyName")

#########################################
#### MAKE CLEAN DISTRICT SHAPE FILES ####
#########################################

districts_shapes <- districts_geo %>%
  left_join(crosswalk, by = c("GEOID"="NCES ID"))

dir.create(paste0("IL_school_districts_", schlyr_short))
st_write(districts_shapes, paste0("IL_school_districts_", schlyr_short, "/IL_school_districts_", schlyr_short, ".shp"))

#######################################
#### GET SCHOOL LOCALE INFORMATION ####
#######################################
# Locale Codes Documentation: https://nces.ed.gov/programs/edge/docs/EDGE_GEOCODE_PUBLIC_FILEDOC.pdf
schools_final <- schools_locale %>%
  left_join(crosswalk, by = c("NCESSCH"="NCES ID")) %>%
  select(RCDTS, NAME, CNTY, SCHOOLYEAR, geometry, LOCALE) %>%
  mutate_at("LOCALE", as.numeric) %>%
  mutate(locale_general = case_when(
    LOCALE >= 10 & LOCALE < 20 ~ "Urban",
    LOCALE >= 20 & LOCALE < 30 ~ "Suburban",
    LOCALE >= 30 & LOCALE < 50 ~ "Rural/Town"
  )) %>%
  mutate(locale_specific = case_when(
    LOCALE >= 10 & LOCALE < 20 ~ "Urban",
    LOCALE >= 20 & LOCALE < 30 ~ "Suburban",
    LOCALE >= 30 & LOCALE < 40 ~ "Town",
    LOCALE == 43 ~ "Rural - Remote",
    LOCALE >= 40 ~ "Rural - Non-Remote"
  ))


#QA
table(schools_final$locale_general, useNA = "always")

write_csv(schools_final %>% select(RCDTS, NAME, CNTY, locale_specific, locale_general, LOCALE), 
          paste0("IL_schools_locale_", schlyr_short, ".csv"))

#########################################
#### GET DISTRICT LOCALE INFORMATION ####
#########################################
districts_final <- crosswalk %>%
  right_join(districts_locale, by = c("NCES ID" = "LEAID")) %>%
  select(RCDTS, NAME, SCHOOLYEAR, LOCALE) %>%
  mutate_at("LOCALE", as.numeric) %>%
  mutate(locale_general = case_when(
    LOCALE >= 10 & LOCALE < 20 ~ "Urban",
    LOCALE >= 20 & LOCALE < 30 ~ "Suburban",
    LOCALE >= 30 & LOCALE < 40 ~ "Town",
    LOCALE >= 40 & LOCALE < 50 ~ "Rural"
  )) %>%
  mutate(locale_specific = case_when(
    LOCALE >= 10 & LOCALE < 20 ~ "Urban",
    LOCALE >= 20 & LOCALE < 30 ~ "Suburban",
    LOCALE >= 30 & LOCALE < 40 ~ "Town",
    LOCALE == 43 ~ "Rural - Remote",
    LOCALE >= 40 ~ "Rural - Non-Remote"
  ))

write_csv(districts_final %>% select(RCDTS, NAME, SCHOOLYEAR, locale_general, locale_specific, LOCALE), 
          paste0("IL_districts_locale_", schlyr_short, ".csv"))

#######################################################################################
#### MAKE MASTER CSV WITH MULTIPLE YEARS OF LOCALE INFORMATION AT THE SCHOOL LEVEL ####
#######################################################################################

# Do this for all years that all the component data is available
master_locale_codes <- data.frame()
years = c(2018:2021)
for (year in years) {
  print(year)
  short_year = year - 2000
  if(year >= 2019) {
    schools <- read_sf(paste0("raw/EDGE_GEOCODE_PUBLICSCH_", short_year-1, short_year, 
                              "/EDGE_GEOCODE_PUBLICSCH_", short_year-1 , short_year, 
                              "/Shapefiles_SCH/EDGE_GEOCODE_PUBLICSCH_", short_year-1, short_year, ".shp")) %>%
      filter(STFIP == "17")
  } else {
    schools <- read_sf(paste0("raw/EDGE_GEOCODE_PUBLICSCH_", short_year-1, short_year, 
                              "/EDGE_GEOCODE_PUBLICSCH_", short_year-1 , short_year, 
                              "/EDGE_GEOCODE_PUBLICSCH_", short_year-1, short_year, ".shp")) %>%
      filter(STFIP == "17")
  }
  
  crosswalk <- read_excel(paste0("raw/", year-1, "-", year, "-dir-ed-entities.xls"), sheet = 2) %>%
    mutate(RCDTS = paste0(`Region-2\nCounty-3\nDistrict-4`, Type, School)) %>%
    select("NCES ID", RCDTS, "CountyName")
  
  df <- schools %>%
    left_join(crosswalk, by = c("NCESSCH"="NCES ID")) %>%
    select(RCDTS, NAME, CNTY, LOCALE) %>%
    mutate_at("LOCALE", as.numeric) %>%
    mutate(localeType = case_when(
      LOCALE >= 10 & LOCALE < 20 ~ "Urban",
      LOCALE >= 20 & LOCALE < 30 ~ "Suburban",
      LOCALE >= 30 & LOCALE < 50 ~ "Rural/Town"
    )) %>%
    mutate(year = year)
  
  if (nrow(master_locale_codes) > 0) {
    master_locale_codes <- bind_rows(master_locale_codes, df)
  } else {
    master_locale_codes <- df
  }
}

# QA data
table(master_locale_codes$year)
table(master_locale_codes$year, master_locale_codes$localeType, useNA = "always")

write_csv(master_locale_codes, "School_Locale_Codes_2018-2021.csv")

###########################
#### MAKE ISBE REGIONS ####
###########################

sf::sf_use_s2(FALSE)

county_to_region <- districts_shapes %>%
  mutate(region = substr(RCDTS, 1, 2),
         county = substr(RCDTS, 3, 5)) %>%
  mutate_at("CountyName", toupper) %>%
  mutate(CountyName = case_when(
    CountyName == "LA SALLE" ~ "LASALLE",
    CountyName == "SAINT CLAIR" ~ "ST. CLAIR",
    TRUE ~ CountyName
  )) %>%
  select(region, county, CountyName) %>%
  distinct()

st_geometry(county_to_region) <- NULL

regions <- counties %>%
  full_join(county_to_region, by = c("COUNTY_NAM" = "CountyName")) %>%
  group_by(region) %>%
  summarise(do_union = TRUE)

dir.create(paste0("isbe_regions_", schlyr_short))
st_write(regions, paste0("isbe_regions_", schlyr_short, "/isbe_regions_", schlyr_short, ".shp"))