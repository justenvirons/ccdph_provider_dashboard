## ---------------------------
##
## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-10-19
## Date Last Updated: 2021-10-19
## Email: christopher.smith@cookcountyhealth.org
## ---------------------------
##
## Notes: data processing for COVID-19 provider SCC dashboard
##   
##
## ---------------------------


# install packages --------------------------------------------------------
library(dplyr)
library(censusapi)
library(tigris)
library(tidyverse)
library(sf)
library(utils)
library(readxl)
library(units)

# Cook County Census Tracts and 2020 census population --------------------
# cc_tracts_2020 <- st_read("layers/cc_tracts_2020_epsg3435.shp") %>%
#   select(fips)
# cc_tracts_2020_pop_by_race <- read_csv("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-data-sets/2020/decennial-2020-race-ethnicity-by-tract.csv") %>%  mutate(fips=as.character(fips),
#          tract=as.character(tract),
#          district=str_to_title(district),
#          location=str_to_title(location))
# cc_tracts_geo <- cc_tracts_2020 %>%
#   left_join(cc_tracts_2020_pop_by_race)
# 
# cc_tracts_geo <- cc_tracts_geo %>%
#   mutate(district=str_to_title(district),
#          location=str_to_upper(location),
#          location = if_else(location=="CCDPH", str_to_upper(district), location))

# st_write(cc_tracts_geo,"layers/cc_tracts.shp", append=FALSE)


# Create district boundaries ----------------------------------------------
# cc_districts <- cc_tracts_geo %>%
#   group_by(location) %>%
#   summarize(geometry = st_union(geometry)) %>%
#   st_as_sf() %>%
#   mutate(sqmi = as.numeric(st_area(geometry))*0.00000003587006428 %>% set_units(mi^2))
# 
# st_write(cc_districts,"layers/cc_districts.shp", append=FALSE)

# Cook County municipalities and 2020 census population -------------------
# cc_munis <- st_read("layers/ccdph_munis.shp")
# cc_munis_sub <- cc_munis %>%
#   drop_na(district) %>%
#   filter(exclude != 1) %>%
#   mutate(fips_muni = as.character(fips),
#        sqmi_muni = st_area(geometry)*0.00000003587006428) %>%
#   select(muni_name,
#          district,
#          sqmi_muni)
# 
# cc_munis_pop_2020 <- read_csv("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-data-sets/2020/decennial-2020-race-ethnicity-by-muni-cleaned.csv")
# cc_munis_pop_2020_wide <- cc_munis_pop_2020 %>% 
#   pivot_wider(names_from=race_ethnicity, values_from=population) %>%
#   rename(fips_muni=census_place_code,
#          hisp=4,
#          nh_asian=5,
#          nh_black=6,
#          nh_white=9,
#          nh_other=8,
#          nh_multi=7) %>%
#   mutate(total_muni = hisp + nh_asian + nh_black + nh_white + nh_multi + nh_other) %>%
#   select(fips_muni,
#          muni_name = clean_name,
#          hisp:total_muni)
# 
# cc_munis_geo <- cc_munis_sub %>%
#   left_join(cc_munis_pop_2020_wide, by="muni_name")
# 

# add Elevate Eleven "priority community" flag ----------------------------
# cc_munis_geo <- cc_munis_geo %>%
#   mutate(priority=case_when(muni_name=="Burnham" ~ "YES",
#                             muni_name=="Calumet City" ~ "YES",
#                             muni_name=="Dixmoor" ~ "YES",
#                             muni_name=="Dolton" ~ "YES",
#                             muni_name=="Ford Heights" ~ "YES",
#                             muni_name=="Harvey" ~ "YES",
#                             muni_name=="Markham" ~ "YES",
#                             muni_name=="Phoenix" ~ "YES",
#                             muni_name=="Riverdale" ~ "YES",
#                             muni_name=="Robbins" ~ "YES",
#                             muni_name=="Sauk Village" ~ "YES"),
#          priority=replace_na(priority,"NO"))
# 
st_write(cc_munis_geo,"layers/cc_munis.shp", append=FALSE)

# Clean environment -------------------------------------------------------
rm(list=c("cc_tracts_2020",
          "cc_tracts_2020_pop_by_race",
          "cc_munis",
          "cc_munis_pop_2020",
          "cc_munis_sub",
          "cc_munis_pop_2020_wide"))

# Input Suburban Cook County COVID-19 vaccine providers ------------------
# scc_vaccine_providers_name <- read_excel("E:/OneDrive - Cook County Health/git_repos/justenvirons/CCDPH-vaccine-mgmt/data/scc_vaccine_providers_geocoded_20210917.xls", sheet="providers_091721_geocoded") %>%
#   select(objectid = ObjectID,
#         name = USER_name)

# mutate(name_rev=str_to_title(USER_name)) %>%
# mutate(name_rev = str_replace_all(name_rev,"Amg","AMG"),
#        name_rev = str_replace_all(name_rev,"AMG - ","AMG"),
#        name_rev = str_replace_all(name_rev,"Cvs","CVS"),
#        name_rev = str_replace_all(name_rev,"Cchhs","CCHHS"))

# scc_vaccine_providers <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/CCDPH-vaccine-mgmt/layers/SCC_covid_vaccine_providers_20210930.shp") %>%
#   select(-name) %>%
#   rename(objectid = OBJECTID) %>%
#   mutate(pharm = str_replace_all(pharm,"Sam's","Sams"))

# scc_vaccine_providers_geo <- scc_vaccine_providers %>% 
#   left_join (scc_vaccine_providers_name,by="objectid") 

# st_write(scc_vaccine_providers_geo, "layers/scc_providers.shp", append=FALSE)

# cc_mcds <- st_read("layers/cc_mcds.shp") %>%
#   st_transform(crs=3435) %>%
#   select(mcd = NAME) %>%
#   mutate(mcd_loc = case_when(mcd=="Chicago" ~ "Chicago",
#                               mcd=="Oak Park" ~ "Outside",
#                               mcd=="Evanston" ~ "Outside",
#                               mcd=="Stickney" ~ "Outside",
#                               mcd=="Niles" ~ "CCDPH"),
#          mcd_loc = replace_na(mcd_loc,"CCDPH"))
# 
# st_write(cc_mcds, "layers/cc_mcds.shp", append=FALSE)

scc_providers <- st_read("layers/scc_providers.shp")
cc_districts <- st_read("layers/cc_districts.shp") %>%
  select(-sqmi)
cc_mcds <- st_read("layers/cc_mcds.shp")

scc_providers_districts <- scc_providers %>% 
  st_join(cc_districts) %>%
  st_join(cc_mcds)

st_write(scc_providers_districts,"layers/scc_providers_latest.shp", append=FALSE)

# zip("layers/Disparity_counts.zip",files=c("layers/Disparity_counts.shp","layers/Disparity_counts.dbf","layers/Disparity_counts.shx","layers/Disparity_counts.prj"))
shell.exec(file.path(paste0(getwd(), "/zipfiles.bat")))

# SVI using 2010 census tracts
ccdph_svi_tracts <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph_cha/layers/ccdph_svi_tracts_districts_sum.shp")
ccdph_svi_tracts <- ccdph_svi_tracts %>% st_transform(crs=3435)
ccdph_svi_tracts <- ccdph_svi_tracts

scc_providers <- scc_providers_districts %>% 
  bind_cols(as.data.frame(st_coordinates(scc_providers))) %>%
  mutate(unique_xy = paste0(X,";",Y))

scc_providers_unique <- scc_providers %>%
  group_by(unique_xy) %>%
  summarise(count = n(),
            max_id = as.integer(max(objectid)))

scc_providers <- scc_providers %>% 
  right_join(scc_providers_unique %>% st_drop_geometry(),by="unique_xy")

rm(scc_providers_unique)
  
st_write(scc_providers,"layers/scc_providers_latest.shp", append=FALSE)

case_when("3250 N Arlington Heights Rd Suite 300")
  