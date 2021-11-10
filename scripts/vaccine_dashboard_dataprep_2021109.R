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
library(tidyverse)
library(sf)
library(utils)
library(readxl)
library(units)

# Format municipalities ----------------------------------------------------
# reformat population by municipality table to wide format

ccdph_munis_svi <- read_excel("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph_scchealthsurvey/data/health_survey_grouping_data_by_muni_20210928.xlsx", sheet=1) %>%
  mutate(
    name=str_to_upper(muni_name)
  ) %>%
  select(name,
         svi)

# cc_munis_pop_2020_wide <- read_csv("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-data-sets/2020/decennial-2020-race-ethnicity-by-muni-cleaned.csv") %>%
#   select(-census_name) %>%
#   pivot_wider(names_from=race_ethnicity, values_from=population) %>%
#   rename(
#     name=clean_name,
#     hisp=3,
#     nh_asian=4,
#     nh_black=5,
#     nh_white=8,
#     nh_other=7,
#     nh_multi=6) %>%
#   mutate(total = hisp + nh_asian + nh_black + nh_white + nh_multi + nh_other) %>%
#   select(name,
#          hisp:total)

cc_munis <- st_read("layers/cc_munis.shp") %>%
  mutate(
    location=if_else(district=="OOJ","SCC",district),
    location=if_else(name=="Chicago","Chicago",location)
    ) %>%
  mutate(priority=case_when(name=="Burnham" ~ "YES",
                            name=="Calumet City" ~ "YES",
                            name=="Dixmoor" ~ "YES",
                            name=="Dolton" ~ "YES",
                            name=="Ford Heights" ~ "YES",
                            name=="Harvey" ~ "YES",
                            name=="Markham" ~ "YES",
                            name=="Phoenix" ~ "YES",
                            name=="Riverdale" ~ "YES",
                            name=="Robbins" ~ "YES",
                            name=="Sauk Village" ~ "YES",
                            name=="East Hazel Crest" ~ "YES",
                            name=="Justice" ~ "YES",
                            name=="Lansing" ~ "YES",
                            name=="Lemont" ~ "YES",
                            name=="Lynwood" ~ "YES",
                            name=="South Chicago Heights" ~ "YES",
                            name=="Steger" ~ "YES"),
         priority=replace_na(priority,"NO")) %>%
  mutate(
    name=str_to_upper(name),
    location=str_to_upper(location)
    )

cc_munis <- st_read("layers/cc_munis.shp") %>%
  left_join(ccdph_munis_svi, by="name")

st_write(cc_munis,"layers/cc_munis.shp", append=FALSE)

# Population by municipality csv
cc_munis_sub <- cc_munis %>%
  st_drop_geometry() %>%
  select(name,
         location,
         priority)

cc_munis_pop_2020 <- read_csv("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-data-sets/2020/decennial-2020-race-ethnicity-by-muni-cleaned.csv") %>%
  select(fips=census_place_code,
         name = clean_name,
         race = race_ethnicity,
         total = population) %>%
  mutate(name = str_to_upper(name)) %>%
  left_join(cc_munis_sub, by="name")

write_csv(cc_munis_pop_2020,"data/cc_munis.csv")

# format census tract layer -----------------------------------------------

ccdph_tracts_svi <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph_scchealthsurvey/layers/ccdph_svi_tracts.shp") %>%
  st_drop_geometry() %>%
  select(fips = fips_tract,
         svi = SVI_rnk)

cc_tracts <- st_read("layers/cc_tracts.shp") %>%
  select(fips,
         sqmi,
         nh,
         hisp,
         location = locatin,
         total = totl_pp,
         nh_asian = nh_asin,
         nh_black = nh_blck,
         nh_white=nh_whit,
         nh_multi=nh_twpl
         ) %>%
  mutate(nh_other=nh-nh_asian-nh_black-nh_white-nh_multi) %>%
  select(-nh) %>%
  left_join(ccdph_tracts_svi,by="fips")

st_write(cc_tracts,"layers/cc_tracts.shp", append = FALSE)

# District boundaries ----------------------------------------------
cc_districts %>% group_by(district) %>% summarise(n())

cc_districts <- cc_tracts_geo %>%
  group_by(location) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_as_sf() %>%
  mutate(sqmi = as.numeric(st_area(geometry))*0.00000003587006428 %>% set_units(mi^2))

cc_districts <- st_read("layers/cc_districts.shp")
st_write(cc_districts,"layers/cc_districts.shp", append=FALSE)

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
  