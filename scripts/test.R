library(sf)
ccdph_districts_boundaries_muni_epsg3435 <- st_read("E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/epsg-3435-illinois-stateplane-east/ccdph_districts_boundaries_muni_epsg3435.shp")
ccdph_districts_boundaries_muni_epsg4326 <- ccdph_districts_boundaries_muni_epsg3435 %>% st_transform(crs=4326)
st_write(ccdph_districts_boundaries_muni_epsg4326, "E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/epsg-4326-wgs84-for-leaflet/ccdph_districts_boundaries_muni_epsg4326.shp")


IL_ZCTAs_2019_geom <- zctas(cb=TRUE, class="sf", year="2019")
IL_ZCTAs_2010_geom <- zctas(cb=TRUE, class="sf", year="2010")

grouplist <- c("B07001")
yearlist <- c(2018)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "county (or part):031",
                           regionin = "zip code tabulation area:*", 
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    head(acs_group)
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("NAME_1"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group$year<-ayear 
    # acs_group$GEOID<-paste(state,county,tract,sep="")
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
  }
}

cc_zctas <- read.csv("https://raw.githubusercontent.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/main/cook-county-zip-codes.csv")

cc_zctas_unique_epsg4326 <- cc_zctas %>%
  mutate(zip_code=as.character(zip_code),
         zcta=as.character(zcta)) %>%
  left_join(IL_ZCTAs_2010_geom, by=c("zcta"="ZCTA5")) %>%
  st_as_sf() %>%
  st_transform(crs=4326) %>%
  distinct(zcta, .keep_all = TRUE)

cc_zctas_unique_epsg3435 <- cc_zctas_unique_epsg4326 %>%
  st_transform(crs=3435)

st_write(cc_zctas_unique_epsg4326, "E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/epsg-4326-wgs84-for-leaflet/cook_county_zctas_epsg4326.shp")
st_write(cc_zctas_unique_epsg4326, "E:/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-shapefiles/epsg-3435-illinois-stateplane-east/cook_county_zctas_epsg3435.shp")

plot(cc_zctas_unique_epsg3435['zcta'])

test <- cc_zctas %>%
  distinct(zcta,.keep_all = TRUE)

yearlist <- c(2010)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "place:*", # tracts
                           regionin="state:17", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group$year<-ayear 
    acs_group$GEOID_place<-paste0(state,place)
    assign(paste(agroup,"place",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}