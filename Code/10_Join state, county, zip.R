#---------------[   Purpose    ]--------------------
#
# Create a table of state, county, and zipcode
#---------------[ Pinned Notes ]--------------------
#
# Use centroids of zip codes
#
#---------------[   Process    ]--------------------
setwd("/home/shlee101/weather")
source("Code/00_Preamble.R")

state <- states() %>%
  mutate(st_code = as.integer(STATEFP), st_abb = STUSPS, st_name = NAME) %>%
  select(st_code, st_abb, st_name)

county <- S$county %>%
  mutate(
    st_code = as.integer(STATEFP),
    county_name = NAME
  ) %>%
  left_join(st_drop_geometry(state), by = "st_code") %>%
  select(st_name, st_abb, st_code, county_name, fips = id)

zip_2019 <- zctas(year = 2019) %>%
  st_centroid() %>%
  mutate(zipcode = as.integer(ZCTA5CE10)) %>%
  select(zipcode)

df_scz <- st_join(zip_2019, county) %>%
  st_drop_geometry() %>%
  select(
    st_abb, st_name,
    st_code,
    county_name, fips,
    zipcode
  ) %>%
  na.omit() %>%
  data.table()

setorder(df_scz, "st_name", "county_name", "zipcode")

fwrite(df_scz, "Release/misc/state_county_zipcode.csv")
