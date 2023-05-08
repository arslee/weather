#---------------[   Purpose    ]--------------------
#
# Compute the average cropland fraction for PRISM grid cell
#
#---------------[ Pinned Notes ]--------------------
#
#---------------[   Process    ]--------------------


# preamble ----------------------------------------------------------------
source("Code/00_Preamble.R")

#--- NLCD zip files ---#
zips <- "Data/Raw/NLCD_landcover_2019_release_all_files_20210604/" %>%
  list.files(full.names = T, recursive = T, pattern="land_cover")

#--- load PRISM grid and polygonize it ---#
prism_R <- raster(paste0(dir$weight,"/PRISM_grid_pop.tif"))
prism_P <- rasterToPolygons(prism_R)
prism_P <- st_as_sf(prism_P)

# compute yearly cropland fraction ----------------------------------------

lapply(zips, function(zip) {

  #--- clear temporary folder to unzip one year NLCD landcover ---#
  unlink(
    list.files("Data/Raw/nlcd/", full.names = T)
  )
  
  #--- unzip one year NLCD landcover ---#
  unzip(zip, exdir = "Data/Raw/nlcd")

  #--- NLCD landcover ---#
  nlcd <- raster(
    paste0(
      "Data/Raw/nlcd/",
      str_replace(basename(zip), ".zip", ".img")
    )
  )

  #--- compute fraction of cultivated crop and pasture ---#
  prism_P$crop_pasture <- exactextractr::exact_extract(
    nlcd, prism_P,
    function(value, coverage_fraction) {
      sum(coverage_fraction[value %in% c(81, 82)]) / sum(coverage_fraction)
    }
  )

  #--- rasterize ---#
  crop_R <- fasterize(prism_P, prism_R, "crop_pasture")
  names(crop_R) <- "PRISM_grid_cropland"

  #--- export ---#
  year <- str_extract(basename(zip), "[0-9]{4}")
  crop_R <- fasterize(prism_P, prism_R, "crop_pasture")
  names(crop_R) <- paste0("cropland", year)

  writeRaster(crop_R, paste0(dir$weight, "PRISM_grid_", names(crop_R), ".tif"), overwrite = T)
})


# average all available years ---------------------------------------------
#--- stack available annual cropland weighting grids ---#
cropland_S <- list.files(dir$weight, pattern = "cropland[0-9]{4}", full.names = T) %>%
  lapply(raster) %>%
  stack()

#--- compute mean ---#
cropland_R <- calc(cropland_S, function(x) {
  mean(x, na.rm = T)
})

#--- export ---#
names(cropland_R) <- "cropland"
writeRaster(cropland_R, paste0(dir$weight, "PRISM_grid_", names(cropland_R), ".tif"), overwrite = T)


