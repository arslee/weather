#---------------[   Purpose    ]--------------------
#
# Calculate population density for each PRISM grid cell
#
#---------------[ Pinned Notes ]--------------------
#
#
#---------------[   Process    ]--------------------

# preamble ----------------------------------------------------------------

source("Code/00_Preamble.R")


# average population estimates between years for each pop grid cell------------
pop <- list.files(dir$pop,full.names = T, recursive = T, pattern = ".tif$") %>% 
  lapply(raster) %>% 
  stack() %>% 
  crop(st_transform(S$county,crs(.)))

pop <- calc(pop,function(x){mean(x,na.rm=T)})



# load PRISM grid cell and polygonize it ----------------------------------
#--- download and load one PRISM data ---#
prism_set_dl_dir(dir$prism)
get_prism_dailys(
    type = "ppt", dates = Sys.Date()-30,
    keepZip = FALSE
  )

prism_file <- dir$prism %>% 
  list.files(full.names = T, recursive = T, pattern = ".bil$") %>% 
  .[1]
prism_R <- raster(prism_file)
prism_P <- rasterToPolygons(prism_R)
prism_P <- st_as_sf(prism_P)
#--- clear PRISM folder ---#
unlink(dir$prism)

# calculate population density for each PRISM griddcell  ------------------
prism_P$pop <- exactextractr::exact_extract(
  pop, prism_P,
  function(value, coverage_fraction) {
    sum(value*coverage_fraction, na.rm=T)
  }
)

# rasterize  -------------------------------------
pop_R <- fasterize(prism_P, prism_R, "pop")
names(pop_R) <- "pop"

# export pop weight grid -------------------------------------
writeRaster(pop_R,paste0(dir$pop, "PRISM_grid_pop.tif"), overwrite = T)



