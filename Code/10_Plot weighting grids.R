#---------------[   Purpose    ]--------------------
#
# Plot weighting grids based on cropland and population
#
#---------------[ Pinned Notes ]--------------------
#
#---------------[   Process    ]--------------------

source("Code/00_Preamble.R")

# cropland ----------------------------------------------------------------

R <- raster(paste0(dir$weight, "/PRISM_grid_cropland.tif"))
P <- tm_shape(R) +
  tm_raster(
    palette = brewer.greens(11)[2:11], title = "",
    n = 10, legend.show = T
  ) +
  tm_scale_bar(
    width = 0.25, text.size = 1, text.color = "black",
    color.dar = "lightsteelblue4", color.light = "white",
    position = c("left", "bottom"), lwd = 1
  ) +
  tm_compass(type = "radar", position = c("right", "bottom")) +
  tm_graticules(
    ticks = T, lines = T, col = "azure3", lwd = 1,
    labels.rot = c(15, 15), labels.size = 1
  ) +
  tm_layout(
    scale = 1,
    main.title = "Fraction of Cropland in PRISM Gridcell",
    main.title.position = "center",
    main.title.color = "black",
    main.title.fontface = "bold",
    legend.position = c("right", "bottom"),
    legend.bg.color = "grey90",
    legend.bg.alpha = .2,
    legend.frame = "gray50",
    legend.outside = F,
    legend.width = .5,
    legend.height = 1.2
  )


tmap_save(P, paste0(dir$fig, "/PRISM_cropland.png"), height = 7, dpi = 1000)


# population --------------------------------------------------------------


R <- raster(paste0(dir$weight, "/PRISM_grid_pop.tif"))
P <- tm_shape(log(R + 1)) +
  tm_raster(
    palette = cividis(14), title = "",
    n = 14, legend.show = T
  ) +
  tm_scale_bar(
    width = 0.25, text.size = 1, text.color = "black",
    color.dar = "lightsteelblue4", color.light = "white",
    position = c("left", "bottom"), lwd = 1
  ) +
  tm_compass(type = "radar", position = c("right", "bottom")) +
  tm_graticules(
    ticks = T, lines = T, col = "azure3", lwd = 1,
    labels.rot = c(15, 15), labels.size = 1
  ) +
  tm_layout(
    scale = 1,
    main.title = "Log(Population+1) in PRISM Gridcell",
    main.title.position = "center",
    main.title.color = "black",
    main.title.fontface = "bold",
    legend.position = c("right", "bottom"),
    legend.bg.color = "grey90",
    legend.bg.alpha = .2,
    legend.frame = "gray50",
    legend.outside = F,
    legend.width = .5,
    legend.height = 1.2
  )


tmap_save(P, paste0(dir$fig, "/PRISM_pop_temp.png"), height = 7, dpi = 1000)
