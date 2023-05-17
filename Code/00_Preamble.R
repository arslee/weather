rm(list=ls())
pacman::p_load(tidyverse, data.table, lubridate, rvest, prism, raster, furrr, pals, tmap, haven, ggpubr, sf, tigris, ggtext, fasterize, prism)
select <- dplyr::select

## Set directory -----------------------------------------------------------
dir <- list()
dir$prism <- "Data/Raw/PRISM/"
dir$weight <- "Data/Weight/"
dir$fig <- "Figure/"
dir$pop <- "Data/Raw/gpw-v4-population-density/"

## Set temperature thresholds -----------------------------------------------------------
lower_thresholds <- c(0, 5, 8, 10, 12, 15, 20, 25, 29, 30, 31, 32, 33, 34)
new_lower_thresholds <- c(0, 5, 8, 10, 12, 15, 18, 20, 21, 25, 29, 30, 31, 32, 33, 34)
upper_thresholds <- seq(15,23,3)

## function -----------------------------------------------------------
#--- function to calculate degree days ---#
degree_days <- function(T_min, T_max, LDT = NA, UDT = NA, method = "single.sine") {
  stopifnot(T_max >= T_min, method %in% c(
    "single.sine","double.sine", "single.triangulation", "double.triangulation"
  ))
  alpha <- (T_max - T_min) / 2
  dd <- 0
  if (method == "single.sine") {
    if (T_min >= UDT && T_max >= UDT) {
      dd <- (T_max - T_min)
    } else if (T_min >= LDT && T_max >= UDT) {
      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      dd <- 1 / pi * (((T_max + T_min) / 2 - LDT) * (theta2 + pi / 2) + (UDT - LDT) * (pi / 2 - theta2) - alpha *
        cos(theta2))
    } else if (T_min <= LDT && T_max >= UDT) {
      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      theta1 <- asin((LDT - (T_max + T_min) / 2) / alpha)
      dd <- 1 / pi * ((((T_max + T_min) / 2) - LDT) * (theta2 - theta1) + alpha * (cos(theta1) - cos(theta2)) + (UDT - LDT) * (pi / 2 - theta2))
    } else if (T_min >= LDT && T_max <= UDT) {
      dd <- ((T_max + T_min) / 2) - LDT
    } else if (T_min <= LDT && T_max >= LDT) {
      theta1 <- asin(pmax(-1, pmin(1, (LDT - (T_max + T_min) / 2) / alpha)))
      dd <- 1 / pi * ((((T_max + T_min) / 2) - LDT) * ((pi / 2) - theta1) + alpha * cos(theta1))
    } else if (T_min < LDT && T_max <= LDT) {
      dd <- 0
    }
  }
  if (method == "double.sine") {
    if (T_min >= LDT && T_max >= UDT) {
      dd <- (UDT - LDT) / 2
    } else if (T_min >= LDT && T_max >= UDT) {
      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      dd <- 1 / (2 * pi) * (((T_max + T_min) / 2 - LDT) * (theta2 +
        pi / 2) + (UDT - LDT) * (pi / 2 - theta2) - alpha *
        cos(theta2))
    } else if (T_min <= LDT && T_max >= UDT) {
      theta2 <- asin((UDT - (T_max + T_min) / 2) / alpha)
      theta1 <- asin((LDT - (T_max + T_min) / 2) / alpha)
      dd <- 1 / (2 * pi) * (((T_max + T_min) / 2 - LDT) * (theta2 -
        theta1) + alpha * (cos(theta1) - cos(theta2)) +
        (UDT - LDT) * (pi / 2 - theta2))
    } else if (T_min >= LDT && T_max <= UDT) {
      dd <- 0.5 * ((T_max + T_min) / 2 - LDT)
    } else if (T_min <= LDT && T_max >= LDT) {
      theta1 <- asin(pmax(-1, pmin(1, (LDT - (T_max + T_min) / 2) / alpha)))
      dd <- 1 / (2 * pi) * (((T_max + T_min) / 2 - LDT) * (pi / 2 -
        theta1) + alpha * cos(theta1))
    } else if (T_min <= LDT && T_max <= LDT) {
      dd <- 0
    }
    dd <- dd * 2
  }
  if (method == "single.triangulation") {
    MT <- (T_max + T_min) / 2
    if (T_min >= UDT && T_max >= UDT) {
      dd <- (UDT - LDT)
    } else if (T_min >= LDT && T_max >= UDT) {
      dd <- (MT - LDT) - ((T_max - UDT)^2 / ((T_max - T_min) *
        2))
    } else if (T_min <= LDT && T_max >= UDT) {
      dd <- ((T_max - LDT)^2 - (T_max - UDT)^2) / ((T_max -
        T_min) * 2)
    } else if (T_min >= LDT && T_max <= UDT) {
      dd <- MT - LDT
    } else if (T_min <= LDT && T_max >= LDT) {
      dd <- (T_max - LDT)^2 / ((T_max - T_min) * 2)
    } else if (T_min <= LDT && T_max <= LDT) {
      dd <- 0
    }
  }
  if (method == "double.triangulation") {
    MT <- (T_max + T_min) / 2
    if (T_min >= UDT && T_max >= UDT) {
      dd <- (UDT - LDT) / 2
    } else if (T_min >= LDT && T_max >= UDT) {
      dd <- (MT - LDT) - ((T_max - UDT)^2 / ((T_max - T_min) *
        4))
    } else if (T_min <= LDT && T_max >= UDT) {
      dd <- ((T_max - LDT)^2 - (T_max - UDT)^2) / ((T_max -
        T_min) * 4)
    } else if (T_min >= LDT && T_max <= UDT) {
      dd <- (MT / 4) - (LDT / 2)
    } else if (T_min <= LDT && T_max >= LDT) {
      dd <- (T_max - LDT)^2 / ((T_max - T_min) * 4)
    } else if (T_min <= LDT && T_max <= LDT) {
      dd <- 0
    }
    dd <- dd * 2
  }
  return(round(dd, 7))
}



#--- function to do zonal statistics ---#
zone_stat <- function(r, s, w) {
  paste(basename(r), s, w) %>% print()
  if (is.na(w)) {
    df <- data.table(
      s,
      var = str_extract(basename(r), "tmin|tmax|ppt"),
      id = S[[s]]$id,
      stability = str_extract(basename(r), "provisional|early|stable"),
      weight = "no_weight",
      date = str_extract(basename(r), "[0-9]{8}"),
      value = exactextractr::exact_extract(R[[r]], S[[s]], fun = "mean")
    )
    df
  } else {
    df <- data.table(
      s,
      var = str_extract(basename(r), "tmin|tmax|ppt"),
      id = S[[s]]$id,
      stability = str_extract(basename(r), "provisional|early|stable"),
      weight = w,
      date = str_extract(basename(r), "[0-9]{8}"),
      value = exactextractr::exact_extract(R[[r]], S[[s]], weights = W[[w]], fun = "weighted_mean")
      # prism_file = basename(r),
    )
    df
  }
}

# Load shape files --------------------------------------------------------

S <- list(
  county =
    tigris::counties(cb = T, year = 2020) %>%
    filter(!STATEFP %in% c("02", "72", "78", "15", "66", "60", "69", "11")) %>%
    mutate(id = as.integer(GEOID)),
  zip = tigris::zctas(cb = T, year = 2019) %>%
    mutate(id = as.integer(ZCTA5CE10))
)


# Load weighting grids  ---------------------------------------------------

W <- list(
  population =
    raster(paste0(dir$weight, "/PRISM_grid_pop.tif")),
  cropland =
    raster(paste0(dir$weight, "/PRISM_grid_cropland.tif"))
)


# Theme for map ---------------------------------------------------------
theme_map <- function(...) {
  theme_minimal() +
    theme(
      strip.background = element_rect(colour = "black", fill = "#f5f5f2"),
      strip.text = element_text(
        size = 12,
        face = "bold",
        margin = margin(3, 3, 3, 3)
      ),
      plot.title = element_text(size = 14, face="bold", hjust=.5),
      legend.text = element_text(size = 9, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(), ...
    )
}

# download function -------------------------------------------------------

download_prism <- function(states, # e.g., c("CA","OR")
                           years, # e.g., 2000:2020
                           months, # e.g., 1:12
                           temporalUnit, # one of "daily", "monthly"
                           spatialUnit, # one of "state", "county","zip"
                           weighting, # one of "noweight", "cropland", "population"
                           variables) {
  #---ID columns---#
  IDs <- list(
    zip = c("st_abb", "st_code", "county_name", "fips", "zipcode"),
    county = c("st_abb", "st_code", "county_name", "fips"),
    state = c("st_abb", "st_code"),
    daily = c("date", "stability"),
    monthly = c("ym")
  )
  
  #--- yyyymm to include  ---#
  df_ym <- expand_grid(
    y = years,
    m = str_pad(months, side = "left", pad = "0", width = 2)
  ) %>%
    filter(as.integer(paste0(y, m)) < as.integer(str_sub(str_remove(today(), "-"), 1, 6)))
  
  
  #--- URLs to include  ---#
  URLs <- paste0(
    "http://files.asmith.ucdavis.edu/weather/",
    temporalUnit, "/",
    spatialUnit, "_",
    weighting, "/",
    df_ym$y, df_ym$m,
    ".csv"
  )
  
  #--- pull data ---#
  map(URLs, function(url) {
    fread(url)[st_abb %in% states, c(IDs[[spatialUnit]], IDs[[temporalUnit]], variables), with = F]
  }) %>%
    rbindlist()
}
