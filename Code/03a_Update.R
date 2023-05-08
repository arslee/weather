#---------------[   Purpose    ]--------------------
#
# construct PRISM data by Zip code, county, and state
#
#---------------[ Pinned Notes ]--------------------
# This code will be executed 2nd of every month with cronjob
#---------------[   Process    ]--------------------


# preamble ----------------------------------------------------------------
setwd("/home/shlee101/weather")
source("Code/00_Preamble.R")

#--- clear the PRISM folder ---#
unlink(list.files(dir$prism, full.names = T), recursive = T)

#--- specify ym to process ---#
ym_starts <- seq(
  ym(str_sub(today(), 1, 7)) %m-% months(8),
  ym(str_sub(today(), 1, 7)) %m-% months(1),
  by = "month"
)
ym_ends <- (ym_starts %m+% months(1)) - 1


M <- data.table(
  start = ym_starts,
  end = ym_ends
)

# plan(multisession, workers = 5)
for (ym in 1:nrow(M)) {
  ## Download PRISM data -----------------------------------------------------
  grid <- expand_grid(
    v = c("tmin", "tmax", "ppt"),
    d = seq(M[ym, ]$"start", M[ym, ]$"end", by = "day")
  )

  pmap(
    .progress = T,
    grid,
    function(v, d) {
      prism_set_dl_dir(dir$prism)
      get_prism_dailys(
        type = v, dates = d,
        keepZip = FALSE
      )
    }
  )


  ## Load prism rasters -----------------------------------------------------
  files_prism <-
    list.files(dir$prism, recursive = T, pattern = ".bil$", full.names = T)
  R <- lapply(files_prism, raster)
  names(R) <- files_prism

  #--- Prep grid ---#
  grid <- expand_grid(
    r = names(R),
    s = names(S),
    w = c(NA, names(W))
  ) %>%
    data.table()

  grid <- setdiff(grid, grid[s == "zip" & (w == "cropland" | w == "population")])

  ## Construct tmin, tmax, ppt, and tavg -------------------------------------
  df <- pmap(.progress = T, grid, zone_stat) %>%
    rbindlist() %>%
    spread(var, value) %>%
    data.table() %>%
    .[!is.na(tmin) & !is.na(tmax)]
  df[, tavg := (tmin + tmax) / 2]

  ## Do some minor clean -------------------------------------
  cols <- c("tmin", "tmax", "tavg", "ppt")
  df[, paste0(cols) := lapply(.SD, function(x) round(x, digits = 3)), .SDcols = cols]
  setnames(df, "s", "agg_level")

  ## Construct degree days -------------------------------------
  df_dday_a <- map_dfc(.progress = T, lower_thresholds, function(thr) {
    dday <- data.table(v = pmap_dbl(
      list(df$tmin, df$tmax),
      function(tmin, tmax) {
        degree_days(tmin, tmax, thr, 100) %>%
          round(digits = 3)
      }
    ))
    setnames(dday, "v", paste0("dday_a", thr, "C"))
  })

  df_dday_b <- map_dfc(.progress = T, upper_thresholds, function(thr) {
    dday <- data.table(v = pmap_dbl(
      list(df$tmin, df$tmax),
      function(tmin, tmax) {
        degree_days(tmin, tmax, tmin, max(thr + 0.000001, tmin + 0.000001)) %>%
          round(digits = 3)
      }
    ))
    setnames(dday, "v", paste0("dday_b", thr, "C"))
  })

  df <- cbind(df, df_dday_a, df_dday_b)
  ## Export -------------------------------------
  AW <- df[, .(agg_level, weight)] %>% unique()
  file <- paste0(str_sub(str_remove(M[ym, ]$start, "-"), 1, 6), ".csv")

  for (r in 1:nrow(AW)) {
    a <- AW[r, ]$agg_level
    w <- AW[r, ]$w
    path_out <- paste0("Release/daily/", a, "_", str_remove(w, "_"), "/", file)

    out <- copy(df) %>%
      .[agg_level == a & weight == w] %>%
      .[, `:=`(agg_level = NULL, weight = NULL)]

    if (a == "county") {
      setnames(out, "id", "fips")
    } else {
      setnames(out, "id", "zipcode")
    }
    fwrite(out, file = path_out)
  }

  #--- clear ---#
  unlink(list.files(dir$prism, full.names = T), recursive = T)
  rm(R)
  rm(df)
  rm(out)
}


# aggregate daily county-level data to state level -------------------------------------------------------------

## prep data for weights -------------------------------------------------------------

df_weight <- data.table(
  st_abb = S$county$STUSPS,
  st_code = as.integer(S$county$STATEFP),
  fips = as.integer(S$county$GEOID),
  noweight = S$county$ALAND,
  cropland = exactextractr::exact_extract(
    W$cropland, S$county,
    function(value, coverage_fraction) {
      sum(value * coverage_fraction, na.rm = T)
    }
  ),
  population = exactextractr::exact_extract(
    W$pop, S$county,
    function(value, coverage_fraction) {
      sum(value * coverage_fraction, na.rm = T)
    }
  )
)


files_in <- list.files("Release/", recursive = T, full.names = T) %>%
  grep("daily", ., value = T) %>%
  grep("county", ., value = T)

# plan(multisession, workers = 5)
map(
  .progress = T,
  files_in,
  function(f) {
    library(data.table)
    library(tidyverse)
    #--- load daily county-level data ---#
    df_fips <- fread(f) %>%
      melt(id.vars = c("fips", "stability", "date"))

    #--- extract weighting scheme ---#
    ww <- str_remove(basename(dirname(f)), "county_")

    #--- aggregate to state level ---#
    df_state_daily <- df_weight[, c("st_abb", "st_code", "fips", ww), with = F][df_fips, on = "fips"][
      ,
      .(value = round(weighted.mean(value, get(ww)), digits = 3)),
      .(st_abb, st_code, stability, date, variable)
    ]

    #--- long to wide ---#
    df_state_daily_out <- df_state_daily %>%
      dcast(
        paste0("st_abb+st_code+date+stability~variable"),
        value.var = "value"
      )

    #--- export ---#
    fwrite(df_state_daily_out,
      file =
        str_replace(f, "county", "state")
    )
  }
)

# rm(w, yr)

# aggregate daily to monthly ---------------------------------------------------------------------
files_in <- list.files("Release/daily", recursive = T, full.names = T)

map(.progress = T, files_in, function(f) {
  #--- id variables by admin unit ---#
  if (str_detect(f, "state")) {
    id_vars <- c("st_abb", "st_code", "ym")
  } else if (str_detect(f, "county")) {
    id_vars <- c("fips", "ym")
  } else {
    id_vars <- c("zipcode", "ym")
  }

  #--- variable to compute average of (otherwise sum) ---#
  v_avg <- c("tmin", "tmax", "tavg")

  #--- load daily data and reshape to long ---#
  d <- fread(f) %>%
    .[, ym := str_sub(date, 1, 6)] %>%
    .[, date := NULL] %>%
    .[, stability := NULL] %>%
    melt(id.vars = id_vars)

  #--- compute monthly avg or sum ---#
  out <- rbind(
    d[variable %in% v_avg, .(value = round(mean(value), digits = 3)), c("variable", id_vars)],
    d[!variable %in% v_avg, .(value = round(sum(value), digits = 3)), c("variable", id_vars)]
  ) %>%
    dcast(
      paste0(paste(id_vars, collapse = "+"), "~variable"),
      value.var = "value"
    )

  #--- export ---#
  fwrite(out, file = str_replace(f, "daily", "monthly"))
})


# append state and county info columns ------------------------------------

#--- IDs for county-level data ---#
df_c <- S$county %>%
  data.table() %>%
  .[, .(
    st_abb = STUSPS,
    # st_name=STATE_NAME,
    st_code = as.integer(STATEFP),
    county_name = NAME,
    fips = id
  )]

#--- IDs for Zip code-level data ---#
df_z <- fread("http://files.asmith.ucdavis.edu/weather/misc/state_county_zipcode.csv") %>%
  .[, .(st_abb, st_code, county_name, fips, zipcode)]

#--- load all county and Zip code level data ---#
files <- list.files("Release/", recursive = T, full.names = T) %>%
  grep("county_|zip_", ., value = T)

#--- loop over files ---#
map(files, function(f) {
  df <- fread(f)

  if (str_detect(f, "county")) {
    out <- df_c[df, on = "fips"]
  } else {
    out <- df_z[df, on = "zipcode"]
  }

  fwrite(out, file = f)
})
