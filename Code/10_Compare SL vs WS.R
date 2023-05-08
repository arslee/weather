#---------------[   Purpose    ]--------------------
#
# Compare with WS using daily and monthly data
#
#---------------[ Pinned Notes ]--------------------
# WS: 1950-2019
#---------------[   Process    ]--------------------

source("Code/00_Preamble.R")

# randomly draw 100 counties and 100 dates --------------------------------

random_counties <- S$county %>%
  pull(GEOID) %>%
  as.integer() %>%
  sample(300, replace = F)

avail_days <- seq.Date(as.Date("1981-01-01"), as.Date("2019-12-01"), by = "day")
random_days <- avail_days[sample(1:length(avail_days), 300, replace = F)] %>%
  str_remove_all("-")

random_yms <- seq.Date(as.Date("1981-01-01"), as.Date("2019-12-01"), by = "month") %>%
  str_remove_all("-") %>%
  str_sub(1, 6) %>%
  sample(100)

# prep SL -----------------------------------------------------------------

df_SL <- map(
  .progress = T,
  random_yms,
  function(ym) {
    df <- fread(paste0("http://files.asmith.ucdavis.edu/weather/daily/county_cropland/", ym, ".csv"))
    df[sample(1:nrow(df), floor(nrow(df) / 10)), .SD, .SDcols = patterns("date|fips|dday_a|tm|tav|ppt")]
    # df
  }
) %>% rbindlist()

df_SL <- df_SL %>%
  melt(id = c("fips", "date"), value.name = "SL")


# prep WS -----------------------------------------------------------------
rd_fips <- paste(unique(df_SL$fips), collapse = "|")
rd_dates <- unique(df_SL$date)

plan(multisession, workers = 10)
df_WS <- list.files("Data/Misc/daily1950_2019/",
  full.names = T
) %>%
  future_map(.progress = T, function(x) {
    print(x)
    df <- read_dta(x) %>%
      data.table() %>%
      .[str_remove_all(dateNum, "-") %in% rd_dates] %>%
      .[, date := str_remove_all(dateNum, "-")] %>%
      .[, dateNum := NULL]

    setnames(df,  c("prec", "tMin", "tMax", "tAvg"),  c("ppt", "tmin", "tmax", "tavg"))
    setnames(df, names(df), str_replace(names(df), "dday", "dday_a"))

    df
  }) %>%
  rbindlist() %>%
  melt(id = c("fips", "date"), value.name = "WS")

# using daily data -------------------------------------------------------------------
df_WS$variable <- as.character(df_WS$variable)
df_SL$variable <- as.character(df_SL$variable)
df_SL$date <- as.character(df_SL$date)

df_plot <- df_WS[df_SL, on = c("fips", "date", "variable")]


P <- df_plot %>%
  ggplot(aes(x = WS, y = SL)) +
  geom_point(color = alpha("black", .2)) +
  geom_smooth(method = "lm", formula = "y~x", lwd = 2, color = "red") +
  stat_regline_equation(
    aes(label = paste(..adj.rr.label..)),
    formula = "y~x"
  ) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  theme_classic(base_size = 10) +
  theme(
    axis.text = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 30, face = "bold"),
    strip.text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 40)
  ) +
  ggtitle("WS vs SL: Daily Weather Data")

png(paste0(dir$fig, "/WS_SL_daily.png"), width = 20, height = 10, res = 100, units = "in")
print(P)
dev.off()

# using monthly data ------------------------------------------------------

df_month <- df_plot[, .(SL = mean(SL), WS = mean(WS)), .(fips, variable, month = str_sub(date, 1, 6))]

P <- df_month %>%
  ggplot(aes(x = WS, y = SL)) +
  geom_point(color = alpha("black", .4)) +
  geom_smooth(method = "lm", formula = "y~x", lwd = 2, color = "red") +
  stat_regline_equation(
    aes(label = paste(..adj.rr.label..)),
    formula = "y~x"
  ) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  theme_classic(base_size = 10) +
  theme(
    axis.text = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 30, face = "bold"),
    strip.text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 40)
  ) +
  ggtitle("WS vs SL: Monthly Weather Data")

png(paste0(dir$fig, "/WS_SL_monthly.png"), width = 20, height = 10, res = 100, units = "in")
print(P)
dev.off()

