#---------------[   Purpose    ]--------------------
#
# visualize percent difference of ppt
#
#---------------[ Pinned Notes ]--------------------
#
#
#---------------[   Process    ]--------------------
source("Code/00_Preamble.R")
S$county$fips <- as.integer(S$county$GEOID)
df_scz <- fread("http://files.asmith.ucdavis.edu/weather/misc/state_county_zipcode.csv")
sb <- tigris::states(cb = T) %>%
  dplyr::filter(STUSPS %in% df_scz$st_abb)



# prep data ---------------------------------------------------------------

#--- randomly select 5 years ---#
set.seed(2023)
yrs <- sample(1981:2022, 5, replace = F)


#--- pull data ---#
grid <- expand_grid(
  t = c("daily", "monthly"),
  w = c("noweight", "cropland", "population")
)

df <- future_pmap(.progress = T, grid, function(t, w) {
  if (t == "daily") {
    download_prism(unique(df_scz$st_abb),
      years = yrs,
      1:12,
      t,
      "county",
      w
    ) %>%
      .[, .(fips, date, ppt, t, w)]
  } else {
    download_prism(unique(df_scz$st_abb),
      years = yrs,
      1:12,
      t,
      "county",
      w
    ) %>%
      .[, .(fips, date = ym, ppt, t, w)]
  }
}) %>% rbindlist()

#--- compute percent diff  ---#

df_plot <- df %>%
  melt(id.cols = c("fips", "date", "t", "w"), measure.vars = c("ppt")) %>%
  dcast("fips+date+variable+t ~ w", value.var = c("value")) %>%
  .[, .(
    `Cropland vs No Weight` = mean(abs(cropland - noweight) / (.5 * (abs(cropland + noweight) + 0.0000001)), na.rm = T) * 100,
    `Population vs No Weight` = mean(abs(population - noweight) / (.5 * (abs(population + noweight) + 0.0000001)), na.rm = T) * 100
  ), .(fips, t)] %>%
  melt(id.cols = c("t", "fips"), measure.vars = c("Cropland vs No Weight", "Population vs No Weight")) %>%
  .[!is.na(value)]

df_plot <- right_join(S$county, df_plot, by = c("fips"))

df_plot$q <-
  cut(df_plot$value, breaks = round(quantile(df_plot$value,
    probs = seq(0, 1, .1),
    na.rm = T
  ), digits = 1))

df_plot$q <- factor(
  df_plot$q,
  levels(df_plot$q),
  levels(df_plot$q) %>%
    str_remove_all("\\(") %>%
    str_replace(",", "%-") %>%
    str_replace("\\]", "%")
)

#--- plot ---#

fig <- ggplot(df_plot) +
  geom_sf(aes(fill = q), lwd = .1) +
  geom_sf(data = sb, fill = NA, color = "black") +
  facet_grid(str_to_title(t) ~ variable) +
  scale_fill_manual(name = "", values = rev(pals::brewer.rdylbu(10)), na.translate = FALSE) +
  ggtitle("Percent Difference of Precipitation b/w Weighting Schemes") +
  theme_map()


png("Figure/diff_rate_ppt.png", width = 8, height = 4, units = "in", res = 500)
print(fig)
dev.off()
