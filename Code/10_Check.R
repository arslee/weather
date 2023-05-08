grid <- expand_grid(
  t = c("daily", "monthly"),
  aw = c(
    "zip_noweight",
    "state_noweight",
    "state_cropland",
    "state_population",
    "county_noweight",
    "county_cropland",
    "county_population"
  ),
  ym = seq(as.Date("1981-01-01"), as.Date("2023-04-05"), by = "month") %>%
    str_remove_all("-") %>%
    str_sub(1, 6)
)

nrow(grid)
df_sum <- list.files("Release", recursive = T, full.names = T) %>% map(.progress = T, function(f) {
  df <- fread(f)
  data.table(
    file = f,
    colname = list(names(df)),
    df[, lapply(.SD, mean), .SDcols = c("tavg", "ppt", "dday_a10C", "dday_b18C")],
    ndate = length(unique(df$date)),
    nym = length(unique(df$ym)),
    nfips = length(unique(df$fips)),
    nzip = length(unique(df$zip)),
    nstate = length(unique(df$st_abb))
  )
})

df_plot <- df_sum %>%
  rbindlist() %>%
  mutate(
    t = str_extract(file, "daily|monthly"),
    s = str_extract(file, "zip|county|state"),
    w = str_extract(file, "noweight|cropland|population"),
    y = str_sub(basename(file), 1, 4) %>% as.integer(),
    m = str_sub(basename(file), 5, 6) %>% as.integer()
  )

df_plot[, .(tavg = mean(tavg)), .(s, t, w, m)] %>%
  ggplot(aes(x = m, y = tavg, color = s)) +
  geom_line() +
  facet_wrap(vars(t, w))
