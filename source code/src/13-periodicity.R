library(forecast)

# Median Periodicity across participants by activity
daily_sport_activity_data %>%
  as_tibble() %>%
  left_join(activity_descriptions) %>%
  select(activity_desc, participant, RL_xacc) %>%
  group_by(activity_desc, participant) %>%
  mutate(x = row_number()) %>%
  summarise(period = ts(RL_xacc) %>% findfrequency()) %>%
  group_by(activity_desc) %>%
  summarise(median_period = median(period) %>% as.integer())

# Time Series Decomposition
ts_data <-
  daily_sport_activity_data %>%
  as_tibble() %>%
  left_join(activity_descriptions) %>%
  select(activity_desc, participant, RL_xacc) %>%
  group_by(activity_desc, participant) %>%
  mutate(x = row_number()) %>%
  filter(activity_desc == "ascending stairs", participant == "p1") %>%
  ungroup() %>%
  select(RL_xacc) %>%
  unlist() %>%
  ts(frequency = findfrequency(.))

fit <- stl(ts_data, s.window="periodic")

autoplot(cbind(
  Data=ts_data,
  Seasonal=seasonal(fit),
  Trend=trendcycle(fit),
  Remainder=remainder(fit)),
  facets=TRUE) +
  ylab("") + xlab("Year")
