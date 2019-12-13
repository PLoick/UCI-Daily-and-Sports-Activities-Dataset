# Confirm that the total magnitude of a given accelerometer is 1g
daily_sport_activity_data %>%
  as_tibble() %>%
  filter(file_num == 23) %>%
  select(RA_xacc, RA_yacc, RA_zacc) %>%
  group_by_all() %>%
  mutate(RA = norm(c(RA_xacc, RA_yacc, RA_zacc), "2"))

library(ggplot2)
heat_map_data <-
  daily_sport_activity_data %>%
  as_tibble() %>%
  select(-c(file_num, participant, segment)) %>%
  group_by(activity) %>%
  summarise_all(mean) %>%
  tidyr::gather("sensor", "value", -c(activity)) %>%
  tidyr::separate(sensor, into = c("position", "axis_sensor"), sep = "_") %>%
  tidyr::separate(axis_sensor, into = c("axis", "sensor"), sep = 1) %>%
  mutate(axis = str_remove(axis, "_"))# %>%
  # group_by(position, axis, sensor) %>%
  # select(-activity) %>%
  # summarise_all(mean)

heat_map_data %>%
  filter(sensor == "gyro") %>%
ggplot(aes(axis, position)) +
  facet_wrap(~ activity) +
  geom_tile(aes(fill = value)) + 
  scale_fill_distiller(palette = "PRGn")

heat_map_data %>%
  filter(sensor == "acc") %>%
  ggplot(aes(axis, position)) +
  facet_wrap(~ activity) +
  geom_tile(aes(fill = value)) + 
  scale_fill_distiller(palette = "GnBu")

heat_map_data %>%
  filter(sensor == "mag") %>%
  ggplot(aes(axis, position)) +
  facet_wrap(~ activity) +
  geom_tile(aes(fill = value)) + 
  scale_fill_distiller(palette = "Purples")


