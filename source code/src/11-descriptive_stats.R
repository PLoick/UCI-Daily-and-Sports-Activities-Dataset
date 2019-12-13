

library(ggplot2)

daily_sport_activity_data %>%
  as_tibble() %>%
  left_join(activity_descriptions) %>%
  filter(participant == "p1" &
           activity_desc %in% c(
     "walking in a parking lot"
    ,"rowing"
    ,"lying on back"
    ,"moving in an elevator"
    )) %>%
  select(activity_desc, RA_xacc, RA_yacc, RA_zacc) %>%
  group_by(activity_desc) %>%
  mutate(x = row_number()) %>%
  ungroup() %>%
  tidyr::gather("sensor", "value", -c(activity_desc, x)) %>%
  ggplot(aes(x = x, y = value, colour = sensor)) +
  facet_wrap(~activity_desc) +
  geom_line() +
  ggtitle("Right Arm Acceleration for four different activities")
