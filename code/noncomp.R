#kable(non_comp_reason, digits = 3)
#write_csv(non_comp_reason, path = paste("outputs/Fig",item,".csv",sep=""))
fig = data() %>%
  group_by(kill_month, kill_year) %>% 
  summarise(`Non-compliant` = mean(!comp),
            pH = mean(!phcomp),
            `Rib fat` = mean(!rfcomp)) %>% 
  gather(key = key, value = value, -kill_month, -kill_year) %>% 
  ggplot(aes(x = interaction(kill_month, kill_year),
             y = value, group = key, colour = key)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Non-compliance",
       x = "",
       y = "Percent of graded carcases",
       colour = "",
       caption = "Note: Carcases can be recorded as not meeting specifications for multiple attributes.") + 
  coord_cartesian(ylim = c(0, 0.15)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = c(.85, .9),
        axis.text.x = element_text(angle = 90, hjust = 0))
