akd_long = akd %>% gather(key = "state", value = "total_count", -year)
pdat = akdstate %>% gather(key = "state", value = "count", -year) %>% 
  drop_na(count) %>% 
  filter(year %in% input$state_years) %>% 
  left_join(akd_long) %>% 
  mutate(prop = count/total_count)
fig = pdat %>% 
  ggplot(aes(x = state, y = prop, fill = year)) + 
  geom_bar(stat="identity", position = "dodge")  +
  theme_minimal() +
  scale_fill_brewer() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  scale_y_continuous(label=scales::percent) + 
  labs(y = "", x = "", title = "Proportion of Australian adult cattle slaughter \npresented for MSA grading by state")
