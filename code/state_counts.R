pdat = akdstate %>% gather(key = "state", value = "count", -year) %>% 
  drop_na(count) %>% 
  filter(year %in% input$state_years)
fig = pdat %>% 
  ggplot(aes(x = state, y = count, fill = year)) + 
  geom_bar(stat="identity", position = "dodge")  +
  theme_minimal() +
  scale_fill_brewer() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  scale_y_continuous(label=scales::comma) + 
  labs(y = "", x = "", title = "Number of MSA graded carcases by state")
