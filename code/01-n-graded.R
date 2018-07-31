item = "01"
# new = data() %>% group_by(year) %>%
#   summarise(total_msa_head = n())
# new_msa_count = bind_rows(total_msa_count, new)
# new_msa_count
pdat = total_msa_count %>% drop_na() 
fig = pdat %>% 
  mutate(
    new_msa_head = as.numeric(total_msa_head)/1000000
  ) %>% 
  ggplot(aes(x = year, y = new_msa_head)) + 
  geom_bar(stat = "identity", alpha = 0.7) + 
  labs(title = "Growth in national MSA beef grading",
       x = "",
       y = "Million head"#,caption = cap
  ) + 
  theme_minimal() +
  scale_fill_brewer() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        legend.position = "none",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())