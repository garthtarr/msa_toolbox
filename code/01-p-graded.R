aus_annual_kill_data = akd %>% select(year, AUS) 
kill_prop = left_join(total_msa_count, aus_annual_kill_data) %>% drop_na(AUS) %>% 
  mutate(
    AUS = as.numeric(AUS),
    non_msa = AUS - total_msa_head,
    msa_prop = total_msa_head/AUS) %>% 
  rename(`MSA` = total_msa_head,
         `Non MSA` = non_msa,
         `Total AUS` = AUS,
         `Proportion MSA` = msa_prop)

fig = kill_prop %>% 
  select(year,MSA,`Non MSA`) %>% 
  gather(key = msa_status, value = head_count, -year) %>% 
  ggplot(aes(x = year, y = head_count, fill = forcats::fct_rev(msa_status))) + 
  geom_bar(position = "fill",stat = "identity",alpha=0.7) +
  labs(title = "Proportion of Australian adult cattle slaughter \npresented for MSA grading",
       x = "",
       y = "",
       fill = ""#,
       #caption = "Source: ABS and Meat & Livestock Australia.\nNote that total adult cattle includes all adult cattle and selling pathways.\nSome of these animals would not be eligible for MSA grading."
  ) + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
