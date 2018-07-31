## trait quantiles

#quantile_group_vars <- rlang::sym(input$quantile_group)
feed_perc = data() %>% group_by_(.dots = input$quantile_group) %>% 
  summarise(n = n(),
            `1%` = quantile(msa_index,probs=0.01 , na.rm=TRUE) %>% round(2),
            `5%` = quantile(msa_index,probs=0.05 , na.rm=TRUE) %>% round(2),
            `10%` = quantile(msa_index,probs=0.10, na.rm=TRUE) %>% round(2),
            `25%` = quantile(msa_index,probs=0.25, na.rm=TRUE) %>% round(2),
            `50%` = quantile(msa_index,probs=0.50, na.rm=TRUE) %>% round(2),
            `75%` = quantile(msa_index,probs=0.75, na.rm=TRUE) %>% round(2),
            `90%` = quantile(msa_index,probs=0.90, na.rm=TRUE) %>% round(2),
            `95%` = quantile(msa_index,probs=0.95, na.rm=TRUE) %>% round(2),
            `99%` = quantile(msa_index,probs=0.99, na.rm=TRUE) %>% round(2))
feed_bands = feed_perc %>% 
  mutate(`Top 1%` = `99%`,
         `Top 5%` = `95%`,
         `Top 10%` = `90%`,
         `Top 25%` = `75%`,
         `Top 50%` = `50%`,
         `Bottom 25%` = `25%`,
         `Bottom 10%` = `10%`,
         `Bottom 5%` = `5%`,
         `Bottom 1%` = `1%`) %>% 
  select(-c(`1%`:`99%`))

