#kable(non_comp_reason, digits = 3)
#write_csv(non_comp_reason, path = paste("outputs/Fig",item,".csv",sep=""))

mean_var = rlang::sym(input$dist_var2)
group_var = rlang::sym(input$dist_group2)

dat1 = data() %>%
  group_by(kill_month, kill_year) %>% 
  summarise(value = mean(!! mean_var, na.rm = TRUE),
            n = n()) %>% 
  mutate(!! group_var := "NATIONAL")
if(input$dist_group2 != "none"){
  dat2 = data() %>%
    group_by(!! group_var, kill_month, kill_year) %>% 
    summarise(value = mean(!! mean_var, na.rm = TRUE),
              n = n())
  dat = bind_rows(dat2, dat1)
  fig = dat %>% 
    gather(key = key, value = value, -kill_month, -kill_year, -(!! group_var),-n) %>% 
    ggplot(aes(x = interaction(kill_month, kill_year),
               y = value, group = !! group_var, colour = !! group_var)) +
    geom_line() + 
    geom_point() + 
    labs(title = input$dist_var2,
         x = "",
         y = "",
         colour = "") + 
    #coord_cartesian(ylim=c(55,60)) + 
    theme_minimal() # +
  # theme(legend.position = c(.85, .9),
  #       axis.text.x = element_text(angle = 90, hjust = 0))
} else{ 
  fig = dat1 %>% 
    # gather(key = key, value = value, -kill_month, -kill_year,-n) %>% 
    ggplot(aes(x = interaction(kill_month, kill_year),
               y = value, group = !! group_var)) +
    geom_line() + 
    geom_point() + 
    labs(title = "",
         x = "",
         y = "",
         colour = "") + 
    #coord_cartesian(ylim=c(55,60)) + 
    theme_minimal() # +
  # theme(legend.position = c(.85, .9),
  #       axis.text.x = element_text(angle = 90, hjust = 0))
  
  }
