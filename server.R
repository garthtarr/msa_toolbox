# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize=500*1024^2) 
  
  ## Data in
  
  datain <- reactive({
    inFile <- input$file1
    validate(
      need(!is.null(input$file1), "Please upload a data set")
    )
    if (is.null(inFile))
      return(NULL)
    
    input_file_format <- tools::file_ext(inFile$name)
    new_file_name <- paste0(inFile$datapath, ".", input_file_format)
    file.rename(inFile$datapath, new_file_name)
    dat = fread_all(new_file_name, colClasses = 'character', data.table = FALSE, na.strings = c("NA","")) # readr::read_csv(new_file_name, guess_max = 0, col_types = cols(.default = "c")) # 
    dat = janitor::clean_names(dat)
    if(is.element("ossification_cold",names(dat)) & is.element("ossification_hot",names(dat))){
      dat = dat %>% 
        mutate(ossification = coalesce(ossification_hot, ossification_cold)) %>% 
        select(-ossification_hot, -ossification_cold)  
    } else if(is.element("ossification_cold",names(dat))) {
      dat = dat %>% rename(ossification = ossification_cold)
    } else if(is.element("ossification_hot",names(dat))) {
      dat = dat %>% rename(ossification = ossification_hot)
    }
    if(is.element("hump_cold",names(dat)) & is.element("hump_hot",names(dat))){
      dat = dat %>% 
        mutate(hump = coalesce(hump_hot, hump_cold)) %>% 
        select(-hump_hot, -hump_cold)  
    } else if(is.element("hump_cold",names(dat))) {
      dat = dat %>% rename(hump = hump_cold)
    } else if(is.element("hump_hot",names(dat))) {
      dat = dat %>% rename(hump = hump_hot)
    }
    if(is.element("ribfat_cold",names(dat)) & is.element("ribfat_hot",names(dat))){
      dat = dat %>% 
        mutate(ribfat = coalesce(ribfat_hot, ribfat_cold)) %>% 
        select(-ribfat_cold, -ribfat_hot)  
    } else if(is.element("ribfat_cold",names(dat))) {
      dat = dat %>% rename(ribfat = ribfat_cold)
    } else if(is.element("ribfat_hot",names(dat))) {
      dat = dat %>% rename(ribfat = ribfat_hot)
    }
    if(is.element("left_hscw", names(dat))) {
      dat = dat %>% mutate(left_hscw = as.numeric(left_hscw))
    }
    if(is.element("right_hscw", names(dat))) {
      dat = dat %>% mutate(right_hscw = as.numeric(right_hscw))
    }
    if(is.element("producer_state", names(dat))){
      dat = dat %>% mutate(
        producer_state = toupper(producer_state),
        producer_state = recode(producer_state,
                                "ACT" = "NSW",
                                "NT" = "QLD/NT",
                                "QLD" = "QLD/NT"
        )
      )
    }
    dat = dat %>% mutate(
      kill_date = dplyr::coalesce(lubridate::dmy(kill_date), lubridate::ymd(kill_date)), #parsedate::parse_date(kill_date), #readr::parse_date(kill_date),
      grade_date = dplyr::coalesce(lubridate::dmy(grade_date), lubridate::ymd(grade_date)), #parsedate::parse_date(grade_date), #readr::parse_date(grade_date),
      msa_index = as.numeric(msa_index),
      total_hscw = as.numeric(total_hscw),
      p8fat = as.numeric(p8fat),
      hump = as.numeric(hump),
      ossification = as.numeric(ossification),
      ema = as.numeric(ema),
      aus_marbling = as.numeric(aus_marbling),
      msa_marbling = as.numeric(msa_marbling),
      ribfat = as.numeric(ribfat),
      ph = as.numeric(ph),
      loin_temp = as.numeric(loin_temp),
      rib = as.numeric(rib),
      no_days_on_feed = as.numeric(no_days_on_feed),
      tdr062grl = as.numeric(tdr062grl),
      cub045grl = as.numeric(cub045grl),
      stp045grl = as.numeric(stp045grl),
      rmp131grl = as.numeric(rmp131grl)
    )
    dat = dat %>% mutate(
      feed_type = str_replace(feed_type, "\xa0",""),
      feed_type = stringr::str_trim(feed_type),
      feed = case_when(
        feed_type == "Grass" ~ "Grass",
        no_days_on_feed >= 60 & sex == "F" ~ "Grain",
        no_days_on_feed >= 70 & sex == "M" ~ "Grain",
        TRUE ~ "Grass"
      )
    )
    dat = dat %>% mutate(
      month = months(grade_date),
      year = case_when(
        kill_date<=dmy("30-06-2013") ~ NA_character_,
        kill_date<=dmy("30-06-2014") ~ "2013-14",
        kill_date<=dmy("30-06-2015") ~ "2014-15",
        kill_date<=dmy("30-06-2016") ~ "2015-16",
        kill_date<=dmy("30-06-2017") ~ "2016-17",
        kill_date<=dmy("30-06-2018") ~ "2017-18",
        kill_date<=dmy("30-06-2019") ~ "2018-19",
        kill_date<=dmy("30-06-2020") ~ "2019-20",
        kill_date<=dmy("30-06-2021") ~ "2020-21",
        kill_date<=dmy("30-06-2022") ~ "2021-22",
        kill_date<=dmy("30-06-2023") ~ "2022-23",
        kill_date<=dmy("30-06-2024") ~ "2023-24",
        kill_date<=dmy("30-06-2025") ~ "2024-25",
        TRUE ~ NA_character_),
      plant_boning_run = stringr::str_pad(as.character(plant_boning_run),
                                          width = 2, side = "left", pad = "0")
    )
    # Eating quality info to keep:
    dat = dat %>% select(-ends_with("sfr"),-ends_with("yak"),
                         -ends_with("ssb"),-ends_with("sc"),-ends_with("tsl"),
                         -ends_with("cn"),-ends_with("rst"),-ends_with("grl"),
                         c("tdr062grl", "cub045grl", "stp045grl", "rmp131grl")) 
    dat = dat %>% mutate(
      noncomp = is.na(msa_index),
      comp = !is.na(msa_index),
      high_oss = if_else(ossification>300,">300 Oss","<=300 Oss"),
      phcomp = ph<5.71,
      rfcomp = ribfat > 2,
      kill_month = factor(month(kill_date, label = TRUE),
                          levels = month.abb, 
                          ordered = TRUE),
      kill_year = year(kill_date)
    )
    dat = dat %>% mutate_if(.predicate = is.character, toupper)
    return(dat)
  })
  
  ## Subset the data
  
  output$uiselect = renderUI({
    
    dateRangeInput(inputId = "daterange",
                   label = "Kill date range:",
                   start = min(datain()$kill_date, na.rm=TRUE),
                   end = max(datain()$kill_date, na.rm=TRUE))
    
  })
  
  data = reactive({
    dat = datain()
    # if(!is.null(input$daterange)) {
    #   dat = dat %>% 
    #     filter(kill_date >= input$daterange[1],
    #            kill_date <= input$daterange[2])
    # }
    return(dat)
  })
  
  
  ## Data table output
  
  output$tab_out = DT::renderDataTable(
    data() %>% DT::datatable(rownames = FALSE, options = list(scrollX = TRUE))
  )
  
  ## Outputs
  
  ## National counts by financial year
  
  ## File in the background is data/total_msa_count.csv
  ## Present tally in the uploaded data set
  ## Provide instructions that the table needs to be edited
  
  output$total_msa_count = DT::renderDT(total_msa_count, options = list(dom = "tp", ordering = FALSE), editable = TRUE)
  proxy_msa = dataTableProxy('total_msa_count')
  observeEvent(input$total_msa_count_cell_edit, {
    write_csv(total_msa_count,paste("data/historical_msa_count",Sys.time(),".csv",sep = ""))
    info_msa = input$total_msa_count_cell_edit
    str(info_msa)
    i_msa = info_msa$row
    j_msa = info_msa$col
    v_msa = info_msa$value
    total_msa_count[i_msa, j_msa] <<- DT::coerceValue(v_msa, as.numeric(total_msa_count[i_msa, j_msa]))
    replaceData(proxy_msa, total_msa_count, resetPaging = FALSE)  # important
    write_csv(total_msa_count,"data/historical_msa_count.csv")
  })
  
  output$akd2 = DT::renderDT(akd, options = list(dom = "tp", ordering = FALSE), editable = TRUE)
  proxy_akd = dataTableProxy('akd2')
  observeEvent(input$akd2_cell_edit, {
    # write a copy of the original data
    write_csv(akd,paste("data/annual_kill_data",Sys.time(),".csv",sep = ""))
    info_akd = input$akd2_cell_edit
    str(info_akd)
    i_akd = info_akd$row
    j_akd = info_akd$col
    v_akd = info_akd$value
    akd[i_akd, j_akd] <<- DT::coerceValue(v_akd, as.numeric(akd[i_akd, j_akd]))
    replaceData(proxy_akd, akd, resetPaging = FALSE)  # important
    write_csv(akd,"data/annual_kill_data.csv")
  })
  
  output$akdstate2 = DT::renderDT(akdstate, options = list(dom = "tp", ordering = FALSE), editable = TRUE)
  proxy_akdstate = dataTableProxy('akdstate2')
  observeEvent(input$akdstate2_cell_edit, {
    # write a copy of the original data
    write_csv(akdstate,paste("data/msa_kill_data_by_state",Sys.time(),".csv",sep = ""))
    info_akdstate = input$akdstate2_cell_edit
    str(info_akdstate)
    i_akdstate = info_akdstate$row
    j_akdstate = info_akdstate$col
    v_akdstate = info_akdstate$value
    akdstate[i_akdstate, j_akdstate] <<- DT::coerceValue(v_akdstate, as.numeric(akdstate[i_akdstate, j_akdstate]))
    replaceData(proxy_akdstate, akdstate, resetPaging = FALSE)  # important
    write_csv(akdstate,"data/msa_kill_data_by_state.csv")
  })
  
  # report counts in this data set
  output$new_msa_count = DT::renderDT(
    data() %>% group_by(year) %>% summarise(n = n()) %>% 
      DT::datatable(options = list(dom = "t",pageLength = 1000, ordering = FALSE), rownames = FALSE)
  )
  
  output$new_msa_state_count = DT::renderDT(
    data() %>% group_by(year, producer_state) %>% summarise(n = n()) %>% 
      DT::datatable(options = list(dom = "t",pageLength = 1000, ordering = FALSE), rownames = FALSE)
  )
  
  output$fig01_n = renderPlot({
    input$total_msa_count_cell_edit
    input$akd2_cell_edit
    source("code/01-n-graded.R",local = TRUE)
    return(fig)
  })
  
  # output$national_n = renderDT({
  #   input$total_msa_count_cell_edit
  #   input$akd2_cell_edit
  #   source("code/01-n-graded.R",local = TRUE)
  #   pdat %>%       
  #     DT::datatable(extensions = 'Buttons', 
  #                   options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
  #                                  buttons = c('excel'), ordering = FALSE), rownames = FALSE) %>% 
  #     DT::formatPercentage(columns = 3:5,digits = 1)
  # })
  
  output$national_p = renderDT({
    input$total_msa_count_cell_edit
    input$akd2_cell_edit
    source("code/01-p-graded.R",local = TRUE)
    kill_prop %>%       
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), rownames = FALSE) %>% 
      DT::formatPercentage(columns = 5,digits = 1)
  })
  
  output$fig01_p = renderPlot({
    input$total_msa_count_cell_edit
    input$akd2_cell_edit
    source("code/01-p-graded.R", local = TRUE)
    return(fig)
  })
  
  output$quantile_tab = renderDT({
    source("code/quantile_tables.R", local = TRUE)
    DT::datatable(feed_bands, 
                  extensions = 'Buttons', 
                  options = list(dom = "Bt", 
                                 pageLength = 1000, 
                                 compact = TRUE, 
                                 ordering = FALSE,
                                 buttons = c('excel')), 
                  rownames = FALSE)
  })
  
  output$quantile_plot = renderPlot({
    source("code/quantile_tables.R", local = TRUE)
    if(length(input$quantile_group)==0){
      p = gather(feed_bands, key = "quant", value = "value", -1) %>% 
        mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
        ggplot(aes_string(x = "quant", y = "value")) + 
        geom_point() + geom_line() + theme_bw()
    } else if(length(input$quantile_group)==1){
      p = gather(feed_bands, key = "quant", value = "value", -c(1:2)) %>% 
        mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
        ggplot(aes_string(x = "quant", y = "value", 
                          group = input$quantile_group, 
                          colour = input$quantile_group)) + 
        geom_point() + geom_line() + theme_bw()
    } else if(length(input$quantile_group)==2){
      facet_1 = as.formula(paste("~",input$quantile_group[2]))
      p = gather(feed_bands, key = "quant", value = "value", -c(1:3)) %>% 
        mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
        ggplot(aes_string(x = "quant", y = "value", 
                          group = input$quantile_group[1], 
                          colour = input$quantile_group[1])) + 
        geom_point() + geom_line() + theme_bw() + facet_grid(facet_1)
    } else if(length(input$quantile_group)==3){
      facet_2 = as.formula(paste(input$quantile_group[3],"~",input$quantile_group[2]))
      p = gather(feed_bands, key = "quant", value = "value", -c(1:4)) %>% 
        mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
        ggplot(aes_string(x = "quant", y = "value", 
                          group = input$quantile_group[1], 
                          colour = input$quantile_group[1])) + 
        geom_point() + geom_line() + theme_bw() + facet_grid(facet_2)
    }
    return(p)
  })
  
  
  output$quantile_dist_plot = renderPlot({
    aus_perc = data() %>% 
      summarise(n= n(),
                `1%`  = quantile(msa_index,probs=0.01,na.rm=TRUE) %>% round(2),
                `5%`  = quantile(msa_index,probs=0.05,na.rm=TRUE) %>% round(2),
                `10%` = quantile(msa_index,probs=0.10,na.rm=TRUE) %>% round(2),
                `25%` = quantile(msa_index,probs=0.25,na.rm=TRUE) %>% round(2),
                `50%` = quantile(msa_index,probs=0.50,na.rm=TRUE) %>% round(2),
                `75%` = quantile(msa_index,probs=0.75,na.rm=TRUE) %>% round(2),
                `90%` = quantile(msa_index,probs=0.90,na.rm=TRUE) %>% round(2),
                `95%` = quantile(msa_index,probs=0.95,na.rm=TRUE) %>% round(2),
                `99%` = quantile(msa_index,probs=0.99,na.rm=TRUE) %>% round(2))
    
    dens = density(data()$msa_index,na.rm=TRUE)
    getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))
    df = data_frame(x=dens$x, 
                    y=dens$y,
                    quant = factor(findInterval(dens$x,rev(as.numeric(c(100,aus_perc))))))
    write_csv(df, path = paste("outputs/Fig",item,".csv",sep=""))
    fig = df %>% 
      ggplot(aes(x = x, y = y)) +
      geom_ribbon(aes(ymin=0, ymax = y, fill=quant)) +
      labs(title = "Figure 7. Visualising MSA Index rankings",
           x = "",
           y = "",
           caption = cap) +
      scale_x_continuous(breaks = unlist(aus_perc)) + 
      coord_cartesian(x = c(42,70)) + 
      scale_fill_manual(values = getPalette(10)) + 
      geom_line() +
      theme_minimal() + 
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) 
    # 
    # source("code/quantile_tables.R", local = TRUE)
    # if(length(input$quantile_group)==0){
    #   p = gather(feed_bands, key = "quant", value = "value", -1) %>% 
    #     mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
    #     ggplot(aes_string(x = "quant", y = "value")) + 
    #     geom_point() + geom_line() + theme_bw()
    # } else if(length(input$quantile_group)==1){
    #   p = gather(feed_bands, key = "quant", value = "value", -c(1:2)) %>% 
    #     mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
    #     ggplot(aes_string(x = "quant", y = "value", 
    #                       group = input$quantile_group, 
    #                       colour = input$quantile_group)) + 
    #     geom_point() + geom_line() + theme_bw()
    # } else if(length(input$quantile_group)==2){
    #   facet_1 = as.formula(paste("~",input$quantile_group[2]))
    #   p = gather(feed_bands, key = "quant", value = "value", -c(1:3)) %>% 
    #     mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
    #     ggplot(aes_string(x = "quant", y = "value", 
    #                       group = input$quantile_group[1], 
    #                       colour = input$quantile_group[1])) + 
    #     geom_point() + geom_line() + theme_bw() + facet_grid(facet_1)
    # } else if(length(input$quantile_group)==3){
    #   facet_2 = as.formula(paste(input$quantile_group[3],"~",input$quantile_group[2]))
    #   p = gather(feed_bands, key = "quant", value = "value", -c(1:4)) %>% 
    #     mutate(quant = factor(quant, ordered = TRUE, levels = c("Top 1%","Top 5%","Top 10%","Top 25%","Top 50%","Bottom 25%","Bottom 10%","Bottom 5%","Bottom 1%"))) %>% 
    #     ggplot(aes_string(x = "quant", y = "value", 
    #                       group = input$quantile_group[1], 
    #                       colour = input$quantile_group[1])) + 
    #     geom_point() + geom_line() + theme_bw() + facet_grid(facet_2)
    # }
    # return(p)
  })
  
  output$state_years = renderUI({
    checkboxGroupInput("state_years", "Which years to compare",
                       choices = akdstate$year, selected = akdstate$year,
                       inline = TRUE)
  })
  
  output$state_counts = renderPlot({
    input$akdstate2_cell_edit
    source("code/state_counts.R", local = TRUE)
    return(fig)
  })
  
  output$state_props = renderPlot({
    input$akdstate2_cell_edit
    source("code/state_props.R", local = TRUE)
    return(fig)
  })
  
  output$state_props_data = renderDT({
    input$akdstate2_cell_edit
    source("code/state_props.R", local = TRUE)
    pdat %>% 
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), 
                    rownames = FALSE) %>% 
      DT::formatPercentage(columns = 5,digits = 1)
  })
  
  output$state_counts_data = renderDT({
    input$akdstate2_cell_edit
    source("code/state_counts.R", local = TRUE)
    pdat %>% spread(key = state, value = count) %>% 
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), rownames = FALSE)
  })
  
  output$noncomp_plot = renderPlot({
    source("code/noncomp.R", local = TRUE)
    return(fig)
  })
  
  output$trait_time_month_plot = renderPlot({
    source("code/trait_time_month_plot.R", local = TRUE)
    return(fig)
  })
  
  output$trait_time_tab = renderDT({
    
    mean_var = rlang::sym(input$dist_var2)
    group_var = rlang::sym(input$dist_group2)
    
    dat1 = data() %>%
      group_by(kill_year, kill_month) %>% 
      summarise(`MSA index` = mean(!! mean_var, na.rm=TRUE),
                n = n()) %>% 
      mutate(!! group_var := "National")
    if(input$dist_group2 != "none"){
      dat2 = data() %>%
        group_by(!! group_var, kill_year, kill_month) %>% 
        summarise(`MSA index` = mean(!! mean_var, na.rm=TRUE),
                  n = n())
      dat3 = data() %>% 
        group_by(!! group_var) %>% 
        summarise(`MSA index` = mean(!! mean_var, na.rm=TRUE),
                  n = n()) %>% 
        mutate(kill_month = "Total")
      dat4 = data() %>% 
        summarise(`MSA index` = mean(!! mean_var, na.rm=TRUE),
                  n = n()) %>% 
        mutate(kill_month = "Total")
      dat = bind_rows(dat2, dat1) %>%
        bind_rows(dat3) %>% 
        bind_rows(dat4) %>% 
        DT::datatable(extensions = 'Buttons', 
                      options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                     buttons = c('excel'), ordering = FALSE), 
                      rownames = FALSE) %>% 
        formatRound(digits = 2, columns = 4)
    } else {
      dat4 = data() %>% 
        summarise(`MSA index` = mean(!! mean_var, na.rm=TRUE),
                  n = n()) %>% 
        mutate(kill_month = "Total")
      dat = bind_rows(dat1, dat4) %>%
        DT::datatable(extensions = 'Buttons', 
                      options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                     buttons = c('excel'), ordering = FALSE), 
                      rownames = FALSE) %>% 
        formatRound(digits = 2, columns = 3)
    }
   
  })
  
  output$noncomp_trait_plot = renderPlot({
    source("code/noncomp_trait.R", local = TRUE)
    return(fig)
  })
  
  output$noncomp_count = renderDT({
    grpd = data() %>%
      group_by(kill_year, kill_month) %>% 
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                pH = sum(!phcomp),
                `Rib fat` = sum(!rfcomp))
    oall = data() %>%
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                pH = sum(!phcomp),
                `Rib fat` = sum(!rfcomp)) %>% 
      mutate(kill_month = "Total")
    bind_rows(grpd, oall) %>% 
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), rownames = FALSE)
  })
  
  output$noncomp_prop = renderDT({
    grpd = data() %>%
      group_by(kill_year, kill_month) %>% 
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                `Proportion non-compliant` = mean(!comp),
                `Total pH fail` = sum(!phcomp),
                `Proportion pH fail` = mean(!phcomp),
                `Total rib fat fail` = sum(!rfcomp),
                `Proportion rib fat fail` = mean(!rfcomp))
    oall = data() %>%
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                `Proportion non-compliant` = mean(!comp),
                `Total pH fail` = sum(!phcomp),
                `Proportion pH fail` = mean(!phcomp),
                `Total rib fat fail` = sum(!rfcomp),
                `Proportion rib fat fail` = mean(!rfcomp)) %>% 
      mutate(kill_month = "Total")
    bind_rows(grpd, oall) %>% 
      rename(`Month` = kill_month,
             `Year` = kill_year) %>% 
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), rownames = FALSE) %>% 
      DT::formatPercentage(columns = c(5,7,9), digits = 1)
  })
  
  
  output$noncomp_trait_prop = renderDT({
    # trait = feed at the moment
    grpd = data() %>%
      group_by(feed, kill_year, kill_month) %>% 
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                `Proportion non-compliant` = mean(!comp),
                `Total pH fail` = sum(!phcomp),
                `Proportion pH fail` = mean(!phcomp),
                `Total rib fat fail` = sum(!rfcomp),
                `Proportion rib fat fail` = mean(!rfcomp))
    oall = data() %>%
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                `Proportion non-compliant` = mean(!comp),
                `Total pH fail` = sum(!phcomp),
                `Proportion pH fail` = mean(!phcomp),
                `Total rib fat fail` = sum(!rfcomp),
                `Proportion rib fat fail` = mean(!rfcomp)) %>% 
      mutate(kill_month = "Total")
    oall_trait = data() %>%
      group_by(feed) %>% 
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                `Proportion non-compliant` = mean(!comp),
                `Total pH fail` = sum(!phcomp),
                `Proportion pH fail` = mean(!phcomp),
                `Total rib fat fail` = sum(!rfcomp),
                `Proportion rib fat fail` = mean(!rfcomp)) %>% 
      mutate(kill_month = "Total")
    bind_rows(grpd, oall_trait) %>% 
      bind_rows(oall) %>% 
      rename(`Month` = kill_month,
             `Year` = kill_year) %>% 
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), rownames = FALSE) %>% 
      DT::formatPercentage(columns = c(6,8,10), digits = 1)
  })
  
  output$hmi_plot = renderPlot({
    input$hmi_data2_cell_edit
    hmi_data %>%
      na.omit() %>% 
      ggplot(aes(x = year, y = msa_index, group = "")) + 
      geom_point() + geom_line() + theme_bw()
  })
  
  output$hmi_current = DT::renderDT({
    data() %>% group_by(year) %>% 
      summarise(
        n = sum(!is.na(msa_index)),
        `Mean MSA index` = mean(msa_index, na.rm = TRUE) %>% round(2)
      ) %>% 
      DT::datatable(options = list(dom = "t", pageLength = 1000, compact = TRUE,
                                   ordering = FALSE), rownames = FALSE)
  })
  
  output$hmi_data2 = DT::renderDT(hmi_data,  
                                  extensions = 'Buttons',
                                  options = list(dom = "Bt", 
                                                 pageLength = 1000, 
                                                 ordering = FALSE, 
                                                 buttons = c('excel')),
                                  editable = TRUE, 
                                  rownames = FALSE)
  proxy_hmi_data = dataTableProxy('hmi_data2')
  observeEvent(input$hmi_data2_cell_edit, {
    # write a copy of the original data
    write_csv(hmi_data,paste("data/historical_msa_index",Sys.time(),".csv",sep = ""))
    info_hmi_data = input$hmi_data2_cell_edit
    str(info_hmi_data)
    i_hmi_data = info_hmi_data$row
    j_hmi_data = info_hmi_data$col
    v_hmi_data = info_hmi_data$value
    hmi_data[i_hmi_data, j_hmi_data] <<- DT::coerceValue(v_hmi_data, as.numeric(hmi_data[i_hmi_data, j_hmi_data]))
    replaceData(proxy_hmi_data, hmi_data, resetPaging = FALSE)  # important
    write_csv(hmi_data,"data/historical_msa_index.csv")
  })
  
  output$n_by_month_plot = renderPlot({
    data() %>% group_by(kill_month) %>% 
      ggplot(aes(x = kill_month)) + geom_bar() + 
      labs(y = "No. of cattle", x = "", title = "Cattle graded by month") + 
      geom_text(aes(y = ..count..,
                    label = ifelse((..count..)==0,"",
                                   scales::percent((..count..)/sum(..count..)))),
                stat = "count",vjust = -0.2) +
      theme_bw()
  })
  
  output$n_by_month_data = DT::renderDT({
    data() %>% group_by(kill_month) %>% 
      summarise(`Total carcases` = n()) %>% 
      mutate(`% of Annual kill` = `Total carcases`/sum(`Total carcases`)) %>% 
      DT::datatable(extensions = 'Buttons', 
                    options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
                                   buttons = c('excel'), ordering = FALSE), rownames = FALSE) %>% 
      DT::formatPercentage(columns = 3, digits = 1)
  })
  
  output$pct_ungrade_by_month_plot = renderPlot({
    data() %>% group_by(kill_month) %>% 
      ggplot(aes(x = kill_month)) + geom_bar() + 
      labs(y = "Non-compliance rate (%)", x = "", title = "Percentage of non-compliance by month") + 
      theme_bw()
  })
  
  output$pct_ungrade_by_month_data = renderTable({ #DT::renderDT({
    grpd = data() %>%
      group_by(kill_month) %>% 
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                pH = sum(!phcomp),
                `Rib fat` = sum(!rfcomp))
    oall = data() %>%
      summarise(`Total graded` = sum(comp),
                `Total non-compliant` = sum(!comp),
                pH = sum(!phcomp),
                `Rib fat` = sum(!rfcomp)) %>% 
      mutate(kill_month = "Total")
    bind_rows(grpd, oall) # %>% 
      # DT::datatable(extensions = 'Buttons', 
      #               options = list(dom = "Bt", pageLength = 1000, compact = TRUE,
      #                              buttons = c('excel'), ordering = FALSE), rownames = FALSE) %>% 
      # DT::formatPercentage(columns = 3, digits = 1)
  })
  
  output$pivot_table = rpivotTable::renderRpivotTable({
    if(input$show_pivot>0){
      data() %>% rpivotTable::rpivotTable()  
    } else {
      NULL
    }
  })
  
  output$dist_plot = renderPlot({
    validate(
      need(length(input$dist_group)<3, "Please reduce the number of variable that you're trying to group by.")
    )
    if(!is.null(input$dist_var)){
      p = data() %>% ggplot(aes_string(x = input$dist_var)) + 
        geom_histogram()
      if(length(input$dist_group)==0){
        return(p)
      } else if(length(input$dist_group)==1){
        f1 = as.formula(paste("~",input$dist_group))
        if(input$dist_free_y){
          p = p + facet_wrap(f1, scales = "free_y")  
        } else{
          p = p + facet_wrap(f1)
        }
        
        return(p)
      } else if(length(input$dist_group)==2){
        f2 = as.formula(paste(input$dist_group[2],"~",input$dist_group[1]))
        if(input$dist_free_y){
          p = p + facet_grid(f2,scales = "free_y")  
        } else{
          p = p + facet_grid(f2)
        }
        return(p)
      } 
      
    }
    
    
  })
  
  output$dist_ui = renderUI({
    list(
      selectizeInput("dist_var",label = "Trait",
                     choices = c("MSA index" = "msa_index",
                                 "HSCW" = "total_hscw",
                                 "Hump height" = "hump",
                                 "Ossification" = "ossification",
                                 "MSA marbling" = "msa_marbling",
                                 "AUS-MEAT marbling" = "aus_marbling",
                                 "p8 fat depth" = "p8fat",
                                 "Eye muscle area" = "ema",
                                 "Rib fat" = "ribfat",
                                 "pH" = "ph",
                                 "Loin temperature" = "loin_temp"),
                     selected = "msa_index"),
      selectizeInput("dist_group",label = "Group by", multiple = TRUE,
                     choices = c("Feed type" = "feed",
                                 "Sex" = "sex",
                                 "HGP" = "hgp",
                                 "Ossification" = "high_oss",
                                 "MFV" = "mfv",
                                 "Dentition" = "dentition",
                                 "Operator" = "operator",
                                 "Destination" = "dest",
                                 "Plant boning run" = "plant_boning_run",
                                 "Grader" = "grader"
                     ),
                     selected = NULL),
      checkboxInput("dist_free_y",label = "Free up the y-axis scales", value = FALSE)
    )
  })
  
  output$dist_ui2 = renderUI({
    list(
      selectizeInput("dist_var2",label = "Trait",
                     choices = c("MSA index" = "msa_index",
                                 "Non-compliance" = "noncomp",
                                 "HSCW" = "total_hscw",
                                 "Hump height" = "hump",
                                 "Ossification" = "ossification",
                                 "MSA marbling" = "msa_marbling",
                                 "AUS-MEAT marbling" = "aus_marbling",
                                 "p8 fat depth" = "p8fat",
                                 "Eye muscle area" = "ema",
                                 "Rib fat" = "ribfat",
                                 "pH" = "ph",
                                 "Loin temperature" = "loin_temp"),
                     selected = "msa_index"),
      selectizeInput("dist_group2",label = "Group by",
                     choices = c("None" = "none",
                                 "Feed type" = "feed",
                                 "Sex" = "sex",
                                 "HGP" = "hgp",
                                 "Ossification" = "high_oss",
                                 "MFV" = "mfv",
                                 "Dentition" = "dentition",
                                 "Operator" = "operator",
                                 "Destination" = "dest",
                                 "Plant boning run" = "plant_boning_run",
                                 "Grader" = "grader",
                                 "State" = "producer_state"
                     ),
                     selected = NULL)
    )
  })
  
})
