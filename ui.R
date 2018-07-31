dashboardPage(
  skin = "green",
  dashboardHeader(title = "MSA toolbox"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "data_import", icon = icon("upload")),
      menuItem("Data table", tabName = "data_table", icon = icon("table")),
      menuItem("AOR", tabName = "counts", icon = icon("globe"),
               menuSubItem("National", tabName = "national_counts"),
               menuSubItem("State", tabName = "state_counts"),
               menuSubItem("MSA index average", tabName = "historical_msa_index"),
               menuSubItem("MSA index distribution", tabName = "historical_msa_index"),
               menuSubItem("Data update", tabName = "update_counts")
      ),
      menuItem("Compliance", icon = icon("times-circle"),
               menuSubItem("Non-compliance",tabName = "noncomp"),
               menuSubItem("By trait", tabName = "noncomp_trait"),
               menuSubItem("By season", tabName = "noncomp_season"),
               menuSubItem("By month",tabName = "pct_ungrade")
               ),
      menuItem("Percentile bands",tabName = "quantiles"),
      menuItem("Traits over time",tabName = "trait_time"),
      menuItem("Cattle graded by month",tabName = "n_by_month"),
      menuItem("Distributions", tabName = "distributions", icon = icon("area-chart")),
      menuItem("Pivot table", tabName = "pivot_table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "data_import",
        fluidRow(
          box(width = 6, title = "Import data", solidHeader = TRUE, status = "primary",
              fileInput('file1', 'Upload csv file (or zipped csv file)')),
          box(width = 6, title = "Subset data", solidHeader = TRUE, status = "primary",
              uiOutput("uiselect"))
        )
      ),
      tabItem(
        tabName = "data_table",
        box(width = 12, title = "Imported and filtered data", solidHeader = TRUE, status = "primary",
            DT::dataTableOutput("tab_out"))
      ),
      tabItem(
        tabName = "update_counts",
        fluidRow(
          box(width = 6,
              p("For the plots to update, you'll need to take the counts from below and edit the
                    table to the right (by double clicking in the cell you want to edit). This will then
                    be saved in the file data/historical_msa_count.csv so when you come back next time
                    you won't need to do this again.  Similarly for the proportion plot, the national slaughter
                    counts need to be added to the table at the bottom of this page and the results will be saved in
                    data/annual_kill_data.csv"),
              h3("Counts in current data set"),
              DT::DTOutput("new_msa_count"),
              h3("Counts by producer state in current data set"),
              DT::DTOutput("new_msa_state_count")),
          box(width = 6, title = "Historical counts; historical_msa_count.csv",
              DT::DTOutput("total_msa_count")),
          box(width=12, title = "Historical state based MSA counts: msa_kill_data_by_state.csv",
              p("The table below needs to be manually updated with data sourced from above 
                (e.g. copy and paste the numbers from the current data into the table below).
                Sorry that this step is a little manual, but you can also update other numbers
                in the table if required, and your changes will be saved for next time."),
              DT::DTOutput("akdstate2")),
          box(width=12, title = "Historical production data: annual_kill_data.csv",
              p("The table below needs to be manually updated with data sourced from MIDAS. http://statistics.mla.com.au/Report/List"),
              DT::DTOutput("akd2"))
        )
      ),
      tabItem(tabName = "national_counts",
              h1("National counts"),
              fluidRow(
                box(plotOutput("fig01_n", height = 250)),
                box(plotOutput("fig01_p", height = 250)),
                # box(DTOutput("national_n")),
                box(DTOutput("national_p"), width = 12)
              )
      ),
      tabItem(tabName = "state_counts",
              h1("State counts"),
              p("To update the data below see the *Data update* section to the left."),
              uiOutput("state_years"),
              box(title = "Plot output of counts", plotOutput("state_counts", height = 400), width=12),
              box(title = "Data table output of counts", DTOutput("state_counts_data"), width = 12),
              box(title = "Plot output of proportions", plotOutput("state_props", height = 400), width=12),
              box(title = "Data table output of proportions", DTOutput("state_props_data"), width = 12)
              
      ),
      tabItem(tabName = "historical_msa_index",
              h1("Historical MSA index"),
              box(title = "Plot output historical MSA index", plotOutput("hmi_plot", height = 400), width=12),
              p("You might need to add to the MSA index table on the right (double click the cell to update) with the summary data on the left."),
              box(title = "Average MSA index of uploaded data: ", DTOutput("hmi_current"), width = 6),
              box(title = "Historical MSA index data: ", DTOutput("hmi_data2"), width = 6)
      ),
      tabItem(tabName = "noncomp",
              h1("Non-compliance"),
              box(width = 12,
                  plotOutput("noncomp_plot")),
              # box(width = 6, title = "Counts of non-compliance",
              #     DT::DTOutput("noncomp_count")),
              box(width = 12, title = "Proportions of non-compliance",
                  DT::DTOutput("noncomp_prop"))
      ),
      tabItem(tabName = "noncomp_trait",
              h1("Non-compliance by trait"),
              box(width = 12,
                  plotOutput("noncomp_trait_plot")),
              # box(width = 6, title = "Counts of non-compliance",
              #     DT::DTOutput("noncomp_count")),
              box(width = 12, title = "Proportions of non-compliance",
                  DT::DTOutput("noncomp_trait_prop"))
      ),
      tabItem(tabName = "trait_time",
              uiOutput("dist_ui2"),
              plotOutput("trait_time_month_plot"),
              DT::DTOutput("trait_time_tab")
              ),
      tabItem(tabName = "quantiles",
              h1("Trait quantiles"),
              selectizeInput("quantile_group",label = "Group by", multiple = TRUE,
                             choices = c("Feed type" = "feed",
                                         "Plant boning run" = "plant_boning_run",
                                         "HGP" = "hgp"),
                             selected = "feed"),
              DT::DTOutput("quantile_tab"),
              plotOutput("quantile_dist_plot"),
              plotOutput("quantile_plot")),
      tabItem(tabName = "n_by_month",
              h1("Cattle graded by month"),
              box(plotOutput("n_by_month_plot"), width = 12),
              box(DT::DTOutput("n_by_month_data"), width = 12)),
      tabItem(tabName = "pct_ungrade",
              h1("Percentage of non-compliance by month"),
              box(plotOutput("pct_ungrade_by_month_plot"), width = 12),
              box(tableOutput("pct_ungrade_by_month_data"), width = 12)),
              #box(DT::DTOutput("pct_ungrade_by_month_data"), width = 12)),
      tabItem(tabName = "pivot_table",
              h1("Pivot table"),
              p("On very large data sets, the pivot table may take a long time to load.  
                If you are sure you want to show the pivot table click the button below."),
              br(),
              actionButton("show_pivot",label = "Show pivot table", icon = icon("play")),
              rpivotTable::rpivotTableOutput("pivot_table")),
      tabItem(tabName = "distributions",
              h1("Distribution of various traits"),
              box(title = "Specify parameters", width = 4,
                  uiOutput("dist_ui")),
              box(title = "Plot output", width = 8,
                  plotOutput("dist_plot")),
              box(title = "Table output", width = 12))
    )
  )
)
