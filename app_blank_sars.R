###################################
# BLANK APP                       #
###################################

#### PACKAGE GROUPS #### 

# shiny app support packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
#library(htmlwidgets) # check this one
library(htmltools) # check this one 


# data processing (spatial and nonspatial)
library(sf)
library(aws.s3)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(lubridate)
#library(tigris) # do we use this still?
#library(rgdal) # do we still use this one?
#library(scales) # check this one

# leaflet packages
library(leaflet)
library(leaflet.extras)

# figure and table creation
library(ggplot2)
#library(ggthemes) #check this one
library(plotly)
library(gt)

#------------------------------------------------------------------------------------------------------------------------------------------------------

### GLOBAL ENVIRONMENT DATA LOADING AND PREPROCESSING PLACE HERE ###

### REACTIVE DATES ###
update_date <- lubridate::today(tzone = 'America/New_York') %>% format('%b %d, %Y')
last_sample_date <- today() %>% format('%b %d, %Y')

#------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------

#############  APP STARTS HERE #######################

# Define UI 
ui <-  
  function(req){
    
    dashboardPage(
      
      dashboardHeader(title = "Title", # title in the box at top of the page
                      titleWidth = 450), # this titlewidth is linked to the sidebar
      
      # SIDEBAR CONTENT #
      dashboardSidebar( width = 225,
                        
                        
                        # SIDEBAR MENU #
                        sidebarMenu(id = "sidebarid", 
                                    
                                    # STYLE ARGUMENT USES CSS CODE
                                    style = "position:fixed; width:auto; overflow-x: clip;", # keeps the sidebar content from scrolling
                                    
                                    # MENU ITEMS THAT ARE TABS ON THE SIDEBAR
                                    menuItem("State Dashboard", tabName = "dashboard", icon = icon("dashboard")), # tabName is called below in the tab argument
                                    
                                    menuItem("Resources and toolkit", tabName = "resources", icon = icon("file-alt")),
                                    
                                    menuItem("Information", icon = icon("info"), tabName = "Information"),
                                    
                                    ### CONDITIONAL PANEL FOR SIDEBAR BUTTONS ON MAIN DASHBAORD PAGE ###
                                    # we add this here so  that the buttons only show up on the dashboard page (see condition = below)
                                    conditionalPanel(
                                      condition = 'input.sidebarid == "dashboard"',
                                      
                                      
                                      # RADIO BUTTONS FOR FIRST PLOT SHOWING TRENDS
                                      radioButtons("ww_trend", # id for radio button series
                                                   "Wastewater trend", # title of button series
                                                   c("SARS-CoV-2 intensity" = "intensity", # plot 1 is  the intensity, log refers to the ID of the object
                                                     "Gene copies" = "raw gene copies" #plot 2 is the gene copies, gene is the ID of  the object to display
                                                     
                                                   ), 
                                                   width = 225),
                                      
                                      # TOOLTIP FOR TREND PLOTS
                                      bsTooltip("ww_trend", 
                                                "Switch between intensity and gene copies ", ),
                                      
                                      # RADIO BUTTONS FOR SECOND PLOT SHOWING CASE DATA
                                      radioButtons("case_switch", # id for the radio button series
                                                   "Cases data", # title of button series
                                                   c("New cases" = "new_cases", 
                                                     "Active cases" = "active_cases",
                                                     "Test positivity" = "positivity"),
                                                   width = 225),
                                      
                                      # TOOLTIP FOR CASE PLOT
                                      bsTooltip("case_switch", #"county_case_plotly", 
                                                "Switch the case plots between new cases, active cases, and positivity.",
                                                "bottom", )
                                    ) # close conditional panel 
                        ) # close sidebar menu
                        
                        
      ), # close dashboard sidebar argument
      
      dashboardBody(
        useShinyjs(), #for shinyjs code to work
        useShinyalert(),  # Set up shinyalert
        
        # CSS style arguments (e.g., font size)
        # increase size of acutal map display based on window
        tags$style(type = "text/css", "#NYBetaMap {height: calc(100vh - 80px) !important;}"), # NYBetaMap is the ID of the object to modify
        tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), # this makes the state_map_toggle overlay the leaflet
        
        tabItems(
          
          tabItem(tabName = "dashboard",
                  
                  # web issues box
                  # update list
                  #fluidRow(
                  #  box(width = 12,
                  #     title = "Note: Browser issues", background = "red",
                  #    "We are aware of issues regarding the dashboard not working as it should in certain browsers (i.e., Google Chrome). We recommend trying
                  #   a different browser (e.g., Safari, Edge) or clearing the cache on your browser."
                  
                  #) # end update box
                  
                  #    ), # end fluid row update box
                  
                  # update list
                  # fluidRow(
                  #   box(width = 12,
                  #       title = "Update: August 2022", background = "green",
                  #       p("New York City data are now available and reported weekly. For more information on methods used for SARS-CoV-2 detection in New York City, please visit",
                  #         tags$a(href = "https://doi.org/10.1039/D1EW00747E",  "this website.", style = "color: darkblue;"),
                  #         "For access to New York City historical data, please visit: ",
                  #         tags$a(href = "https://data.cityofnewyork.us/Health/SARS-CoV-2-concentrations-measured-in-NYC-Wastewat/f7dc-2q9f/data", "this website.",
                  #                style = "color: darkblue;")
                  #       )
                  # 
                  #   ) # end update box
                  
                  #), # end fluid row update box
                  fluidRow(
                    column(12, 
                           h2("Intro title"),
                           p("Optional introductory text"),
                           br(), # line break between paragraphs
                    )# end column argument
                  ), # end fluid row opening paragraph

                  #  VALUE BOXES WITH STATE SUMMARIES  #
                  fluidRow(column(width = 12, h2(HTML(paste("Statewide participation summary", "<sup>", "1", "</sup>"))))),
                  
                  fluidRow(
                    # number of counties actively reporting
                    valueBox( value = 1000,
                              subtitle = "Participating counties",
                              color = "light-blue",
                              icon = icon("map")
                    ),
                    # number of  treatment plants reporting
                    valueBox( value = 100,
                              #subtitle = "Participating WWTPs",
                              subtitle = "Participating WWTPs",
                              color = "aqua",
                              icon = icon("water")
                    ),
                    # population covered
                    valueBox( value = formatC(14500000, format="d", big.mark=","),
                              subtitle = "Population covered",
                              color = "blue",
                              icon = icon("users"))
                  ), # end fluid row summary boxes
                  fluidRow(column(width = 12, p(HTML(paste("<sup>", "1", "</sup>", "Participation as of ", lubridate::today(tzone = 'America/New_York') %>% format('%b %d, %Y')))))),
                  
                  # UPDATE INFORMATION ROW
                  fluidRow(column(12,
                                  titlePanel(h3(paste("Last Updated: ", update_date, sep = ""))),
                                  titlePanel(h3(paste("Most recent sample: ", last_sample_date, sep = "")))
                  )
                  
                  ), # end fluid row
                  br(),
                  
                  # MAIN DASHBOARD MAP (ID = NYBetaMap)
                  fluidRow(box(width = 12, id = "map_container",
                               title = "Map of participating treatment plants and counties",
                               column(width = 8,
                                      # withspinner adds loading icon
                                      withSpinner(
                                        leafletOutput('NYBetaMap'), 
                                        type = 8), # type of loading icon to show
                                      fluidRow(# create action button to show or hide description
                                        actionButton(inputId = "button_map", label = "Map description show / hide", width = 250)
                                      ) # close fluid row
                               ), # close column
                               
                               # RIGHT OF MAP SIDEBAR #
                               column(width = 4, 
                                      # MAIN MAP TOGGLE SELECTIONS AND BUTTONS
                                      fluidRow(selectInput(inputId = "state_map_toggle", label = "Wastewater Metric",
                                                           choices = c("SARS-CoV-2 detection level" = "Alert_factor", 
                                                                       "Two week trend" = "Trend_factor"),
                                                           selected = "SARS-CoV-2 detection level"
                                      )# end select input
                                      ), # close fluid row
                                      # CATEGORY CHANGE TABLES
                                      fluidRow(wellPanel(id = "category_tables",
                                                         conditionalPanel(condition = "input.state_map_toggle == 'Alert_factor'",
                                                                          gt_output("alert_table")
                                                         ), # close conditional panel
                                                         conditionalPanel(condition = "input.state_map_toggle == 'Trend_factor'",
                                                                          gt_output("trend_table")
                                                         ) # close conditional panel
                                      ) # close well panel
                                      ), # close fluid row
                                      
                                      # CATEGORY CHANGE DESCRIPTION 
                                      fluidRow(wellPanel(
                                        conditionalPanel(condition = "input.state_map_toggle == 'Alert_factor'",
                                                         box(width = 12, title = "SARS-CoV-2 detection level",
                                                             p("SARS-CoV-2 detection level is displayed in three categories: Low, Moderate, and Substantial to High.
                                                    Current estimated levels are based on the highest detection reported from the most recent three samples. These detection levels have
                                                    been shown to correlate with estimated community transmission levels. Category change in the last 15 days is calculated by subtracting 
                                                    the current number of sites in each current level category from the number of sites in the same category 15 days earlier and dividing by 
                                                    the number of sites in the same category 15 days earlier.
                                                      See the Map description for more detailed information."))
                                        ), # close conditional panel
                                        conditionalPanel(condition = "input.state_map_toggle == 'Trend_factor'",
                                                         box(width = 12, title = "Two-week trend", 
                                                             p("To identify how wastewater results are changing over time, trend analysis is conducted on the most recent data. A two-week trend is 
                                                    calculated for each location using all data points within the fifteen previous days of the most recent sample for that location. 
                                                    The trend analysis calculates the average change in the SARS-CoV-2 Intensity over the fifteen-day period using a linear 
                                                    regression. Sites with fewer than two samples within the fifteen-day window appear as NA values. Category change in the last 15 days is 
                                                    calculated by subtracting the current number of sites in each current level category from the number of sites in the same category 15 
                                                    days earlier and dividing by the number of sites in the same category 15 days earlier.
                                                    See the Map description for more
                                                    detailed information.")
                                                         ) # close box
                                        )# close conditional panel
                                      )# close well panel
                                      ) # close fluid row
                               ) # close column

                  ), #close map box

                  ), # end map fluidrow

                  # new tooltip to try
                  # NYBetaMap TOOLTIP #
                  bsTooltip("NYBetaMap", 
                            "Zoom in or click a county to see sewersheds. Click a marker or catchment to see wastewater trends (displayed below).",
                            trigger = "hover",
                            "bottom", ),
                  
                  # map container second TOOLTIP #
                  bsTooltip("map_container", 
                            "Zoom in or click a county to see sewersheds. Click a marker or catchment to see wastewater trends (displayed below).",
                            trigger = "hover",
                            "top", ),
                  
                  br(),
                  
                  # NYBetaMap DESCRIPTION (observe event is in the server)
                  hidden(div(id = "maptext_wrapper",
                             box(id = "mapText", title = "Map description", width = 12,
                                 h4("Header"),
                                 # disclaimer on state of the science
                                 p("optional description text")
                             ) # end box
                  )# end div
                  ), # end hidden,
                  
                  # WASTEWATER TREND PLOTS 
                  # created in a well panel and conditional panel linked to the radio buttons above
                  br(),
                  fluidRow(
                    box(width = 12, title = "Wastewater trend",
                        wellPanel(id = "sewershed_plots", 
                                  conditionalPanel(
                                    condition = "input.ww_trend == 'intensity'",
                                    withSpinner(plotlyOutput("sewershed_plotly_default_log"), type = 8) # add loading icon
                                  ),
                                  conditionalPanel(
                                    condition = "input.ww_trend == 'raw gene copies'",
                                    withSpinner(plotlyOutput("sewershed_plotly_default"), type = 8) # add loading icon
                                  )
                        ),# end well panel
                        column(12, actionButton(inputId = "button_trendText", label = "Trend graph description show / hide", width = 250)
                        )# end  column
                    )# end box
                  ), # end fluidrow

                  bsTooltip("sewershed_plots", #"sewershed_plotly_default", 
                            "This plot shows the trend values for SARS CoV-2 detected in wastewater at the selected treatment plant.",
                            "bottom", ),
                  
                  # sewershed trend plot description 
                  
                  hidden(
                    div(id = "trendPlotText_wrapper",
                        box(id = "trendPlotText", title = "Wastewater trend description", width = 12,
                            h4("Header"),
                            p("optional text")
                        ) # end box
                    )# end div
                  ), # end hidden
                   br(),
                  #fluidRow(
                  # box(width = 12,
                  # plotlyOutput("county_case_plotly"))
                   #),
                  fluidRow(
                    box(width = 12, title = "Case data",
                        wellPanel(id = "case_plots", conditionalPanel(
                          condition = "input.case_switch == 'new_cases'",
                          withSpinner(plotlyOutput("county_case_plotly"), type = 8) # add loading icon
                        ),
                        conditionalPanel(
                          condition = "input.case_switch == 'active_cases'",
                          withSpinner(plotlyOutput("county_active_plotly"), type = 8) # add loading icon
                        ),
                        conditionalPanel(
                          condition = "input.case_switch == 'positivity'",
                          withSpinner(plotlyOutput("county_positivity_plotly"), type = 8) # add loading icon
                        )
                        ), 
                        column(12, actionButton(inputId = "caseText_button", label = "Case plot description show / hide", width = 250))
                    )# end box
                  ), # end fluid row
                  
                  # case plot tooltip
                  bsTooltip("case_plots", #"county_case_plotly", 
                            "This plot shows the total positive test results for the county.",
                            "bottom", ),
                  
                  # case plot description popup
                  
                  hidden(
                    div(id = "caseText_wrapper",
                        box(id = "caseText", title = "Case plot description", width = 12,
                            h4("Header"),
                            p("optional text")
                        ) # end box
                    )# end div
                  )# end hidden

                  
                  ###############
                  
                  
          ),
          tabItem(tabName = "resources",
                  fluidRow(
                    column(12
                    ) # close column
                  ) # close fluid row
                  
          ),
          
          tabItem(tabName = "Information",
                  fluidRow(
                    column(12
                    ) # end column
                  )# end fluid row
          ) # end tab item
          
        )# end tab items plural
        
      ) # end dashboard body
      
    )# end dashboardPage
    
  } # close function for ui

###################### SERVER ############################

server <- function(input, output, session) {
  
  ## helper command to keep app open longer ##
  output$clock = renderText({
    invalidateLater(4500)
    Sys.time()
  })
  
  ###### LEAFLET MAP #######

  # create map for renders
  nybetamap_preset <- leaflet() %>% 
    addMapPane("tiles", zIndex = 400) %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Base map",
                     options = c(pathOptions(pane = "tiles"))) %>%
    addProviderTiles(providers$Stamen.TonerLines, group = "Base map",
                     options = c(providerTileOptions(opacity = 0.35), pathOptions(pane = "tiles")))%>%
    addProviderTiles(providers$Stamen.TonerLabels,group = "Base map",
                     options = pathOptions(pane = "tiles"))
  
  ##### PRESET STATE MAP FOR STATE MAP VIEW BUTTON #####
  # render output
  output$NYBetaMap <- renderLeaflet({
    nybetamap_preset
    
  })  
  
  # button press to display description of map
  observeEvent(input$button_map, {
    toggle("maptext_wrapper")
    
  })
  
  # sample ggplot for the first plot
  default_ww_plot <-ggplotly(ggplot(mpg, aes(displ, hwy, colour = class)) + 
    geom_point())
  output$sewershed_plotly_default <- renderPlotly(default_ww_plot)
  
  # sample ggplot for the second plot behind the wellpanel
  default_ww_plot_log <-ggplotly(ggplot(mpg, aes(displ, hwy, colour = class)) + 
    geom_line())
  output$sewershed_plotly_default_log <- renderPlotly(default_ww_plot_log)
  
  # button press to display description of plot
  observeEvent(input$button_trendText, {
    toggle("trendPlotText_wrapper")
    
  })
  
  # barplot
  case_plotly <- ggplotly(ggplot(mpg, aes(displ, hwy, fill = class)) + 
                            geom_bar(position = "stack", stat= "identity"))
  output$county_case_plotly <- renderPlotly(case_plotly)
  
  active_plotly <- ggplotly(ggplot(mpg, aes(displ, year, fill = class)) + 
                              geom_bar(position = "stack", stat= "identity"))
  output$county_active_plotly <- renderPlotly(active_plotly)
  
  positivity_plotly <-  ggplotly(ggplot(mpg, aes(displ, trans, fill = class)) + 
                                   geom_bar(position = "stack", stat= "identity"))
  output$county_positivity_plotly <- renderPlotly(positivity_plotly)
  
  
  # button press to display description of case plot
  observeEvent(input$caseText_button, {
    toggle("caseText_wrapper")
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)