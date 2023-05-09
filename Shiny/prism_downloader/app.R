#--- preamble ---#
rm(list = ls())
library(shiny)
library(readr)
library(progressr)
library(purrr)
library(tidyverse)
library(data.table)
library(lubridate)
library(shinyalert)
library(shinybusy)
library(shinyWidgets)
library(shinydashboard)
library(progress)

# setwd("Shiny/prism_downloader/")
# default_style <-
#   "
# .form-group { margin-bottom: -8px !important;}
# 
#  h4 { font-size: 15px;
#   font-weight: bold;
#   font-family: ACalibri;
#   color: #80bfff;
#     text-align: center
# }
# /* logo */
#   .skin-blue .main-header .logo {
#     background-color: #f4b943;
#   }
# 
# /* logo when hovered */
#   .skin-blue .main-header .logo:hover {
#     background-color: #f4b943;
#   }
# 
# /* navbar (rest of the header) */
#   .skin-blue .main-header .navbar {
#     background-color: #f4b943;
#   }
# 
# /* main sidebar */
#   .skin-blue .main-sidebar {
#     background-color: #ff69b4;
#   }
# 
# /* active selected tab in the sidebarmenu */
#   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
#     background-color: #ff0000;
#   }
# 
# /* other links in the sidebarmenu */
#   .skin-blue .main-sidebar .sidebar .sidebar-menu a{
#     background-color: #00ff00;
#       color: #000000;
#   }
# 
# /* other links in the sidebarmenu when hovered */
#   .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
#     background-color: #ff69b4;
#   }
# /* toggle button when hovered  */
#   .skin-blue .main-header .navbar .sidebar-toggle:hover{
#     background-color: #ff69b4;
#   }
# 
# .span8 .well { background-color: white; }
# .shiny-output-error:before { visibility: hidden; }
# "
default_style <-''
options(datatable.showProgress = F)


# df_scz <- fread("http://files.asmith.ucdavis.edu/weather/misc/state_county_zipcode.csv")
df_ex <- fread("http://files.asmith.ucdavis.edu/weather/monthly/state_cropland/198101.csv")
v_list <- names(df_ex)[-c(1:4)]
s_list <- c("All States", unique(df_ex$st_abb))
y_max <- str_sub(today() - 30, 1, 4) %>% as.integer()


# UI ----------------------------------------------------------------------

ui <- shinyUI(
  dashboardPage(
    skin = "black",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(tags$head(
      tags$style(HTML(default_style)),
      tabsetPanel(
        tabPanel(
          style = "background-color: white;",
          "About", includeHTML("readme.html")
        ),
        tabPanel(
          "Download Data Using Web Tool",
          tabBox(
            width = 4,
            tabPanel(
              "Inputs",
              # style = "label{font-size:30px; color:  #4059AD; font-weight: bold;}",
              selectInput(
                "t",
                "Choose temporal unit",
                c("daily", "monthly"),
                "monthly"
              ),
              selectInput(
                "r",
                "Choose spatial unit",
                list("state", "county", "ZIP code" = "zip"),
                "county"
              ),
              selectInput(
                "y_min",
                "Choose start year",
                1981:y_max,
                1981
              ),
              selectInput(
                "y_max",
                "Choose end year",
                1981:y_max,
                2023
              ),
              sliderInput(
                inputId = "m",
                label = "Choose month(s)",
                min = 1,
                max = 12,
                value = c(1, 12), sep = "", step = 1, ticks = F
              ),
              selectizeInput("s",
                "Choose state(s)",
                s_list,
                multiple = T,
                "All States"
              ),
              selectizeInput("v",
                "Choose variable(s)",
                v_list,
                multiple = T,
                c("tmin", "tmax", "tavg")
              ),
              selectInput(
                "w",
                "Choose weighting scheme",
                list("no weight" = "noweight", "cropland", "population"),
                "no weight"
              ),
              h1(""),
              actionButton("preview", "Proceed to download data",
                style = "color: white; background-color:  #1F3B4D"
              )
            )
          ),
          tabBox(
            width = 8,
            tabPanel(
              "Download Progress",
              conditionalPanel(
                condition = "input.preview == 0",
                h2("Specify download parameters and click 'Proceed to download'")
              ),
              conditionalPanel(
                condition = "input.preview != 0",
                textInput("fname", "Type file name excluding .csv"),
                h1(""),
                h1(""),
                downloadButton("downloadData", "Download",
                  style = "color: white; background-color: #1F3B4D"
                ),
                h1(""),
                progressBar(id = "pb", value = 0, display_pct = T)
              )
            )
          )
        )
      )
    ))
  )
)


# server ------------------------------------------------------------------



server <- function(input, output, session) {
  output$readme <- renderUI({
    includeHTML("readme.html")
  })

  observe({
    y_min <- input$y_min %>% as.integer()

    if (input$t == "daily" & input$r == "zip") {
      updateSelectInput(session,
        "y_max",
        label = "Choose ending year (up to 5 years)", choices = y_min:(y_min + 4),
        selected = min(y_min + 4, y_max)
      )

      updateSelectizeInput(session, "s",
        "Choose state(s) (max=5)",
        s_list[-1],
        "CA",
        options = list(maxItems = 5)
      )

      updateSelectizeInput(session, "v",
        "Choose variables (max=5)",
        v_list,
        c("tmin", "tmax", "tavg"),
        options = list(maxItems = 5)
      )
    } else if (input$t == "monthly" & input$r == "zip") {
      updateSelectInput(session,
        "y_max",
        label = "Choose ending year (up to 5 years)", choices = y_min:(y_min + 4),
        selected = min(y_min + 4, y_max)
      )

      updateSelectizeInput(session, "v",
        "Choose variables (max=5)",
        v_list,
        c("tmin", "tmax", "tavg"),
        options = list(maxItems = 5)
      )
    } else if (input$t == "daily" & input$r == "county") {
      updateSelectInput(session,
        "y_max",
        label = "Choose ending year (up to 10 years)", choices = y_min:(y_min + 9),
        selected = min(y_min + 9, y_max)
      )

      updateSelectizeInput(session, "s",
        "Choose state(s)",
        s_list,
        "All States",
        options = list(maxItems = 48)
      )
      updateSelectizeInput(session, "v",
        "Choose variables (max=8)",
        v_list,
        c("tmin", "tmax", "tavg"),
        options = list(maxItems = 8)
      )
    } else if (input$t == "monthly" & input$r %in% c("state", "county")) {
      updateSelectInput(session,
        "y_max",
        label = "Choose ending year", choices = y_min:(y_max),
        selected = y_max
      )

      updateSelectizeInput(session, "s",
        "Choose state(s)",
        s_list,
        "All States",
        options = list(maxItems = 48)
      )

      updateSelectizeInput(session, "v",
        "Choose variables",
        v_list,
        c("tmin", "tmax", "tavg"),
        options = list(maxItems = 21)
      )
    } else if (input$t == "daily" & input$r %in% c("state")) {
      updateSelectInput(session,
        "y_max",
        label = "Choose ending year", choices = y_min:(y_max),
        selected = y_max
      )

      updateSelectizeInput(session, "s",
        "Choose state(s)",
        s_list,
        "All States",
        options = list(maxItems = 48)
      )

      updateSelectizeInput(session, "v",
        "Choose variables",
        v_list,
        c("tmin", "tmax", "tavg"),
        options = list(maxItems = 21)
      )
    }
  })


  data <- reactive({
    #--- table of state, county, zip  ---#
    # df_scz <- fread("http://files.asmith.ucdavis.edu/weather/misc/state_county_zipcode.csv")

    #--- selected parameters ---#
    years <- input$y_min:input$y_max
    months <- input$m[1]:input$m[2]
    temporalUnit <- input$t
    spatialUnit <- input$r
    weighting <- input$w

    if ("All States" %in% input$s) {
      states <- unique(df_ex$st_abb)
    } else {
      states <- input$s
    }
    variables <- input$v

    #--- ID columns ---#

    IDs <- list(
      zip = c("st_abb", "st_code", "county_name", "fips", "zipcode"),
      county = c("st_abb", "st_code", "county_name", "fips"),
      state = c("st_abb", "st_code"),
      daily = c("date", "stability"),
      monthly = c("ym")
    )

    #--- yyyymm to include  ---#
    df_ym <- expand_grid(
      y = years,
      m = str_pad(months, side = "left", pad = "0", width = 2)
    ) %>%
      data.table() %>%
      .[as.integer(paste0(y, m)) < as.integer(str_sub(str_remove(today(), "-"), 1, 6))]

    #--- URLs to include  ---#
    URLs <- paste0(
      "http://files.asmith.ucdavis.edu/weather/",
      temporalUnit, "/",
      spatialUnit, "_",
      weighting, "/",
      df_ym$y, df_ym$m,
      ".csv"
    )

    #--- pull data ---#

    pb <- progress_bar$new(
      format = "  :bar [:percent] :current/:total | Elapsed: :elapsed | ETA: :eta",
      total = length(URLs)
    )

    map(1:length(URLs), function(x) {
      Sys.sleep(0.1)
      updateProgressBar(session = session, id = "pb", value = (x / length(URLs)) * 100)
      fread(URLs[x])[st_abb %in% states, c(IDs[[spatialUnit]], IDs[[temporalUnit]], variables), with = F]
    }) %>%
      rbindlist()
  })

  observeEvent(input$preview, {
    shinyalert(title = "", type = "warning", text = "Please review the selected parameters before clicking download as changes may not be possible until the download is complete. For heavy requests, we strongly recommend downloading the data to a local computer as described in the 'About' tab.")
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$fname, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = F)
    }
  )
}

shiny::shinyApp(ui, server)