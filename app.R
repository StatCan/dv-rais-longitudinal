library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(circlize)
library(Cairo)
library(shinyWidgets)
library(httr)
options(shiny.usecairo=T)

source("R/module_pathway.R")
source("R/module_mobility_matrix.R")
source("R/module_mobility_measure.R")
source("R/module_cs_income.R")
source("R/module_long_income.R")

source("R/embeded_tabs.R")


ui <- bootstrapPage(
  
  tags$head(
    tags$script(
      '$(document).on("shiny:connected", function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
           $(window).resize(function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
        ')),
  
  navbarPage(
    
    title=NULL,
    
    tabPanel(
      textOutput("title_pathway"),
      pathway_ui("pathway")
    ), 

    # tabPanel(
    #   textOutput("title_inc_cs"),
    #   income_cs_ui("income_cs")
    # ),
    # tabPanel(
    #   textOutput("title_inc_long"),
    #   income_long_ui("income_long")
    # ),
    
        
    # tabPanel(
    #   textOutput("title_mob_measures"),
    #   mob_measure_ui("mob_measure")
    # ),
    
    
    tabPanel(
      textOutput("inc_title_embeded_tabs"),
      inc_embedded_tabs_ui("income")
    ),
    
    tabPanel(
      textOutput("mob_title_embeded_tabs"),
      mob_embedded_tabs_ui("mobility")
    )
        
    
    # tabPanel(
    #   textOutput("title_mob_matrix"),
    #   mob_matrix_ui("mob_matrix")
    # )
    
  ) # navbar page
) # bootstrap page


server <- function(input, output, session) {
  
  language <- reactiveVal("en")
  
  dictionary <- read.csv('dictionary/dict_main.csv') %>%
    split(.$key)
  
  # uses a reactiveVal language.
  tr <- function(key) {
    dictionary[[key]][[language()]]
  }
  
  output$title_pathway <- renderText(tr("title_pathway"))
  
  # output$title_inc_cs <- renderText(tr("title_inc_cs"))
  # output$title_inc_long <- renderText(tr("title_inc_long"))
  # output$title_mob_measures <- renderText(tr("title_mob_measures"))
  # output$title_mob_matrix <- renderText(tr("title_mob_matrix"))
  
  output$inc_title_embeded_tabs <- renderText(tr("title_inc_tab"))
  output$mob_title_embeded_tabs <- renderText(tr("title_mob_tab"))
  
  
  pathway_server("pathway", language)
  inc_embeded_tabs_server("income",language)
  mob_embeded_tabs_server("mobility",language)
  
  income_cs_server("income_cs", language)
  income_long_server("income_long", language)
  mob_measure_server("mob_measure", language)
  mob_matrix_server("mob_matrix", language, reactive(input$innerSize))
  
}

shinyApp(ui, server)
