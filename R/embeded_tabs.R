
# embeded tabs module for income tables
inc_embedded_tabs_ui <- function(id){
  
  fluidPage(
    uiOutput(NS(id, 'table_control')),
    uiOutput(NS(id, 'table_module'))
    
  )
}



inc_embeded_tabs_server <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {

    
    dictionary <- read.csv('dictionary/dict_main.csv') %>%
      split(.$key)
    
    # uses a reactiveVal language.
    tr <- function(key) {
      dictionary[[key]][[language()]]
    }    
    
    output$table_control <- renderUI({
      radioGroupButtons(
        inputId = NS(id,"table"),
        # label = "Label",
        choices = setNames(1:2, tr("title_incs")),
        selected = 1,
        justified = TRUE
      )
    })
    
    module <- reactive({
      req(input$table)
      
      if (input$table == 1){
        income_cs_ui("income_cs")
      } 
      else {
        income_long_ui("income_long")
      }
    })
    
    output$table_module <-  renderUI({
      tags$div(module())
    })
  })
}
    
# embeded tabs module for mobility tables    
mob_embedded_tabs_ui <- function(id){
  
  fluidPage(
    uiOutput(NS(id, 'table_control')),
    uiOutput(NS(id, 'table_module'))
    
  )
}



mob_embeded_tabs_server <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
    
    
    dictionary <- read.csv('dictionary/dict_main.csv') %>%
      split(.$key)
    
    # uses a reactiveVal language.
    tr <- function(key) {
      dictionary[[key]][[language()]]
    }    
    
    output$table_control <- renderUI({
      radioGroupButtons(
        inputId = NS(id,"table"),
        # label = "Label",
        choices = setNames(1:2, tr("title_mobs")),
        selected = 1,
        justified = TRUE
      )
    })
    
    module <- reactive({
      req(input$table)
      
      if (input$table == 1){
        mob_measure_ui("mob_measure")
      } 
      else {
        mob_matrix_ui("mob_matrix")
      }
    })
    
    output$table_module <-  renderUI({
      tags$div(module())
    })
  })
}
