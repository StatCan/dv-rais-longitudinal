income_long_ui <- function(id) {
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(NS(id, 'direc_control')),
      uiOutput(NS(id, 'year_control')),
      uiOutput(NS(id, "geo_control")),
      uiOutput(NS(id, "trade_control")),
      uiOutput(NS(id, "mode_control")),
      uiOutput(NS(id, 'gender_control')),
      uiOutput(NS(id, 'sei_control')),
      width = 4
    ), 
    
    mainPanel(
      fluidRow(
        valueBoxOutput(NS(id, "vbox_prov"), width = 8),
        valueBoxOutput(NS(id, "vbox_year"), width = 4)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_trade"), width = 12)),
      
      
      
      fluidRow(
        valueBoxOutput(NS(id, "vbox_cohort"), width = 4),
        valueBoxOutput(NS(id, "vbox_tax_filers"), width = 4),
        valueBoxOutput(NS(id, "vbox_age_cert"), width = 4)),
      
      # fluidRow(
      #   tableOutput(NS(id, "outtable"))),
      
      fillRow(
        plotlyOutput(NS(id, "outBarChart"), height = "550px"),
        width = "100%")
    )
  )
}





income_long_server <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
    
    # Preparation --------------------------------------------------------------
    source("R/format_number.R")
    
    # language <- reactiveVal(language)
    
    dictionary <- read.csv('dictionary/dict_income_long.csv') %>%
      split(.$key)
    
    # uses a reactiveVal language.
    tr <- function(key) {
      dictionary[[key]][[language()]]
    }
    
    # extract list of trades and geos
    refGeo <- reactive(tr("mem_geo"))
    
    grp <- reactive(setNames(c(1:3,29,30,36,37), tr("mem_trade_grp")))
    rs <- reactive(setNames(c(4:28), tr("mem_trade_rs")))
    nrs <- reactive(setNames(c(31:35), tr("mem_trade_nrs")))
    refTrade <- reactive({
      c(grp(), rs(), nrs()) %>% sort() %>% names()
    })
    
    #  Data processing----------------------------------------------------------
    #  load in the data file
    #  first, try to download the Rds file from GitHub
    tmp <- tempfile()
    resp <-
      GET(
        "https://github.com/StatCan/dv-rais-longitudinal/raw/master/data/income_long.Rds",
        write_disk(tmp)
      )
    # check if the response was "successful" (200)
    if (resp$status_code == 200) {
      # then load the data from downloaded RDS file.
      full <- readRDS(tmp)
      unlink(tmp)
    } else {
      # if it was unsuccessful, use included data.
      full <- readRDS("data/income_long.Rds")
    }    

    
    full2 <- full %>%
      mutate(flag = paste(
        ifelse(is.na(STATUS) | STATUS == "..", " ", STATUS),
        ifelse(is.na(SYMBOL), " ", SYMBOL))) %>%
      subset(REF_DATE <= (max(.$REF_DATE) - 2)) %>% # keep only if they have at leat with 2 year's range
      as.data.frame() 
    
    #  sidebar widgets----------------------------------------------------------
    
    last_years <- full2 %>%
      subset(!is.na(VALUE),
             dim_ind = 1,
             select = REF_DATE) %>%
      summarize(last_year = max(REF_DATE))
    
    #  menu for year (cohort)
    # note last_yr() is used as it's reactive.
    # last_yr() appears first in the list, and it goes back to 2008.
    output$year_control <- renderUI({
      req(input$direc)
      if (input$direc == 3) {
        sliderTextInput(
          inputId = NS(id,"year"),
          label = tr("lab_cert_year"), 
          choices = c(2008:(last_years$last_year)),
          selected = c(2010, (last_years$last_year - 2))
        )
      } else {
        pickerInput(
          inputId = NS(id, "year"),
          label = tr("lab_cert_year"),
          choices = c((last_years$last_year):2008),
          selected = last_years$last_year - 2,
          multiple = FALSE
        )
      }
    })
    
    
    # dropdown menu for "Sex"
    output$gender_control <- renderUI({
      selectInput(
        inputId = NS(id,"sex"),
        label = tr("lab_sex"),
        choices = setNames(1:3, tr("sex_mem")),
        selected = 1
      )
    })
    
    
    # dropdown to select trade/trade list
    output$trade_control <- renderUI({
      req(input$direc)
      
      choice_set = list(
        grp = grp(), # these are defined at the beginning of the server logic.
        rs = rs(),
        nrs = nrs() 
      )
      names(choice_set) <- c(tr("lab_trade_grp"), tr("lab_rs"), tr("lab_nrs"))
      
      if (input$direc == 1) {
        multi_selection <- TRUE
        default_selection <- c(1:3,29,30,36,37)
        options_set <- pickerOptions(
          actionsBox = TRUE,
          selectAllText = tr("sAll_lbl"),
          deselectAllText = tr("dsAll_lbl"),
          noneSelectedText = tr("text_no_trade"))
      } else {
        multi_selection <- FALSE
        default_selection <- 1
        options_set <- NULL
      }
      
      pickerInput(
        inputId = NS(id, "trade"),
        label = tr("lab_trade"),
        choices = choice_set,
        multiple = multi_selection,
        selected = default_selection,
        options = options_set
      ) 
    })
    
    # dropdown menu for "Geography"
    output$geo_control <- renderUI({
      req(input$direc)
      
      if (input$direc == 2) {
        multi_selection <- TRUE
        default_selection <- c(1:2,7:12)
        options_set <- pickerOptions(
          actionsBox = TRUE,
          selectAllText = tr("sAll_lbl"),
          deselectAllText = tr("dsAll_lbl"),
          noneSelectedText = tr("text_no_geo"))
      } else {
        multi_selection <- FALSE
        default_selection <- 1
        options_set <- NULL
      }
      pickerInput(
        inputId = NS(id, "geo"),
        label = tr("lab_geo"),
        choices = setNames(c(1:13), tr("mem_geo")),
        multiple = multi_selection,
        selected = default_selection,
        options = options_set
      )  
      
    })
    
    # radio Buttons for "Compare by"
    output$direc_control <- renderUI({
      radioButtons(
        inputId = NS(id,"direc"),
        label = tr("lab_comp"),
        choices = setNames(1:3, tr("mem_comp") ),
        selected = 1
      )
    })
    
    # mode of certification
    output$mode_control <- renderUI({
      selectInput(
        inputId = NS(id, "mode"),
        label = tr("lab_mode"),
        choices = setNames(1:3, tr("mem_mode")),
        selected = 1
      )
    })
    
    # a check box to include/exlcude self employment
    output$sei_control <- renderUI({
      awesomeCheckbox(
        inputId = NS(id, "sei"),
        label =  tr("lbl_sei"), 
        value = TRUE
      )
    })
    
    #  create plotly chat-------------------------------------------------------
    df <- reactive({
      req(input$year, input$direc)
      
      selected_year <- if (input$direc == 3) {
        c(min(input$year):max(input$year))
      } else {
        input$year
      }
      
      # when nothing is selected because of the 'unselect all' button,
      # behave as if the default options are selected.
      if (is.null(input$trade)) {
        selected_trades <- c(1:3,29,30,36,37)
      } else {
        selected_trades <- input$trade    
      }
      
      if (is.null(input$geo)) {
        selected_geo <- c(1:2,7:12)
      } else {
        selected_geo <- input$geo
      }
      
      df <- full2 %>%
        subset(
          REF_DATE %in% selected_year &
            dim_geo %in% selected_geo &
            dim_sex == input$sex &
            dim_mode == input$mode &
            dim_trade %in% selected_trades) %>%
        arrange(dim_geo, dim_trade, desc(REF_DATE)) %>%
        mutate(
          
          time_point = case_when(dim_ind %in% c(4,12) ~ tr("mem_year_m4"),
                                 dim_ind %in% c(5,13) ~ tr("mem_year_m2"),
                                 dim_ind %in% c(6,14) ~ tr("mem_year_0"),
                                 dim_ind %in% c(7,15) ~ tr("mem_year_2"),
                                 dim_ind %in% c(8,13) ~ tr("mem_year_4")),
          
          label1 = refTrade()[dim_trade],
          label2 = refGeo()[dim_geo],
          label3 = as.character(REF_DATE),
          
          label  = (if(input$direc == 1) refTrade()[dim_trade] 
                    else if((input$direc == 2)) refGeo()[dim_geo] 
                    else as.character(REF_DATE))
        ) 
      
      
      
      
    })
    
    
    
    # render the table for testing / debugging
    # output$outtable <- renderTable(
    #   df()
    # )    
    
    # render the plotly plot
    output$outBarChart <- renderPlotly({
      
      # full3 <- df()
      # full3 <- full3 %>%
      #   filter(is.na(time_point) == FALSE)
      # 
      full3 <- df()%>%
        filter(
          (if (input$sei) dim_ind<9 else dim_ind>9)) %>%
        filter(is.na(time_point) == FALSE)
      
      full3$label <- factor(full3$label, rev(unique(full3$label)), ordered=TRUE)
      
      # print(full3)
      
      req(input$direc)
      
      validate(need(all(is.na(c(full3$VALUE ))) == FALSE, message = tr("mesg_val") ))
      
      
      inc_long_text <- format_dollar(full3$VALUE, locale=language())
      
      # if (input$direc == 1) {
      fig <- plot_ly( x = full3$time_point, 
                      y = full3$VALUE, 
                      # y = replace_na(full3$VALUE,0), 
                      type = 'scatter', mode = 'lines', 
                      linetype = full3$label, 
                      color =  full3$label,
                      text = paste0(inc_long_text, "<sup>", full3$flag, "</sup>"),
                      source = "inc_long",
                      hovertemplate = "%{x}: %{text}")  %>%
        layout(
          # hovermode = "x unified",
          # clickmode = "event+select",
          xaxis = list(
            ticktext = full3$time_point
          )
        )
      
      
      # } 
      
    }) #renderPlotly
    
    # # creating value boxes to all indicators  -------------------------------
    
    
    
    # render the table for testing / debugging
    
    
    df_valuebox <- reactive({
      df() %>%
        pivot_wider(id_cols=c(REF_DATE, dim_geo, dim_sex, dim_trade,dim_mode),
                    names_from=dim_ind, values_from=c(VALUE, STATUS, flag)) %>%
        mutate(
          real_taxfiler        = (if(input$sei) VALUE_3 else VALUE_11),
          real_taxfiler_status = (if(input$sei) STATUS_3 else STATUS_11),
          real_taxfiler_flag   = (if(input$sei) flag_3 else flag_11),
          
          
          supp = c(0:max((nrow(.) -1), 0)),
          label1 = refTrade()[dim_trade],
          label2 = refGeo()[dim_geo],
          label3 = as.character(REF_DATE))
    })
    
    
    output$outtable <- renderTable(
      df_valuebox()
    )  
    # render the table for testing / debugging
    
    
    selected_supp <- reactiveVal(0)
    
    get_clicked <- function() {
      clk <- event_data("plotly_click", source = "inc_long")
      # print(clk)
      if (is.null(clk)) {selected_supp(0)} else {
        selected_supp(clk$curveNumber)
      }
    }
    
    
    reset_selection <- function() {
      selected_supp(0)
    }
    
    
    # if click on plotly, read the selected index
    observeEvent(event_data("plotly_click", source = "inc_long"), get_clicked())
    
    
    # if anything changes in df(), reset the selection
    observeEvent(df_valuebox(), reset_selection())
    
    # render value boxes
    output$vbox_year <- renderValueBox({
      my_valueBox(
        df_valuebox()$REF_DATE[df_valuebox()$supp == selected_supp()], tr("lab_year"),
        icon = "calendar")
    })
    
    output$vbox_prov <- renderValueBox({
      my_valueBox(
        df_valuebox()$label2[df_valuebox()$supp == selected_supp()], tr("lab_geo"),
        icon = "map-marker")
    })
    
    output$vbox_trade <- renderValueBox({
      my_valueBox(
        df_valuebox()$label1[df_valuebox()$supp == selected_supp()], tr("lab_trade"),
        icon = "toolbox")
    })
    
    value_status_flag <- function(value, status, flag, is_percent = FALSE) {
      if (is.na(value)) {
        status
      } else {
        HTML(
          paste0(
            ifelse(is_percent,
                   format_pct(value, language()),
                   format_number(value, language())),
            "<sup>", flag, "</sup>", collapse = NULL))
      }
    }
    
    output$vbox_cohort <- renderValueBox({
      my_valueBox(
        value_status_flag(
          df_valuebox()$VALUE_1[df_valuebox()$supp == selected_supp()],
          df_valuebox()$STATUS_1[df_valuebox()$supp == selected_supp()],
          df_valuebox()$flag_1[df_valuebox()$supp == selected_supp()]
        ),
        tr("cohort"), icon = "users", size = "small")
    })
    
    
    output$vbox_tax_filers <- renderValueBox({
      my_valueBox(
        value_status_flag(
          df_valuebox()$real_taxfiler[df_valuebox()$supp == selected_supp()],
          df_valuebox()$real_taxfiler_status[df_valuebox()$supp == selected_supp()],
          df_valuebox()$real_taxfiler_flag[df_valuebox()$supp == selected_supp()],
          is_percent = TRUE
        ),
        tr("lbl_taxfiler"), size = "small",
        icon = "coins")
    })
    
    
    output$vbox_age_cert <- renderValueBox({
      my_valueBox(
        value_status_flag(
          df_valuebox()$VALUE_2[df_valuebox()$supp == selected_supp()],
          df_valuebox()$STATUS_2[df_valuebox()$supp == selected_supp()],
          df_valuebox()$flag_2[df_valuebox()$supp == selected_supp()]
        ),
        tr("age_cert"), size = "small",
        icon = "award")
    })
    
    
    
  }) # module func
}

# for testing

# income_long_demo <- function() {
#   ui <- fluidPage(
#     income_long_ui("income_long"))
#   server <- function(input, output, session) {
#     income_long_server("income_long", "en")
#   }
#   shinyApp(ui, server)
# }
# 
# 
# income_long_demo()


