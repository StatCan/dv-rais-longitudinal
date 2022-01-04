
income_cs_ui <- function(id) {
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(NS(id, 'direc_control')),
      uiOutput(NS(id, 'year_control')),
      uiOutput(NS(id, "time_control")),
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




income_cs_server <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
    
    # Preparation --------------------------------------------------------------
    # source("R/utils.R")
    # source("R/valuebox.R")
    
    # language <- reactiveVal("en")
    
    dictionary <- read.csv('dictionary/dict_income_cs.csv') %>%
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
        "https://github.com/StatCan/dv-rais-longitudinal/raw/master/data/income_cs.Rds",
        write_disk(tmp)
      )
    # check if the response was "successful" (200)
    if (resp$status_code == 200) {
      # then load the data from downloaded RDS file.
      full <- readRDS(tmp)
      unlink(tmp)
    } else {
      # if it was unsuccessful, use included data.
      full <- readRDS("data/income_cs.Rds")
    }    
    
    
    full_year2 <- full %>%
      filter(dim_ind  %in% c(1,2,3,4,7,8)) %>%
      mutate(dim_years = 1,
             dim_ind = (ifelse(dim_ind == 7,5,dim_ind)),
             dim_ind = (ifelse(dim_ind == 8,6,dim_ind))) 
    
    full_year5 <- full %>%
      filter(dim_ind  %in% c(1,2,5,6,9,10)) %>%
      mutate(dim_years = 2,
             dim_ind = (ifelse(dim_ind == 5,3,dim_ind)),
             dim_ind = (ifelse(dim_ind == 6,4,dim_ind)),
             dim_ind = (ifelse(dim_ind == 9,5,dim_ind)),
             dim_ind = (ifelse(dim_ind == 10,6,dim_ind))) 
    
    full <- rbind(full_year2,full_year5) %>%
      mutate(flag = paste(
        ifelse(is.na(STATUS) | STATUS == "..", " ", STATUS),
        ifelse(is.na(SYMBOL), " ", SYMBOL))) %>%
      pivot_wider(id_cols=c(REF_DATE, dim_geo, dim_sex, dim_trade,dim_mode,dim_years),
                  names_from=dim_ind, values_from=c(VALUE, STATUS, flag)) %>%
      subset(REF_DATE <= (max(.$REF_DATE) - 2)) %>% # keep only if they have at leat with 2 year's range
      as.data.frame() 
    
    #  sidebar widgets----------------------------------------------------------
    
    last_years <- full %>%
      subset(!is.na(VALUE_3),
             select = c(REF_DATE, dim_years)) %>%
      group_by(dim_years) %>%
      summarize(last_year = max(REF_DATE))
    
    # time (year after certification)
    output$time_control <- renderUI({
      radioButtons(
        inputId = NS(id, "time"),
        label = NULL,
        choices = setNames(1:2, tr("mem_year")),
        selected = 1
      )
    })
    
    # the most recent year (cohort) is used to define the range of
    # cohorts in the drop down menu.
    # but it depends on the time (year after certification)
    # so define "last_yr" as reactive
    last_yr <- reactive({
      req(input$time)
      
      last_years %>%
        filter(dim_years == input$time) %>%
        pull(last_year)
    })
    
    # to make the cohort selection persistent even if input$time changed,
    # define it as a reactiveVal.
    # initialize it with the most recent available cohort.
    selected_cohort <- reactiveVal(
      max(filter(full, !is.na(VALUE_3))$REF_DATE))
    
    # get the selected cohort value and update selected_cohort.
    get_cohort <- function() {
      selected_cohort(max(input$year))
    }
    
    # reset the stored value when the selected_chort is invalid.
    reset_cohort <- function() {
      selected_cohort(last_yr())
    }
    
    # observe changes in input$year and update the stored value in selected_cohort.
    observeEvent(input$year, get_cohort())
    # observe changes in input$time
    # if the stored value in selected_cohort is invalid, reset it.
    observeEvent(input$time, {
      if (selected_cohort() > last_yr()) {reset_cohort()}
    })
    
    
    #  menu for year (cohort)
    # note last_yr() is used as it's reactive.
    # last_yr() appears first in the list, and it goes back to 2008.
    output$year_control <- renderUI({
      req(input$direc)
      if (input$direc == 3) {
        sliderTextInput(
          inputId = NS(id,"year"),
          label = tr("lab_cert_year"), 
          choices = c(2008:last_yr()),
          selected = c(2008, selected_cohort())
        )
      } else {
        pickerInput(
          inputId = NS(id, "year"),
          label = tr("lab_cert_year"),
          choices = c(last_yr():2008),
          selected = selected_cohort(),
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
       req(input$year, input$time, input$direc)
      
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
      
      df <- full %>%
        subset(
          REF_DATE %in% selected_year &
            dim_geo %in% selected_geo &
            dim_sex == input$sex &
            dim_mode == input$mode &
            dim_years == input$time &
            dim_trade %in% selected_trades) %>%
        arrange(dim_geo, dim_trade, desc(REF_DATE)) %>%
        mutate(
          real_taxfiler        = (if(input$sei) VALUE_3 else VALUE_5),
          real_taxfiler_status = (if(input$sei) STATUS_3 else STATUS_5),
          real_taxfiler_flag   = (if(input$sei) flag_3 else flag_5),

          real_inc             = (if(input$sei) VALUE_4 else VALUE_6),
          real_inc_status      = (if(input$sei) STATUS_4 else STATUS_6),
          real_inc_flag        = (if(input$sei) flag_4 else flag_6),

          supp = c(1:max(nrow(.), 1)),
          label1 = refTrade()[dim_trade],
          label2 = refGeo()[dim_geo],
          label3 = as.character(REF_DATE))
    })
    
    # render the table for testing / debugging
    output$outtable <- renderTable(
      df()
    )
    
    # render the plotly plot
    output$outBarChart <- renderPlotly({
      req(input$direc)
      
      validate(need(all(is.na(c(df()$real_inc ))) == FALSE, message = tr("mesg_val") ))
      
      
      inc_text <- format_dollar(df()$real_inc, locale=language())

      
      if (input$direc != 3) {
        tick_label <- if (input$direc == 1) {df()$label1} else {df()$label2}

        fig <- plot_ly(
          x = replace_na(df()$real_inc, 0), y = df()$supp, name = tr("lbl_inc"), type = "bar",
          orientation = "h", marker = list(color = '332288'),
          text = paste0(inc_text, "<sup>", str_trim(df()$real_inc_flag), "</sup>"),
          textposition = "none",
          source = "inc",
          hovertemplate = paste0("%{y}", format_colon(locale=language()),
                                 "%{text}<extra></extra>")) %>%
          layout(
            separators = ifelse(language() == "en", ".,", ", "),
            yaxis = list(
              ticktext = tick_label,
              tickvals = df()$supp,
              autorange = "reversed"
            ),
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1.05, xanchor="left", x=0),
            xaxis = list(
              tickformat = ",~r"
            ))
      } else { #comparing over time
        fig <- plot_ly(
          x = df()$supp, y = replace_na(df()$real_inc, 0), name = tr("lbl_inc"), type = "bar",
          marker = list(color = '#387cb4'),
          text = paste0(inc_text, "<sup>", str_trim(df()$real_inc_flag), "</sup>"),
          textposition = "none",
          source = "inc",
          hovertemplate = paste0("%{x}", format_colon(locale=language()),
                                 "%{text}<extra></extra>")) %>%
         
          layout(
            separators = ifelse(language() == "en", ".,", ", "),
            xaxis = list(
              ticktext = df()$label3,
              tickvals = df()$supp,
              autorange = "reversed"
            ),
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1.05, xanchor="left", x=0),
            yaxis = list(
              tickformat = ",~r"
            ))
      }
    }) #renderPlotly
    
    # # creating value boxes to all indicators  -------------------------------
    selected_supp <- reactiveVal(1)
    
    get_clicked <- function() {
      clk <- event_data("plotly_click", source = "inc")
      if (is.null(clk)) {selected_supp(1)} else {
        if (input$direc == 3) { selected_supp(clk$x) } else { selected_supp(clk$y) }
      }
    }

    reset_selection <- function() {
      selected_supp(1)
    }

    # if click on plotly, read the selected index
    observeEvent(event_data("plotly_click", source = "inc"), get_clicked())

    # if anything changes in df(), reset the selection
    observeEvent(df(), reset_selection())

    # render value boxes
    output$vbox_year <- renderValueBox({
      my_valueBox(
        df()$REF_DATE[df()$supp == selected_supp()], tr("lab_cert_year"),
        icon = "calendar")
    })

    output$vbox_prov <- renderValueBox({
      my_valueBox(
        df()$label2[df()$supp == selected_supp()], tr("lab_geo"),
        icon = "map-marker")
    })

    output$vbox_trade <- renderValueBox({
      my_valueBox(
        df()$label1[df()$supp == selected_supp()], tr("lab_trade"),
        icon = "toolbox")
    })

    output$vbox_cohort <- renderValueBox({
      my_valueBox(
        value_status_flag(
          df()$VALUE_1[df()$supp == selected_supp()],
          df()$STATUS_1[df()$supp == selected_supp()],
          df()$flag_1[df()$supp == selected_supp()],
          locale = language()
        ),
        tr("cohort"), icon = "users", size = "small")
    })
    
    output$vbox_tax_filers <- renderValueBox({
      my_valueBox(
        value_status_flag(
          df()$real_taxfiler[df()$supp == selected_supp()],
          df()$real_taxfiler_status[df()$supp == selected_supp()],
          df()$real_taxfiler_flag[df()$supp == selected_supp()],
          is_percent = TRUE,
          locale = language()
        ),
        tr("lbl_taxfiler"), size = "small",
        icon = "coins")
    })
    

    output$vbox_age_cert <- renderValueBox({
      my_valueBox(
        value_status_flag(
          df()$VALUE_2[df()$supp == selected_supp()],
          df()$STATUS_2[df()$supp == selected_supp()],
          df()$flag_2[df()$supp == selected_supp()],
          locale = language()
        ),
        tr("age_cert"), size = "small",
        icon = "award")
    })

    
  }) # module func
}

# for testing

 # income_cs_demo <- function() {
 #   ui <- fluidPage(
 #     income_cs_ui("income_cs"))
 #   server <- function(input, output, session) {
 #     income_cs_server("income_cs", "en")
 #   }
 #   shinyApp(ui, server)
 # }
 # 
 # 
 # income_cs_demo()
 # 



 
