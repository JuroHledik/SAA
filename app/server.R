shinyServer(function(input, output, session){
  print("Initialize Start")
  id <- NULL
  
  #Import asset constraints:
  # constraints_df = data.frame(read_excel(paste0(path_data,"portfolio_constraints.xlsx")))
  constraints_df = read.csv(paste0(path_data,"portfolio_constraints.csv"), header=TRUE, sep=";", colClasses = c(vol_max = "character") )
  asset_names = constraints_df$asset
  asset_names_no_escape = str_replace(asset_names,"/", " ")
  K = length(asset_names_no_escape)
  col_assets = distinctColorPalette(K)
  
  # Reactive Values --------------------------
  values <- reactiveValues()
  values$data <- data.frame(stringsAsFactors = F)
  values$work_data <- data.frame(stringsAsFactors = F)
  values$pmf_data <- data.frame(stringsAsFactors = F)
  values$reject_line <- 0
  values$selected_columns <- c()
  
  values$results <-list()
  
  values$density_x_df <- data.frame(x_axis=numeric(), y_axis=numeric(), record_number=numeric(), stringsAsFactors=FALSE)
  values$density_x_pa_df <- data.frame(x_axis=numeric(), y_axis=numeric(), record_number=numeric(), stringsAsFactors=FALSE)
  values$w_df <- data.frame(index = numeric(), w=numeric(), record_number=numeric(), stringsAsFactors=FALSE)
  
  values$constraints_df = constraints_df
  
  # Global values --------------------------
  id_notification <- ""
  number_of_records = 0

  
  
  
  # Home page boxes
  output$button_upload <- renderInfoBox({
    infoBox("", a("File Upload", onclick = "openTab('file_upload')", href="#"),
            icon = icon("upload"), color = "light-blue"
    )
  })
  output$button_return_simulation <- renderInfoBox({
    infoBox("", a("Return Estimation", onclick = "openTab('return_simulation')", href="#"),
            icon = icon("gears"), color = "light-blue"
    )
  })
  output$button_return_correlations_visualization <- renderInfoBox({
    infoBox("", a("Pairwise Correlations", onclick = "openTab('return_correlations_visualization')", href="#"),
            icon = icon("yin-yang"), color = "light-blue"
    )
  })
  output$button_copula_visualization <- renderInfoBox({
    infoBox("", a("Copula Visualization", onclick = "openTab('copula_visualization')", href="#"),
            icon = icon("code-branch"), color = "light-blue"
    )
  })
  output$button_portfolio_optimization <- renderInfoBox({
    infoBox("", a("Portfolio Optimization", onclick = "openTab('portfolio_optimization')", href="#"),
            icon = icon("calculator"), color = "light-blue"
    )
  })
  output$button_insample_evaluation <- renderInfoBox({
    infoBox("", a("In-sample Evaluation", onclick = "openTab('insample_evaluation')", href="#"),
            icon = icon("chart-line"), color = "light-blue"
    )
  })
  output$button_menu_about <- renderInfoBox({
    infoBox("", a("About", onclick = "openTab('menu_about')", href="#"),
            icon = icon("question-circle-o"), color = "light-blue"
    )
  })
  output$button_github <- renderInfoBox({
    infoBox("", a("Github", href= "https://github.com/JuroHledik/SAA"),
            icon = icon("github"), color = "light-blue"
    )
  })
  
  
  # Dynamic UI by per method --------------------------
  
  output$ui_portfolio_optimization_maturities <- renderUI({
    if(input$portfolio_optimization_frequency == "daily"){
      column(4,
             p(HTML("<b>Maturity</b>"),span(shiny::icon("info-circle"), id = "info_maturity"),
               selectInput("maturity", NULL,
                           choices = maturities_choices_daily, selected = "1 year"
               ),
               tippy::tippy_this(elementId = "info_maturity",tooltip = "Maturity horizon",placement = "right")
             )
      )
    } else {
      if(input$portfolio_optimization_frequency == "weekly"){
        column(4,
               p(HTML("<b>Maturity</b>"),span(shiny::icon("info-circle"), id = "info_maturity"),
                 selectInput("maturity", NULL,
                             choices = maturities_choices_weekly, selected = "1 year"
                 ),
                 tippy::tippy_this(elementId = "info_maturity",tooltip = "Maturity horizon",placement = "right")
               )
        )
      } else {
        if(input$portfolio_optimization_frequency == "monthly"){
          column(4,
                 p(HTML("<b>Maturity</b>"),span(shiny::icon("info-circle"), id = "info_maturity"),
                   selectInput("maturity", NULL,
                               choices = maturities_choices_monthly, selected = "1 year"
                   ),
                   tippy::tippy_this(elementId = "info_maturity",tooltip = "Maturity horizon",placement = "right")
                 )
          )
        } else {
          if(input$portfolio_optimization_frequency == "quarterly"){
            column(4,
                   p(HTML("<b>Maturity</b>"),span(shiny::icon("info-circle"), id = "info_maturity"),
                     selectInput("maturity", NULL,
                                 choices = maturities_choices_quarterly, selected = "1 year"
                     ),
                     tippy::tippy_this(elementId = "info_maturity",tooltip = "Maturity horizon",placement = "right")
                   )
            )
          }
        }
      }  
    }
  })

  
  
  output$copula_structure_text <- renderText({
    filePath <- paste0(path_model_output_copula_structure,input$copula_visualization_frequency, "/", "summary.txt")
    fileText <- paste(readLines(filePath), collapse = "\n")
    fileText
  })

  #output the constraints datatable (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(values$constraints_df,
                  editable = TRUE,
                  rownames= FALSE,
                  options = list(pagingType = NULL,
                                 bPaginate=FALSE,
                                 searching = FALSE,
                                 ordering=FALSE,
                                 info=FALSE,
                                 lengthChagne=FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(constraints_df)-1)))
                                 )
                  ) %>%
    formatStyle(
      'ID',
      backgroundColor = styleEqual(1:K, col_assets)
    )
  })
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)

    #write values to reactive
    values$constraints_df[i,j+1] <- k
    print(values$constraints_df)
  })
  
  #render plot
  output$my_plot <- renderPlot({
    req(input$go) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    isolate(v$data) %>%  #don't react to any changes in the data
      ggplot(aes(x,y)) +
      geom_point() +
      geom_smooth(method = "lm")
  })
  
  output$ui_Omega_method_parameters1 <- renderUI({
    if(input$Omega_method == "fixed"){
      column(4,
        fluidRow(
          column(6, 
                 p(HTML("<b>Omega</b>"),span(shiny::icon("info-circle"), id = "info_Omega"),numericInput("Omega", NULL, 11520),
                   tippy::tippy_this(elementId = "info_Omega",tooltip = "Total portfolio size in EUR millions. If set to zero, Omega is determined endogenously.",placement = "right")
                 )
          ),
          column(6, 
                 p(HTML("<b>max_CVaR</b>"),span(shiny::icon("info-circle"), id = "info_Lambda"),numericInput("Lambda", NULL, 11520),
                   tippy::tippy_this(elementId = "info_Lambda",tooltip = "Maximum allowed CVAR (expected shortfall).",placement = "right")
                 )
          )   
        )
      )
    }
  })
  
  output$ui_theta3_method_parameters1 <- renderUI({
    if(input$theta3 > 0){
      column(2, 
             p(HTML("<b>alpha</b>"),span(shiny::icon("info-circle"), id = "info_alpha"),numericInput("alpha", NULL, 0.05),
               tippy::tippy_this(elementId = "info_alpha",tooltip = "Expected shortfall level",placement = "right")
             )
      ) 
    }
  })  
  
  output$ui_dlbtn <- renderUI({
    if(nrow(values$data) > 0){
      downloadButton("dl_data", "Download")
    }
  })
  
  output$ui_copula_types_portfolio_optimization <- renderUI({
    if(!is.null(input$return_model)){
      if("Pearson" %in% input$return_model){
        column(6, 
               p(HTML("<b>Copulas</b>"),span(shiny::icon("info-circle"), id = "info_allowed_copula_types_portfolio_optimization"),
                 checkboxGroupInput("allowed_copula_types_portfolio_optimization", NULL,
                                    allowed_copula_types),
                 tippy::tippy_this(elementId = "info_allowed_copula_types_portfolio_optimization", tooltip = "Select relevant copula families.", placement = "right")
               )
        )
      }
    }
  })
  
  # --------------------------
  # Insert invisible columns
  output$ui_invisible_columns <- renderUI({
    checkboxGroupInput(
      "invisible_columns", NULL, 
      choices = c("number_of_records" = "number_of_records","theta1" = "theta1", "theta2" = "theta2", "theta3" = "theta3", "theta4" = "theta4", "N_computation" = "N","N_visual" = "N_visual", "frequency" = "frequency", "return_model" = "return_model", "maturity" = "maturity",
                  "alpha" = "alpha", "Omega_method" = "Omega_method", "Omega" = "Omega", "min_return" = "min_return", "min_CVaR_return" = "min_CVaR_return", "max_CVaR" = "max_CVaR", 
                  "CVaR_return" = "CVaR_return", "CVaR_vol" = "CVaR_vol",
                  "P_neg_return" = "P_neg_return", "expected_profit" = "expected_profit",
                  "expected_return" = "expected_return", "variance" = "variance", "expected_return_pa" = "expected_return_pa", "w" = "w"), 
      selected = c("number_of_records", "theta1","theta2","theta3","theta4","N","N_visual", "frequency", "return_model", "maturity",
                    "Omega_method", "Omega", "CVaR_return", "CVaR_vol", 
                   "P_neg_return", "expected_profit", "expected_return", "variance" ),
      inline = T
    )
  })
  
  # --------------------------
  # User wishes to simulate returns:
  
  observeEvent(input$btn_returns, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      return_simulation_inputs = list()
      return_simulation_inputs$frequency = input$return_simulation_frequency
      return_simulation_inputs$selected_copula_types = input$selected_copula_types
      
      return_simulation(return_simulation_inputs)

    },
    message = function(m) {
      shinyjs::html(id = "text", html = paste0(m$message), add = TRUE)      
    })
  })
  
  # --------------------------
  # New record added in portfolio optimization:
  observeEvent(input$btn_go, {
    # from input
    theta1 = input$theta1
    theta2 = input$theta2
    theta3 = input$theta3
    theta4 = input$theta4    
    N = as.numeric(input$N)
    N_visual = as.numeric(input$N_visual)
    maturity = input$maturity
    if (theta3==0) {
      alpha = 0.05
    } else {
      alpha = input$alpha      
    }
    
    if (theta3==0) {
      alpha = 0.05
    } else {
      alpha = input$alpha      
    }
    
    alpha_cvar_total = input$alpha_cvar_total
    min_cvar_return = input$min_cvar_return
    Lambda = input$Lambda
    Omega = input$Omega
    min_return = input$min_return
    Omega_method = input$Omega_method
    frequency = input$portfolio_optimization_frequency
    return_model = input$return_model
    
    invisible_columns <- input$invisible_columns
    
    constraints_df = values$constraints_df
    
    if (Omega_method=="variable") {
      Omega = NA
      Lambda = NA
    }

    optimization_inputs = list()
    optimization_inputs$constraints_df = constraints_df
    optimization_inputs$theta1 = theta1
    optimization_inputs$theta2 = theta2
    optimization_inputs$theta3 = theta3
    optimization_inputs$theta4 = theta4    
    optimization_inputs$N = N
    optimization_inputs$N_visual = N_visual
    optimization_inputs$maturity = input$maturity
    optimization_inputs$alpha = alpha
    optimization_inputs$Omega = Omega
    optimization_inputs$min_return = min_return
    optimization_inputs$min_cvar_return = min_cvar_return
    optimization_inputs$Lambda = Lambda
    optimization_inputs$alpha_cvar_total = alpha_cvar_total
    optimization_inputs$frequency = frequency
    optimization_inputs$return_model = return_model
    
    
    #Optimize BITCH! :)
    results = portfolio_optimization(optimization_inputs)
    print(results$feasible)
    
    if (results$solvable==TRUE) {
      number_of_records <<- number_of_records + 1
      density_x = results$density_x
      density_x_pa = results$density_x_pa
      w = results$w
      w_string = 100*w[1]
      for (k in 2:K) {
        w_string = paste0(w_string, ", ", 100*w[k])  
      }
      Omega = round(results$Omega)
      cvar_return = paste0(as.character(round(results$cvar_return,4)*100),"%")
      cvar_vol = round(results$cvar_vol,2)
      P_neg_return = paste0(as.character(round(results$P_neg_return,4)*100),"%")
      expected_profit = round(results$expected_profit,2)
      expected_return = paste0(as.character(round(results$expected_return,4)*100),"%")
      expected_return_pa = paste0(as.character(round(results$expected_return_pa,4)*100),"%")    
      variance = results$variance
      
      # save
      values$work_data <- data.frame(number_of_records = number_of_records,
                                     theta1 = theta1,
                                     theta2 = theta2,
                                     theta3 = theta3,
                                     theta4 = theta4,                                   
                                     frequency = frequency,
                                     return_model = return_model,
                                     maturity = maturity,
                                     min_return = min_return,
                                     min_CVaR_return = min_cvar_return,
                                     max_CVaR = Lambda,
                                     Omega_method = Omega_method,
                                     Omega = Omega,
                                     N = N,
                                     N_visual = N_visual,
                                     alpha = alpha,
                                     expected_return = expected_return,
                                     CVaR_return = cvar_return,
                                     CVaR_vol = cvar_vol,
                                     P_neg_return = P_neg_return,
                                     expected_profit = expected_profit,
                                     variance = variance,
                                     expected_return_pa = expected_return_pa,
                                     w=w_string,
                                     stringsAsFactors = F)
      
      # bind data
      values$data <- bind_rows(values$data, values$work_data) 
      
      # Merge with other records' data
      density_x_df_temp = data.frame(x_axis=density_x$x, y_axis = density_x$y, stringsAsFactors = F)
      density_x_pa_df_temp = data.frame(x_axis=density_x_pa$x, y_axis = density_x_pa$y, stringsAsFactors = F)
      density_x_df_temp$record_number = number_of_records
      density_x_pa_df_temp$record_number = number_of_records
      colnames(density_x_df_temp) = c("x_axis","y_axis", "record_number")
      colnames(density_x_pa_df_temp) = c("x_axis","y_axis", "record_number")
      values$density_x_df = rbind(values$density_x_df, density_x_df_temp)
      values$density_x_pa_df = rbind(values$density_x_pa_df, density_x_pa_df_temp)
      
      w_df_temp = data.frame(index = 1:length(w), w=w, stringsAsFactors = F)
      w_df_temp$record_number = number_of_records
      colnames(w_df_temp) = c("index", "w", "record_number")
      values$w_df = rbind(values$w_df, w_df_temp)
    } else {
      if (results$error_type=="unfeasible") {
        id <<- showNotification(
          "This optimization problem is not feasible.",
          duration = 3, 
          closeButton = TRUE,
          type = "error"
        )
      } else {
        if (results$error_type=="not_enough_RAM") {
          id <<- showNotification(
            "Problem is too big. Reduce N_computation.",
            duration = 3, 
            closeButton = TRUE,
            type = "error"
          )
        }
      }
    }
    
  })
  # --------------------------
  #Remove Row of Table
  # --------------------------
  observeEvent(input$btn_remove,{
    if(nrow(values$data) > 0){
      values$data <- dplyr::slice(values$data, 1:nrow(values$data)-1)

      row_to_keep = which(values$density_x_df$record_number < number_of_records)
      values$density_x_df = values$density_x_df[row_to_keep,]
      
      row_to_keep = which(values$density_x_pa_df$record_number < number_of_records)
      values$density_x_pa_df = values$density_x_pa_df[row_to_keep,]
      
      row_to_keep = which(values$w_df$record_number < number_of_records)
      values$w_df = values$w_df[row_to_keep,]
      
      number_of_records <<- number_of_records - 1
    }
  })
  
  # --------------------------
  # Download CSV Table
  # --------------------------
  output$dl_data <- downloadHandler(
    filename = function() { 
      paste0("data-", format(Sys.time(),"%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(values$data, file)
    }
  )

  # --------------------------
  # Output : summary table
  # --------------------------
  output$kable_proportion <- function() {
    shiny::req(values$data)
    
    if(nrow(values$data) == 0){
      return(NULL)
    }
    temp_names = input$invisible_columns
    temp_names[temp_names=="number_of_records"] = "#"
    values$data %>%
      replace(is.na(.), "") %>%
      # #mutate(sampling_rate = paste0(round(sampling_rate * 100, 2),"%")) %>%
      mutate_if(is.numeric, function(x){formatC(x,format = "f", big.mark = ",", drop0trailing = T)}) %>%
      mutate(w = cell_spec(w, color = "blue")) %>%
      mutate(CVaR_return = cell_spec(CVaR_return, color = "blue")) %>%
      mutate(CVaR_vol = cell_spec(CVaR_vol, color = "blue")) %>%
      mutate(P_neg_return = cell_spec(P_neg_return, color = "blue")) %>%
      mutate(expected_profit = cell_spec(expected_profit, color = "blue")) %>%
      mutate(expected_return = cell_spec(expected_return, color = "blue")) %>%
      mutate(expected_return_pa = cell_spec(expected_return_pa, color = "blue")) %>%
      mutate(variance = cell_spec(variance, color = "blue")) %>%
      # #mutate(required_sample_size = cell_spec(required_sample_size, color = "blue")) %>%
      # #mutate(sampling_rate = cell_spec(sampling_rate, color = "blue")) %>%
      dplyr::select(input$invisible_columns) %>%
      save_to(num_cols, ncol) %>%
      knitr::kable(align = "r", escape = F, col.names = temp_names) %>% 
      kable_styling(c("striped", "bordered"), full_width = T) %>%
      collapse_rows(columns = 1:num_cols, valign = "top")
  }
  
  # --------------------------
  # Output : Portfolio weights comparison
  # --------------------------
  output$weights_comparison_plot <- renderPlotly({
    p <- plot_ly(type="bar") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>% 
      config(displayModeBar = F)
    p$elementId <- NULL
    # 
    # if(input$btn_go == 0 | !"weights_comparison_plot" %in% isolate(input$optional_plot)){
    #   return(p) 
    # }
    
    if(input$btn_go == 0){
      return(p) 
    }

    df_temp = values$w_df
    df_temp$asset = factor(df_temp$index)
    df_temp$record_number = factor(df_temp$record_number)
    g <- ggplot(df_temp, aes(fill=asset, y=w, x=record_number)) + 
      geom_bar(position="fill", stat="identity") +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) + 
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(values = col_assets) +
      labs(y = "Portfolio weights") +
      labs(x = "Record number")
    p <- ggplotly(g) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = F)
    p
  })
  
  # --------------------------
  # Output : Current portfolio weights
  # --------------------------
  output$weights_last_record_plot <- renderPlotly({
    p <- plot_ly(type="pie", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>% 
      config(displayModeBar = F)
    p$elementId <- NULL
 
#     if(!"weights_last_record_plot" %in% isolate(input$optional_plot)){
#       return(p) 
#     }
    
    df_temp = values$w_df[values$w_df$record_number==number_of_records,]
    df_temp$asset = factor(df_temp$index)
    drops <- c("index","record_number")
    df_temp = df_temp[ , !(names(df_temp) %in% drops)]

    p <- plot_ly(df_temp, labels = ~asset, values = ~w, type = 'pie', marker = list(colors = col_assets))
    
    p <- p %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    p$elementId <- NULL
    p
  })

  # --------------------------
  # Output : Density
  # --------------------------
  output$density_plot <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>% 
      config(displayModeBar = F)
    p$elementId <- NULL
    
    # if(input$btn_go == 0 | !"density_plot" %in% isolate(input$optional_plot)){
    #   return(p) 
    # }
    # 
    if(input$btn_go == 0 | number_of_records==0){
      return(p) 
    }

    df_temp = values$density_x_df
    df_temp$portfolio_number = factor(values$density_x_df$record_number)
    g <- ggplot(df_temp, aes(x = x_axis, y = y_axis, group = portfolio_number, color = portfolio_number)) + 
      geom_line(size = 0.5) +
      geom_point(shape = 21, size = 0.8) +
      theme_bw() +
      theme(legend.position = "top") + 
      labs(y = "Density") +
      labs(x = "Portfolio return density function") +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    p <- ggplotly(g) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = F)
    p$elementId <- NULL
    p
  })
  
  
  # --------------------------
  # Output : Density p.a.
  # --------------------------
  output$density_pa_plot <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>% 
      config(displayModeBar = F)
    p$elementId <- NULL
    
    # if(input$btn_go == 0 | !"density_plot" %in% isolate(input$optional_plot)){
    #   return(p) 
    # }
    # 
    if(input$btn_go == 0){
      return(p) 
    }
    
    df_temp = values$density_x_pa_df
    df_temp$portfolio_number = factor(values$density_x_pa_df$record_number)
    g <- ggplot(df_temp, aes(x = x_axis, y = y_axis, group = portfolio_number, color = portfolio_number)) + 
      geom_line(size = 0.5) +
      geom_point(shape = 21, size = 0.8) +
      theme_bw() +
      theme(legend.position = "top") + 
      labs(y = "Density") +
      labs(x = "Portfolio return density function") +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    p <- ggplotly(g) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = F)
    p$elementId <- NULL
    p
  })  

# --------------------------
# Output : Correlations plot
# --------------------------
output$return_visualization_correlations_plot <- renderPlotly({
  p <- plot_ly(type="scatter", mode = "markers") %>%
    layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F),
           yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>%
    config(displayModeBar = F)
  p$elementId <- NULL

  # if(input$btn_go == 0 | !"density_plot" %in% isolate(input$optional_plot)){
  #   return(p)
  # }
  #
  if(input$btn_visualization == 0){
    return(p)
  }
  models = input$return_visualization_model
  frequency = input$return_visualization_frequency
  load(file = paste0(path_model_output_simulated_returns,frequency, "/", "vine_copula/df_sim.Rdata"))
  load(file = paste0(path_model_output_simulated_returns,frequency, "/", "gaussian/df_sim_MG.Rdata"))
  load(file = paste0(path_model_output_historical_returns, frequency, "/", "df.Rdata"))
  df_temp = df
  df_temp$model = "Historical"
  df_temp = df_temp[0,]

  if ("Gaussian" %in% models) {
    df_temp_temp = df_sim_MG
    df_temp_temp$model = "Gaussian"
    df_temp = rbind(df_temp,df_temp_temp)
  }
  if ("Pearson" %in% models) {
    df_temp_temp = df_sim
    df_temp_temp$model = "Pearson"
    df_temp = rbind(df_temp,df_temp_temp)
  }
  if ("Historical" %in% models) {
    df_temp_temp = df
    df_temp_temp$model = "Historical"
    df_temp = rbind(df_temp,df_temp_temp)
  }

  lowerFn <- function(data, mapping, ...) {
    p <- ggplot(data = df_temp, mapping = mapping) +
      geom_point(size=0.1)
    p
  }
  
  g <- ggpairs(df_temp, columns = 1:K, ggplot2::aes(colour=model),
               lower = list(
                 continuous =  wrap(lowerFn) #wrap("smooth", alpha = 0.3, color = "blue", lwd=1) 
               ),)
  # g <- ggplot(df_temp, aes(x = x_axis, y = y_axis, group = portfolio_number, color = portfolio_number)) +
  #   geom_line(size = 0.5) +
  #   geom_point(shape = 21, size = 0.8) +
  #   theme_bw() +
  #   theme(legend.position = "top") +
  #   labs(y = "Density") +
  #   labs(x = "Portfolio return density function") +
  #   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  p <- ggplotly(g, width = 1000, height= 1000, dynamicTicks = TRUE) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = T)  %>% config(scrollZoom = TRUE)
  p <- p %>% layout(autosize = T)

  p$elementId <- NULL
  p
})


  
  
  
  
  
  
  ##############################################################################
  
  #                HISTORICAL HYPOTHETICAL PORTFOLIO EVOLUTION
  
  ##############################################################################
  
  # --------------------------
  # Output : Historical in-sample evaluation
  # --------------------------
  output$insample_evaluation_daily_plot <- renderPlotly({
    p <- plot_ly(type="scatter", mode = "markers") %>% 
      layout(xaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F), 
             yaxis = list(showticklabels = F, showline = F, zeroline = F, showgrid = F)) %>% 
      config(displayModeBar = F)
    p$elementId <- NULL
    
    # if(input$btn_go == 0 | !"density_plot" %in% isolate(input$optional_plot)){
    #   return(p) 
    # }
    # 
    if(input$btn_go == 0){
      return(p) 
    }
    
    #Import historical returns:
    load(paste0(path_model_output_historical_returns, "df_hist.Rdata"))
    df_temp = df_hist$Date
    print(summary(df_temp))
    max_record = max(values$w_df$record_number)
    for (record in 1:max_record) {
      w = values$w_df[values$w_df$record_number==record,"w"]
      temp = as.matrix(df_hist[,2:ncol(df_hist)]) %*% as.matrix(w)
      df_temp = cbind(df_temp, temp)
      print(ncol(df_temp))
    }
    df_temp = as.data.frame(df_temp)
    print(ncol(df_temp))
    column_names = c("Date", paste0('record',as.character(1:max_record)))
    names(df_temp) = column_names
    print(df_temp)
    for (record in 1:max_record) {
      df_temp[,record+1] = cumprod(as.numeric(df_temp[,record+1])+1)
    }
    
    print(summary(df_temp))
    
    p <- plot_ly(df_temp, x = ~Date)
    print(asset_names_no_escape)
    for (record in 1:max_record) {
      eval(parse(text = paste0(" p <- p %>% add_lines(y = ~",column_names[record+1],", name = 'portfolio ", record,"')"))) #Terrible way to do this honestly... Change it if you have a better idea
    }
    p <- p %>% layout(
      title = "",
      xaxis = list(
        rangeselector = list(
          buttons = list(
            list(
              count = 3,
              label = "3 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6,
              label = "6 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 yr",
              step = "year",
              stepmode = "backward"),
            list(
              count = 1,
              label = "YTD",
              step = "year",
              stepmode = "todate"),
            list(step = "all"))),
        rangeslider = list(type = "date")),
      yaxis = list(title = "Relative portfolio value")) %>% layout(legend = list(x = 0.8, y = 0.9)) %>% config(displayModeBar = F)
    p$elementId <- NULL
    p
  })      
  
    
  # --------------------------
  # Output : Summary of the optimization
  # --------------------------
  output$kable_error_matrix <- function() {
    df.error <- tibble(
      ` ` = c("Decision","Decision","Decision","Decision"),
      `  ` = c("Do not reject H0","Do not reject H0","Reject H0","Reject H0"),
      `H0 is True` = c("Correct<br>True Negative<br>", paste0(100*(1 - input$alpha),"%"), "TypeⅠ Error<br>False Positive<br>", paste0(100*input$alpha,"%")),
      `H1 is True` = c("TypeⅡ Error<br>False Negative<br>", paste0(100*(1 - input$alpha),"%"), "Correct<br>True Positive<br>", paste0(100*input$alpha,"%"))
    )
    df.error[2, 3] <- cell_spec(df.error[2, 3], "html", color = "#3A5FCD")
    df.error[4, 3] <- cell_spec(df.error[4, 3], "html", color = "#CD3333")
    df.error[2, 4] <- cell_spec(df.error[2, 4], "html", color = "#CD3333")
    df.error[4, 4] <- cell_spec(df.error[4, 4], "html", color = "#3A5FCD")
    df.error %>%
      mutate(` ` = cell_spec(` `, "html", angle = -90)) %>%
      knitr::kable(align = "c", escape = F) %>% 
      kable_styling(c("striped", "bordered"), full_width = T) %>%
      column_spec(1, bold = T, width = "1cm") %>%
      column_spec(2, bold = T) %>%
      collapse_rows(columns = 1:2, valign = "middle") %>%
      add_header_above(c("　" = 2, "Real" = 2), bold = T)
  }
  
  
  
  
  
  
  
  
  ##############################################################################
  
  #                          COPULA VISUALIZATION
  
  ##############################################################################
  
  
  
  
  ## function to load the RVM object
  f.loadData <- reactive({
      myData = new.env()
      load(paste0(path_model_output_copula_structure,input$copula_visualization_frequency, "/", "RVM.RData"), envir=myData)
      return(myData)
  })
  
  output$RVineTree <- renderSimpleNetwork({
    plotInput()
  })
  
  plotInput <- reactive({
    
    out2 <- f.loadData()
    
    if(!is.null(out2)){
      
      ## TODO: Name of the RVM-object may be different
      out <- eval(parse(text=paste0("out2$","RVM")))
      
      if(!is.null(out) & !is.null(input$tree) & !is.na(input$tree)){
        Matrix <- out$Matrix
        family <- out$family
        par <- out$par
        par2 <- out$par2
        names <- out$names
        d <- dim(Matrix)[1]
        if(is.null(names)) names <- paste0("V", 1:d)
        
        # define RVineMatrix object
        RVM <- RVineMatrix(Matrix = Matrix, family = family,
                           par = par, par2 = par2,
                           names = names)
        
        
        M <- RVM$Matrix
        edge.labels <- c("family")
        legend <- FALSE
        empTauMat <- matrix(NA, d, d)
        
        edges <- list()
        for (j in 1:(d - 1)) edges[[j]] <- array(NA, dim = c(d - j, 2, j))
        
        weight <- list()
        for (j in 1:(d - 1)) weight[[j]] <- rep(NA, d - j)
        
        if (edge.labels[1] != FALSE) {
          numlabels <- length(edge.labels)
          elabels <- list()
          for (j in 1:(d - 1)) elabels[[j]] <- matrix(NA, d - j, numlabels)
        }
        
        # initial edge
        edges[[1]][1, , ] <- sort(c(M[d - 1, d - 1], M[d, d - 1]))
        weight[[1]][1] <- ifelse(is.null(data), theoTauMat[d, d - 1], empTauMat[d, d - 1])
        if (edge.labels[1] != FALSE) {
          for (jj in 1:numlabels) {
            if (edge.labels[jj] == "family") 
              elabels[[1]][1, jj] <- BiCopName(RVM$family[d, d - 1],
                                               short = TRUE)
            if (edge.labels[jj] == "par") 
              elabels[[1]][1, jj] <- parMat[d, d - 1]
            if (edge.labels[jj] == "par2") 
              elabels[[1]][1, jj] <- parMat2[d, d - 1]
            if (edge.labels[jj] == "theotau") 
              elabels[[1]][1, jj] <- theoTauMat[d, d - 1]
            if (edge.labels[jj] == "emptau") 
              elabels[[1]][1, jj] <- empTauMat[d, d - 1]
            if (edge.labels[jj] == "pair") 
              if (legend == TRUE) {
                elabels[[1]][1, jj] <- paste(RVM$Matrix[d - 1, d - 1],
                                             RVM$Matrix[d, d - 1],
                                             sep = ",")
              } else {
                elabels[[1]][1, jj] <- paste(RVM$names[RVM$Matrix[d - 1, d - 1]],
                                             RVM$names[RVM$Matrix[d, d - 1]],
                                             sep = ",")
              }
          }
        }
        
        for (i in (d - 2):1) {
          
          # new edge in first tree
          ee <- sort(c(M[i, i], M[d, i]))
          edges[[1]][d - i, , ] <- ee
          weight[[1]][d - i] <- ifelse(is.null(data), theoTauMat[d, i], empTauMat[d, i])
          if (edge.labels[1] != FALSE) {
            for (jj in 1:numlabels) {
              if (edge.labels[jj] == "family") 
                elabels[[1]][d - i, jj] <- BiCopName(RVM$family[d, i], 
                                                     short = TRUE)
              if (edge.labels[jj] == "par") 
                elabels[[1]][d - i, jj] <- parMat[d, i]
              if (edge.labels[jj] == "par2") 
                elabels[[1]][d - i, jj] <- parMat2[d, i]
              if (edge.labels[jj] == "theotau") 
                elabels[[1]][d - i, jj] <- theoTauMat[d, i]
              if (edge.labels[jj] == "emptau") 
                elabels[[1]][d - i, jj] <- empTauMat[d, i]
              if (edge.labels[jj] == "pair") 
                if (legend == TRUE) {
                  elabels[[1]][d - i, jj] <- paste(RVM$Matrix[i, i],
                                                   RVM$Matrix[d, i],
                                                   sep = ",")
                } else {
                  elabels[[1]][d - i, jj] <- paste(RVM$names[RVM$Matrix[i, i]],
                                                   RVM$names[RVM$Matrix[d, i]],
                                                   sep = ",")
                }
            }
          }
          # edges in further trees
          for (k in 1:(d - i - 1)) {
            edges[[k + 1]][d - i - k, 1, ] <- ee
            
            # identify conditioned and conditioning sets
            if (length(M[(d - k):d, i]) >= 3) {
              if (setequal(M[(d - k):d, i], ee_old)) {
                edges[[k + 1]][d - i - k, 2, ] <- ee_old
              } else {
                for (j in 1:(d - i - k)) {
                  if (setequal(M[(d - k):d, i], edges[[k + 1]][j, 1, ])) 
                    edges[[k + 1]][d - i - k, 2, ] <- edges[[k + 1]][j, 1, ]
                  if (setequal(M[(d - k):d, i], edges[[k + 1]][j, 2, ])) 
                    edges[[k + 1]][d - i - k, 2, ] <- edges[[k + 1]][j, 2, ]
                }
              }
            } else {
              edges[[k + 1]][d - i - k, 2, ] <- sort(M[(d - k):d, i])
            }
            
            # create edge lables
            weight[[k + 1]][d - i - k] <- ifelse(is.null(data), theoTauMat[d - k, i], empTauMat[d - k, i])
            if (edge.labels[1] != FALSE) {
              for (jj in 1:numlabels) {
                if (edge.labels[jj] == "family") 
                  elabels[[k + 1]][d - i - k, jj] <- BiCopName(RVM$family[d - k, i], short = TRUE)
                if (edge.labels[jj] == "par") 
                  elabels[[k + 1]][d - i - k, jj] <- parMat[d - k, i]
                if (edge.labels[jj] == "par2") 
                  elabels[[k + 1]][d - i - k, jj] <- parMat2[d - k, i]
                if (edge.labels[jj] == "theotau") 
                  elabels[[k + 1]][d - i - k, jj] <- theoTauMat[d - k, i]
                if (edge.labels[jj] == "emptau") 
                  elabels[[k + 1]][d - i - k, jj] <- empTauMat[d - k, i]
                if (edge.labels[jj] == "pair") {
                  if (legend == TRUE) {
                    handle1 <- paste(RVM$Matrix[i, i], 
                                     RVM$Matrix[d - k, i],
                                     sep = ",")
                    handle2 <- paste(RVM$Matrix[(d - k + 1):d, i],
                                     collapse = ",")
                    handle3 <- paste(handle1, 
                                     handle2, 
                                     sep = ";")
                  } else {
                    handle1 <- paste(RVM$names[RVM$Matrix[i, i]], 
                                     RVM$names[RVM$Matrix[d - k, i]],
                                     sep = ",")
                    handle2 <- paste(RVM$names[RVM$Matrix[(d - k + 1):d, i]],
                                     collapse = ",")
                    handle3 <- paste(handle1, 
                                     handle2, 
                                     sep = ";")
                  }
                  elabels[[k + 1]][d - i - k, jj] <- handle3  #paste(handle1,handle2,sep=';')
                }
              }
            }
            
            # identify conditioned and conditioning sets
            ee <- c(sort(c(setdiff(ee, M[(d - k):d, i]), 
                           setdiff(M[(d - k):d, i], ee))),
                    sort(intersect(ee, M[(d - k):d, i])))
          }
          
          ee_old <- ee
          
        }
        
        # label the nodes
        if (legend == FALSE) {
          for (j in 1:(d - 1)) for (k in 1:d) edges[[j]][edges[[j]] == k] <- RVM$names[k]
        }
        
        # convert to edge lists
        edgelist <- list()
        for (j in 1:(d - 1)) edgelist[[j]] <- matrix(NA, d - j, 2)
        
        edgelist[[1]] <- matrix(as.character(edges[[1]][, , 1]), d - 1, 2)
        
        for (j in 1:(d - 2)) edgelist[[2]][j, ] <- c(paste(edges[[2]][j, 1, ], collapse = ","), 
                                                     paste(edges[[2]][j, 2, ], collapse = ","))
        
        # separate conditioned and conditioning sets
        if (d > 3) {
          for (i in 3:(d - 1)) {
            for (j in 1:(d - i)) {
              edgelist[[i]][j, 1] <- paste(paste(edges[[i]][j, 1, 1:2], collapse = ","),
                                           paste(edges[[i]][j, 1, 3:i], collapse = ","), sep = ";")
              edgelist[[i]][j, 2] <- paste(paste(edges[[i]][j, 2, 1:2], collapse = ","),
                                           paste(edges[[i]][j, 2, 3:i], collapse = ","), sep = ";")
            }
          }
        }
        
        # combine edge lables
        if (edge.labels[1] != FALSE) {
          elabels2 <- list()
          for (j in 1:(d - 1)) {
            elabels2[[j]] <- rep(NA, d - j)
            for (i in 1:(d - j)) elabels2[[j]][i] <- paste(elabels[[j]][i, ], collapse = ",")
          }
        }
        
        
        # Create data.frame for plot
        tree <- 1  # for test here the first tree
        src <- c(edgelist[[input$tree]][,1])
        target <- c(edgelist[[input$tree]][,2])
        
        networkData <- data.frame(src, target)
        
        # Plot
        simpleNetwork(Data=networkData, fontSize = 15)
      }
    }
    
  })
  
  
  ##############################################################################
  
  #                             FILE UPLOAD
  
  ##############################################################################
  
  output$file_upload_table_returns <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file_returns)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file_returns$datapath,
                       header = input$header_returns,
                       sep = input$sep_returns
                       )
        write.table(df,paste0(path_data,"historical_daily_returns.csv"), row.names = FALSE, sep=";")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp_returns == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$file_upload_table_constraints <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file_constraints)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file_constraints$datapath,
                       header = input$header_constraints,
                       sep = input$sep_constraints
        )
        write.table(df,paste0(path_data,"portfolio_constraints.csv"), row.names = FALSE, sep=";")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp_constraints == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  ##############################################################################
  
  #                         ABOUT DOCUMENTATION PDF
  
  ##############################################################################
  
  output$pdfview <- renderUI({
      tags$iframe(style="height:100vw; width:100vw; scrolling=yes", src="StrategicAssetAllocation_JurajHledik.pdf")  
  })
})













