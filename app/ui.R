shinyUI(
  dashboardPage(title = "JuroHledik_SAA",
    dashboardHeader(title = logo_grey_light, titleWidth = 200),
    dashboardSidebar(
      collapsed = T,
      width = 200,
      sidebarMenu(
        menuItem("Home", icon = icon("home"), tabName = "home"),
        menuItem("File Upload", icon = icon("upload"), tabName = "file_upload"),
        menuItem("Return Estimation", icon = icon("gears"), tabName = "return_simulation"),
        menuItem("Pairwise Correlations", icon = icon("yin-yang"), tabName = "return_correlations_visualization"),
        menuItem("Copula Visualization", icon = icon("code-branch"), tabName = "copula_visualization"),
        menuItem("Portfolio Optimization", icon = icon("calculator"), tabName = "portfolio_optimization"),
        menuItem("In-sample Evaluation", icon = icon("chart-line"), tabName = "insample_evaluation"),
        menuItem("Advanced settings", icon = icon("radiation"), tabName = "advanced_settings"),
        menuItem("About", icon = icon("question-circle-o"), tabName = "menu_about"),
        menuItem("  Github", icon = icon("github"), href = "https://github.com/JuroHledik/SAA")
      )
    ),
    dashboardBody(
      tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
      theme_grey_light,
      tabItems(
        tabItem(tabName = "home",
                fluidRow(class = "text-center",
                  column(width = 2, align="center"),
                  column(width = 8, align="center",
                    infoBoxOutput("button_upload"),
                    infoBoxOutput("button_return_simulation"),
                    infoBoxOutput("button_portfolio_optimization"),
                  ),
                  column(width = 2, align="center")
                ),
                fluidRow(class = "text-center",
                         column(width = 2, align="center"),
                         column(width = 8, align="center",
                                infoBoxOutput("button_return_correlations_visualization"),
                                infoBoxOutput("button_copula_visualization"),
                                infoBoxOutput("button_insample_evaluation"),
                         ),
                         column(width = 2, align="center")
                ),
                fluidRow(class = "text-center",
                         column(width = 2, align="center"),
                         column(width = 8, align="center",
                                infoBoxOutput("button_advanced_settings"),
                                infoBoxOutput("button_menu_about"),
                                infoBoxOutput("button_github")
                         ),
                         column(width = 2, align="center")
                )
                
        ),
        tabItem(tabName = "file_upload",
                fluidRow(
                  column(width = 4,
                         box(title = "Historical returns", width = 12, solidHeader = T, status = "primary",
                             column(width=4,
                                    # Input: Select a file ----
                                    fileInput("file_returns", "Choose CSV File",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    checkboxInput("header_returns", "Header", TRUE)                               
                             ),
                             column(width=4,
                                    radioButtons("sep_returns", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ",")                          
                             ),
                             column(width=4,
                                    # Input: Select number of rows to display ----
                                    radioButtons("disp_returns", "Display",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "head")                     
                             )
                         )
                  ),
                  column(width = 4,
                         box(title = "Portfolio constraints", width = 12, solidHeader = T, status = "primary",
                             column(width=4,
                                    # Input: Select a file ----
                                    fileInput("file_constraints", "Choose CSV File",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    checkboxInput("header_constraints", "Header", TRUE)                               
                             ),
                             column(width=4,
                                    radioButtons("sep_constraints", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ",")                          
                             ),
                             column(width=4,
                                    # Input: Select number of rows to display ----
                                    radioButtons("disp_constraints", "Display",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "all")                     
                             )
                         )
                  ),
                  column(width = 4,
                         box(title = "User imported returns", width = 12, solidHeader = T, status = "primary",
                             column(width=4,
                                    # Input: Select a file ----
                                    fileInput("file_user_imported_returns", "Choose CSV File",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    checkboxInput("header_user_imported_returns", "Header", TRUE)                               
                             ),
                             column(width=4,
                                    radioButtons("sep_user_imported_returns", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ",")                          
                             ),
                             column(width=4,
                                   p(HTML("<b>Maturity</b>"),span(shiny::icon("info-circle"), id = "info_maturity_user_imported_returns"),
                                     selectInput("maturity_user_imported_returns", NULL,
                                                 choices = maturities_choices_daily, selected = "1 year"
                                     ),
                                     tippy::tippy_this(elementId = "info_maturity_user_imported_returns",tooltip = "Maturity horizon",placement = "right")
                                   )
                             )
                         )
                  )
                ),
                fluidRow(
                  column(width = 12,
                           box(title = "Preview", width = 12, solidHeader = T, collapsible = T, collapsed = F, status = "primary",
                               column(width=12,
                                      tableOutput("file_upload_table_constraints")                       
                               ),
                               column(width=12,
                                      tableOutput("file_upload_table_returns")                               
                               ),
                               column(width=12,
                                      tableOutput("file_upload_table_user_imported_returns")                               
                               )
                                         
                           )
                  )
                ),
        ),
        tabItem(tabName = "return_simulation",
          fluidRow(
            column(width = 4,
              fluidRow(
                box(title = "Parameters", width = 12, solidHeader = T, status = "primary",
                  fluidRow(
                    column(6,
                           p(HTML("<b>Allowed copulas</b>"),span(shiny::icon("info-circle"), id = "info_selected_copula_types"),
                             checkboxGroupInput("selected_copula_types", NULL,
                                                allowed_copula_types, selected = allowed_copula_types),
                             tippy::tippy_this(elementId = "info_selected_copula_types", tooltip = "Select relevant copula families.", placement = "right")
                           )
                    ),
                    column(6,
                           fluidRow(
                             column(12, 
                                    p(HTML("<b>Return frequency</b>"),span(shiny::icon("info-circle"), id = "info_frequency"),
                                      radioButtons('return_simulation_frequency', NULL, choices  = frequencies, selected = "monthly", inline = F),
                                      tippy::tippy_this(elementId = "info_frequency",tooltip = "Frequency of return observation.",placement = "right")
                                    )
                             )
                           ),
                           # fluidRow(
                           #   column(12, 
                           #          p(HTML("<b>N_simulation</b>"),span(shiny::icon("info-circle"), id = "info_N_simulation"),numericInput('N_simulation', NULL, 1000000),
                           #            tippy::tippy_this(elementId = "info_N_simulation",tooltip = "Number of simulated returns out of which the final sample is uniformly selected. Has to be higher than the highest sample_size.",placement = "right")
                           #          )
                           #   )
                           # ),
                           # fluidRow(
                           #   column(12, 
                           #          p(HTML("<b>Sample size</b>"),span(shiny::icon("info-circle"), id = "info_sample_size"),
                           #            checkboxGroupInput("sample_sizes", NULL, choices=sample_sizes, selected = sample_sizes),
                           #            tippy::tippy_this(elementId = "info_sample_size", tooltip = "Select sample sizes of returns that you would like to simulate. Higher sample size produces more accurate results at the cost of longer computation time.", placement = "right")
                           #          )
                           #   )
                           # ),
                           # fluidRow(
                           #   uiOutput("ui_return_estimation_maturities")    
                           # ),
                           fluidRow(shinyjs::useShinyjs(), column(6, actionButton("btn_returns", "Generate returns")))
                    ),
                  )
                )
              )
            ),
            column(8,
              box(title = "Console Output", width = 12, solidHeader = T, status = "primary", 
  
                  verbatimTextOutput("text"),
                  tags$head(tags$style("#text{color:black; font-size:12px; font-style:italic;
                    overflow-y:scroll; max-height: 1000px; position: relative; background: ghostwhite;}"))
  
  
              )
            )
          )
        ),
        tabItem(tabName = "return_correlations_visualization",
                fluidRow(
                  column(width = 12,
                         fluidRow(
                           box(title = "Parameters", width = 12, solidHeader = T, status = "primary",
                               fluidRow(
                                          column(4, 
                                                 p(HTML("<b>Frequency</b>"),span(shiny::icon("info-circle"), id = "info_return_visualization_frequency"),
                                                   radioButtons('return_visualization_frequency', NULL, choices  = frequencies, selected = "monthly", inline = T),
                                                   tippy::tippy_this(elementId = "info__return_visualization_frequency",tooltip = "Frequency of return observation.",placement = "right")
                                                 )
                                          ),
                                          column(6,
                                                 p(HTML("<b>Model</b>"),span(shiny::icon("info-circle"), id = "info_return_visualization_model"),
                                                   checkboxGroupInput(
                                                     "return_visualization_model", NULL, 
                                                     choices = c("Historical" = "Historical", "Gaussian" = "Gaussian", "Pearson" = "Pearson"), 
                                                     selected = c("Historical","Gaussian","Pearson"),
                                                     inline = T
                                                   ),
                                                   tippy::tippy_this(elementId = "info_return_visualization_model",tooltip = "Used models.",placement = "right")
                                                 )
                                          ),
                                          shinyjs::useShinyjs(), column(6, actionButton("btn_visualization", "Show figures"))
                                        )
                                 ),
                               )
                         )
                   ),
                fluidRow(
                  column(12, align="center",
                         plotlyOutput("return_visualization_correlations_plot") %>% withSpinner(type = 5)
                  )
                )
        ),
        tabItem(tabName = "copula_visualization",
          fluidRow(
            column(12,
              box(title = "Parameters", width = 12, solidHeader = T, status = "primary", 
                  column(6, 
                         p(HTML("<b>Frequency</b>"),span(shiny::icon("info-circle"), id = "info_frequency"),
                           radioButtons('copula_visualization_frequency', NULL, choices  = frequencies, selected = "monthly", inline = T),
                           tippy::tippy_this(elementId = "info_frequency",tooltip = "Frequency of return observation.",placement = "right")
                         )
                  ),
                  column(6, 
                         p(HTML("<b>Tree</b>"),span(shiny::icon("info-circle"), id = "info_tree"),
                           numericInput("tree", "Which tree should be plotted?", min=1, value = 1),
                           tippy::tippy_this(elementId = "info_tree",tooltip = "Vine copula tree number.",placement = "right")
                         )
                  )
                )
              )
          ),
          fluidRow(
            column(6, 
              box(title = "Plot", width = 12, solidHeader = T, status = "primary", 
                simpleNetworkOutput("RVineTree"),
                tableOutput("table")#,
                #htmlOutput("text")
              )
            ),
            column(6, 
                   box(title = "Structure", width = 12, solidHeader = T, status = "primary", 
                       verbatimTextOutput("copula_structure_text")
                       #htmlOutput("text")
                   )
            )
          )
        ),
        tabItem(tabName = "portfolio_optimization",
                fluidRow(
                  column(width = 6,
                         fluidRow(
                           box(title = "Investor parameters", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                               fluidRow(
                                 column(3,
                                        p(HTML("<b>theta1 [return]</b>"),span(shiny::icon("info-circle"), id = "info_theta1"),numericInput('theta1', NULL, 1),
                                          tippy::tippy_this(elementId = "info_theta1",tooltip = "Expected return investor weight",placement = "right")
                                        )
                                 ),
                                 column(3,
                                        p(HTML("<b>theta2 [variance]</b>"),span(shiny::icon("info-circle"), id = "info_theta2"),numericInput('theta2', NULL, 1),
                                          tippy::tippy_this(elementId = "info_theta2",tooltip = "Variance investor weight",placement = "right")
                                        )
                                 ),
                                 column(3,
                                        p(HTML("<b>theta3 [CVaR]</b>"),span(shiny::icon("info-circle"), id = "info_theta3"),numericInput('theta3', NULL, 0),
                                          tippy::tippy_this(elementId = "info_theta3",tooltip = "Expected shortfall investor weight",placement = "right")
                                        )
                                 ),
                                 column(3,
                                        p(HTML("<b>theta4 [change]</b>"),span(shiny::icon("info-circle"), id = "info_theta4"),numericInput('theta4', NULL, 0),
                                          tippy::tippy_this(elementId = "info_theta4",tooltip = "Previous portfolio similiarity investor weight",placement = "right")
                                        )
                                 )
                               ),
                           ),
                         ),
                         fluidRow(
                           box(title = "Returns", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                               fluidRow(
                                 column(4,
                                        p(HTML("<b>Model</b>"),span(shiny::icon("info-circle"), id = "info_return_models"),
                                          radioButtons('return_model', NULL, choices  = return_models, selected = "Pearson", inline = F),
                                          tippy::tippy_this(elementId = "info_return_models", tooltip = "Select which return prediction model to use.", placement = "right")
                                        )
                                 ),
                                 uiOutput("ui_portfolio_optimization_frequencies"),
                                 uiOutput("ui_portfolio_optimization_maturities")
                               )
                           ),
                           box(title = "Optional Portfolio Constraints", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                               fluidRow(
                                 column(3,
                                        p(HTML("<b>min_return</b>"),span(shiny::icon("info-circle"), id = "info_min_return"),numericInput("min_return", NULL, -1),
                                          tippy::tippy_this(elementId = "info_min_return",tooltip = "Minimum expected return accepted by the investor",placement = "right")
                                        )
                                 ),
                                 column(3,
                                        p(HTML("<b>min_CVaR_return</b>"),span(shiny::icon("info-circle"), id = "info_min_cvar_return"),numericInput("min_cvar_return", NULL, -1),
                                          tippy::tippy_this(elementId = "info_min_cvar_return",tooltip = "Minimum expected return in the alpha % of the worst cases accepted by the investor",placement = "right")
                                        )
                                 ),
                                 uiOutput("ui_Omega_method_parameters1_Lambda"),
                                 uiOutput("ui_theta3_method_parameters1")
                               ),
                           ),
                         ),
                         fluidRow(
                           box(title = "Visualization", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                               fluidRow(
                                 column(4,
                                        p(HTML("<b>N_computation</b>"),span(shiny::icon("info-circle"), id = "info_N"),
                                          selectInput("N", NULL,
                                                      sample_sizes, selected=sample_sizes[length(sample_sizes)]
                                          ),
                                          tippy::tippy_this(elementId = "info_N",tooltip = "Sample size of simulated returns used to compute the optimal weights",placement = "right")
                                        )
                                 ),
                                 column(4,
                                        p(HTML("<b>N_visualization</b>"),span(shiny::icon("info-circle"), id = "info_N_visual"),
                                          selectInput("N_visual", NULL,
                                                      sample_sizes, selected=sample_sizes[length(sample_sizes)]
                                          ),
                                          tippy::tippy_this(elementId = "info_N_visual",tooltip = "Sample size of simulated returns used only to visualize the results",placement = "right")
                                        )
                                 ),
                                 column(4,
                                        p(HTML("<b>alpha_tot</b>"),span(shiny::icon("info-circle"), id = "info_alpha_cvar_total"),numericInput("alpha_cvar_total", NULL, 0.05),
                                          tippy::tippy_this(elementId = "info_alpha_cvar_total",tooltip = "At which level should the total CVAR be evaluated?",placement = "right")
                                        )
                                 )
                               )
                               ,
                               fluidRow(
                                 column(12, actionButton("btn_go", "Add Portfolio"))
                               )
                           ),
                         ),
                  ),
                  box(title = "Portfolio Constraints of Individual Asset Classes", width = 6, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                      column(6, radioButtons('Omega_method', "Total portfolio size Omega:", choices  = c("fixed", "variable"), selected = "fixed", inline = F)),
                      uiOutput("ui_Omega_method_parameters1_Omega"),
                      DTOutput("my_datatable")

                  ),
                  box(title = "Secondary Horizon Constraints", width = 6, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                      fluidRow(
                        column(4, radioButtons('secondary_horizon', "Include?", choices  = c("yes", "no"), selected = "no", inline = F)
                        ),
                        uiOutput("ui_portfolio_optimization_secondary_horizon_model_selection"),
                        uiOutput("ui_portfolio_optimization_maturities_secondary_horizon"),
                      ),
                      uiOutput("ui_secondary_horizon_constraints")
                  ),
                  
                  box(title = "History", width = 12, solidHeader = T, status = "success", collapsible = T, collapsed = F,
                      tableOutput("kable_proportion"),
                      fluidRow(
                        column(2, actionButton("btn_remove", "Remove Portfolio")),
                        column(1, uiOutput("ui_dlbtn")),
                        column(9, uiOutput("ui_invisible_columns"))
                      )
                  ),
                  tabBox(
                    title = "", width = 6,
                    id = "portfolio_optimization_tabset1",
                    tabPanel("Weights comparison", plotlyOutput("weights_comparison_plot") %>% withSpinner(type = 5)),
                    tabPanel("Weights last record", plotlyOutput("weights_last_record_plot") %>% withSpinner(type = 5))
                  ),
                  # tabBox(
                  #   title = "", width = 4,
                  #   id = "tabset3",
                  #   tabPanel("Weights last record", plotlyOutput("weights_last_record_plot") %>% withSpinner(type = 5))
                  # ),
                  tabBox(title = "", width = 6,
                         id = "portfolio_optimization_tabset2",
                         tabPanel("Return density", plotlyOutput("density_plot") %>% withSpinner(type = 5))
                  )
                )
        ),
        tabItem(tabName = "insample_evaluation",
                column(width = 12,
                       tabBox(
                         title = "", width = 12,
                         id = "insample_evaluation_tabset1",
                         tabPanel("Portfolio value", plotlyOutput("insample_evaluation_daily_plot", width = "auto", height = "768px") %>% withSpinner(type = 5))
                       )
                )
        ),
        tabItem(tabName = "advanced_settings",
                column(width = 6,
                  box(title = "Advanced Constraints", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F,
                      DTOutput("advanced_settings_datatable")
                      
                  ),
                ),
                column(width = 6,
                       box(title = "User Imported Returns - Secondary Horizon", width = 12, solidHeader = T, status = "primary",
                           column(width=4,
                                  # Input: Select a file ----
                                  fileInput("file_user_imported_returns_secondary", "Choose CSV File",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  checkboxInput("header_user_imported_returns_secondary", "Header", TRUE)                               
                           ),
                           column(width=4,
                                  radioButtons("sep_user_imported_returns_secondary", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ",")                          
                           ),
                           column(width=4,
                                  p(HTML("<b>Horizon</b>"),span(shiny::icon("info-circle"), id = "info_maturity_user_imported_returns_secondary"),
                                    selectInput("maturity_user_imported_returns_secondary", NULL,
                                                choices = maturities_choices_daily, selected = "1 year"
                                    ),
                                    tippy::tippy_this(elementId = "info_maturity_user_imported_returns_secondary",tooltip = "Investment horizon",placement = "right")
                                  )
                           )
                       )
                ),
                column(width = 6,
                       box(title = "Preview", width = 12, solidHeader = T, collapsible = T, collapsed = F, status = "primary",
                           column(width=12,
                                  tableOutput("file_upload_table_user_imported_returns_secondary")                               
                           )
                           
                       )
                ),
        ),
        tabItem(tabName = "menu_about",
                # includeMarkdown("docs/about.md")
                column(width = 12,
                       uiOutput("pdfview")
                )
        )
      )
    )
  )
)