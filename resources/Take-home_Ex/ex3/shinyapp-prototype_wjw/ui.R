navbarPage(
    "Don't do crime",
    id = 'navbarID',
    fluid = TRUE,
    theme = shinytheme('sandstone'),
    collapsible = TRUE,
    
    tabPanel('Data',
        sidebarLayout(
            sidebarPanel(
                selectInput("dataset", "Choose Dataset:",
                    choices = list(
                        "crime_boundary" = "crime_boundary",
                        "WEST_MSIA" = "WEST_MSIA",
                        "EAST_MSIA" = "EAST_MSIA"
                        # "crime_district" = "crime_district",
                        # "pop_data" = "pop_data",
                        # "adm2_sf" = "adm2_sf"
                        )),


                radioButtons("display_option", "Display:",
                    choices = list(
                        "Preview" = "preview",
                        "str()" = "str",
                        "glimpse()" = "glimpse"))
            ),
            mainPanel(
                # Conditionally display data preview or text output
                conditionalPanel(
                    condition = "input.display_option == 'preview'",
                    DTOutput("data_table")
                ),
                conditionalPanel(
                    condition = "input.display_option != 'preview'",
                    verbatimTextOutput("code_output")
                ),
                tags$br(),
                textOutput("description_text"),
                tags$br(),
                tableOutput("summary_stats"),
            )
        )
    ),
    
    
    # navbarMenu('EDA',
    tabPanel('EDA',
        sidebarLayout(
            sidebarPanel(
                selectInput("dataset", "Choose Dataset:",
                    choices = list(
                        "crime_boundary" = "crime_boundary",
                        "WEST_MSIA" = "WEST_MSIA",
                        "EAST_MSIA" = "EAST_MSIA"
                        # "crime_district" = "crime_district",
                        # "pop_data" = "pop_data",
                        # "adm2_sf" = "adm2_sf"
                        )),
                
                selectInput("var_type", "Variable Type:",
                    choices = list(
                        "Categorical" = "cat",
                        "Continuous" = "cont")),
                
                uiOutput("var_select"),
                
                selectInput("chart_type", "Chart Type:",
                    choices = NULL,
                    selected = NULL),
                
                actionButton("submit_eda", "Generate Plot")),
            
            mainPanel(
                conditionalPanel(
                    condition = "input.submit_eda > 0",
                    plotOutput("eda_plot")
                ),
                
                conditionalPanel(
                    condition = "input.submit_eda > 0 && input.var_type == 'cont'",
                    verbatimTextOutput("summary_table")
                )
            )
        )
    ),
    # ),

    tabPanel('ESDA',
        sidebarLayout(
            sidebarPanel(
                conditionalPanel(
                    condition="input.tabs=='choroTab'", h3("Choropleth settings"),

                    actionButton("submit_esda", "Generate Map"),hr(),

                    selectInput("esda_dataset", "Choose Dataset:", 
                        choices = list(
                            "crime_boundary" = "crime_boundary",
                            "WEST_MSIA" = "WEST_MSIA",
                            "EAST_MSIA" = "EAST_MSIA"
                            )),
    
                    selectInput("esda_variable", "Select Variable for Choropleth:", 
                        choices = NULL,
                        selected = NULL),
    
                    selectInput("crime_type", "Select type of crime:",
                        choices = NULL,
                        selected = NULL),
    
                    selectInput("region", "Select region:",
                        choices = NULL,
                        selected = NULL),
    
                    selectInput("classification", "Classification option:",
                        choices = NULL,
                        selected = NULL),
    
                    tags$small("*log methods current do not work with NULL values"),
                    tags$br(),
                    tags$small("*continuous: cont,order,log"),
                    tags$br(),
                    tags$small("*categorical: the rest"),
                    tags$br(),tags$br(),
    
                    sliderInput("n_classes", "Number of classes",
                        min = 3, max = 10, value = c(5)),
                    # tags$br(),
                    # tags$br(),
                    # tags$small("test"),
    
                    selectInput("colors", "Color Palette:",
                        choices = NULL,
                        selected = NULL),
                    ),


            conditionalPanel(
                condition="input.tabs=='globalTab'", h3("Global settings"),
                
                actionButton("submit_global", "Update Plot"), hr(),
                
                tags$small("*only west works for now*"),
                selectInput("esda_dataset", "Choose Dataset:",
                    choices = list(
                        "crime_boundary" = "crime_boundary",
                        "WEST_MSIA" = "WEST_MSIA",
                        "EAST_MSIA" = "EAST_MSIA"
                    ), selected = "WEST_MSIA"),

                selectInput("global_time_period", "Select year:",
                    choices = NULL,
                    selected = NULL),

                selectInput("global_crime_type", "Select type of crime:",
                    choices = NULL,
                    selected = NULL),

                selectInput("global_region", "Select region:",
                    choices = NULL,
                    selected = NULL),

                radioButtons("global_contiguity", "Contiguity Method:",
                    choices = c(
                        "Queen" = TRUE,
                        "Rook" = FALSE),
                    selected = "TRUE",
                    inline = TRUE),

                selectInput("global_MoranWeights", "Spatial Weights Style",
                    choices = c(
                        "W: Row standardised" = "W",
                        "B: Binary" = "B",
                        "C: Globally standardised" = "C",
                        "U: C / no of neighbours" = "U",
                        "minmax" = "minmax",
                        "S: Variance" = "S"),
                    selected = "W"),

                sliderInput("global_MoranSims", "Number of Simulations:",
                    min = 49, max = 999, value = 99, step = 100),
            ),


            conditionalPanel(
                condition="input.tabs=='localTab'", h3("Local settings"),

                actionButton("MoranUpdate", "Update Plot"),hr(),

                tags$small("*only west works for now*"),
                selectInput("esda_dataset", "Choose Dataset:", 
                    choices = list(
                        "crime_boundary" = "crime_boundary",
                        "WEST_MSIA" = "WEST_MSIA",
                        "EAST_MSIA" = "EAST_MSIA"
                    ), selected = "WEST_MSIA"),

                selectInput("local_time_period", "Select year:",
                    choices = NULL,
                    selected = NULL),

                selectInput("local_crime_type", "Select type of crime:",
                    choices = NULL,
                    selected = NULL),

                selectInput("local_region", "Select region:",
                    choices = NULL,
                    selected = NULL),

                radioButtons("Contiguity", "Contiguity Method:",
                    choices = c(
                        "Queen" = TRUE, 
                        "Rook" = FALSE),
                    selected = "TRUE",
                    inline = TRUE),

                selectInput("MoranWeights", "Spatial Weights Style",
                    choices = c(
                        "W: Row standardised" = "W",
                        "B: Binary" = "B",
                        "C: Globally standardised" = "C",
                        "U: C / no of neighbours" = "U",
                        "minmax" = "minmax",
                        "S: Variance" = "S"),
                    selected = "W"),

                sliderInput("MoranSims", "Number of Simulations:", 
                    min = 49, max = 999, value = 99, step = 100),

                hr(),

                radioButtons("MoranConf", "Select Confidence level",
                    choices = c(
                        "0.95" = 0.05, 
                        "0.99" = 0.01),
                    selected = 0.05,
                    inline = TRUE),

                selectInput("LisaClass", "Select Lisa Classification",
                    choices = c(
                        "mean" = "mean",
                        "median" = "median",
                        "pysal" = "pysal"),
                    selected = "mean"),

                selectInput("localmoranstats", "Select Local Moran's Stat:",
                    choices = c(
                        "local moran(ii)" = "local moran(ii)",
                        "expectation(eii)" = "expectation(eii)",
                        "variance(var_ii)" = "variance(var_ii)",
                        "std deviation(z_ii)" = "std deviation(z_ii)",
                        "P-value" = "p_value"),
                    selected = "local moran(ii)")
            ),


        ),
            mainPanel(
                tabsetPanel(id='tabs',
                    
                    tabPanel("Crime Rate Choropleth", value = 'choroTab',
                        conditionalPanel(
                            condition = "input.submit_esda",
                            tmapOutput("choro_map"),
                        )
                    ),
                    tabPanel("Global", value = 'globalTab',
                        # Placeholder content for the second map
                        plotOutput("global_hist"),
                        DTOutput("global_output")  # Placeholder for future map
                    ),
                    tabPanel("Local", value = 'localTab',
                        # Placeholder content for the third map
                        conditionalPanel(
                            condition = "input.MoranUpdate",
                            tmapOutput("LocalMoranMap"),
                            tmapOutput("LISA"),
                        )
                    ),
                )
            )
        )
    ),

navbarMenu('Clustering',
    tabPanel('hclust'),
    tabPanel('clustGEO'),
    tabPanel('SKATER - *working*')))