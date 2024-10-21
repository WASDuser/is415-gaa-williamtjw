library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

navbarPage(
    title = "",
    fluid = TRUE,
    theme=shinytheme("flatly"), # pre-made CSS: https://rstudio.github.io/shinythemes/
    id = "",
,
    navbarMenu(),
    navbarMenu(),
    navbarMenu(
        "Tab",
        tabPanel("Local Moran",
            
            sidebarLayout(
                
                sidebarPanel(
                    
                    selectInput(
                        inputId = "variable",
                        label = "Mapping variable",
                        choices = list( # need to match with data
                            "" = ""),
                        selected = ""),


                    radioButtons(
                        inputId = "",
                        label = "",
                        choices = c( 
                            "" = ''),
                        selected = "TRUE",
                        inline = TRUE),


                    sliderInput(
                        inputId = "", 
                        label = "", 
                        min = 1, 
                        max = 2,
                        value = 99, 
                        step = 100),


                    # when site is first loaded, user sees nothing
                    # upon click, the plot is displayed
                    actionButton("MoranUpdate", "Update Plot"),
                    hr(),
                    ),


                
                # OUTPUT - Show a plot of the generated distribution
                mainPanel( # display of 2 maps
                    fluidRow(
                        column(6, tmapOutput("")),
                        column(6, tmapOutput(""))
                    )
                )
            )
        ),
    ),
)
