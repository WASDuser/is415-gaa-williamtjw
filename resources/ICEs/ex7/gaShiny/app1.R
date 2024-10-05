pacman::p_load(shiny, sf, tmap, bslib, tidyverse, sfdep) # bslib: UI toolkit

hunan <- st_read(dsn = "data/geospatial", layer = "Hunan")
data <- read_csv("data/aspatial/Hunan_2012.csv")
hunan_profile <- left_join(hunan, data, by = c("County" = "County"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Notes:
  # always start with fluidPage() because its the most versatile
  # fluidRow : always rows
  # flowLayout : can be moved
  # sidebarLayout : LHS sidebar, RHS main panel, 12-col systems
  
  # 1. Layout Panel
  titlePanel('Chloropleth Mapping'),
  
  sidebarLayout(
    sidebarPanel(
      # 'Sidebar Panel here'
      
      # ------------ 2. INPUT ------------
      selectInput(
        inputId = 'variable',
        label = 'Mapping variable',
        
        # MUST MATCH DATA 
        choices = list(
          "Gross Domestic Product, GDP" = "GDP",
          "Gross Domestic Product Per Capita" = "GDPPC",
          "Gross Industry Output" = "GIO",
          "Output Value of Agriculture" = "OVA",
          "Output Value of Service" = "OVS"
        ),
        selected = 'GDPPC'
      ),
      sliderInput(
        inputId = 'classes',
        label = 'Number of classes',
        min = 5, max = 10, value = c(6)
        )
      ),
    # ------------ 3. OUTPUT ------------
    mainPanel(
      # 'Map Display here'
      tmapOutput(
        'mapPlot', # ID
        width = '100%',
        height = 580
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # ------------ 4. RENDER ------------
  output$mapPlot <- renderTmap({ # render func matches to output func
    tmap_options(check.and.fix = TRUE) + # avoid geometric errors
      tm_shape(hunan_profile) + 
      tm_fill(
        input$variable,
        n = input$classes, # 'MUST BE SAME AS inputID
        style = 'quantile', # hardcoded ver
        palette = blues9, # hardcoded ver
        ) +
      tm_borders(lwd = 0.1, alpha = 1)
    
    # output$mapPlot <- renderTmap({
    #   tmap_options(check.and.fix = TRUE) +
    #     tm_shape(hunan_profile)+
    #     tm_fill(input$variable,
    #       n = input$classes,
    #       style = input$classification,
    #       palette = input$colour,
    #       alpha = input$opacity) +
    #     tm_borders(lwd = 0.1,  alpha = 1) +
    #     tm_view(set.zoom.limits = c(6.5, 8)
    #     )
    # })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
