library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

# Load in datasets
#counties_taxonomic = read_csv("./data/counties_taxonomic.csv")
taxonomic_security = read_csv("./data/taxonomic_security.csv")

taxonomic_groups = counties_taxonomic %>% distinct(taxonomic_group)

# Put two plots side-by-side with the inputs on the bottom
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "tax_group",
                  label = "Taxonomic Group",
                  choices = taxonomic_groups
      )
    ),
    
    mainPanel(
      plotlyOutput(outputId = "scatterplot")
    )
  )
)

server <- function(input, output) {
  
  
  
  
  
  
  # Create the scatterplot of number of species vs. percent not secure
  output$scatterplot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(taxonomic_group == input$tax_group,
             group_secure == input$tax_group) %>% 
      ggplot(aes(x = n, y = perc_secure, col = level, name = county)) + 
      geom_point(alpha = 0.6, size = 1) +
      theme_minimal() +
      labs (
        x = paste("Number of Species of", input$tax_group),
        y = paste("% Not Secure of ", input$tax_group)
      )
    
    # Remove ability to pan and zoom
    ggplotly(plot, height = 400) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  
  
  
  
  
  
  
  
  if (FALSE){
  # Create the scatterplot of number of species vs. percent not secure
  output$scatterplot <- renderPlotly({
    
    plot = counties_taxonomic %>% 
      filter(taxonomic_group == input$tax_group) %>% 
      ggplot(aes(x = n, y = perc_secure, col = level, name = county)) + 
        geom_point(alpha = 0.6, size = 1) +
        theme_minimal() +
        labs (
          x = paste("Number of Species of", input$tax_group),
          y = "% Not Secure"
        )
    
    # Remove ability to pan and zoom
    ggplotly(plot, height = 400) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  }
  
}

shinyApp(ui = ui, server = server)
