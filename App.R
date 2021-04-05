library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

# Load in datasets
#counties_taxonomic = read_csv("./data/counties_taxonomic.csv")
taxonomic_security = read_csv("./data/taxonomic_security.csv")

taxonomic_groups = taxonomic_security %>% distinct(taxonomic_group)

# Put two plots side-by-side with the inputs on the bottom
ui <- fluidPage(
  
  
  splitLayout(
    plotlyOutput(outputId = "stateSecurePlot"),
    plotlyOutput(outputId = "globalSecurePlot")
  ),
  
  selectInput(inputId = "tax_group",
              label = "Taxonomic Group",
              choices = taxonomic_groups
  )
)

server <- function(input, output) {
  
  # Create the scatterplot of number of species vs. percent not secure state
  output$stateSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(taxonomic_group == input$tax_group,
             group_secure == input$tax_group,
             level == "perc_not_secure_state") %>% 
      ggplot(aes(x = n, y = perc_secure, name = county)) + 
      geom_jitter(col = "#F8766D", alpha = 0.6, size = 1, width = 0.3) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs (
        title = "Not Secure State",
        x = paste("Number of Species of", input$tax_group),
        y = paste("% Not Secure of ", input$tax_group)
      ) +
      scale_color_manual(values = c("#F8766D"), labels = c("State"))
    
    # Remove ability to pan and zoom
    ggplotly(plot, height = 400) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  # Create the scatterplot of number of species vs. percent not secure state
  output$globalSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(taxonomic_group == input$tax_group,
             group_secure == input$tax_group,
             level == "perc_not_secure_global") %>% 
      ggplot(aes(x = n, y = perc_secure, name = county)) + 
      geom_jitter(color = "#00BFC4", alpha = 0.6, size = 1, width = 0.3) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs (
        title = "Not Secure Global",
        x = paste("Number of Species of", input$tax_group),
        y = paste("% Not Secure of ", input$tax_group)
      ) +
      scale_color_manual(values = c("#00BFC4"), labels = c("Global"))
    
    # Remove ability to pan and zoom
    ggplotly(plot, height = 400) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
}

shinyApp(ui = ui, server = server)
