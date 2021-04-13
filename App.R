library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

# Load in datasets
#counties_taxonomic = read_csv("./data/counties_taxonomic.csv")
# Remove:
# Shrikes, Loons, Parrots and Parakeets, Larks, Maxwings, Starlings, Old World Sparrows,
# Kingfishers, Grebes, Gnatcatchers, Creepers, Cormorants, Nuthatches, Kinglets, 
# Hummingbirds and Swifts, Cuckoos, Nightbirds, Pigeons and Doves, Chickadees and Titmice
taxonomic_security = read_csv("./data/taxonomic_security.csv") %>% 
  arrange(desc(n)) %>% 
  mutate_at(vars(taxonomic_subgroup), list(~factor(., levels=unique(.))))

taxonomic_subgroups = taxonomic_security %>% distinct(taxonomic_subgroup)

# Plot on top with the inputs on the bottom
ui <- fluidPage(
  
  
  plotlyOutput(outputId = "stateSecurePlot"),
  
  checkboxGroupInput(
    inputId = "tax_subgroup",
    label = "Taxonomic SubGroup",
    choices = taxonomic_subgroups[[1]],
    selected = taxonomic_subgroups[[1]][1],
    inline = TRUE
  )
)

server <- function(input, output) {
  
  # Create the scatterplot of number of species vs. percent not secure state
  output$stateSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(taxonomic_subgroup %in% input$tax_subgroup) %>% 
      ggplot(aes(x = n, y = perc_not_secure_state, name = county, col = taxonomic_subgroup)) + 
      geom_jitter(alpha = 0.5, size = 1, width = 0.3, height = 0.3) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "#f7f7f7"),
        legend.position="none"
      ) +
      labs (
        title = "Not Secure State",
        x = "Number of Species of Subgroup",
        y = "% Not Secure of Subgroup"
      )
    
    # Remove ability to pan and zoom
    ggplotly(plot, height = 400) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
}

shinyApp(ui = ui, server = server)
