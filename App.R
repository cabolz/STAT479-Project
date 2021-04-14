library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")

# Load in dataset, ordering by largest taxonomic subgroup
taxonomic_security = read_csv("./data/taxonomic_security.csv") %>% 
  rename(Num_Species = n,
         Subgroup = taxonomic_subgroup,
         County = county,
         `%Unsecure` = perc_not_secure_state) %>% 
  arrange(desc(Num_Species)) %>% 
  mutate_at(vars(Subgroup), list(~factor(., levels=unique(.))))

# Get a list of all the taxonomic subgroups
taxonomic_subgroups = taxonomic_security %>% distinct(Subgroup)

# Plot on top with the inputs on the bottom
ui <- fluidPage(
  
  splitLayout(
    
    plotlyOutput(outputId = "stateSecurePlot"),
    
    checkboxGroupInput(
      inputId = "tax_subgroup",
      label = "Taxonomic SubGroup",
      choices = taxonomic_subgroups[[1]],
      selected = taxonomic_subgroups[[1]][1],
      inline = FALSE
    )
    
  ),
  
  mainPanel(
    p("This plot aims to show the correlation between the number of species in a county, 
      and the percentage of those species that are not secure. Taxonomic subgroups  are 
      selectable and control which data is plotted. Each point represents a county. The 
      x-value is the number of species belonging to that taxonomic subgroup that are found 
      in that county. The y-value is the percentage of species in that taxonomic subgroup 
      and county that are not secure at a state level. Color corresponds to the taxonomic 
      subgroup, which is shown on mouseover. The \"All\" selection displays values combined 
      for all taxonomic subgroups, even those which are not available for selection here."
      )
  )
  
)

server <- function(input, output) {
  
  # Create the scatterplot of number of species vs. percent not secure state
  output$stateSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(Subgroup %in% input$tax_subgroup) %>% 
      ggplot(aes(x = Num_Species, y = `%Unsecure`, name = County, col = Subgroup)) + 
      geom_jitter(alpha = 0.5, size = 1, width = 0.3, height = 0.3) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "#f7f7f7"),
        legend.position="none"
      ) +
      labs (
        title = "Percent of Species not Secure",
        x = "Number of Species of Subgroup",
        y = "% Not Secure of Subgroup"
      )
    
    # Remove ability to pan and zoom
    ggplotly(plot) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
}

shinyApp(ui = ui, server = server)
