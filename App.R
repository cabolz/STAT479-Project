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

# Plot on top with the inputs on the bottom
ui <- fluidPage(
    
  plotlyOutput(outputId = "stateSecurePlot"),

  mainPanel(
    p("This plot aims to show the correlation between the number of species in a county, 
      and the percentage of those species that are unsecure. Taxonomic subgroups are 
      selectable and control which data is plotted. Each point represents a county. The 
      x-value is the number of species belonging to that taxonomic subgroup that are found 
      in that county. The y-value is the percentage of species in that taxonomic subgroup 
      and county that are unsecure at a state level. Color corresponds to the taxonomic 
      subgroup, which is shown on mouseover."
      ),
    plotlyOutput(outputId = "allSecurePlot"),
    p("The above graph is the same as the previous graph, except that information is plotted
      across all taxonomic subgroups, including those which were previously not available 
      for selection.")
  )
  
  
  
)

server <- function(input, output) {
  
  # Create the scatterplot of number of species vs. percent unsecure state
  output$stateSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(Subgroup != "All") %>% 
      ggplot(aes(x = Num_Species, y = `%Unsecure`, name = County, col = Subgroup)) + 
      geom_jitter(alpha = 0.5, size = 1, width = 0.3, height = 0.3) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "#f7f7f7")#,
        #legend.position="none"
      ) +
      labs (
        title = "Percent of Species Unsecure",
        x = "Number of Species of Subgroup",
        y = "% Unsecure of Subgroup"
      )
    
    # Remove ability to pan and zoom
    ggplotly(plot) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  output$allSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(Subgroup == "All") %>% 
      ggplot(aes(x = Num_Species, y = `%Unsecure`, name = County, col = Subgroup)) + 
      geom_jitter(alpha = 0.5, size = 1, width = 0.3, height = 0.3) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "#f7f7f7"),
        legend.position="none"
      ) +
      labs (
        title = "Percent of Species Unecure",
        x = "Number of Species of Birds",
        y = "% Unsecure of all Birds"
      )
    
    # Remove ability to pan and zoom
    ggplotly(plot, width = 500) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
}

shinyApp(ui = ui, server = server)
