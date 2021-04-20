library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")
library("RColorBrewer")

# Load in dataset, ordering by largest taxonomic subgroup
taxonomic_security = read_csv("./data/taxonomic_security.csv") %>% 
  arrange(desc(Species)) %>% 
  mutate_at(vars(Subgroup), list(~factor(., levels=unique(.))))

species_by_subgroup = read_csv("./data/species_by_subgroup.csv") %>% 
  arrange(desc(Species)) %>% 
  mutate_at(vars(Subgroup), list(~factor(., levels=unique(.))))

choropleth = read_csv("./data/choropleth.csv")

# Plot on top with the inputs on the bottom
ui <- fluidPage(
  titlePanel("Group 11"),
  
  mainPanel("Caitlin Bolz, Faith Kulzer, Sam Peters, Steven Xia"),
  
  HTML("<br><br>"),
  
  mainPanel("Birds are a critical part of many ecosystems. They play an active role in pest 
        control, pollination, and seed dispersion that is vital to keeping the balance of 
        nature. However, many species of birds are in decline and in danger of extinction 
        due to many threats, especially destruction and fragmentation of their natural habitat. 
        This project focuses on species diversity and conservation status of birds in New York, 
        and how these factors vary across the landscape. Our data was taken from the 2nd 
        New York State Breeding Bird Atlas which can be found at this link. Volunteer 
        birders recorded instances of different species of birds breeding in each county 
        in New York from 2000 to 2005."),

  HTML("<br><br><br><br><br><br><br><br><br>"),
    
  plotlyOutput(outputId = "stateSecurePlot"),
  
  HTML("<br><br><br>"),
  
  mainPanel("This plot aims to show the correlation between the number of species in a county, 
        and the percentage of those species that are not secure Taxonomic subgroups are 
        selectable and control which data is plotted. Each point represents a county. The 
        x-value is the number of species belonging to that taxonomic subgroup that are found 
        in that county. The y-value is the percentage of species in that taxonomic subgroup 
        and county that are not secure at a state level. Color corresponds to the taxonomic 
        subgroup, which is shown on mouseover."),
    
    
  HTML("<br><br><br><br><br><br><br><br><br>"),
  
  plotlyOutput(outputId = "allSecurePlot"),
    
  HTML("<br>"),
    
  mainPanel("The above graph is the same as the previous graph, except that information is 
        plotted across all taxonomic subgroups, including those which were previously not 
        available for selection."),
    
  HTML("<br><br><br>"),
    
  plotlyOutput(outputId = "countyChoropleth"),
    
  HTML("<br><br><br>"), 
    
  mainPanel("Graph Description"),
    
  HTML("<br><br><br>"),
    
  plotlyOutput(outputId = "subgroupPlot"),
  
  HTML("<br>"),
    
  mainPanel("This graph provides information on the number of species found in New York for 
        each taxonomic subgroup. The bars show the subgroup along the y-axis and the 
        number of species present in New York on the x-axis. The user can hover over a 
        taxonomic subgroup to show more specifically the number of species in that group."),
    
  HTML("<br><br><br>")
  
)

server <- function(input, output) {
  
  # Create the scatterplot of number of species vs. percent not secure state by taxonomic subgroup
  output$stateSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(Subgroup != "All") %>% 
      ggplot(aes(x = Species, y = `%Not Secure`, name = County, col = Subgroup)) + 
      geom_jitter(alpha = 0.5, size = 1, width = 0.3, height = 0.3) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "#f7f7f7")
      ) +
      labs (
        title = "Percent of Species Not Secure",
        x = "Number of Species of Subgroup",
        y = "% Not Secure of Subgroup"
      ) +
      scale_color_manual(values = colorRampPalette(brewer.pal(8, "Set1"))(12))
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 800, height = 450) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  # Create the scatterplot of number of species vs. percent not secure state with all subgroups combined
  output$allSecurePlot <- renderPlotly({
    
    plot = taxonomic_security %>% 
      filter(Subgroup == "All") %>% 
      ggplot(aes(x = Species, y = `%Not Secure`, name = County, col = Subgroup)) + 
      geom_jitter(alpha = 0.5, size = 1, width = 0.3, height = 0.3) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "#f7f7f7"),
        legend.position="none"
      ) +
      labs (
        title = "Percent of Species Not Secure",
        x = "Number of Species of Birds",
        y = "% Not Secure of all Birds"
      )
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 500) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  # Bar plot of the number of species for each taxonomic subgroup
  output$subgroupPlot <- renderPlotly({
    
    plot = ggplot(species_by_subgroup, aes(x= Species, y = Subgroup, fill = Subgroup)) +
      geom_col() +
      labs(
        x = "Number of Species",
        y = "Taxonomic Subgroup"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size=7),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      scale_x_continuous(breaks = seq(from = 0, to = 35, by = 5))
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 700, tooltip = c("x", "y")) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  # Choropleth map of NY counties, filling by the number of species
  output$countyChoropleth <- renderPlotly({
    
    plot = ggplot(choropleth, aes(long, lat, group = County)) +
      geom_polygon(aes(fill = Species), colour = alpha("black", 1/2), size = 0.1)  +
      labs (
        title = "Number of Species by County",
        fill = "Number of Species"
      ) +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      scale_fill_viridis_c(limits = c(50, 200))
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 700) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  
}

shinyApp(ui = ui, server = server)
