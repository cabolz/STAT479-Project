library("shiny")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")
library("RColorBrewer")

# Load in dataset, ordering by largest taxonomic subgroup
taxonomic_security = read_csv("./data/taxonomic_security.csv") %>% 
  rename(Num_Species = n,
         Subgroup = taxonomic_subgroup,
         County = county,
         `%Unsecure` = perc_not_secure_state) %>% 
  arrange(desc(Num_Species)) %>% 
  mutate_at(vars(Subgroup), list(~factor(., levels=unique(.))))

species_by_subgroup = read_csv("./data/species_by_subgroup.csv") %>% 
  arrange(desc(n_species)) %>% 
  mutate_at(vars(taxonomic_subgroup), list(~factor(., levels=unique(.))))

choropleth = read_csv("./data/choropleth.csv")

# Plot on top with the inputs on the bottom
ui <- fluidPage(

    plotlyOutput(outputId = "stateSecurePlot"),
    
    HTML("<br><br><br>"),
    
    mainPanel(
      p("This plot aims to show the correlation between the number of species in a county, 
        and the percentage of those species that are unsecure. Taxonomic subgroups are 
        selectable and control which data is plotted. Each point represents a county. The 
        x-value is the number of species belonging to that taxonomic subgroup that are found 
        in that county. The y-value is the percentage of species in that taxonomic subgroup 
        and county that are unsecure at a state level. Color corresponds to the taxonomic 
        subgroup, which is shown on mouseover."
        )
    ),
    
    HTML("<br><br><br><br><br><br><br>"),
    
    plotlyOutput(outputId = "allSecurePlot"),
    
    HTML("<br>"),
    
    mainPanel(
      p("The above graph is the same as the previous graph, except that information is plotted
      across all taxonomic subgroups, including those which were previously not available 
      for selection.")
    ),
    
    HTML("<br><br><br>"),
    
    plotlyOutput(outputId = "countyChoropleth"),
    
    HTML("<br><br><br>"), 
    
    plotlyOutput(outputId = "subgroupPlot")
    
  
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
      ) +
        scale_color_manual(values = colorRampPalette(brewer.pal(8, "Set1"))(12))
    
    # Remove ability to pan and zoom
    ggplotly(plot, width = 800, height = 450) %>% 
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
  
  output$subgroupPlot <- renderPlotly({
    
    plot = ggplot(species_by_subgroup, aes(x=n_species, y=taxonomic_subgroup)) +
      geom_col() +
      xlab("Number of Species") +
      ylab("Taxonomic Subgroup") +
      theme(axis.text.y = element_text(size=7))
    
    # Remove ability to pan and zoom
    ggplotly(plot, width = 700) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  # Create the scatterplot of number of species vs. percent unsecure state
  output$countyChoropleth <- renderPlotly({
    
    plot = ggplot(choropleth, aes(long, lat, group = County)) +
      geom_polygon(aes(fill = Species), colour = alpha("black", 1/2), size = 0.1)  +
      scale_fill_viridis_c() +
      theme_void() +
      theme(panel.grid = element_blank()) +
      labs (
        title = "Number of Species by County",
        fill = "County"
      )
    
    # Remove ability to pan and zoom
    ggplotly(plot, width = 600) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  
}

shinyApp(ui = ui, server = server)
