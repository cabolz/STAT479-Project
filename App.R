# Load/Install required packages
packages = c("shiny", "readr", "ggplot2", "dplyr", "plotly", "RColorBrewer")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

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
  
  h1("Biodiversity of Birds"),
  
  h4("Group 11: Caitlin Bolz, Faith Kulzer, Sam Peters, Steven Xia"),
  
  HTML("<br>"),
  
  h3("Introduction"),
  
  mainPanel(
    HTML("<p>Birds are a critical part of many ecosystems. They play an active role in pest 
          control, pollination, and seed dispersion that is vital to keeping the balance of 
          nature. However, many species of birds are in decline and in danger of extinction 
          due to many threats.In recorded history, 159 species of birds have already gone 
          extinct, and 1,481 species are currently considered vulnerable, endangered, or 
          critically <a href=\"https://www.iucnredlist.org/statistics/\"> endangered</a>. 
          This project focuses on species diversity and conservation status of birds in New York, 
          and how these factors vary across the landscape. This data can inform wildlife 
          managers of areas of greater concern for bird species, and can help them make a 
          better plan for how to protect our ecosystems. <p>")
  ),
  
  HTML("<br><br><br><br><br><br><br><br>"),
  
  h3("Data Processing"),
  
  mainPanel(
    HTML("<p>Our data was taken from the 2nd New York State Breeding Bird Atlas which can be 
          found <a href=\"https://data.ny.gov/Energy-Environment/Biodiversity-of-Birds-Distribution-by-County/4asw-6tmc\">here</a>. 
          Volunteer birders recorded instances of different species of birds breeding in each county 
          in New York from 2000 to 2005. Each species of animal is assigned a conservation 
          status rank, which is described in detail <a href=\"https://guides.nynhp.org/definitions/\">here</a>. 
          We condensed the 8+ variables into a binary variable of secure or not secure. 
          Essentially, rankings of 4 and 5 classified as secure, while rankings of 1, 2, 
          and 3 classified as not secure. Some species had a range of rankings. For 
          these species, we used the higher numbered, more-secure ranking. We then used 
          this binary variable to calculate the percentage of species that are not secure 
          by county and by taxonomic subgroup. The code and resulting processed datasets 
          can be found <a href=\"https://github.com/cabolz/STAT479-Project\">here</a>.<p>")
  ),
  
  HTML("<br><br><br><br><br><br><br><br><br>"),
    
  h3("Visualizations"),
  
  HTML("<br>"),
    
  plotlyOutput(outputId = "subgroupPlot"),
  
  HTML("<br>"),
    
  mainPanel("This graph provides information on the number of species found in New York for 
        each taxonomic subgroup. The bars show the subgroup along the y-axis and the 
        number of species present in New York on the x-axis. The user can hover over a 
        taxonomic subgroup to show the exact number of species in that group."),
    
  HTML("<br><br><br><br><br>"),
  
  plotlyOutput(outputId = "countyChoropleth"),
  
  HTML("<br>"),
  
  mainPanel("This map provides comparisons between the total number of 
        species for each county in New York. The number of species in each county has been 
        encoded in color, with more biodiverse counties displaying a lighter shade and 
        less biodiverse counties displaying a darker shade. Hovering over a county also 
        provides the human population density. A notable characteristic of this figure is 
        that the least biodiverse region of New York State appears to be at the furthest 
        southern tip of New York State, right where New York City is located (and 
        consequently the most densely populated region). Contrarily, the most biodiverse 
        region of New York appears to be located near the northern border of the state (the 
        most sparsely populated area)."),
  
  
  HTML("<br><br><br><br><br><br><br><br><br><br>"),
  
  plotlyOutput(outputId = "stateSecurePlot"),
  
  HTML("<br><br><br><br><br>"),
  
  mainPanel("This plot aims to show the correlation between the number of species in a county, 
        and the percentage of those species that are not secure. Taxonomic subgroups are 
        selectable (by clicking on their legend entry) and control which data is plotted. Each 
        point represents a county. The x-value is the number of species belonging to that 
        taxonomic subgroup that are found in that county. The y-value is the percentage of 
        species in that taxonomic subgroup and county that are not secure at a state level. 
        Color corresponds to the taxonomic subgroup, which is shown on mouseover."),
  
  
  HTML("<br><br><br><br><br><br><br><br><br>"),
  
  plotlyOutput(outputId = "allSecurePlot"),
  
  HTML("<br>"),
  
  mainPanel("The above graph is the same as the previous graph, except that information is 
        plotted across all taxonomic subgroups, including those which were previously not 
        available for selection."),
  
  HTML("<br><br><br>"),
  
  h3("Conclusion"),
  
  mainPanel("We can see that there is a loose trend between the number of bird species per 
        county and the percent that are not secure, with more diverse counties generally 
        containing more insecure species as well. However, we also saw that subgroups 
        like wood-warblers tend to be more diverse but have a fairly low level of insecurity, 
        while groups like ducks, geese, and waterfowl are less diverse but have a higher
        level of insecurity. Another unexpected observation is that there appears to exist 
        a trend in species diversity correlating with latitude. In southern counties, 
        there tends to be fewer breeding bird species, but northern counties tend to have 
        more breeding bird species. This could be because NYC, New York's largest 
        metropolitan area, is located at the southern tip of the state and that high levels 
        of urbanization may result in the habitat destruction of various bird species. 
        This could also be due to some of the Northern counties simply having more area, or 
        it could be related to the weather, climate, smaller human population density, or
        greater habitat availability of upstate New York."),
  
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
        title = "Percent of Species Not Secure by County",
        x = "Number of Species of Birds",
        y = "% Not Secure of all Birds"
      )
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 500) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE),
             yaxis=list(fixedrange=TRUE))
  })
  
  # Choropleth map of NY counties, filling by the number of species
  output$countyChoropleth <- renderPlotly({
    
    plot = ggplot(choropleth, aes(long, lat, group = County, name = Density)) +
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
      scale_fill_viridis_c(option = "magma", limits = c(50, 200))
    
    # Remove ability to pan and zoom, set plot dimensions
    ggplotly(plot, width = 700, tooltip = c("County", "Species", "Density")) %>% 
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
        y = "Taxonomic Subgroup",
        title = "Number of Species in each Subgroup"
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
  
}

shinyApp(ui = ui, server = server)
