#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$plot_evolution <- renderPlot({
      # Graphique sur l'évolution du nbre de films par an ( sur un genre donné)
      if (input$Choix_genre != "Tous les genres") {
        data_allocine_plot = data_allocine %>% 
          filter(genre == input$Choix_genre) # Filtrer sur le genre choisi par l'utilisateur
      }
      else {
        data_allocine_plot = data_allocine # Pas de filtre si "tous les genres" choisis
      }
      
      data_allocine %>% 
        filter(genre == input$Choix_genre) %>% # filtrage sur le genre choisi par l'utilisateur 
        mutate(annee_sortie = year(date_sortie)) %>%  # year de lubridate , extraire l'année
        count(annee_sortie) %>% 
        ggplot() +
        geom_line(aes(x = annee_sortie, y=n)) +
        labs(title = "Evolution du nombre de films par an",
             subtitle = paste("Genre choisi:", input$Choix_genre))
        

    })

}
