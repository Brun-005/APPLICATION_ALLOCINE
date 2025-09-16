#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title pour modifier le titre de l'appli
    titlePanel("Apllication Allocine"),

    #Barre latérale avec un bouton de choix du genre
    sidebarLayout(
        sidebarPanel(

          selectInput(inputId = "Choix_genre", "Choisissez le genrre:",
                      choices = c("Tous les genres", unique(data_allocine$genre)))
        ),

        #Affichage du graphique d'évolution du nombre de films par an
        mainPanel(
            plotOutput("plot_evolution")
        )
    )
)
