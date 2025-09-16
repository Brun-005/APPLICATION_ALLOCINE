
#________ PACKAGES ____________

library(readr) #___ Utilisation pour le chargement des fichiers plats
library(dplyr) #___ Tidyverse pour le traitement de données 
library(readxl) # tidyverse : excel
library(ggplot2) # tidyverse : graphique 
library(forcats)
library(lubridate) # tydyverse : date

#________ IMPORTATION DONNEES ____________

data_allocine = read_csv2("DATA FICHIER/data_allocine.csv")

#________ IMPORTATION DONNEES ____________

correspondances = read_excel("DATA FICHIER/correspondances_allocine.xlsx") %>% 
  rename(nationalite = nationalité )


#________ PREMIER APERCU DONNEES ____________

summary(data_allocine)  #____ Apercu variable numérique

glimpse(data_allocine)  #____ Structure des données ( son équivalent est str )

head(data_allocine, 10) #____ Affichage ders 10 premiers entetes du dataframe

class(data_allocine)    #____ Connaissance de la nature de l'objet

count(data_allocine, nationalite) #____ Nombre de films par nationalité

table(data_allocine$nationalite)  #____ Fonction historique, hors tidyverse

#____ SELECT : selection en colonnes
  #____ Suppression de la variable recompenses
   data_sans_recompenses = select(data_allocine, -recompenses)

#____FILTRER : sélectionner des lignes via une condition
  #____ Ne garder que les longs métrages dans data_sans_recompenses
    data_sans_recompenses = filter(data_sans_recompenses, type_film == "Long-métrage")
     #__Vérification 
    count(data_sans_recompenses, type_film)

#____ RENAME : renommer une variable
    #___ Renommer la variable titre en titre_film
    data_sans_recompenses = rename(data_sans_recompenses, titre_film = titre)
    #____Vérification
    glimpse(data_sans_recompenses)
    
#_____ ARRANGE : trier un dataframe / tibble sur une ou des colonne(s)
    #_____ Trier data_sans_recompenses selon l'id_film
    data_sans_recompenses = arrange(data_sans_recompenses, id_film)
    #Vérification
    head(data_sans_recompenses)
    
#_____ ENCHAINEMENT : Lister les 5 films italiens les plus longs
    # Sans utilisation de pipe 
    data_allocine_italien = filter(data_sans_recompenses, nationalite == "italien")
    data_allocine_italien = arrange(data_allocine_italien, desc(duree))
    head(data_allocine_italien, 5)
    
    # Utilisation du Pipe ( %>% ) control + SHIFT + M
    data_sans_recompenses %>% 
      filter(nationalite == "italien") %>%
      arrange(desc(duree)) %>%
      head(5)
    
    
#___ PETIT EXO 
    data_sans_recompenses %>% 
      filter(nationalite == "français" & note_presse >= 4 ) %>% 
      arrange(note_presse) %>% 
      select(titre_film,note_presse) %>% 
      write_excel_csv2("Liste_bons_films_fr.csv")

#____ JOINTURE 
    #___ Enrichir data_sans_recompenses avec les colonnes de correpsondances_allocine
    data_sans_recompenses = data_sans_recompenses %>% 
      left_join(correspondances, by = "nationalite") #Jointure de gauche , on garde tous les films
    # Vérification 
    glimpse(data_sans_recompenses)
    
#______ PETIT EXO : COMBIEN DE FILMS de drame ENN EUROPE DE L'OUEST OU DE L'EST
    data_sans_recompenses %>% 
      filter(genre == "Drame") %>% 
      count(region) %>% 
      arrange(desc(n)) %>% 
      filter(!is.na(region)) # Exlure les NA
    
#_____SUMMARIZE
    #__ Calcul de la moyenne des notes presse et l amoyenne des notes spectateurs 
    
    data_sans_recompenses %>% 
      summarize(
        nbre_films = n(), # comptage du nombre de lignes
        moyenne_presse = mean(note_presse, na.rm = TRUE),
        moyenne_spectateurs = mean(note_spectateurs, na.rm = TRUE)
      )
#____ GROUP BUY : en lien avce summarize, calculs par groupe
#____ Refaire les memes calculs, mais selon la région
    data_sans_recompenses %>% 
      group_by(region) %>% 
      summarize(
        nbre_films = n(), # comptage du nombre de lignes
        moyenne_presse = mean(note_presse, na.rm = TRUE),
        moyenne_spectateurs = mean(note_spectateurs, na.rm = TRUE)
      )

    
#_______ EXO 
#_ Calculer sur les films de l'europe de l'ouest
#_ la durée moyenne  et le nombre de films par genre 
#_ afficher les 5 genres ayant le plus de duree moyenne   
    
data_sans_recompenses %>% 
  filter(region == "Europe de l'ouest") %>% 
  group_by(genre) %>% 
  summarize(
    n = n(),
    duree_moyenne = mean(duree, na.rm = TRUE)
  ) %>% 
  arrange(desc(n)) %>% 
  head(5)


#___ MUTATE pour créer ou mette à jou rune colonne d'un dataframe
#___ Créer une colonne note_totale qui est la somme de presse et spectateurs

data_sans_recompenses = data_sans_recompenses %>% 
  mutate(
    note_totale = note_presse + note_spectateurs
  )
#___ Vérification 
data_sans_recompenses %>% summarize(
  note_totale_moyenne = mean(note_totale, na.rm = TRUE)
)


# MUTATE AVEC IFELSE : créer une variable d'un dataframe sous condition
# Créer une variable tr_note basée sur lla note totale : moins de 5, entre 5 et 7, plus de 7

data_sans_recompenses = data_sans_recompenses %>% 
  mutate(
    tr_note = if_else( is.na(note_totale), "Pas de note",
                       if_else(note_totale < 5, "Mauvais film", 
                               if_else(note_totale < 7, "Moyen Film", 
                                       "Bon Film")
                       ))
  )
# Vérification 

data_sans_recompenses %>% count(tr_note)




#__________________________ VISUALISATION ___________________________________________

# Premier graphique : nombre de films par région, représenté par des barres

data_sans_recompenses %>% 
  filter(!is.na)
count(region) %>%
  arrange(n) %>% 
  mutate(region = as_factor(region)) %>%  # Tranformer Région een facteur selon l'ordre des 
  ggplot() + # Fonction primordiale de base de tout graphique ggplot
  geom_col(aes(x=n, y = region, fill = region )) + # Geom pour faire des barres 
  labs(title = "Nombre de films par région", x = " Nombre de films", y = "") + #Labs pour titre 
  theme_minimal() # Thème : apparence générale du graphique

# Graphique avec DATE
# Représentation de l'évolutiondu nombre de films sortis par an 

data_sans_recompenses %>% 
  mutate(annee_sortie = year(date_sortie)) %>%  # year de lubridate , extraire l'année
  count(annee_sortie) %>% 
  ggplot() +
  geom_line(aes(x = annee_sortie, y=n))

# Exercice : représenter par des barres ordonnées, les notes globales médianes par genre de film 
# Uniquement sur les genres ayant plus de 100 films


data_sans_recompenses %>% 
  group_by(genre) %>% 
  summarize(
    n=n(),
    note_totale_mediane = median(note_totale, na.rm = TRUE)) %>% 
  filter(n>100) %>% 
  arrange(note_totale_mediane) %>% 
  mutate(genre = as_factor(genre)) %>% 
  ggplot() + 
  geom_col(aes(y = genre , x = note_totale_mediane), fill = "blue") +
  labs(title = "Note médiane par genre",
       substitle = "Genre avec plus de 100 films uniquement",
       x = "Note médiane", y = "") +
  theme_classic()
 