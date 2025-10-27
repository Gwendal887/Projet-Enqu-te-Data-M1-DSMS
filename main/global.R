############################# CHARGER LES PACKAGES #############################
#install.packages("readr")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("forcats")
#install.packages("lubridate")
#install.packages("shinythemes")
#install.packages("plotly")

library(readr) 
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(shiny)
library(shinythemes)
library(plotly)
library(stringr)
library(RColorBrewer)



################################ IMPORTATION ###################################

#data_enquete <- read_csv2("main/data/enquete_data_raw.csv")

data_enquete <- read_csv2("data/enquete_data_raw.csv")



################################  FONCTIONS  ###################################


############################## Données de base #################################

### Filtres sur les réponses attendues ###
filtre <- function(origin){
  new <- origin %>% 
    filter(!str_detect(A2_statut, "recherche d'emploi")) %>%  #Exclut les réponses "En recherche d'emploi"
    filter(!str_detect(A2_statut, "Etudiant")) %>%  #Exclut les réponses "Étudiant"
    filter(A3_poste !="Autre")  #Exclut les réponses venant des postes "Autres"
}

### Rename sur les postes ###
rename <- function(origin){
  # Change les "Chargé d’études" en "Data Analyst" 
  a <- 0
  new <- origin
  condition_rename <- str_detect(origin$A3_poste, "Chargé d’études")
  for (i in condition_rename){
    a <- a+1
    if (is.na(i)){
    }else{
      if(i){
        new$A3_poste[a] <- "Data Analyst / analyste"
      }
    }
  }
  # Change les "Statisticien" en "Data Scientist" 
  a <- 0
  condition_rename <- str_detect(origin$A3_poste, "Statisticien")
  for (i in condition_rename){
    a <- a+1
    if (is.na(i)){
    }else{
      if(i){
        new$A3_poste[a] <- "Data Scientist"
      }
    }
  }
  return(new)
}

#data_enquete_renamed <- rename(data_enquete)
#data_enquete_filtered <- filtre(data_enquete)
data_enquete_base <- rename(filtre(data_enquete))
#View(data_enquete_base)
################################################################################


################################ Question A4 ###################################
data_enquete_A4 <- data_enquete_base
#View(data_enquete_A4)

A4_allinone <- function(origin){
  a <- 0
  new <- origin
  condition_fusion <- str_detect(origin$A4_secteur, "Autre")
  for (i in condition_fusion){
    a <- a+1
    if (is.na(i)){
    }else{
      if(i){
        new$A4_secteur[a] <-  new$A4_secteur_autre[a]
      }
    }
  }
  return(new)
}

data_enquete_A4_fusion <- A4_allinone(data_enquete_A4)
#View(data_enquete_A4_fusion)
################################################################################

################################ Question A5 ###################################
### Transforme les Hybrides... en Hybrides et raccourcicement des reponses ###
data_donnee_A5 <- enquete_data_raw
for (i in 1:492){
  if (data_donnee_A5$A5_teleravail[i] == "Hybride avec en moyenne un ou deux jours de télétravail / semaine"){
    data_donnee_A5$A5_teleravail[i] = "Hybride"
  }
  else if (data_donnee_A5$A5_teleravail[i] =="Hybride avec en moyenne trois ou quatre jours de télétravail / semaine"){
    data_donnee_A5$A5_teleravail[i] = "Hybride"
  }
  else if (data_donnee_A5$A5_teleravail[i] == "Distanciel complet (pas de travail sur site)"){
    data_donnee_A5$A5_teleravail[i] = "Distanciel complet"
  }
  else if (data_donnee_A5$A5_teleravail[i] == "Présentiel complet (pas ou quasiment pas de travail à distance)"){
    data_donnee_A5$A5_teleravail[i] = "Présentiel complet"
  }
}

################################################################################

################################ Question A6 ###################################

data_enquete_A6 <- data_enquete_base

#View(data_enquete_A6)
################################################################################

























