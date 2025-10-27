function(input, output, session) {
  
####################################   A4   ####################################
  

  # Variable pour ranger la data selon le choix de l'utilisateur
  choix_A4 <- reactive({
    temp <- data_enquete_A4
    # Filtre sur le poste
    if (input$Choix_poste_A4 == "Tous les postes") {
      temp <- temp
    } else {
      temp <- temp %>% filter(A3_poste == input$Choix_poste_A4)
    }
    # Filtre sur le genre
    if (input$Choix_genre_A4 == "Tous") {
      temp <- temp
    } else {
      if (input$Choix_genre_A4 == "Homme") {
        temp <- temp%>% filter(Y1A_genre == "Un homme")
      }
      if (input$Choix_genre_A4 == "Femme") {
        temp <- temp%>% filter(Y1A_genre == "Une femme")
      }
    }
    return(temp)
  })
  
  choix_A4_fusion <- reactive({
    temp <- data_enquete_A4_fusion
    # Filtre sur le poste
    if (input$Choix_poste_A4 == "Tous les postes") {
      temp <- temp
    } else {
      temp <- temp %>% filter(A3_poste == input$Choix_poste_A4)
    }
    # Filtre sur le genre
    if (input$Choix_genre_A4 == "Tous") {
      temp <- temp
    } else {
      if (input$Choix_genre_A4 == "Homme") {
        temp <- temp%>% filter(Y1A_genre == "Un homme")
      }
      if (input$Choix_genre_A4 == "Femme") {
        temp <- temp%>% filter(Y1A_genre == "Une femme")
      }
    }
    return(temp)
  })
  
  A4_react <- reactiveValues(data = NULL)
  couleur_A4 <- brewer.pal(12, "Set3") 
    
  # Commande reliant le bouton "Autres secteurs"  
  observeEvent(list(input$secteur2, input$Choix_poste_A4, input$Choix_genre_A4), {
    A4_react$data <- choix_A4() %>%
      filter(!is.na(A4_secteur_autre)) %>%
      count(A4_secteur_autre) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur_autre = as_factor(A4_secteur_autre)) %>% 
      dplyr::rename("Secteur"= "A4_secteur_autre")
  })
  # Commande reliant le bouton "Tous les secteurs"  
  observeEvent(list(input$secteur3, input$Choix_poste_A4, input$Choix_genre_A4), {
    A4_react$data <- choix_A4_fusion() %>%
      filter(!is.na(A4_secteur)) %>%
      count(A4_secteur) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur = as_factor(A4_secteur)) %>% 
      dplyr::rename("Secteur"= "A4_secteur")
  })  
  # Commande reliant le bouton "Secteurs principaux"  
  observeEvent(list(input$secteur1, input$Choix_poste_A4, input$Choix_genre_A4), {
    A4_react$data <- choix_A4() %>%
      filter(!is.na(A4_secteur)) %>%
      count(A4_secteur) %>%
      arrange(desc(n)) %>% 
      mutate(A4_secteur = as_factor(A4_secteur)) %>% 
      dplyr::rename("Secteur"= "A4_secteur")
  })
  
  
  # Commandes pour les sorties
  
    perimetre_A4 <- reactive({
    temp <- A4_react$data
    return(temp)
  })

  output$plot_A4 <- renderPlot({
    if (is.null(A4_react$data)) return()
    perimetre_A4()  %>% 
      ggplot() +
      geom_col(aes(x = Secteur, y = n, fill = Secteur),
               alpha = 0.7) +
      geom_text(aes(x = Secteur, y = n, label = n),
                hjust = 0.5,
                vjust = -0.5) +
      labs(
        title = "Répartition des secteurs d'activités",
        x = "Secteurs",
        y = "Réponses"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank()    
      )
  })
  
  output$plot_pie_A4 <- renderPlot({
    if (is.null(A4_react$data)) return()
    temp <- perimetre_A4()
    pie(temp$n, 
        labels = paste0(temp$Secteur, " (", temp$n, ")"),
        border = "white", 
        col = couleur_A4,
        main = "Répartition des secteurs d'activités")
  })
  
  output$table_A4 <- renderTable({
    if (is.null(A4_react$data)) return()
    perimetre_A4() %>% 
      dplyr::rename("Secteur d'activité"= "Secteur") %>% 
      dplyr::rename("Effectif" = "n")
  })
################################################################################ 
  
  
  
####################################   A6   ####################################  
  
  choix_A6 <- eventReactive(
    list(input$Choix_poste_A6, input$Choix_genre_A6),
    {
      temp <- data_enquete_A6
      
      # Filtre sur le poste
      if (input$Choix_poste_A6 == "Tous les postes") {
        temp <- temp
      } else {
        temp <- temp %>% filter(A3_poste == input$Choix_poste_A6)
      }
      # Filtre sur le genre
      if (input$Choix_genre_A6 == "Tous") {
        temp <- temp
      } else {
        if (input$Choix_genre_A6 == "Homme") {
          temp <- temp%>% filter(Y1A_genre == "Un homme")
        }
        if (input$Choix_genre_A6 == "Femme") {
          temp <- temp%>% filter(Y1A_genre == "Une femme")
        }
      }
      
      temp %>%
        filter(!is.na(A6_experience)) %>%
        count(A6_experience) %>%
        arrange(desc(A6_experience)) %>%
        mutate(A6_experience = as_factor(A6_experience)) %>%
        dplyr::rename("Experience" = "A6_experience")
    }
  )
  
  output$plot_A6 <- renderPlot({
      ggplot(choix_A6()) +
      geom_col(aes(x = Experience, y = n, fill = n),
               alpha = 0.7) +
      geom_text(aes(x = Experience, y = n, label = n),
                hjust = 0.5,
                vjust = -0.5) +
      labs(
        title = "Répartition des secteurs d'activités",
        x = "Années d'expérience",
        y = "Réponses"
      ) +
      theme_minimal()
  })
  
  output$table_A6 <- renderTable({
    choix_A6() 
  })
}
################################################################################ 
####################################   B1   ####################################
# Cas du logiciel tableau
fig <- plot_ly(
  domain = list(x = c(0, 10), y = c(0, 10)),
  value = sum(data_donnee$B1_utilisation_tableau == "Oui"),
  title = list(text = "Nombre de personnes utilisant tableau"),
  type = "indicator",
  mode = "gauge+number",
  gauge = list(
    axis = list(range = list(NULL, 491), tickwidth = 1, tickcolor = "darkblue"),
    bar = list(color = "darkblue")))
fig <- fig %>%
  layout(margin = list(l=20,r=30))


fig

################################################################################ 


####################################   B2   ####################################



################################################################################ 
