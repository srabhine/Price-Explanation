# ==============================================================================
# APPLICATION R SHINY - CHALLENGE QRT 2023
# Auteur : Sammy RABHINE
# ==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(zoo)

# ==============================================================================
# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES 
# ==============================================================================

if(file.exists("X_train.csv") && file.exists("y_train.csv")) {
  X_train <- read_csv("X_train.csv", show_col_types = FALSE)
  y_train <- read_csv("y_train.csv", show_col_types = FALSE)
  
  # Fusion
  df_full <- X_train %>% 
    left_join(y_train, by = "ID") %>%
    mutate(DAY_ID = as.numeric(DAY_ID)) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  
} else {
  set.seed(123)
  df_full <- data.frame(
    ID = 1:1000,
    DAY_ID = rep(1:500, each=2),
    COUNTRY = rep(c("FR", "DE"), 500),
    TARGET = rnorm(1000, 0, 1),
    FR_DE_EXCHANGE = rnorm(1000, 0, 2), # Flux
    FR_WINDPOW = runif(1000, 0, 10),
    DE_WINDPOW = runif(1000, 0, 15),
    FR_SOLAR = runif(1000, 0, 5),
    DE_SOLAR = runif(1000, 0, 8),
    FR_CONSUMPTION = rnorm(1000, 50, 10),
    DE_CONSUMPTION = rnorm(1000, 60, 10)
  )
}

# Coordonnées approximatives pour la carte (Centres des pays)
geo_data <- data.frame(
  pays = c("France", "Allemagne"),
  lat = c(46.603354, 51.165691),
  lng = c(1.888334, 10.451526)
)

# ==============================================================================
# 2. UI (INTERFACE UTILISATEUR)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # En-tête
  dashboardHeader(title = ""),
  
  # Barre latérale
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue Cartographique", tabName = "map", icon = icon("globe")),
      menuItem("Distribution (Histo)", tabName = "distrib", icon = icon("chart-bar")),
      menuItem("Corrélations (Scatter)", tabName = "scatter", icon = icon("project-diagram")),
      menuItem("Séries Temporelles", tabName = "time", icon = icon("chart-line")),
      hr(),
      # Filtres globaux
      sliderInput("day_range", "Plage de Jours (DAY_ID):",
                  min = min(df_full$DAY_ID, na.rm=TRUE),
                  max = max(df_full$DAY_ID, na.rm=TRUE),
                  value = c(min(df_full$DAY_ID, na.rm=TRUE), max(df_full$DAY_ID, na.rm=TRUE))),
      selectInput("country_select", "Pays (pour Histo/Scatter):",
                  choices = c("France" = "FR", "Allemagne" = "DE"))
    )
  ),
  
  # Corps de la page
  dashboardBody(
    tabItems(
      
      # --- TAB 1: CARTE ---
      tabItem(tabName = "map",
              h2("Flux Transfrontaliers France - Allemagne"),
              fluidRow(
                box(width = 8, leafletOutput("map_plot", height = 500)),
                box(width = 4, 
                    title = "Statistiques Flux", status = "primary", solidHeader = TRUE,
                    infoBoxOutput("flux_stat", width = 12),
                    p("Cette carte visualise la moyenne des échanges (FR_DE_EXCHANGE) sur la période sélectionnée."),
                    p("Positif = France exporte vers Allemagne."),
                    p("Négatif = France importe depuis Allemagne.")
                )
              )
      ),
      
      # --- TAB 2: HISTOGRAMME ---
      tabItem(tabName = "distrib",
              h2("Distribution de la Variable Cible (Prix)"),
              fluidRow(
                box(width = 12, plotlyOutput("hist_plot", height = 500))
              ),
              p("Observez les queues de distribution (Fat Tails) indiquant la volatilité.")
      ),
      
      # --- TAB 3: SCATTER PLOT ---
      tabItem(tabName = "scatter",
              h2("Analyse des Corrélations"),
              fluidRow(
                column(width = 3,
                       selectInput("x_var", "Variable X:", 
                                   choices = c("Production Éolienne" = "WINDPOW", 
                                               "Production Solaire" = "SOLAR",
                                               "Consommation" = "CONSUMPTION"),
                                   selected = "WINDPOW")
                ),
                column(width = 9,
                       box(width = 12, plotlyOutput("scatter_plot", height = 500))
                )
              )
      ),
      
      # --- TAB 4: TEMPOREL ---
      tabItem(tabName = "time",
              h2("Évolution Temporelle"),
              fluidRow(
                box(width = 12, plotlyOutput("time_plot", height = 500))
              ),
              sliderInput("ma_window", "Fenêtre Moyenne Mobile (jours):", min=1, max=30, value=7)
      )
    )
  )
)

# ==============================================================================
# 3. SERVER 
# ==============================================================================

server <- function(input, output) {
  
  # --- Données Réactives (Filtrées par le slider temps) ---
  filtered_data <- reactive({
    df_full %>%
      filter(DAY_ID >= input$day_range[1] & DAY_ID <= input$day_range[2])
  })
  
  # --- 1. LOGIQUE CARTE ---
  output$map_plot <- renderLeaflet({
    # Calcul de la moyenne des échanges sur la période
    mean_exchange <- mean(filtered_data()$FR_DE_EXCHANGE, na.rm = TRUE)
    
    # Logique de couleur et direction
    color_flux <- if(mean_exchange > 0) "blue" else "red"
    direction_txt <- if(mean_exchange > 0) "FR -> DE (Export)" else "DE -> FR (Import)"
    
    leaflet(geo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lng, ~lat, label = ~pays, color = "black", radius = 5) %>%
      # Ajout de la ligne (Flux)
      addPolylines(lng = c(1.888334, 10.451526), 
                   lat = c(46.603354, 51.165691),
                   color = color_flux,
                   weight = 5 + abs(mean_exchange), 
                   opacity = 0.8,
                   popup = paste("Moyenne Flux:", round(mean_exchange, 2), "<br>", direction_txt))
  })
  
  output$flux_stat <- renderInfoBox({
    val <- mean(filtered_data()$FR_DE_EXCHANGE, na.rm = TRUE)
    infoBox(
      "Balance Moyenne", 
      paste(round(val, 2)), 
      icon = icon("exchange-alt"),
      color = if(val > 0) "blue" else "red",
      fill = TRUE
    )
  })
  
  # --- 2. LOGIQUE HISTOGRAMME ---
  output$hist_plot <- renderPlotly({
    data_hist <- filtered_data() %>% filter(COUNTRY == input$country_select)
    
    p <- ggplot(data_hist, aes(x = TARGET)) +
      geom_histogram(bins = 50, fill = ifelse(input$country_select == "FR", "#3498db", "#e74c3c"), alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Distribution des Prix -", input$country_select), x = "Variation Prix", y = "Fréquence")
    
    ggplotly(p)
  })
  
  # --- 3. LOGIQUE SCATTER PLOT ---
  output$scatter_plot <- renderPlotly({
    data_scatter <- filtered_data() %>% filter(COUNTRY == input$country_select)
    
    # Sélection dynamique de la colonne X
    col_name <- paste0(input$country_select, "_", input$x_var)
    
    
    if(!col_name %in% names(data_scatter)) return(NULL)
    
    p <- ggplot(data_scatter, aes_string(x = col_name, y = "TARGET")) +
      geom_point(alpha = 0.5, color = ifelse(input$country_select == "FR", "#3498db", "#e74c3c")) +
      geom_smooth(method = "lm", color = "black") +
      theme_minimal() +
      labs(title = paste("Relation:", input$x_var, "vs PRIX"),
           x = input$x_var, y = "Variation Prix")
    
    ggplotly(p)
  })
  
  # --- 4. LOGIQUE TEMPOREL ---
  output$time_plot <- renderPlotly({
    # Préparation des données pour le plot temporel
    df_temp <- filtered_data() %>%
      group_by(DAY_ID, COUNTRY) %>%
      summarise(TARGET = mean(TARGET, na.rm=TRUE), .groups="drop") %>%
      arrange(DAY_ID) %>%
      group_by(COUNTRY) %>%
      mutate(MA = rollmean(TARGET, k = input$ma_window, fill = NA, align = "right"))
    
    p <- ggplot(df_temp, aes(x = DAY_ID)) +
      geom_line(aes(y = TARGET, color = COUNTRY), alpha = 0.3) +
      geom_line(aes(y = MA, color = COUNTRY), size = 1) + # Moyenne mobile
      scale_color_manual(values = c("FR" = "#3498db", "DE" = "#e74c3c")) +
      theme_minimal() +
      labs(title = "Évolution Temporelle des Prix",
           subtitle = paste("Moyenne mobile sur", input$ma_window, "jours"))
    
    ggplotly(p)
  })
}

# ==============================================================================
# 4. LANCEMENT
# ==============================================================================
shinyApp(ui, server)