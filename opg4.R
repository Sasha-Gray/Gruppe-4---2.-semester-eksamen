#
# Opgave 4 – Præsentation til Brøndby IF
#
# Udgangspunkt for denne opgave er jeres resultater i opgave 3.
# Det vil sige, at det ikke er nødvendigt at beskrive modellen, samt jeres test og validering af modellen fra opgave 3.
# I skal alene fokusere på kommunikation af jeres resultater til Brøndby IF.
# I skal være klar over jeres rolle fra opgave 3. Hvem er modtager i Brøndby IF af jeres resultater?
# I skal benytte Shiny til at kommunikere jeres resultater.

#
# ---- Opgave 4.1 – Visualisering af clustre for afleveringer ----
# 
# Lav et Dashboard, der giver Brøndby IF et overblik over afleveringerne i indeværende sæson.

# Trin 0 - Hent pakker:
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

# --- Først vil vi gerne oversætte match id til at det er holdnavnene der står under kamp ---

# Trin 1 - Lav en unik kamp-label ud fra passes_df:
# Her tilføjes dato til kampnavnet, så kampe mellem de samme hold
# kan skelnes fra hinanden i dropdown-menuen.
match_labels <- passes_df %>%
  distinct(MATCH_WYID, TEAMNAME, MATCH_DATE_UTC) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    hold1 = first(sort(TEAMNAME)),
    hold2 = last(sort(TEAMNAME)),
    dato = first(MATCH_DATE_UTC),
    kamp_label = paste0(hold1, " - ", hold2, " (", substr(dato, 1, 10), ")"),
    .groups = "drop")

# Trin 2 - Join kamp-label på vores cluster-datasæt:
passes_df_cluster <- passes_df_cluster %>%
  left_join(match_labels, by = "MATCH_WYID")

# Trin 3 - Byg brugerfladen (UI):
# UI-delen bestemmer, hvordan dashboardet ser ud for brugeren.
# Her laves:
# - en overskrift
# - en menu med tre faner
# - filtre i venstre side
# - områder i hovedvinduet til plots, bokse og tabel

ui <- dashboardPage(
  dashboardHeader(title = "Afleveringsdashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overblik", tabName = "overblik"),
      menuItem("Visualisering", tabName = "visualisering"),
      menuItem("Nøgletal", tabName = "noegletal")
    ),
    
    br(),
    
    # Filter 1: Vælg om man vil se hele ligaen eller kun Brøndby
    selectInput(
      "scope",
      "Vælg datasæt:",
      choices = c("Hele ligaen", "Brøndby IF"),
      selected = "Brøndby IF"
    ),
    
    # Disse filtre laves dynamisk i server-delen, fordi deres indhold afhænger af brugerens valg
    uiOutput("player_ui"),
    uiOutput("match_ui"),
    uiOutput("cluster_ui")
  ),

  dashboardBody(
    tabItems(
      # Fane 1 - Overblik:
      # - KPI-bokse med centrale nøgletal
      # - et søjlediagram over afleveringstyper
      # - en forklaringstekst til brugeren

      tabItem(
        tabName = "overblik",
        
        fluidRow(
          valueBoxOutput("antal_passes_box"),
          valueBoxOutput("avg_length_box"),
          valueBoxOutput("avg_progression_box")
        ),
        
        fluidRow(
          box(
            width = 8,
            title = "Fordeling af afleveringstyper",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("cluster_barplot", height = 350)
          ),
          box(
            width = 4,
            title = "Forklaring",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("cluster_text")
          )
        )
      ),
      
      # Fane 2 - Visualisering:
      # Denne fane viser et scatterplot af afleveringerne.

      tabItem(
        tabName = "visualisering",
        
        fluidRow(
          box(
            width = 12,
            title = "Scatterplot af afleveringer",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("scatter_plot", height = 500)
          )
        )
      ),

      # Fane 3 - Nøgletal:
      # Denne fane viser en tabel med opsummerede nøgletal for de valgte afleveringer.

      tabItem(
        tabName = "noegletal",
        
        fluidRow(
          box(
            width = 12,
            title = "Nøgletal for valgte afleveringer",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("summary_table")
          )
        )
      )
    )
  )
)

# Trin 4 - Byg server-logikken:
# Server-delen er hjernen i dashboardet.
# Det er her, filtrering, beregninger og plots bliver lavet.

server <- function(input, output, session) {

  # Trin 4.1 - Vælg grunddatasæt:
  # - alle afleveringer i ligaen eller kun afleveringer fra Brøndby IF

  filtered_base <- reactive({
    if (input$scope == "Brøndby IF") {
      passes_df_cluster %>% filter(TEAMNAME == "Brøndby")
    } else {
      passes_df_cluster
    }
  })
  
  # Trin 4.2 - Lav dynamisk filter for spiller:
  # Her opdateres spiller-listen automatisk afhængigt af, om man har valgt hele ligaen eller kun Brøndby.

  output$player_ui <- renderUI({
    players <- filtered_base() %>%
      distinct(SHORTNAME) %>%
      arrange(SHORTNAME) %>%
      pull(SHORTNAME)
    
    selectInput(
      "player",
      "Vælg spiller:",
      choices = c("Alle", players),
      selected = "Alle"
    )
  })

  # Trin 4.3 - Lav dynamisk filter for kamp:
  # Her laves kamp-listen i dropdown-menuen.
  # Brugeren ser kampnavnet, fx "AGF - Brøndby", men appen gemmer stadig det rigtige MATCH_WYID som værdi.

  output$match_ui <- renderUI({
    matches <- filtered_base() %>%
      distinct(MATCH_WYID, kamp_label) %>%
      arrange(kamp_label)
    
    choices_named <- setNames(matches$MATCH_WYID, matches$kamp_label)
    
    selectInput(
      "match",
      "Vælg kamp:",
      choices = c("Alle" = "Alle", choices_named),
      selected = "Alle"
    )
  })

  # Trin 4.4 - Lav dynamisk filter for afleveringstype:
  # Her vises de clusters, der findes i det valgte datasæt.

  output$cluster_ui <- renderUI({
    clusters <- filtered_base() %>%
      distinct(cluster_label) %>%
      arrange(cluster_label) %>%
      pull(cluster_label)
    
    selectInput(
      "cluster",
      "Vælg afleveringstype:",
      choices = c("Alle", clusters),
      selected = "Alle"
    )
  })

  # Trin 4.5 - Lav det endeligt filtrerede datasæt:
  # Her kombineres alle brugerens filtre.
  # Resultatet er det datasæt, som alle visualiseringer bygger på.

  filtered_data <- reactive({
    df <- filtered_base()
    
    if (input$player != "Alle") {
      df <- df %>% filter(SHORTNAME == input$player)
    }
    
    if (input$match != "Alle") {
      df <- df %>% filter(MATCH_WYID == input$match)
    }
    
    if (input$cluster != "Alle") {
      df <- df %>% filter(cluster_label == input$cluster)
    }
    
    df
  })

  # Trin 5 - KPI-bokse med overblik:
  # Her beregnes tre nøgletal:
  # - antal afleveringer
  # - gennemsnitlig længde
  # - gennemsnitlig fremdrift

  output$antal_passes_box <- renderValueBox({
  valueBox(
    value = nrow(filtered_data()),
    subtitle = "Antal afleveringer",
    color = "blue"
  )
})

output$avg_length_box <- renderValueBox({
  valueBox(
    value = round(mean(filtered_data()$LENGTH, na.rm = TRUE), 1),
    subtitle = "Gennemsnitlig længde",
    color = "green"
  )
})

output$avg_progression_box <- renderValueBox({
  valueBox(
    value = round(mean(filtered_data()$progression_x, na.rm = TRUE), 1),
    subtitle = "Gennemsnitlig fremdrift",
    color = "yellow"
  )
})
  
  # Trin 6 - Lav søjlediagram over afleveringstyper:
  # Her tælles, hvor mange afleveringer der ligger i hvert cluster.
  # Resultatet vises som et vandret søjlediagram.
  # Det gør det nemt at se, hvilke afleveringstyper der fylder mest.

  output$cluster_barplot <- renderPlotly({
    plot_data <- filtered_data() %>%
      count(cluster_label, name = "antal") %>%
      mutate(andel = antal / sum(antal))
    
    p <- ggplot(plot_data, aes(x = reorder(cluster_label, antal), y = antal, fill = cluster_label)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        x = "Afleveringstype",
        y = "Antal afleveringer"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Trin 7 - Lav forklaringstekst til afleveringstyper:
  # Her vises enten:
  # - en generel forklaring på dashboardets formål
  # - eller en specifik forklaring på den valgte afleveringstype

  output$cluster_text <- renderUI({
    if (input$cluster == "Alle") {
      HTML("
        <b>Formål:</b><br>
        Dashboardet giver et hurtigt overblik over Brøndbys eller ligaens afleveringsmønstre.<br><br>
        Brug filtrene til at se, hvordan afleveringstyper varierer på tværs af spillere og kampe.
      ")
    } else {
      forklaring <- case_when(
  input$cluster == "Korte opbygningsafleveringer" ~ 
    "Korte afleveringer med moderat fremdrift, som primært anvendes i opbygningsspillet. De bruges til at fastholde boldbesiddelse og flytte spillet sikkert frem gennem kæderne uden stor risiko.",

  input$cluster == "Lange angrebsafleveringer" ~ 
    "Lange og meget fremadrettede afleveringer med høj progression. Disse bruges til hurtigt at bringe bolden frem i banen og skabe gennembrud, ofte direkte mod sidste tredjedel eller bag modstanderens kæde.",

  input$cluster == "Sidevendende afleveringer" ~ 
    "Afleveringer med stor bredde og minimal fremdrift. De bruges til at flytte spillet fra side til side for at skabe plads og ubalance i modstanderens organisation.",

  input$cluster == "Tilbage/sikre afleveringer" ~ 
    "Afleveringer med lav eller negativ fremdrift, ofte spillet bagud eller på tværs. De bruges til at bevare boldbesiddelse, nulstille spillet og reducere risiko under pres.",

  input$cluster == "Korte fremadrettede opbygningsafleveringer" ~ 
    "Korte afleveringer med kontrolleret fremdrift, der bruges til gradvist at spille bolden frem gennem banen i opbygningsfasen.",

  input$cluster == "Korte tilbage- og støtteafleveringer" ~ 
    "Korte og sikre afleveringer, der primært spilles bagud eller til siden for at støtte boldholderen og bevare struktur i spillet.",

  input$cluster == "Lange direkte angrebsafleveringer" ~ 
    "Direkte og fremadrettede afleveringer over længere afstande, der har til formål hurtigt at bringe bolden i farlige områder og udfordre modstanderens bagkæde.",

  TRUE ~ "Afleveringstype valgt."
)
      
      HTML(paste0( "<b>", input$cluster, "</b><br><br>", forklaring))
    }
  })
  
  # Trin 8 - Lav scatterplot af afleveringer:
  # Her vises afleveringerne i et scatterplot.
  # X-aksen viser afleveringslængde.
  # Y-aksen viser afstand til mål efter afleveringen.
  # Tooltip gør det muligt at holde musen over et punkt og se detaljer om aflevering, spiller, hold og kamp.

  output$scatter_plot <- renderPlotly({
    plot_df <- filtered_data()
    
    if (nrow(plot_df) > 5000) {
      set.seed(123)
      plot_df <- plot_df %>% sample_n(5000)
    }
    
    p <- ggplot(
      plot_df,
      aes(
        x = LENGTH,
        y = afstand_til_maal_efter,
        color = cluster_label,
        text = paste(
          "Spiller:", SHORTNAME,
          "<br>Hold:", TEAMNAME,
          "<br>Kamp:", kamp_label,
          "<br>Længde:", round(LENGTH, 1),
          "<br>Fremdrift:", round(progression_x, 1)
        )
      )
    ) +
      geom_point(alpha = 0.6, size = 2) +
      labs(
        x = "Afleveringslængde",
        y = "Afstand til mål efter aflevering",
        color = "Afleveringstype"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Trin 9 - Lav tabel med nøgletal:
  # Her laves en opsummering pr. afleveringstype.
  # Tabellen viser blandt andet:
  # - antal afleveringer
  # - gennemsnitlig længde
  # - gennemsnitlig fremdrift
  # - andel progressive afleveringer
  # - andel afleveringer til sidste tredjedel og feltet

  output$summary_table <- renderDT({
    summary_df <- filtered_data() %>%
      group_by(cluster_label) %>%
      summarise(
        Antal = n(),
        `Gns. længde` = round(mean(LENGTH, na.rm = TRUE), 2),
        `Gns. fremdrift` = round(mean(progression_x, na.rm = TRUE), 2),
        `Gns. bredde` = round(mean(bredde, na.rm = TRUE), 2),
        `Andel progressive` = round(mean(is_progressive, na.rm = TRUE), 2),
        `Andel til sidste tredjedel` = round(mean(is_final_third, na.rm = TRUE), 2),
        `Andel til feltet` = round(mean(is_penalty_area, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    datatable(summary_df, options = list(pageLength = 10))
  })
}

# Trin 10 - Start appen:
# Denne linje starter dashboardet og kobler UI og server sammen.

shinyApp(ui, server)

#
# ---- Opgave 4.2 – Visualisering af clustre for kampene ----
#
# Lav et Dashboard, der giver Brøndby IF et overblik over kampe i indeværende sæson.

# Trin 1 - Lav kamp-labels:
# Her laves en læsbar kamp-betegnelse ud fra de to hold i samme kamp
match_labels_matches <- matches_df_cluster %>%
  distinct(MATCH_WYID, TEAMNAME, MATCH_DATE_UTC) %>%
  group_by(MATCH_WYID) %>%
  summarise(
    hold1 = first(sort(TEAMNAME)),
    hold2 = last(sort(TEAMNAME)),
    dato = first(MATCH_DATE_UTC),
    kamp_label = paste0(hold1, " - ", hold2, " (", substr(dato, 1, 10), ")"),
    .groups = "drop")

# Trin 2 - Join kamp-label på datasættet
matches_df_cluster <- matches_df_cluster %>%
  left_join(match_labels_matches, by = "MATCH_WYID")

# Trin 3 - Byg UI:
ui <- dashboardPage(
  dashboardHeader(title = "Kampdashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overblik", tabName = "overblik"),
      menuItem("Visualisering", tabName = "visualisering"),
      menuItem("Nøgletal", tabName = "noegletal")
    ),
    
    br(),
    
    uiOutput("team_ui"),
    uiOutput("match_ui"),
    uiOutput("cluster_ui")
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------- Overblik ---------------- #
      tabItem(
        tabName = "overblik",
        
        fluidRow(
          valueBoxOutput("antal_kampe_box"),
          valueBoxOutput("avg_xg_box"),
          valueBoxOutput("avg_passes_box")
        ),
        
        fluidRow(
          box(
            width = 8,
            title = "Fordeling af kamptyper",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("cluster_barplot", height = 350)
          ),
          box(
            width = 4,
            title = "Forklaring",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("cluster_text")
          )
        )
      ),
      
      # ---------------- Visualisering ---------------- #
      tabItem(
        tabName = "visualisering",
        
        fluidRow(
          box(
            width = 12,
            title = "Scatterplot af kampe",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("scatter_plot", height = 500)
          )
        )
      ),
      
      # ---------------- Nøgletal ---------------- #
      tabItem(
        tabName = "noegletal",
        
        fluidRow(
          box(
            width = 12,
            title = "Nøgletal for valgte kamptyper",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("summary_table")
          )
        )
      )
    )
  )
)

# Trin 3 - Server:
server <- function(input, output, session) {
  
  # Grunddatasæt
  filtered_base <- reactive({
  matches_df_cluster
})
  
  # Dynamisk hold-filter
  output$team_ui <- renderUI({
    teams <- filtered_base() %>%
      distinct(TEAMNAME) %>%
      arrange(TEAMNAME) %>%
      pull(TEAMNAME)
    
    selectInput(
      "team",
      "Vælg hold:",
      choices = c("Alle", teams),
      selected = "Alle"
    )
  })
  
  # Dynamisk kamp-filter
  output$match_ui <- renderUI({
    matches <- filtered_base() %>%
      distinct(MATCH_WYID, kamp_label) %>%
      arrange(kamp_label)
    
    choices_named <- setNames(matches$MATCH_WYID, matches$kamp_label)
    
    selectInput(
      "match",
      "Vælg kamp:",
      choices = c("Alle" = "Alle", choices_named),
      selected = "Alle"
    )
  })
  
  # Dynamisk cluster-filter
  output$cluster_ui <- renderUI({
    clusters <- filtered_base() %>%
      distinct(cluster_label) %>%
      arrange(cluster_label) %>%
      pull(cluster_label)
    
    selectInput(
      "cluster",
      "Vælg kamp-type:",
      choices = c("Alle", clusters),
      selected = "Alle"
    )
  })
  
  # Endeligt filtreret datasæt
  filtered_data <- reactive({
    df <- filtered_base()
    
    if (input$team != "Alle") {
      df <- df %>% filter(TEAMNAME == input$team)
    }
    
    if (input$match != "Alle") {
      df <- df %>% filter(MATCH_WYID == input$match)
    }
    
    if (input$cluster != "Alle") {
      df <- df %>% filter(cluster_label == input$cluster)
    }
    
    df
  })
  
  # ---------------- KPI-bokse ---------------- #
  output$antal_kampe_box <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Antal kampe",
      color = "blue"
    )
  })
  
  output$avg_xg_box <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$XG, na.rm = TRUE), 2),
      subtitle = "Gennemsnitlig xG",
      color = "green"
    )
  })
  
  output$avg_passes_box <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$total_passes, na.rm = TRUE), 1),
      subtitle = "Gennemsnitligt antal afleveringer",
      color = "yellow"
    )
  })
  
  # ---------------- Søjlediagram ---------------- #
  output$cluster_barplot <- renderPlotly({
    plot_data <- filtered_data() %>%
      count(cluster_label, name = "antal")
    
    p <- ggplot(plot_data, aes(x = reorder(cluster_label, antal), y = antal, fill = cluster_label)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        x = "Kamp-type",
        y = "Antal kampe"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ---------------- Forklaring ---------------- #
  output$cluster_text <- renderUI({
  if (is.null(input$cluster) || input$cluster == "Alle") {
    HTML("
      <b>Formål:</b><br>
      Dashboardet giver et hurtigt overblik over Brøndbys eller ligaens kamptyper.<br><br>
      Brug filtrene til at se, hvordan kampe varierer på tværs af hold og opgør.
    ")
  } else {
    forklaring <- case_when(
      input$cluster == "Mere direkte kampe" ~ 
        "Kampe med færre afleveringer og et lavere offensivt output end de øvrige kamptyper. Spillet er mere direkte og mindre præget af kontrol, og holdet kommer frem til et moderat antal chancer uden længere perioder med etableret pres.",

      input$cluster == "Kontrollerede og chance-skabende kampe" ~ 
        "Kampe hvor holdet både kontrollerer spillet og skaber mange chancer. Denne type er kendetegnet ved flest afleveringer, flest skud, højeste xG, flest progressive løb og flest berøringer i feltet. Det peger på kampe med både boldkontrol og offensiv styrke.",

      input$cluster == "Hurtige fremadrettede kampe" ~ 
        "Kampe præget af tempo og fremdrift. Bolden flyttes hurtigt frem gennem relativt mange afleveringer, men uden samme chanceproduktion som i de kontrollerede og chance-skabende kampe. Spillet er mere vertikalt og direkte mod mål.",

      TRUE ~ "Kamp-type valgt."
    )

    HTML(paste0("<b>", input$cluster, "</b><br><br>", forklaring))
  }
})
  
  # ---------------- Scatterplot ---------------- #
  output$scatter_plot <- renderPlotly({
    p <- ggplot(
      filtered_data(),
      aes(
        x = XG,
        y = total_passes,
        color = cluster_label,
        text = paste(
          "Hold:", TEAMNAME,
          "<br>Kamp:", kamp_label,
          "<br>xG:", round(XG, 2),
          "<br>Afleveringer:", round(total_passes, 0),
          "<br>Skud:", SHOTS,
          "<br>Touches i felt:", TOUCHESINBOX
        )
      )
    ) +
      geom_point(alpha = 0.8, size = 3) +
      labs(
        x = "Expected Goals (xG)",
        y = "Antal afleveringer",
        color = "Kamp-type"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---------------- Tabel med nøgletal ---------------- #
  output$summary_table <- renderDT({
    summary_df <- filtered_data() %>%
      group_by(cluster_label) %>%
      summarise(
        Antal = n(),
        `Gns. skud` = round(mean(SHOTS, na.rm = TRUE), 2),
        `Gns. xG` = round(mean(XG, na.rm = TRUE), 2),
        `Gns. progressive runs` = round(mean(PROGRESSIVERUNS, na.rm = TRUE), 2),
        `Gns. touches i felt` = round(mean(TOUCHESINBOX, na.rm = TRUE), 2),
        `Gns. afleveringer` = round(mean(total_passes, na.rm = TRUE), 2),
        `Gns. afleveringslængde` = round(mean(gns_length, na.rm = TRUE), 2),
        `Gns. afleveringsprogression` = round(mean(gns_progression, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    datatable(summary_df, options = list(pageLength = 10))
  })
}

# Trin 4 - Start appen:
# Denne linje starter dashboardet og kobler UI og server sammen.
shinyApp(ui, server)

