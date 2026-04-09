#
# Opgave 6 – Videnskabsteori - tæt på virkeligheden
#
#
# ---- Opgave 6.1 – Hermeneutik – mennesket bag facaden ----
#
# I skal lave en top-10 blandt kvinderne. I må selv bestemme hvad I måler på. I skal så bruge den information til at tegne et portræt af den spiller I har valgt. 
#

# Trin 0 - Hent pakker
library(jsonlite)
library(dplyr)
library(ggplot2)

# Trin 1 - Kør util-script
source("util.R")

# Trin 2 - Hent alle kampe
allMatches <- getAllMatches()

# Trin 3 - Filtrér kvindekampe i perioden 2022-2024
women_matches <- allMatches %>%
  filter(
    home_team.home_team_gender == "female",
    away_team.away_team_gender == "female",
    as.Date(match_date) >= as.Date("2022-01-01"),
    as.Date(match_date) <= as.Date("2024-12-31")
  )

# Trin 4 - Hent match-id'er og del dem op i bidder
women_match_ids <- women_matches$match_id

chunks <- split(women_match_ids, ceiling(seq_along(women_match_ids) / 10))

# Trin 5 - Hent eventdata for kampene
women_events <- lapply(chunks, function(ids) {
  getAllEventsMultipleMatches(ids, gender = "f")
}) %>%
  bind_rows()

# Trin 6 - Filtrér skud og lav målvariabel
women_shots <- women_events %>%
  filter(type.name == "Shot") %>%
  mutate(
    goal = ifelse(shot.outcome.name == "Goal", 1, 0)
  )

# Trin 7 - Lav top-10 over spillere målt på samlet xG
top10_kvinder <- women_shots %>%
  group_by(player.name) %>%
  summarise(
    antal_skud = n(),
    xg = sum(shot.statsbomb_xg, na.rm = TRUE),
    antal_maal = sum(goal, na.rm = TRUE),
    difference = antal_maal - xg,
    .groups = "drop") %>%
  arrange(desc(xg)) %>%
  slice(1:10)

# Trin 8 - Lav graf over top-10 spillere målt på xG
ggplot(top10_kvinder,
       aes(x = reorder(player.name, xg),
           y = xg)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0("Mål: ", antal_maal)),
            hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Top-10 kvindelige spillere målt på samlet xG",
    subtitle = "Tallene viser spillernes samlede expected goals i perioden 2022-2024",
    x = "Spiller",
    y = "Samlet xG",
    caption = "Kilde: StatsBomb (2022-2024)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )


#
# ---- Opgave 6.2 – Logisk positivisme i fodbold ----
#
# Lav en prioriteret liste over udvalgte features fra Statsbombs data. I skal prioritere efter hvor ”tætte” de er på ”virkeligheden”.
#

# Trin 0 - Hent alle pakkerne:
library(jsonlite)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(stringr)

# Trin 1 - Kør Wulf's util.R-script:
source("util.R")

# Trin 2 - Hent alle tilgængelige kampe ned i et dataframe:
allMatches = getAllMatches()

# Trin 3 - Kig på de forskellige variabler i allMatches med colnames:
colnames(allMatches)


#
# ---- Opgave 6.3 – Hvad ser man når man går helt tæt på virkeligheden? ----
#
# Med udgangspunkt i trackingdata fra en kamp mellem Vejle BK og Odense BK skal I lave en beregning som viser hvilke spillere der har mest areal omkring sig i modstanderens felt. 
#
#
# 2. Find de frames som dækker det første mål i kampen mellem Odense BK (OB) og Vejle BK (VB).
# Trin 0 - Hent pakke
library(dplyr)

# Trin 1 - Læs trackingdata ind
df <- read.csv("vbob.csv", header = FALSE)

# Trin 2 - Navngiv kolonnerne ud fra det Wulf har sagt.
# Trin 2.1 - Definer antal spillere pr. hold (11):
n_players <- 11

# Trin 2.2 - Opret kolonnenavne:
col_names <- c(
  "frame",
  "timestamp",
  
  # Home spillere ID
  paste0("home_id_", 1:n_players),
  
  # Home x og y
  paste0("home_x_", 1:n_players),
  paste0("home_y_", 1:n_players),
  
  # Away spillere ID
  paste0("away_id_", 1:n_players),
  
  # Away x og y
  paste0("away_x_", 1:n_players),
  paste0("away_y_", 1:n_players),
  
  # Bold
  "ball_x",
  "ball_y")

# Trin 2.3 - Tildel kolonnenavne til datasættet:
colnames(df) <- col_names

# Trin 3 - Find frames hvor bolden er meget tæt på målet:
goal_frames <- df %>%
  filter(
    !is.na(ball_x),
    !is.na(ball_y),
    abs(ball_x) > 50,
    ball_y > -4,
    ball_y < 4)

# Trin 4 - Find første frame i den første målsituation
first_goal_start <- min(goal_frames$frame)

# Trin 5 - Udvælg frames omkring den første målsituation
frames_first_goal <- df %>%
  filter(
    frame >= first_goal_start - 25,
    frame <= first_goal_start + 10)

# Trin 6 - Se de frames der dækker det første mål
frames_first_goal[, c("frame", "timestamp", "ball_x", "ball_y")]

#
# 3. Vælg en eller flere relevante frames og plot banen med Delaunay-trianguleringen omkring spillerne.
# Vi har valgt de 3 frames/plots:
# 28470 → før målet
# 28482 → selve målet
# 28490 → efter målet

# Trin 0 - Hent pakker
# dplyr bruges til datahåndtering
# ggplot2 og ggsoccer bruges til at plotte banen
# deldir bruges til Delaunay-triangulering
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(deldir)

# Trin 1 - Vælg relevante frames omkring det første mål
# Her vælges et frame før målet, selve mål-frame og et frame efter målet
selected_frames <- c(28470, 28482, 28490)

# Trin 2 - Lav en tom liste til plots
plot_list <- list()

# Trin 3 - Gennemgå hvert frame
for (i in seq_along(selected_frames)) {
  
  frame_id <- selected_frames[i]
  
  # Trin 3.1 - Hent data for det valgte frame
  frame_data <- df %>%
    filter(frame == frame_id)
  
  # Trin 3.2 - Udtræk home-spillernes positioner
  # Home: home_x_1:home_x_11 og home_y_1:home_y_11
  home_df <- data.frame(
    x = as.numeric(frame_data %>% select(starts_with("home_x_"))),
    y = as.numeric(frame_data %>% select(starts_with("home_y_"))),
    team = "Home")
  
  # Trin 3.3 - Udtræk away-spillernes positioner
  # Away: away_x_1:away_x_11 og away_y_1:away_y_11
  away_df <- data.frame(
    x = as.numeric(frame_data %>% select(starts_with("away_x_"))),
    y = as.numeric(frame_data %>% select(starts_with("away_y_"))),
    team = "Away")
  
  # Trin 3.4 - Saml alle spillere i én dataframe
  players <- rbind(home_df, away_df) %>%
    filter(!is.na(x), !is.na(y))
  
  # Trin 3.5 - Lav Delaunay-triangulering
  tri <- deldir(players$x, players$y)
  tri_lines <- tri$delsgs
  
  # Trin 3.6 - Plot banen med spillere og triangulering
  p <- ggplot() +
    annotate_pitch(dimensions = pitch_impect, colour = "white", fill = "green") +
    geom_segment(
      data = tri_lines,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      colour = "white") +
    geom_point(
      data = players,
      aes(x = x, y = y, colour = team),
      size = 3) +
    scale_color_manual(values = c("Home" = "red", "Away" = "blue")) +
    theme_pitch() +
    ggtitle(paste("Delaunay-triangulering - frame", frame_id)) +
    labs(
  caption = "Kilde: Egen tilvirkning baseret på trackingdata fra VB-OB, 2022.") +
theme(
  plot.caption = element_text(
    hjust = 0,   # <-- ændret her (venstre)
    face = "italic")
)
  
  # Trin 3.7 - Gem plot i listen
  plot_list[[i]] <- p
}

# Trin 4 - Vis de tre plots
plot_list[[1]]
plot_list[[2]]
plot_list[[3]]

#
# 5. Vis et søjlediagram over de spillere, der har mest rum omkring sig i modstanderens felt.

# -- Del 1 - Målmænd med navn -- #

# Trin 0 - pakker
library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggsoccer)

# Trin 1 - Indlæs data:
# Genbrug df(vbob.csv) data fra tidligere og indlæs metadata fra vbob-meta:
meta <- fromJSON("vbob-meta.json")

# Trin 2 - vælg et frame:
frame_id <- 28482

frame_data <- df %>%
  filter(frame == frame_id)

# Trin 3 -  Identificér målmænd i metadata:
home_gk <- meta$homePlayers %>%
  filter(position == "GK")

away_gk <- meta$awayPlayers %>%
  filter(position == "GK")

# Trin 4 - Udtræk spiller-ID’er fra det valgte frame:
home_ids <- as.character(unlist(frame_data %>% select(starts_with("home_id_"))))

away_ids <- as.character(unlist(frame_data %>% select(starts_with("away_id_"))))

# Trin 5 – Find målmandens placering blandt spillerne:
home_index <- which(home_ids == home_gk$ssiId)

away_index <- which(away_ids == away_gk$ssiId)

# Trin 6 - Hent målmændenes koordinater og navn:
gk_df <- data.frame(
  x = c(
    as.numeric(frame_data[[paste0("home_x_", home_index)]]),
    as.numeric(frame_data[[paste0("away_x_", away_index)]])
  ),
  y = c(
    as.numeric(frame_data[[paste0("home_y_", home_index)]]),
    as.numeric(frame_data[[paste0("away_y_", away_index)]])
  ),
  name = c(home_gk$name, away_gk$name),
  team = c("Home", "Away")
)

# Trin 7 - plot målmændenes placering i det udvalgte mål-frame:
ggplot() +
  annotate_pitch(dimensions = pitch_impect, colour = "white", fill = "green") +
  geom_point(data = gk_df, aes(x = x, y = y, color = team), size = 4) +
  
  geom_text(
    data = gk_df %>% filter(team == "Home"),
    aes(x = x, y = y, label = name),
    color = "black",
    nudge_x = 7,
    hjust = 0,
    vjust = -0.5,
    size = 5) +
  
  geom_text(
    data = gk_df %>% filter(team == "Away"),
    aes(x = x, y = y, label = name),
    color = "white",
    nudge_x = 2,
    hjust = 0,
    vjust = -0.5,
    size = 5) +
  
  scale_color_manual(values = c("Home" = "red", "Away" = "blue")) +
  
  labs(
    title = paste("Målmændenes placering i det udvalgte mål-frame – frame", frame_id),
    caption = "Kilde: Egen tilvirkning baseret på trackingdata fra VB-OB, 2022.") +
  
  theme_pitch() +
  theme(
    plot.title = element_text(size = ),
    plot.caption = element_text(
      size = 8,
      hjust = 0)
  )

# -- Del 2 Start small med nogle få frames -- #

# Data er hentet fra tidligere og alle kolonnerne i df-tabellen er navngivet korrekt.

# Spillernavne kan ikke hentes direkte fra metadata ved blot at vælge de første 11 spillere, da metadata indeholder hele truppen.
# Derfor matches spiller-ID’er fra trackingdata med metadata for korrekt at identificere de spillere, der er på banen i hvert frame.

# Trin 1 - Vælg frames omkring målet
selected_frames <- c(28470, 28482, 28490)

# Trin 2 - Lav tom liste
results_list <- list()

# Trin 3 - Gennemgå hvert frame:
for (f in selected_frames) {
  
  frame_data <- df %>%
    filter(frame == f)
  
  # Trin 3.1 - Hent spiller-ID'er fra trackingdata i det valgte frame
  home_ids <- as.character(unlist(frame_data %>% select(starts_with("home_id_"))))
  away_ids <- as.character(unlist(frame_data %>% select(starts_with("away_id_"))))
  
  # Trin 3.2 - Match spiller-ID'er med metadata for at få de rigtige spillernavne
  home_names <- sapply(home_ids, function(id) {
    match_name <- meta$homePlayers$name[meta$homePlayers$ssiId == id]
    if (length(match_name) == 0) NA else match_name[1]
  })
  
  away_names <- sapply(away_ids, function(id) {
    match_name <- meta$awayPlayers$name[meta$awayPlayers$ssiId == id]
    if (length(match_name) == 0) NA else match_name[1]
  })
  
  # Trin 3.3 - Lav dataframe for home- og away-spillere
  home_df <- data.frame(
    name = home_names,
    team = "Home",
    x = as.numeric(unlist(frame_data %>% select(starts_with("home_x_")))),
    y = as.numeric(unlist(frame_data %>% select(starts_with("home_y_"))))
  )
  
  away_df <- data.frame(
    name = away_names,
    team = "Away",
    x = as.numeric(unlist(frame_data %>% select(starts_with("away_x_")))),
    y = as.numeric(unlist(frame_data %>% select(starts_with("away_y_"))))
  )
  
  players <- bind_rows(home_df, away_df) %>%
    filter(!is.na(x), !is.na(y), !is.na(name))
  
  # Trin 5 - Find spillere i modstanderens felt:
  # Ud fra målmændenes placering ses det, at home-holdet forsvarer venstre side og dermed angriber mod højre (positiv x-retning), 
  # mens away-holdet angriber mod venstre. Derfor defineres modstanderens felt som områder med x > 40 for home og x < -40 for away.
  # Dette er fordi der er i første halvleg. I anden halvleg vil det være omvendt.
  players_box <- players %>%
    filter(
      (team == "Home" & x > 40 & y > -20 & y < 20) |
        (team == "Away" & x < -40 & y > -20 & y < 20)
    )
  
  # Trin 6 - Beregn nærmeste modstander for hver spiller i feltet
  frame_result <- data.frame()
  
  for (i in 1:nrow(players_box)) {
    
    player_i <- players_box[i, ]
    
    opponents <- players %>%
      filter(team != player_i$team)
    
    distances <- sqrt((player_i$x - opponents$x)^2 + (player_i$y - opponents$y)^2)
    
    min_dist <- min(distances, na.rm = TRUE)
    
    temp <- data.frame(
      frame = f,
      name = player_i$name,
      team = player_i$team,
      rum = min_dist)
    
    frame_result <- bind_rows(frame_result, temp)
  }
  
  results_list[[as.character(f)]] <- frame_result
}

# Trin 7 - Saml resultater:
rum_frames <- bind_rows(results_list)

# Trin 8 - Gennemsnitligt rum pr. spiller:
rum_frames_summary <- rum_frames %>%
  group_by(name, team) %>%
  summarise(
    gennemsnitligt_rum = mean(rum),
    antal_frames = n(),
    .groups = "drop") %>%
  arrange(desc(gennemsnitligt_rum))

rum_frames_summary

# Trin 9 - Lav søjlediagram
ggplot(rum_frames_summary,
       aes(x = reorder(name, gennemsnitligt_rum),
           y = gennemsnitligt_rum,
           fill = team)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Home" = "red", "Away" = "blue")) +
  labs(
    title = "Spillere med mest rum i modstanderens felt (frames: 28470, 28482, 28490)",
    x = "Spiller",
    y = "Afstand til nærmeste modstander",
    caption = "Kilde: Egen tilvirkning baseret på trackingdata fra VB-OB, 2022.") +
  theme_minimal() +
  theme(
    plot.caption = element_text(
      hjust = 0)
  )


#
# -- Del 3 - For hele kampen -- #

# Data er hentet fra tidligere og alle kolonnerne i df-tabellen er navngivet korrekt.

# Trin 1 - Definér halvlege:
# Her tages der tal fra listen. 1 er det første element i listen der er valgt og 2 er andet element i listen der er valgt.
# Det er starten og slutningen på 1. halvleg og starten og slutningen på anden halvleg.
first_half_start <- meta$periods$startFrameIdx[1]
first_half_end <- meta$periods$endFrameIdx[1]
second_half_start <- meta$periods$startFrameIdx[2]
second_half_end <- meta$periods$endFrameIdx[2]

# Trin 2 - Vælg chunk-størrelse:
# Trin 2.1 - Angiv hvor mange rækker der behandles ad gangen
chunk_size <- 5000

# Trin 2.2 - Lav en liste over startpunkter for hvert chunk
chunk_starts <- seq(1, nrow(df), by = chunk_size)

# Trin 3 - Lav tom liste til resultater:
# Trin 3.1 - Opret en liste til at gemme alle resultater:
all_results <- list()

# Trin 3.2 - Lav en tæller til at holde styr på resultaterne:
# Vi laver en variabel der starter på 1 og som bliver brugt til at tælle op
result_nr <- 1

# Trin 4 - Gå gennem data i chunks:
for (start_row in chunk_starts) {
  
  # Trin 4.1 - Find start og slut for det aktuelle chunk
  end_row <- min(start_row + chunk_size - 1, nrow(df))
  df_chunk <- df[start_row:end_row, ]
  
  # Trin 4.2 - Lav tom liste til resultater i dette chunk
  chunk_results <- list()
  chunk_nr <- 1
  
  # Trin 5 - Gå gennem alle frames i chunket:
  for (i in 1:nrow(df_chunk)) {
    
    # Trin 5.1 - Hent én række = ét frame
    row <- df_chunk[i, ]
    f <- row$frame
    
    # Trin 5.2 - Hent spiller-ID'er fra trackingdata i det valgte frame
    home_ids <- as.character(unlist(row %>% select(starts_with("home_id_"))))
    away_ids <- as.character(unlist(row %>% select(starts_with("away_id_"))))
    
    # Trin 5.3 - Match spiller-ID'er med metadata for at få de rigtige spillernavne
    home_names <- sapply(home_ids, function(id) {
      match_name <- meta$homePlayers$name[meta$homePlayers$ssiId == id]
      if (length(match_name) == 0) NA else match_name[1]
    })
    
    away_names <- sapply(away_ids, function(id) {
      match_name <- meta$awayPlayers$name[meta$awayPlayers$ssiId == id]
      if (length(match_name) == 0) NA else match_name[1]
    })
    
    # Trin 5.4 - Lav dataframes med spillernes navne, hold og koordinater
    home_df <- data.frame(
      name = home_names,
      team = "Home",
      x = as.numeric(unlist(row %>% select(starts_with("home_x_")))),
      y = as.numeric(unlist(row %>% select(starts_with("home_y_"))))
    )
    
    away_df <- data.frame(
      name = away_names,
      team = "Away",
      x = as.numeric(unlist(row %>% select(starts_with("away_x_")))),
      y = as.numeric(unlist(row %>% select(starts_with("away_y_"))))
    )
    
    # Trin 5.5 - Saml alle spillere i ét datasæt
    players <- bind_rows(home_df, away_df) %>%
      filter(!is.na(x), !is.na(y), !is.na(name))
    
    # Trin 5.6 - Spring frame over hvis der er for få spillere
    # Hvis der er færre end to spillere, så hop videre, da vi skal bruge mindst to til at beregne afstand mellem hinanden.
    if (nrow(players) < 2) next
    
    # Trin 6 - Find spillere i modstanderens felt:
    if (f >= first_half_start & f <= first_half_end) {
      
      # Trin 6.1 - Brug korrekt angrebsretning i 1. halvleg
      players_box <- players %>%
        filter(
          (team == "Home" & x > 40 & y > -20 & y < 20) |
            (team == "Away" & x < -40 & y > -20 & y < 20)
        )
      
    } else if (f >= second_half_start & f <= second_half_end) {
      
      # Trin 6.2 - Brug korrekt angrebsretning i 2. halvleg
      players_box <- players %>%
        filter(
          (team == "Home" & x < -40 & y > -20 & y < 20) |
            (team == "Away" & x > 40 & y > -20 & y < 20)
        )
      
    } else {
      next
    }
    
    # Trin 6.3 - Spring frame over hvis ingen spillere er i modstanderens felt
    # Hvis ingen spillere i modstanderens felt, så hop videre.
    if (nrow(players_box) == 0) next
    
    # Trin 7 - Beregn rum som afstand til nærmeste modstander:
    
    # Trin 7.1 - Lav tom tabel til resultater for det aktuelle frame
    frame_result <- data.frame()
    
    for (j in 1:nrow(players_box)) {
      
      # Trin 7.2 - Vælg én spiller i feltet
      player_j <- players_box[j, ]
      
      # Trin 7.3 - Find alle modspillere
      opponents <- players %>%
        filter(team != player_j$team)
      
      # Trin 7.4 - Beregn afstande til alle modspillere
      distances <- sqrt((player_j$x - opponents$x)^2 + (player_j$y - opponents$y)^2)
      
      # Trin 7.5 - Gem spillerens rum som afstand til nærmeste modstander
      temp <- data.frame(
        frame = f,
        name = player_j$name,
        team = player_j$team,
        rum = min(distances, na.rm = TRUE)
      )
      
      frame_result <- bind_rows(frame_result, temp)
    }
    
    # Trin 7.6 - Gem frame-resultatet i chunk-listen
    if (nrow(frame_result) > 0) {
      chunk_results[[chunk_nr]] <- frame_result
      chunk_nr <- chunk_nr + 1
    }
  }
  
  # Trin 8 - Saml alle resultater fra chunket
  if (length(chunk_results) > 0) {
    all_results[[result_nr]] <- bind_rows(chunk_results)
    result_nr <- result_nr + 1
  }
  
  # Trin 9 - Vis hvor langt koden er nået
  print(paste("Færdig med rækker", start_row, "til", end_row))
}

# Trin 10 - Saml alle resultater:
rum_whole_match <- bind_rows(all_results)

# Trin 11 - Beregn gennemsnitligt rum pr. spiller:
rum_summary <- rum_whole_match %>%
  group_by(name, team) %>%
  summarise(
    gennemsnitligt_rum = mean(rum, na.rm = TRUE),
    antal_frames_i_felt = n(),
    .groups = "drop") %>%
  arrange(desc(gennemsnitligt_rum))

# Trin 12 - Lav søjlediagram:
ggplot(rum_summary,
       aes(x = reorder(name, gennemsnitligt_rum),
           y = gennemsnitligt_rum,
           fill = team)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Home" = "red", "Away" = "blue")) +
  labs(
    title = "Spillere med mest gns. rum i modstanderens felt (hele kampen, alle frames)",
    x = "Spiller",
    y = "Afstand til nærmeste modstander",
    caption = "Kilde: Egen tilvirkning baseret på trackingdata fra VB-OB, 2022.") +
  theme_minimal() +
  theme(
    plot.caption = element_text(
      size = 8,
      hjust = 0)
  )
