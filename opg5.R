#
# Opgave 5 – ”Beskrivende” statistik og visualisering
#

#
# ---- Opgave 5.1 – Kampe i tal ----
#
# Det er en bunden opgave at finde statistisk belæg for, at der er forskel på mænd og kvinder når det drejer sig om fodbold
# I vælger selv variabler fra tilgængelige Statsbombdata men I skal altså kunne nå frem til resultater som visualiserer forskelle mellem mænd og kvinder. 

# Trin 0 - Hent alle pakkerne
library(jsonlite)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(stringr)

# Trin 1 - Kør Wulf's util.R-script (Statsbomb-data):
source("util.R")

# Trin 2 - Hent alle tilgængelige kampe ned i et dataframe:
allMatches <- getAllMatches()

# Trin 3 - Undersøg kønsvariablen:
table(allMatches$home_team.home_team_gender, useNA = "ifany")
table(allMatches$away_team.away_team_gender, useNA = "ifany")

# Trin 4 - Filtrér kampene på periode og køn
# Vi har valgt at kigge på perioden 2022-2024, da et større datamængde ville være for tungt at hente ned på computeren.

# Trin 4.1 - Filtrér først kampene på periode:
matches_period <- allMatches %>%
  mutate(match_date = as.Date(match_date)) %>%
  filter(
    match_date >= as.Date("2022-01-02") &
    match_date <= as.Date("2024-07-15")
  )

# Trin 4.2 - Filtrér derefter på kampe, hvor køn er angivet:
matches_gender <- matches_period %>%
  filter(!is.na(home_team.home_team_gender)) %>%
  mutate(gender = tolower(home_team.home_team_gender))

# Trin 4.3 - Hent match-id'er i hver sin vektor:
female_match_ids <- matches_gender %>%
  filter(gender == "female") %>%
  pull(match_id)

male_match_ids <- matches_gender %>%
  filter(gender == "male") %>%
  pull(match_id)

# Trin 5 - Hent events for begge grupper
# Da computeren ikke kan hente så store datamængder på én gang, hentes eventdata ned i mindre bidder (chunks), som samles bagefter.

# Trin 5.1 - Definér en chunk size
chunk_size <- 50

# --- KVINDER

# Trin 5.2 - Opdel kvindekampe i mindre bidder:
chunks_f <- split(
  female_match_ids,
  ceiling(seq_along(female_match_ids) / chunk_size)
)

# Trin 5.3 - Hent events for kvindekampe, en bid ad gangen:
events_f_list <- lapply(seq_along(chunks_f), function(i) {
  cat("Henter female chunk", i, "ud af", length(chunks_f), "\n")
  getAllEventsMultipleMatches(chunks_f[[i]], gender = "f")
})

# Trin 5.4 - Saml kvindedata i ét datasæt:
events_f <- bind_rows(events_f_list)

# --- MÆND

# Trin 5.5 - Opdel herrekampe i mindre bidder:
chunks_m <- split(
  male_match_ids,
  ceiling(seq_along(male_match_ids) / chunk_size)
)

# Trin 5.6 - Hent events for herrekampe, en bid ad gangen:
events_m_list <- lapply(seq_along(chunks_m), function(i) {
  cat("Henter male chunk", i, "ud af", length(chunks_m), "\n")
  getAllEventsMultipleMatches(chunks_m[[i]], gender = "m")
})

# Trin 5.7 - Saml herredata i ét datasæt:
events_m <- bind_rows(events_m_list)

# Trin 5.8 - Omdøb herrekampenes match-id til samme format som kvindernes:
events_m <- events_m %>%
  mutate(match_id = matchId)

# Trin 6 - Tilføj køn til de to datasæt:
events_f <- events_f %>%
  mutate(gender = "female")

events_m <- events_m %>%
  mutate(gender = "male")

# Trin 7 - Saml alle events i ét datasæt:
events_all <- bind_rows(events_f, events_m)

# Trin 8 - Tjek at datasættet ser rigtigt ud:
glimpse(events_all)
table(events_all$gender, useNA = "ifany")

# Trin 9 - Tjek antal kampe fordelt på køn:
matches_gender %>%
  count(gender)

# -- LAV STATISTIK PR. KAMP --
# Trin 10 - Opgør relevante events og ekstra variable pr. kamp fordelt på køn:
match_stats <- events_all %>%
  group_by(gender, match_id) %>%
  summarise(
    # Afleveringer
    afleveringer = sum(type.name == "Pass", na.rm = TRUE),
    fejlafleveringer = sum(
      type.name == "Pass" & pass.outcome.name == "Incomplete",
      na.rm = TRUE),
    # Skud
    skud = sum(type.name == "Shot", na.rm = TRUE),
    # Mål
    mål = sum(
      type.name == "Shot" & shot.outcome.name == "Goal",
      na.rm = TRUE),
    # Missede skud (robust metode)
    missede_skud = sum(type.name == "Shot", na.rm = TRUE) - mål,
    # xG
    samlet_xg = sum(shot.statsbomb_xg, na.rm = TRUE),
    # Intensitet
    antal_events = n(),
    
    under_pressure = sum(under_pressure == TRUE, na.rm = TRUE),
    
    .groups = "drop")

# Trin 11 - Beregn relative tal for hver variabel:
match_stats <- match_stats %>%
  mutate(
    fejlafleveringer_pct = 100 * fejlafleveringer / afleveringer,
    mål_pct = 100 * mål / skud,
    misset_skud_pct = 100 * missede_skud / skud,
    xg_pr_skud = samlet_xg / skud,
    under_pressure_pct = 100 * under_pressure / antal_events)

# Trin 12 - Lav gennemsnittet af hver variabel pr. køn:
summary_stats <- match_stats %>%
  group_by(gender) %>%
  summarise(
    antal_kampe = n(),
    afleveringer = mean(afleveringer, na.rm = TRUE),
    fejlafleveringer = mean(fejlafleveringer, na.rm = TRUE),
    skud = mean(skud, na.rm = TRUE),
    mål = mean(mål, na.rm = TRUE),
    missede_skud = mean(missede_skud, na.rm = TRUE),

    fejlafleveringer_pct = mean(fejlafleveringer_pct, na.rm = TRUE),
    mål_pct = mean(mål_pct, na.rm = TRUE),
    misset_skud_pct = mean(misset_skud_pct, na.rm = TRUE),

    samlet_xg = mean(samlet_xg, na.rm = TRUE),
    xg_pr_skud = mean(xg_pr_skud, na.rm = TRUE),

    under_pressure = mean(under_pressure, na.rm = TRUE),
    under_pressure_pct = mean(under_pressure_pct, na.rm = TRUE)
  )

# Trin 13 – Lav figurerne

# Trin 13.1 - Fejlafleveringer pr. kamp
ggplot(summary_stats, aes(x = gender, y = fejlafleveringer, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(fejlafleveringer_pct, 1), "%")),
            vjust = -0.5) +
  labs(
    title = "Kvinder laver flere fejlafleveringer end mænd",
    subtitle = "Procenterne viser andelen af afleveringer, der blev til fejlafleveringer",
    x = "Køn",
    y = "Fejlafleveringer pr. kamp",
    caption = "Kilde: StatsBomb (2022-2024)") +
  scale_x_discrete(labels = c("female" = "Kvinder", "male" = "Mænd")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Trin 13.2 - Missede skud pr. kamp
ggplot(summary_stats, aes(x = gender, y = missede_skud, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(misset_skud_pct, 1), "%")),
            vjust = -0.5) +
  labs(
    title = "En større andel af kvinders skud bliver ikke til mål",
    subtitle = "Procenterne viser andelen af skud, der ikke resulterede i mål",
    x = "Køn",
    y = "Missede skud pr. kamp",
    caption = "Kilde: StatsBomb (2022-2024)") +
  scale_x_discrete(labels = c("female" = "Kvinder", "male" = "Mænd")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Trin 13.3 - Mål pr. kamp
ggplot(summary_stats, aes(x = gender, y = mål, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(mål_pct, 1), "%")),
            vjust = -0.5) +
  labs(
    title = "Mænd har en højere andel af skud, der bliver til mål",
    subtitle = "Procenterne viser andelen af skud, der blev til mål",
    x = "Køn",
    y = "Mål pr. kamp",
    caption = "Kilde: StatsBomb (2022-2024)") +
  scale_x_discrete(labels = c("female" = "Kvinder", "male" = "Mænd")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Trin 13.4 - xG pr. kamp
ggplot(summary_stats, aes(x = gender, y = samlet_xg, fill = gender)) +
  geom_col() +
  geom_text(aes(label = round(xg_pr_skud, 3)),
            vjust = -0.5) +
  labs(
    title = "Små forskelle i xG mellem kvinder og mænd",
    subtitle = "Tallene viser gennemsnitlig xG pr. skud",
    x = "Køn",
    y = "Samlet xG pr. kamp",
    caption = "Kilde: StatsBomb (2022-2024)") +
  scale_x_discrete(labels = c("female" = "Kvinder", "male" = "Mænd")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Trin 13.5 - Under pressure pr. kamp
ggplot(summary_stats, aes(x = gender, y = under_pressure, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(under_pressure_pct, 1), "%")),
            vjust = -0.5) +
  labs(
    title = "Kvindekampe foregår oftere under pres end herrekampe",
    subtitle = "Procenterne viser andelen af alle events, der foregår under pres",
    x = "Køn",
    y = "Under pressure-events pr. kamp",
    caption = "Kilde: StatsBomb (2022-2024)") +
  scale_x_discrete(labels = c("female" = "Kvinder", "male" = "Mænd")) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

# Trin 14 - Vis konkret forskel på mænd og kvinder.
# Trin 14.1 - Beregn forskelle mellem kvinder og mænd:
female <- summary_stats %>% filter(gender == "female")
male   <- summary_stats %>% filter(gender == "male")

# Trin 14.2 - Forskelle i procent:
diff_df <- data.frame(
  variabel = c("Fejlafleveringer", "Missede skud", "Mål", "xG pr. skud", "Under pressure"),
  forskel_pct = c(
    (female$fejlafleveringer_pct - male$fejlafleveringer_pct) / male$fejlafleveringer_pct * 100,
    (female$misset_skud_pct - male$misset_skud_pct) / male$misset_skud_pct * 100,
    (female$mål_pct - male$mål_pct) / male$mål_pct * 100,
    (female$xg_pr_skud - male$xg_pr_skud) / male$xg_pr_skud * 100,
    (female$under_pressure_pct - male$under_pressure_pct) / male$under_pressure_pct * 100)
)

# Trin 14.3 - Lav diagram:
ggplot(diff_df, aes(x = variabel, y = forskel_pct, fill = variabel)) +
  geom_col() +
  geom_text(aes(
    label = ifelse(forskel_pct > 0,
                   paste0("Kvinder +", round(forskel_pct, 1), "%"),
                   paste0("Mænd +", round(abs(forskel_pct), 1), "%"))
  ),
  vjust = ifelse(diff_df$forskel_pct > 0, -0.5, 1.5)) +
  labs(
    title = "Tydelige forskelle mellem kvinder og mænd på flere spilparametre",
    subtitle = "Viser hvor meget højere/lavere kvinder ligger ift. mænd",
    x = "",
    y = "Forskel i procent (%)",
    caption = "Kilde: StatsBomb (2022-2024)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )



#
# ---- Opgave 5.2 – Freeze-Frame i tal ----
#
# I skal forsøge at underbygge følgende hypotese med dataanalyse:
# Vi forventer at finde en større procentvis andel af skud som skulle have været afleveret til en bedre placeret spiller hos mænd end hos kvinder.
#

# Fortsæt fra opg 5.1.

# Trin 1 - Filtrér alle skud ud og behold kun skud med freeze-frame-data:
shots <- events_all %>%
  filter(type.name == "Shot") %>%
  filter(!is.na(shot.freeze_frame)) %>%
  filter(lengths(shot.freeze_frame) > 0)

# Trin 2 - Vælg de relevante kolonner til analysen:
shotv <- c(
  "match_id",
  "gender",
  "team.name",
  "player.name",
  "player.id",
  "position.name",
  "location",
  "shot.end_location",
  "shot.freeze_frame",
  "shot.statsbomb_xg",
  "shot.outcome.name")

# Trin 2.1 - Behold kun kolonner, der faktisk findes i datasættet:
shotv <- shotv[shotv %in% colnames(shots)]

# Trin 2.2 - Udvælg de relevante variabler til analysen:
shots_sub <- shots %>%
  select(all_of(shotv))

# Trin 3 - Definér målstolpernes placering i StatsBomb-koordinater:
goal_x <- 120
left_post_y <- 36
right_post_y <- 44

# Trin 4 - Lav funktion der afgør, om et punkt ligger inde i trekanten mellem spiller og målstolper:
# Her ser vi om én bestemt modspiller er inde i trekanten, altså tjekker en af gangen.
point_in_triangle <- function(px, py, ax, ay, bx, by, cx, cy) {
  denominator <- ((by - cy) * (ax - cx) + (cx - bx) * (ay - cy))
  
  if (denominator == 0) {
    return(FALSE)
  }
  
  a <- ((by - cy) * (px - cx) + (cx - bx) * (py - cy)) / denominator
  b <- ((cy - ay) * (px - cx) + (ax - cx) * (py - cy)) / denominator
  c <- 1 - a - b
  
  return(a >= 0 & a <= 1 & b >= 0 & b <= 1 & c >= 0 & c <= 1)
}

# Trin 5 - Definér funktioner til at måle pres og afleveringsmuligheder.
# Trin 5.1 - Lav funktion der tæller, hvor mange modspillere der står i trekanten:
count_opponents_in_triangle <- function(opponents_df, sx, sy) {
  # Tjek: hvis der ikke er modspillere → returnér 0
  if (is.null(opponents_df) || nrow(opponents_df) == 0) {
    return(0)
  }
  # Tjek for hver modspiller om de er inde i trekanten (skytte → målstolper)
  inside_vec <- mapply(
    point_in_triangle,
    px = opponents_df$x,  # modspillerens x-koordinat
    py = opponents_df$y,  # modspillerens y-koordinat
    MoreArgs = list(
      ax = sx, ay = sy,               # skyttens position
      bx = goal_x, by = left_post_y,  # venstre stolpe
      cx = goal_x, cy = right_post_y) # højre stolpe
  )
  # Tæl hvor mange TRUE (modspillere i trekanten)
  sum(inside_vec, na.rm = TRUE)
}

# Trin 5.2 - Lav funktion der beregner afstanden fra et punkt til afleveringslinjen mellem to spillere:
# hvor tæt står en modspiller på linjen mellem skytten og medspilleren?
point_to_line_distance <- function(px, py, x1, y1, x2, y2) {
  # Beregn længden af linjen (skytte → medspiller)
  denominator <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
  # Hvis linjen er 0 (samme punkt), undgå fejl
  if (denominator == 0) {
    return(Inf)
  }
  # Beregn korteste afstand fra modspiller til linjen
  abs((y2 - y1) * px - (x2 - x1) * py + x2 * y1 - y2 * x1) / denominator
}

# Trin 5.3 - Lav funktion der tæller modspillere tæt på afleveringslinjen:
# En modspiller tæller som blokerende, hvis afstanden til afleveringslinjen er højst 2 meter.
count_opponents_near_pass_line <- function(opponents_df, x1, y1, x2, y2, threshold = 2) {
  # Hvis ingen modspillere → returnér 0
  if (is.null(opponents_df) || nrow(opponents_df) == 0) {
    return(0)
  }
  # Beregn afstand fra hver modspiller til afleveringslinjen
  distances <- mapply(
    point_to_line_distance,
    px = opponents_df$x,
    py = opponents_df$y,
    MoreArgs = list(x1 = x1, y1 = y1, x2 = x2, y2 = y2)
  )
  # Tæl hvor mange modspillere der står tæt på linjen (≤ 2 meter)
  sum(distances <= threshold, na.rm = TRUE)
}

# Trin 6 - Lav funktioner der analyserer ét skud og vurderer, om spilleren burde have afleveret til en anden:
# Trin 6.1 - Lav en tom resultat-række til tilfælde, hvor data mangler:
# Den laver en “tom” svar-række.
empty_shot_result <- function() {
  
  data.frame(
    shooter_opponents = NA,
    best_teammate_opponents = NA,
    pass_line_opponents = NA,
    number_of_teammates_checked = NA,
    should_pass = NA
  )
}

# Trin 6.2 - Lav freeze-frame om til dataframe:
# Den tager freeze-frame-data fra ét skud og laver det om til et datasæt, vi kan arbejde med.
prepare_freeze_frame <- function(freeze_frame) {
  
  # Del 1 - Stop hvis freeze-frame mangler helt
  if (is.null(freeze_frame) || length(freeze_frame) == 0) {
    return(NULL)
  }
  
  # Del 2 - Forsøg at lave freeze-frame om til et almindeligt dataframe
  ff <- tryCatch({
    jsonlite::flatten(as.data.frame(freeze_frame))
  }, error = function(e) {
    return(NULL)
  })
  
  # Del 3 - Stop hvis resultatet stadig er tomt eller ugyldigt
  if (is.null(ff) || nrow(ff) == 0) {
    return(NULL)
  }
  
  # Del 4 - Vælg kun de kolonner vi faktisk skal bruge
  ffv <- c("location", "teammate", "player.name", "player.id", "position.name")
  ffv <- ffv[ffv %in% colnames(ff)]
  
  ff %>%
    select(all_of(ffv))
}

# Trin 6.3 - Tilføj skytten og udtræk dens koordinater:
# Den sætter skytten ind i freeze-frame-datasættet og laver spillerpositioner om til x- og y-koordinater.
add_shooter_and_coordinates <- function(ff, shooter_name, shooter_id, shooter_pos, shooter_loc) {
  
  # Del 1 - Opret en række med skyttens oplysninger
  shooter_row <- data.frame(
    location = I(list(shooter_loc)),
    teammate = TRUE,
    player.name = shooter_name,
    player.id = shooter_id,
    position.name = shooter_pos)
  
  # Del 2 - Sæt skytten ind som første række i freeze-frame-data
  ff <- bind_rows(shooter_row, ff)
  
  # Del 3 - Udtræk x- og y-koordinater fra location-listen
  ff <- ff %>%
    mutate(
      x = sapply(location, function(z) ifelse(length(z) >= 1, z[1], NA)),
      y = sapply(location, function(z) ifelse(length(z) >= 2, z[2], NA))
    ) %>%
    filter(!is.na(x), !is.na(y))
  
  ff
}

# Trin 6.4 - Beregn trekant-tal for skytte og medspillere:
# Det er her selve vurderingen sker.
# Den spørger: Hvor lukket står skytten? Hvor lukket står medspillerne? Står mindst én medspiller bedre end skytten?
calculate_triangle_counts <- function(ff) {
  
  # Del 1 - Stop hvis freeze-frame-data er tomt
  if (is.null(ff) || nrow(ff) == 0) {
    return(empty_shot_result())
  }
  
  # Del 2 - Gem skyttens position
  shooter_x <- ff$x[1]
  shooter_y <- ff$y[1]
  
  # Del 3 - Del spillerne op i modspillere og medspillere
  opponents <- ff %>%
  filter(teammate == FALSE & position.name != "Goalkeeper")
  
  teammates <- ff %>%
    filter(teammate == TRUE) %>%
    slice(-1)
  
  # Del 4 - Tæl hvor mange modspillere der står i skyttens trekant
  shooter_opponents <- count_opponents_in_triangle(opponents, shooter_x, shooter_y)
  
  # Del 5 - Hvis der ikke er medspillere at sammenligne med
  if (nrow(teammates) == 0) {
    return(data.frame(
      shooter_opponents = shooter_opponents,
      best_teammate_opponents = NA,
      pass_line_opponents = NA,
      number_of_teammates_checked = 0,
      should_pass = FALSE
    ))
  }
  
  # Del 6 - Beregn både trekant og afleveringslinje for hver medspiller
  teammate_eval <- teammates %>%
    rowwise() %>%
    mutate(
      teammate_opponents = count_opponents_in_triangle(opponents, x, y),
      pass_line_opponents = count_opponents_near_pass_line(
        opponents_df = opponents,
        x1 = shooter_x,
        y1 = shooter_y,
        x2 = x,
        y2 = y,
        threshold = 2
      )
    ) %>%
    ungroup()
  
  # Del 7 - Behold kun medspillere hvor afleveringslinjen er fri
  # Her siger vi, at afleveringen er realistisk hvis der ikke står nogen modspiller tæt på linjen
  viable_teammates <- teammate_eval %>%
    filter(pass_line_opponents == 0)
  
  # Del 8 - Hvis ingen medspillere har en fri afleveringslinje
  if (nrow(viable_teammates) == 0) {
    return(data.frame(
      shooter_opponents = shooter_opponents,
      best_teammate_opponents = NA,
      pass_line_opponents = NA,
      number_of_teammates_checked = 0,
      should_pass = FALSE
    ))
  }
  
  # Del 9 - Find den bedst placerede realistiske medspiller
  best_row <- viable_teammates %>%
    slice_min(order_by = teammate_opponents, n = 1, with_ties = FALSE)
  
  best_teammate_opponents <- best_row$teammate_opponents
  pass_line_opponents <- best_row$pass_line_opponents
  
  # Del 10 - Afgør om skytten burde have afleveret
  should_pass <- best_teammate_opponents < shooter_opponents
  
  # Del 11 - Returnér resultatet
  data.frame(
    shooter_opponents = shooter_opponents,
    best_teammate_opponents = best_teammate_opponents,
    pass_line_opponents = pass_line_opponents,
    number_of_teammates_checked = nrow(viable_teammates),
    should_pass = should_pass
  )
}

# Trin 6.5 - Saml det hele i én overordnet funktion:
# Det er hovedfunktionen, som bruger de tre mindre funktioner i rækkefølge.
analyse_single_shot <- function(freeze_frame, shooter_name, shooter_id, shooter_pos, shooter_loc) {
  
  # Del 1 - Gør freeze-frame-data klar
  ff <- prepare_freeze_frame(freeze_frame)
  
  # Del 2 - Stop hvis freeze-frame ikke kunne bruges
  if (is.null(ff)) {
    return(empty_shot_result())
  }
  
  # Del 3 - Tilføj skytten og udtræk koordinater
  ff <- add_shooter_and_coordinates(
    ff = ff,
    shooter_name = shooter_name,
    shooter_id = shooter_id,
    shooter_pos = shooter_pos,
    shooter_loc = shooter_loc
  )
  
  # Del 4 - Stop hvis der ikke er brugbare koordinater
  if (nrow(ff) == 0) {
    return(empty_shot_result())
  }
  
  # Del 5 - Beregn om skuddet burde have været en aflevering
  calculate_triangle_counts(ff)
}

# Helt simpel samlet forklaring
# De fem funktioner gør tilsammen dette:

# 1. empty_shot_result()
# Laver en tom nød-løsning, hvis data mangler.

# 2. prepare_freeze_frame()
# Gør freeze-frame-data læsbar.

# 3. add_shooter_and_coordinates()
# Sætter skytten ind og laver koordinater.

# 4. calculate_triangle_counts()
# Sammenligner skytte og medspillere.

# 5. analyse_single_shot()
# Binder det hele sammen til én analyse pr. skud.

# Trin 7 - Kør analysen på alle skud:
# Laver en liste af tal fra 1 til antal rækker (skud)
# Kør noget for hvert skud (hver række)
shot_analysis_list <- lapply(1:nrow(shots_sub), function(i) { # lapply laver en liste
# Den tager data for ét skud
  analyse_single_shot(
    freeze_frame = shots_sub$shot.freeze_frame[[i]],
    shooter_name = shots_sub$player.name[i],
    shooter_id = shots_sub$player.id[i],
    shooter_pos = shots_sub$position.name[i],
    shooter_loc = shots_sub$location[[i]]
  )
})

# Trin 7.1 - Samle alle resultater (liste) til ét dataframe:
shot_analysis <- bind_rows(shot_analysis_list)

# Trin 8 - Saml analyseresultaterne med de oprindelige skuddata:
shots_eval <- bind_cols(shots_sub, shot_analysis)

# Trin 9 - Lav en samlet oversigt pr. køn:
freeze_summary <- shots_eval %>%
  filter(!is.na(should_pass)) %>%
  group_by(gender) %>%
  summarise(
    antal_skud = n(),
    antal_skud_der_burde_afleveres = sum(should_pass, na.rm = TRUE),
    andel_burde_afleveres_pct = 100 * mean(should_pass, na.rm = TRUE),
    gennemsnit_modspillere_i_skyttens_trekant = mean(shooter_opponents, na.rm = TRUE),
    gennemsnit_bedste_medspiller_trekant = mean(best_teammate_opponents, na.rm = TRUE),
    .groups = "drop")

# Trin 10 - Lav en simpel visualisering af resultatet:
ggplot(freeze_summary, aes(x = gender, y = andel_burde_afleveres_pct, fill = gender)) +
  geom_col() +
  geom_text(aes(label = paste0(round(andel_burde_afleveres_pct, 1), "%")),
            vjust = -0.5) +
  labs(
    title = "Mænd afslutter oftere i situationer, hvor en aflevering er en bedre løsning",
    subtitle = "En aflevering vurderes som realistisk, hvis medspilleren har færre modspillere i trekanten og afleveringslinjen er fri",
    x = "Køn",
    y = "Andel af skud (%)",
    caption = "Kilde: StatsBomb (2022-2024)") +
  scale_x_discrete(labels = c("female" = "Kvinder", "male" = "Mænd")) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0),
    legend.position = "none")

# Trin 11 - Beregn forskellen mellem mænd og kvinder:
freeze_diff <- freeze_summary %>%
  select(gender, andel_burde_afleveres_pct) %>%
  pivot_wider(names_from = gender, values_from = andel_burde_afleveres_pct) %>%
  mutate(forskel_pctpoint = male - female)

# Andelen er 6.8 procentpoint højere hos mænd end hos kvinder.

# Trin 12 - Find én case hvor spilleren burde have afleveret:
case_should_pass <- shots_eval %>%
  filter(should_pass == TRUE) %>%
  slice(1)

# Trin 13 - Find én case hvor skuddet vurderes som rimeligt:
case_ok_shot <- shots_eval %>%
  filter(should_pass == FALSE) %>%
  slice(1)

# Trin 14 - Lav funktion der plotter én case på en bane:
plot_case <- function(shot_row) {
  
  # Del 1 - Lav freeze-frame om til dataframe og behold relevante kolonner
  ff <- jsonlite::flatten(as.data.frame(shot_row$shot.freeze_frame[[1]])) %>%
    select(any_of(c("location", "teammate", "player.name", "player.id", "position.name")))
  
  # Del 2 - Tilføj skytten som første række
  shooter_row <- data.frame(
    location = I(list(shot_row$location[[1]])),
    teammate = TRUE,
    player.name = shot_row$player.name[[1]],
    player.id = shot_row$player.id[[1]],
    position.name = shot_row$position.name[[1]]
  )
  
  # Lav koordinater og marker skytte
  ff <- bind_rows(shooter_row, ff) %>%
    mutate(
      x = sapply(location, function(z) z[1]),
      y = sapply(location, function(z) z[2]),
      shooter = c(TRUE, rep(FALSE, n() - 1))
    )
  
  # Del 3 - Gem skyttens position og del spillerne op
  shooter_x <- ff$x[1]
  shooter_y <- ff$y[1]
  
  opponents <- ff %>%
    filter(teammate == FALSE)
  
  teammates <- ff %>%
    filter(teammate == TRUE) %>%
    slice(-1)
  
  # Del 4 - Beregn afleveringslinje og trekantværdi for alle medspillere
  if (nrow(teammates) > 0) {
    teammates <- teammates %>%
      rowwise() %>%
      mutate(
        teammate_opponents = count_opponents_in_triangle(opponents, x, y),
        pass_line_opponents = count_opponents_near_pass_line(
          opponents_df = opponents,
          x1 = shooter_x,
          y1 = shooter_y,
          x2 = x,
          y2 = y,
          threshold = 2),
        pass_is_free = pass_line_opponents == 0) %>%
      ungroup()
  }
  
  # Del 5 - Lav trekanten for skytten
  tri_df <- data.frame(
    x = c(ff$x[1], goal_x, goal_x),
    y = c(ff$y[1], left_post_y, right_post_y)
  )
  
  # Del 6 - Lav trekanter for alle medspillere
  if (nrow(teammates) > 0) {
    teammate_tri_df <- teammates %>%
      rowwise() %>%
      do(data.frame(
        player.name = .$player.name,
        x = c(.$x, goal_x, goal_x),
        y = c(.$y, left_post_y, right_post_y)
      ))
  } else {
    teammate_tri_df <- NULL
  }
  
  # Del 7 - Start plottet
  p <- ggplot() +
    # Tegn banen
    annotate_pitch(
      dimensions = pitch_statsbomb,
      colour = "white",
      fill = "#3ab54a") +
    # Tegn skyttens trekant
    geom_polygon(
      data = tri_df,
      aes(x = x, y = y),
      fill = "yellow",
      alpha = 0.25)
  
  # Del 8 - Tegn alle medspillernes trekanter
  if (!is.null(teammate_tri_df)) {
    p <- p +
      geom_polygon(
        data = teammate_tri_df,
        aes(x = x, y = y, group = player.name),
        fill = "blue",
        alpha = 0.08)
  }
  
  # Del 9 - Plot spillere
  p <- p +
    geom_point(
      data = ff,
      aes(x = x, y = y, color = teammate, shape = shooter),
      size = 3) +
    # Tilføj navne
    geom_text(
      data = ff,
      aes(x = x, y = y, label = player.name),
      size = 3,
      vjust = -0.8) +
    # Layout
    theme_pitch() +
    coord_flip(xlim = c(49, 121)) +
    scale_y_reverse() +
    # Titel
    labs(
      title = paste("Case:", shot_row$player.name[[1]]),
      subtitle = paste(
        "Køn:",
        ifelse(shot_row$gender[[1]] == "female", "Kvinder", "Mænd"),
        "| Burde aflevere:",
        ifelse(shot_row$should_pass[[1]], "Ja", "Nej")
      )
    )
  p
}

# Trin 15 - Plot case hvor spilleren burde have afleveret:
plot_case(case_should_pass)

# Trin 16 - Plot case hvor skuddet vurderes som rimeligt:
plot_case(case_ok_shot)
