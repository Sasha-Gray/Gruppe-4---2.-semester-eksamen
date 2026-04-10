#
# Opgave 3 – Overblik over kampe i superligaen
#
#
# ---- Opgave 3.1 – Afleveringer ----
# Lav en Clustering model af afleveringer i indeværende sæson. I bestemmer selv antallet af clustre.
# Hvad kendetegner jeres clustre som modellen valgt at opdele afleveringerne på? 

# Trin 1 - Hent pakker:
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(cluster)
library(factoextra)

# Trin 2 -  Opret forbindelse til MySQL:
con <- dbConnect(MariaDB(),
                 host="www.talmedos.com",
                 dbname="superliga2",
                 user="dalremote",
                 password="kode")

# Trin 3 - Lav MySQL kode for nyeste sæson i Superligaen:
sql_passes <- "
SELECT
    c.COMPETITION_WYID,
    c.MATCH_WYID,
    c.EVENT_WYID,
    m.DATEUTC AS MATCH_DATE_UTC,
    c.MATCHPERIOD,
    c.MINUTE,
    c.SECOND,
    c.TEAM_WYID,
    t.TEAMNAME,
    c.PLAYER_WYID,
    pl.SHORTNAME,

    c.LOCATIONX,
    c.LOCATIONY,

    c.PRIMARYTYPE,

    p.ACCURATE,
    p.ANGLE,
    p.HEIGHT,
    p.LENGTH,
    p.RECIPIENT_WYID,
    p.ENDLOCATIONX,
    p.ENDLOCATIONY,

    st.SECONDARYTYPE1,
    st.SECONDARYTYPE2,
    st.SECONDARYTYPE3,
    st.SECONDARYTYPE4,
    st.SECONDARYTYPE5,
    st.SECONDARYTYPE6

FROM wyscout_matchevents_passes p

LEFT JOIN wyscout_matchevents_common c
    ON p.EVENT_WYID = c.EVENT_WYID
   AND p.MATCH_WYID = c.MATCH_WYID
   AND p.COMPETITION_WYID = c.COMPETITION_WYID

LEFT JOIN wyscout_matches m
  ON c.MATCH_WYID = m.MATCH_WYID
 AND c.COMPETITION_WYID = m.COMPETITION_WYID

LEFT JOIN wyscout_matchevents_secondarytype st
    ON p.EVENT_WYID = st.EVENT_WYID
   AND p.MATCH_WYID = st.MATCH_WYID
   AND p.COMPETITION_WYID = st.COMPETITION_WYID

LEFT JOIN wyscout_players pl
    ON c.PLAYER_WYID = pl.PLAYER_WYID
   AND m.SEASON_WYID = pl.SEASON_WYID
   AND c.COMPETITION_WYID = pl.COMPETITION_WYID

LEFT JOIN wyscout_teams t
    ON c.TEAM_WYID = t.TEAM_WYID
   AND m.SEASON_WYID = t.SEASON_WYID
   AND c.COMPETITION_WYID = t.COMPETITION_WYID

WHERE c.PRIMARYTYPE = 'pass'
AND  m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = 191611
"

# Trin 4 - Hent data ned i R for 25/26 sæson:
passes_df <- dbGetQuery(con, sql_passes)

# Trin 5 - Luk forbindelse:
dbDisconnect(con)

# Trin 6 - Rens data
# Trin 6.1 - Lav tal-variablerne om til numeric:
passes_df <- passes_df %>%
  distinct() %>%
  mutate(
    LOCATIONX = as.numeric(LOCATIONX),
    LOCATIONY = as.numeric(LOCATIONY),
    ENDLOCATIONX = as.numeric(ENDLOCATIONX),
    ENDLOCATIONY = as.numeric(ENDLOCATIONY),
    LENGTH = as.numeric(LENGTH)
  )

# Trin 6.2 - Fjern dubletter:
passes_df <- passes_df %>%
  distinct(MATCH_WYID, EVENT_WYID, .keep_all = TRUE)

# Trin 7 - Feature engineering
# Trin 7.1 - Beregn progression og offensive mål:
# Her beregner vi hvordan afleveringen bevæger bolden.
passes_df <- passes_df %>%
  mutate(
    progression_x = ENDLOCATIONX - LOCATIONX,
    bredde = abs(ENDLOCATIONY - LOCATIONY),
    afstand_til_maal_efter = sqrt((100 - ENDLOCATIONX)^2 + (50 - ENDLOCATIONY)^2)
  )

# Trin 7.2 - Saml secondary types i én variabel:
# Her samler vi alle secondary types, så vi nemt kan søge i dem.
passes_df <- passes_df %>%
  mutate(
    secondary_all = paste(
      SECONDARYTYPE1, SECONDARYTYPE2, SECONDARYTYPE3, SECONDARYTYPE4, SECONDARYTYPE5,
      SECONDARYTYPE6,
      sep = ",")
  )

# Trin 7.3 - Opret dummy-variable for afleveringstyper:
# Her oversætter vi tekst til brugbare variable (0/1), som gør det nemmere at analysere og forklare clusters bagefter.
# Disse bruge ikke til selve clustering, men kun til at analysere på de forskellige clustre.
passes_df <- passes_df %>%
  mutate(
    # Retning / spiltype
    is_forward = ifelse(str_detect(secondary_all, "forward_pass"), 1, 0),
    is_back = ifelse(str_detect(secondary_all, "back_pass"), 1, 0),

    # Progression / gennembrud
    is_progressive = ifelse(str_detect(secondary_all, "progressive_pass"), 1, 0),

    # Offensive zoner
    is_final_third = ifelse(str_detect(secondary_all, "pass_to_final_third"), 1, 0),
    is_penalty_area = ifelse(str_detect(secondary_all, "pass_to_penalty_area"), 1, 0)
  )

# --- ALLE KAMPE/HOLD I INDEVÆRENDE SÆSON --- #

# Trin 8 - Vælg variable til clustering:
# Vi bruger få, men meningsfulde variable, der beskriver afleveringens længde, fremdrift, bredde og offensive værdi.
cluster_data <- passes_df %>%
  select(
    LENGTH,
    progression_x,
    bredde,
    afstand_til_maal_efter) %>%
  na.omit()

# Gem matchende afleveringsdata
passes_df_cluster <- passes_df %>%
  filter(
    !is.na(LENGTH),
    !is.na(progression_x),
    !is.na(bredde),
    !is.na(afstand_til_maal_efter)
  )

# Trin 9 - Standardiser dataene, så de alle får samme skala:
# Variablerne blev standardiseret før clustering for at sikre, at forskelle i skala ikke påvirker afstandsberegningen og dermed segmenteringsresultatet.
# (Da k-means baseres på euklidisk afstand, blev variablerne z-standardiseret for at undgå, at variabler med større numerisk skala dominerer analysen.)
cluster_scaled <- scale(cluster_data)

# Trin 10 - Find antal clustre:
# Antal clusters bliver valgt ud fra elbow-metoden, hvor vi vælger k ved knækket i kurven.

# Trin 10.1 - Så man får samme resultat hver gang
set.seed(123)

# Trin 10.2 -  Udvælgelse af stikprøve fra datasættet:
sample_idx <- sample(1:nrow(cluster_scaled), 10000)

# Trin 10.3 -  Oprettelse af datasample til clustering:
cluster_sample <- cluster_scaled[sample_idx, ]

# Trin 10.4 - Bestemmelse af antal clusters (Elbow-metoden):
fviz_nbclust(cluster_sample, kmeans, method = "wss")

# Ud fra elbow-plottet, vurderes det at det mest optimale antal clustre er 4.

# Trin 11 – Kør K-means (med k = 4):
set.seed(123)

k <- 4

kmeans_model <- kmeans(cluster_scaled, centers = k, nstart = 25)

# Trin 12 – Gem cluster på afleveringerne:
passes_df_cluster$cluster <- kmeans_model$cluster

# Trin 13 - Lav navne til de forskellige clustre:
passes_df_cluster <- passes_df_cluster %>%
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Korte opbygningsafleveringer",
    cluster == 2 ~ "Lange angrebsafleveringer",
    cluster == 3 ~ "Sidevendende afleveringer",
    cluster == 4 ~ "Tilbage/sikre afleveringer" ))

# Trin 14 - Beskriv clustrene med gns af hver variabel:
cluster_summary <- passes_df_cluster %>%
  group_by(cluster) %>%
  summarise(
    antal = n(),
    gns_længde = mean(LENGTH, na.rm = TRUE),
    gns_progression = mean(progression_x, na.rm = TRUE),
    gns_bredde = mean(bredde, na.rm = TRUE),
    gns_afstand_til_maal_efter = mean(afstand_til_maal_efter, na.rm = TRUE),

    andel_forward = mean(is_forward, na.rm = TRUE),
    andel_back = mean(is_back, na.rm = TRUE),
    andel_progressive = mean(is_progressive, na.rm = TRUE),
    andel_final_third = mean(is_final_third, na.rm = TRUE),
    andel_penalty_area = mean(is_penalty_area, na.rm = TRUE),

    .groups = "drop"
  )
# For at fortolke de identificerede clusters blev der beregnet gennemsnit for centrale variable samt andele af forskellige afleveringstyper.
# Dette gør det muligt at karakterisere hver cluster og identificere forskellige typer afleveringer.

# Trin 14.1 - Tilføj navne på clustrene i summarytabellen:
cluster_summary <- cluster_summary %>%
  mutate(cluster_navn = case_when(
    cluster == 1 ~ "Korte opbygningsafleveringer",
    cluster == 2 ~ "Lange angrebsafleveringer",
    cluster == 3 ~ "Sidevendende afleveringer",
    cluster == 4 ~ "Tilbage/sikre afleveringer"
  ))

# Trin 15 - Scatterplot af clustre:
set.seed(123)

ggplot(passes_df_cluster %>% sample_n(3000), 
       aes(x = LENGTH, y = afstand_til_maal_efter, color = cluster_label)) +
  geom_point(alpha = 0.5, size = 1) +
  labs(
    title = "Clustering af afleveringer",
    x = "Længde",
    y = "Afstand til mål efter aflevering",
    color = "Type aflevering",
    caption = "Kilde: Wyscout data (Superligaen 2025/2026)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )


# --- CLUSTRE KUN PÅ BRØNDBY I INDEVÆRENDE SÆSON --- #

# Trin 16 - Udtræk Brøndby:
brondby_df <- passes_df %>%
  filter(TEAMNAME == "Brøndby")

# Trin 17 – Lav clustering-data for Brøndby:
cluster_data_brondby <- brondby_df %>%
  select(
    LENGTH,
    progression_x,
    bredde,
    afstand_til_maal_efter) %>%
  na.omit()

# Trin 18 - Standardiser:
cluster_scaled_brondby <- scale(cluster_data_brondby)

# Trin 19 – Find antal clustre:
set.seed(123)

sample_idx <- sample(1:nrow(cluster_scaled_brondby), 
                     min(5000, nrow(cluster_scaled_brondby)))

cluster_sample_brondby <- cluster_scaled_brondby[sample_idx, ]

fviz_nbclust(cluster_sample_brondby, kmeans, method = "wss")

# Trin 20 – K-means på Brøndby:
set.seed(123)

k_brondby <- 4

kmeans_brondby <- kmeans(cluster_scaled_brondby, 
                         centers = k_brondby, 
                         nstart = 25)

brondby_df$cluster_brondby <- kmeans_brondby$cluster

# Trin 21 – Beskriv Brøndby-clustre:
brondby_summary <- brondby_df %>%
  group_by(cluster_brondby) %>%
  summarise(
    antal = n(),
    gns_længde = mean(LENGTH, na.rm = TRUE),
    gns_progression = mean(progression_x, na.rm = TRUE),
    gns_bredde = mean(bredde, na.rm = TRUE),
    gns_afstand_til_maal_efter = mean(afstand_til_maal_efter, na.rm = TRUE),

    andel_forward = mean(is_forward, na.rm = TRUE),
    andel_back = mean(is_back, na.rm = TRUE),
    andel_progressive = mean(is_progressive, na.rm = TRUE),
    andel_final_third = mean(is_final_third, na.rm = TRUE),
    andel_penalty_area = mean(is_penalty_area, na.rm = TRUE),

    .groups = "drop")

# Trin 22 - Navngiv clustrene i summary:
brondby_summary <- brondby_summary %>%
  mutate(cluster_navn = case_when(
    cluster_brondby == 1 ~ "Korte fremadrettede opbygningsafleveringer",
    cluster_brondby == 2 ~ "Korte tilbage- og støtteafleveringer",
    cluster_brondby == 3 ~ "Lange direkte angrebsafleveringer",
    cluster_brondby == 4 ~ "Sidevendende afleveringer"
  ))

# Trin 23- Navngiv clustrene i df'en:
brondby_df <- brondby_df %>%
  mutate(cluster_label_brondby = case_when(
    cluster_brondby == 1 ~ "Korte fremadrettede opbygningsafleveringer",
    cluster_brondby == 2 ~ "Korte tilbage- og støtteafleveringer",
    cluster_brondby == 3 ~ "Lange direkte angrebsafleveringer",
    cluster_brondby == 4 ~ "Sidevendende afleveringer"))

# Trin 24 - Scatterplot af clustre for Brøndby:
set.seed(123)

ggplot(brondby_df %>% sample_n(5000),
       aes(x = LENGTH, y = afstand_til_maal_efter, color = cluster_label_brondby)) +
  geom_point(alpha = 0.5, size = 1) +
  labs(
    title = "Clustering af Brøndbys afleveringer",
    x = "Længde",
    y = "Afstand til mål efter aflevering",
    color = "Type aflevering",
    caption = "Kilde: Wyscout data (Superligaen 2025/2026)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )


#
# ---- Opgave 3.2 – kampe ---- 
#
# Lav en Clustering model af kampe i indeværende sæson for hvert hold - altså to rækker pr kamp.
# I bestemmer selv antallet af clustre. Hvad kendetegner jeres clustre som modellen valgt at opdele kampene? 
#

# Trin 1 - Lav MySQL kode for nyeste sæson i Superligaen for tabellen med aggregeret data:
sql_matches <- "SELECT 
    g.*,
    t.TEAMNAME,
    m.DATEUTC AS MATCH_DATE_UTC
FROM wyscout_matchadvancedstats_general g

LEFT JOIN wyscout_matches m
    ON g.MATCH_WYID = m.MATCH_WYID
   AND g.COMPETITION_WYID = m.COMPETITION_WYID

LEFT JOIN wyscout_teams t
    ON g.TEAM_WYID = t.TEAM_WYID
   AND m.SEASON_WYID = t.SEASON_WYID
   AND g.COMPETITION_WYID = t.COMPETITION_WYID

WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = 191611
"

# Trin 2 - Hent data ned i R for kampe i 25/26 sæson:
matches_df <- dbGetQuery(con, sql_matches)

# Trin 3 - Lav passes tabellen til gns pr. kamp
# Trin 3.1 - Lav én række pr kamp og hold, med de udvalgte variabler:
passes_match_summary <- passes_df %>%
  group_by(MATCH_WYID, TEAM_WYID) %>%
  summarise(
    total_passes = n(),
    gns_length = mean(LENGTH, na.rm = TRUE),
    gns_progression = mean(progression_x, na.rm = TRUE),
    .groups = "drop")

# Trin 3.2 - Join passes med mathes-tabellen (join på matches og hold):
matches_df <- matches_df %>%
  left_join(passes_match_summary, by = c("MATCH_WYID", "TEAM_WYID"))

# Trin 4 - Vælg variable til clustering:
cluster_data_matches <- matches_df %>%
  select(
    # Skud
    SHOTS,
    XG,
    PROGRESSIVERUNS,
    TOUCHESINBOX,
    # Afleveringer
    total_passes,
    gns_length,
    gns_progression) %>%
  na.omit()

# Trin 5 - Standardiser dataene, så de alle får samme skala:
cluster_scaled_matches <- scale(cluster_data_matches)

# --- ALLE KAMPE I SÆSONEN ---

# Trin 6 - Find antal clustre:
# Antal clusters bliver valgt ud fra elbow-metoden, hvor vi vælger k ved knækket i kurven.

# Trin 6.1 - Så man får samme resultat hver gang
set.seed(123)

# Trin 6.2 -  Udvælgelse af stikprøve fra datasættet:
# Datasættet er for stort til at kunne køre det hele.
sample_idx <- sample(1:nrow(cluster_scaled_matches), 
                     min(5000, nrow(cluster_scaled_matches)))

# Trin 6.3 -  Oprettelse af datasample til clustering:
cluster_sample_matches <- cluster_scaled_matches[sample_idx, ]

# Trin 6.4 - Bestemmelse af antal clusters (Elbow-metoden):
fviz_nbclust(cluster_sample_matches, kmeans, method = "wss")

# Ud fra elbow-plottet, vurderes det at det mest optimale antal clustre er 3.

# Trin 7  – Kør K-means:
set.seed(123)

k_matches <- 3

kmeans_matches <- kmeans(cluster_scaled_matches, 
                         centers = k_matches, 
                         nstart = 25)

# Trin 8 - Gem clusters:
matches_df_cluster <- matches_df %>%
  filter(
    !is.na(SHOTS),
    !is.na(XG),
    !is.na(PROGRESSIVERUNS),
    !is.na(TOUCHESINBOX),
    !is.na(total_passes),
    !is.na(gns_length),
    !is.na(gns_progression)
  )

matches_df_cluster$cluster <- kmeans_matches$cluster

# Trin 9 - Beskriv clusters:
matches_summary <- matches_df_cluster %>%
  group_by(cluster) %>%
  summarise(
    antal = n(),
    gns_skud = mean(SHOTS, na.rm = TRUE),
    gns_xg = mean(XG, na.rm = TRUE),
    gns_progressive_runs = mean(PROGRESSIVERUNS, na.rm = TRUE),
    gns_touch_box = mean(TOUCHESINBOX, na.rm = TRUE),
    gns_passes = mean(total_passes, na.rm = TRUE),
    gns_pass_length = mean(gns_length, na.rm = TRUE),
    gns_pass_progression = mean(gns_progression, na.rm = TRUE),
    .groups = "drop" )

# Trin 10 - Giv clusterne navne:
# I summary
matches_summary <- matches_summary %>%
  mutate(cluster_navn = case_when(
    cluster == 1 ~ "Mere direkte kampe",
    cluster == 2 ~ "Kontrollerede og chance-skabende kampe",
    cluster == 3 ~ "Hurtige fremadrettede kampe"))

matches_df_cluster <- matches_df_cluster %>%
  mutate(cluster_label = case_when(
    cluster == 1 ~ "Mere direkte kampe",
    cluster == 2 ~ "Kontrollerede og chance-skabende kampe",
    cluster == 3 ~ "Hurtige fremadrettede kampe"))

# Trin 11 - Lav visualiseringer:
# Scatterplot af alle kampene opdelt i de tre clustre
ggplot(matches_df_cluster,
       aes(x = XG, y = total_passes, color = cluster_label)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Clustering af kampe",
    x = "Expected Goals (xG)",
    y = "Antal afleveringer",
    color = "Kamp-type",
    caption = "Kilde: Wyscout data (Superligaen 2025/2026)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# Gennemsnits-plot af clusterne
ggplot(matches_summary,
       aes(x = gns_xg, y = gns_passes, label = cluster_navn)) +
  geom_point(size = 4) +
  geom_text(vjust = -1) +
  labs(
    title = "Overblik over kamptyper",
    x = "Gennemsnitlig xG",
    y = "Gennemsnitligt antal afleveringer",
    caption = "Kilde: Wyscout data (Superligaen 2025/2026)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )

# --- UDTRÆK KUN BRØNDBY KAMPE ---

# Trin 12 – Udtræk Brøndbys kampe fra de eksisterende clustre:
brondby_matches <- matches_df_cluster %>%
  filter(TEAMNAME == "Brøndby")

# Trin 13 – Se hvordan Brøndbys kampe fordeler sig på clustrene:
brondby_cluster_count <- brondby_matches %>%
  group_by(cluster, cluster_label) %>%
  summarise(
    antal_kampe = n(),
    .groups = "drop" ) %>%
  mutate(
    andel = antal_kampe / sum(antal_kampe)
  )

# Trin 14 - Summary for Brøndbys kampe fordelt på cluster:
brondby_matches_summary <- brondby_matches %>%
  group_by(cluster, cluster_label) %>%
  summarise(
    antal_kampe = n(),
    gns_skud = mean(SHOTS, na.rm = TRUE),
    gns_xg = mean(XG, na.rm = TRUE),
    gns_progressive_runs = mean(PROGRESSIVERUNS, na.rm = TRUE),
    gns_touch_box = mean(TOUCHESINBOX, na.rm = TRUE),
    gns_passes = mean(total_passes, na.rm = TRUE),
    gns_pass_length = mean(gns_length, na.rm = TRUE),
    gns_pass_progression = mean(gns_progression, na.rm = TRUE),
    .groups = "drop")

# Trin 15 - Visualisering af Brøndbys kamptyper
# Trin 15.1 – Visualisering af Brøndbys fordeling på kamp-typer:
ggplot(brondby_cluster_count,
       aes(x = cluster_label, y = antal_kampe, fill = cluster_label)) +
  geom_col() +
  labs(
    title = "Brøndbys kampe er primært præget af kontrol og direkte angrebsspil",
    x = "Kamp-type",
    y = "Antal kampe",
    fill = "Kamp-type",
    caption = "Kilde: Wyscout data (Superligaen 2025/2026)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.caption = element_text(hjust = 0)
  )

# Trin 15.2 – Visualisering af kun Brøndbys kampe i scatterplot:
ggplot(brondby_matches,
       aes(x = XG, y = total_passes, color = cluster_label)) +
  geom_point(alpha = 0.8, size = 3) +
  labs(
    title = "Brøndbys kampe fordelt på kamp-typer",
    x = "Expected Goals (xG)",
    y = "Antal afleveringer",
    color = "Kamp-type",
    caption = "Kilde: Wyscout data (Superligaen 2025/2026)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )



