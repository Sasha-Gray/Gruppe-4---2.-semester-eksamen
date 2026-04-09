#
# Opgave 2 – Expected points model (xP model)
#
#
# ---- Opgave 2.1 – Opsætning af model for Expected Points (xP) -----
#
# Baseret på xG i data for Superligaen 2024/2025 skal I udregne Expected Points (xP) for os.
# Dette kræver, at I simulererkampene et passende antal gange for at finde sandsynligheden for forskellige udfald i de enkelte kampe.
# Når det er gjort, kan I udregne, hvor mange point vi burde have.


# Kør hele koden fra opgave 1 først, så har vi alle datasættene korrekt renset, og logistiske regressions-modellen.
source("opg1.R")

# ---- SHOTXG fra Wyscout -----

# Trin 1 - Lav et datasæt med kun Brøndbys kampe:
# Her beholder vi alle skud i kampene, så både Brøndby og modstanderen er med
brondby_kampe <- master_shots %>%
  group_by(MATCH_WYID) %>%
  filter(any(TEAM_NAME == "Brøndby")) %>%
  ungroup()

# Trin 2 - Behold kun de variable vi skal bruge:
xp_data <- brondby_kampe %>%
  filter(PRIMARYTYPE == "shot") %>%
  select(MATCH_WYID, TEAM_NAME, SHOTXG, maal)

# Trin 3 - Sørg for at xG er numerisk:
xp_data <- xp_data %>%
  mutate(
    SHOTXG = as.numeric(SHOTXG)
  )

# Trin 4 - Lav en liste med Brøndbys kamp-id'er:
kampe <- unique(xp_data$MATCH_WYID)

# Trin 5 - Opret en tom liste til resultater:
xp_resultater <- list()

# Trin 6 - Simulation af kampe og beregning af expected points (xP):
# Få de samme resultater hver gang vi kører koden:
set.seed(123)

# Trin 6.1 - Find informationer til hver kamp:
for (i in 1:length(kampe)) {
  
  # Vælg én kamp
  kamp_id <- kampe[i]
  
  kamp_data <- xp_data %>%
    filter(MATCH_WYID == kamp_id)
  
  # Find Brøndbys xG i kampen
  brondby_xg <- kamp_data %>%
    filter(TEAM_NAME == "Brøndby") %>%
    pull(SHOTXG)
  
  # Find modstanderens navn
  modstander <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    distinct(TEAM_NAME) %>%
    pull(TEAM_NAME)
  
  # Find modstanderens xG i kampen
  modstander_xg <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    pull(SHOTXG)
  
  # Opret tomme vektorer til simulerede mål
  brondby_maal_sim <- c()
  modstander_maal_sim <- c()


# Trin 6.2 - Simulér hver kamp 1000 gange:
  for (j in 1:1000) {
    
    # Simulér Brøndbys mål
    brondby_maal <- sum(rbinom(length(brondby_xg), 1, brondby_xg))
    
    # Simulér modstanderens mål
    modstander_maal <- sum(rbinom(length(modstander_xg), 1, modstander_xg))
    
    # Gem målene
    brondby_maal_sim <- c(brondby_maal_sim, brondby_maal)
    modstander_maal_sim <- c(modstander_maal_sim, modstander_maal)
  }
  
# Trin 6.3 - Beregn sandsynligheder for sejr, uafgjort og nederlag:
  p_sejr <- mean(brondby_maal_sim > modstander_maal_sim)
  p_uafgjort <- mean(brondby_maal_sim == modstander_maal_sim)
  p_nederlag <- mean(brondby_maal_sim < modstander_maal_sim)
  
# Trin 6.4 - Beregn xP
  xP <- 3 * p_sejr + 1 * p_uafgjort
  
# Trin 6.5 - Beregn faktisk antal mål i kampen
  faktiske_brondby_maal <- kamp_data %>%
    filter(TEAM_NAME == "Brøndby") %>%
    summarise(maal = sum(maal)) %>%
    pull(maal)
  
  faktiske_modstander_maal <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    summarise(maal = sum(maal)) %>%
    pull(maal)
  
# Trin 6.6 - Beregn faktiske point
  faktiske_point <- ifelse(faktiske_brondby_maal > faktiske_modstander_maal, 3,
                           ifelse(faktiske_brondby_maal == faktiske_modstander_maal, 1, 0))
  
# Trin 6.7 - Gem resultatet
  xp_resultater[[i]] <- data.frame(
    MATCH_WYID = kamp_id,
    modstander = modstander[1],
    brondby_maal = faktiske_brondby_maal,
    modstander_maal = faktiske_modstander_maal,
    faktiske_point = faktiske_point,
    sandsynlighed_sejr = p_sejr,
    sandsynlighed_uafgjort = p_uafgjort,
    sandsynlighed_nederlag = p_nederlag,
    xP = xP
  )
}

# Trin 7  - Saml alle kampe i én tabel:
xp_kampe <- bind_rows(xp_resultater)

# Trin 8 - Afrund tallene:
xp_kampe <- xp_kampe %>%
  mutate(
    sandsynlighed_sejr = round(sandsynlighed_sejr, 3),
    sandsynlighed_uafgjort = round(sandsynlighed_uafgjort, 3),
    sandsynlighed_nederlag = round(sandsynlighed_nederlag, 3),
    xP = round(xP, 2)
  )

# Trin 9 - Vis resultaterne pr. kamp:
xp_kampe

# Trin 10 - Beregn samlet xP og samlede faktiske point for Brøndby:
xp_overblik <- xp_kampe %>%
  summarise(
    antal_kampe = n(),
    samlet_xP = sum(xP),
    samlede_faktiske_point = sum(faktiske_point)
  )

# Trin 11 - Afrund resultatet
xp_overblik <- xp_overblik %>%
  mutate(
    samlet_xP = round(samlet_xP, 2)
  )

# Trin 12 - Vis samlet overblik
xp_overblik


# ---- Expected points model (xP) for Brøndby IF med egen xG ----

# Trin 1 - Lav et nyt datasæt til opgave 2, hvor vi tilføjer en ny kolonne med vores egen xG:
master_shots_opg2 <- master_shots_model %>%
  mutate(
    xg_logit_opg2 = predict(model, newdata = master_shots_model, type = "response")
  )

# Trin 2 - Lav et datasæt med kun Brøndbys kampe:
# Her beholder vi alle skud i kampene, så både Brøndby og modstanderen er med
brondby_kampe_opg2 <- master_shots_opg2 %>%
  group_by(MATCH_WYID) %>%
  filter(any(TEAM_NAME == "Brøndby")) %>%
  ungroup()

# Trin 3 - Behold kun de variable vi skal bruge:
xp_data_opg2 <- brondby_kampe_opg2 %>%
  filter(PRIMARYTYPE == "shot") %>%
  select(MATCH_WYID, TEAM_NAME, xg_logit_opg2, maal)

# Trin 4 - Sørg for at xG er numerisk:
xp_data_opg2 <- xp_data_opg2 %>%
  mutate(
    xg_logit_opg2 = as.numeric(xg_logit_opg2)
  )

# Trin 5 - Lav en liste med Brøndbys kamp-id'er:
kampe_opg2 <- unique(xp_data_opg2$MATCH_WYID)

# Trin 6 - Opret tomme lister til resultater:
xp_resultater_logit_opg2 <- list()

# Trin 7 - Simulation af kampe og beregning af expected points (xP):
# Få de samme resultater hver gang vi kører koden:
set.seed(123)

# Trin 7.1 - Find informationer til hver kamp:
for (i in 1:length(kampe_opg2)) {
  
 # Vælg én kamp
  kamp_id <- kampe_opg2[i]
  
  kamp_data <- xp_data_opg2 %>%
    filter(MATCH_WYID == kamp_id)
  
 # Find Brøndbys xG i kampen
  brondby_logitxg <- kamp_data %>%
    filter(TEAM_NAME == "Brøndby") %>%
    pull(xg_logit_opg2)
  
  # Find modstanderens navn
  modstander <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    distinct(TEAM_NAME) %>%
    pull(TEAM_NAME)
  
  # Find modstanderens xG i kampen
  modstander_logitxg <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    pull(xg_logit_opg2)
  
  # Opret tomme vektorer til simulerede mål
  brondby_maal_sim_logit <- c()
  modstander_maal_sim_logit <- c()
  
  # Trin 7.2 - Simulér kampen 1000 gange:
  for (j in 1:1000) {
    
   # Simulering med egen logistisk xG
    brondby_maal_logit <- sum(rbinom(length(brondby_logitxg), 1, brondby_logitxg))
    modstander_maal_logit <- sum(rbinom(length(modstander_logitxg), 1, modstander_logitxg))
    
    brondby_maal_sim_logit <- c(brondby_maal_sim_logit, brondby_maal_logit)
    modstander_maal_sim_logit <- c(modstander_maal_sim_logit, modstander_maal_logit)
  }
  
  # Trin 7.3 - Beregn sandsynligheder og xP for logistisk xG
  p_sejr_logit <- mean(brondby_maal_sim_logit > modstander_maal_sim_logit)
  p_uafgjort_logit <- mean(brondby_maal_sim_logit == modstander_maal_sim_logit)
  p_nederlag_logit <- mean(brondby_maal_sim_logit < modstander_maal_sim_logit)
  
  # Trin 7.4 - Beregn xP for vores xG
  xP_logit <- 3 * p_sejr_logit + 1 * p_uafgjort_logit
  
  # Trin 7.5 - Beregn faktiske antal mål i kampen
  faktiske_brondby_maal <- kamp_data %>%
    filter(TEAM_NAME == "Brøndby") %>%
    summarise(maal = sum(maal)) %>%
    pull(maal)
  
  faktiske_modstander_maal <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    summarise(maal = sum(maal)) %>%
    pull(maal)
  
  # Trin 7.6 - Beregn faktiske point
  faktiske_point <- ifelse(faktiske_brondby_maal > faktiske_modstander_maal, 3,
                           ifelse(faktiske_brondby_maal == faktiske_modstander_maal, 1, 0))
  
 # Trin 7.7 - Gem resultat for logistisk xG
  xp_resultater_logit_opg2[[i]] <- data.frame(
    MATCH_WYID = kamp_id,
    modstander = modstander[1],
    brondby_maal = faktiske_brondby_maal,
    modstander_maal = faktiske_modstander_maal,
    faktiske_point = faktiske_point,
    sandsynlighed_sejr = p_sejr_logit,
    sandsynlighed_uafgjort = p_uafgjort_logit,
    sandsynlighed_nederlag = p_nederlag_logit,
    xP = xP_logit)
}

# Trin 8 - Saml alle kampe i en tabel:
xp_kampe_logit_opg2 <- bind_rows(xp_resultater_logit_opg2)

# Trin 9 - Afrund tallene
xp_kampe_logit_opg2 <- xp_kampe_logit_opg2 %>%
  mutate(
    sandsynlighed_sejr = round(sandsynlighed_sejr, 3),
    sandsynlighed_uafgjort = round(sandsynlighed_uafgjort, 3),
    sandsynlighed_nederlag = round(sandsynlighed_nederlag, 3),
    xP = round(xP, 2)
  )

# Trin 10 - Vis resultaterne pr. kamp:
xp_kampe_logit_opg2

# Trin 11 - Beregn samlet xP og samlede faktiske point for Brøndby:
xp_overblik_logit_opg2 <- xp_kampe_logit_opg2 %>%
  summarise(
    antal_kampe = n(),
    samlet_xP = sum(xP),
    samlede_faktiske_point = sum(faktiske_point)
  )

# Trin 12 - Afrund resultatet:
xp_overblik_logit_opg2 <- xp_overblik_logit_opg2 %>%
  mutate(samlet_xP = round(samlet_xP, 2))

# Trin 13 - Vis samlet overblik:
xp_overblik_logit_opg2


# ---- Expected points model (xP) med POST-SHOT xG ----

# Trin 1 - Lav nyt datasæt (nyt navn, rører ikke det gamle):
xp_post_master <- master_shots_model %>%
  mutate(
    xp_post_xg = as.numeric(SHOTPOSTSHOTXG)
  )

# Trin 2 - Lav datasæt med kun Brøndbys kampe:
xp_post_kampe <- xp_post_master %>%
  group_by(MATCH_WYID) %>%
  filter(any(TEAM_NAME == "Brøndby")) %>%
  ungroup()

# Trin 3 - Behold kun relevante variable:
xp_post_data <- xp_post_kampe %>%
  filter(PRIMARYTYPE == "shot") %>%
  select(MATCH_WYID, TEAM_NAME, xp_post_xg, maal) %>%
  filter(!is.na(xp_post_xg),
         xp_post_xg >= 0,
         xp_post_xg <= 1)

# Trin 4 - Liste med kampe:
xp_post_kampeliste <- unique(xp_post_data$MATCH_WYID)

# Trin 5 - Tom liste til resultater:
xp_post_resultater <- list()

# Trin 6 - Simulation af kampe og beregning af expected points (xP):
# Få de samme resultater hver gang vi kører koden:
set.seed(123)

# Trin 6.1 - Find informationer til hver kamp
for (i in 1:length(xp_post_kampeliste)) {
  
  kamp_id <- xp_post_kampeliste[i]
  
  kamp_data <- xp_post_data %>%
    filter(MATCH_WYID == kamp_id)
  
  # Brøndby post_xG
  brondby_xg <- kamp_data %>%
    filter(TEAM_NAME == "Brøndby") %>%
    pull(xp_post_xg)
  
  # Modstander navn
  modstander <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    distinct(TEAM_NAME) %>%
    pull(TEAM_NAME)
  
  # Modstander post_xG
  modstander_xg <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    pull(xp_post_xg)
  
  # Simulerede mål
  brondby_maal_sim <- c()
  modstander_maal_sim <- c()

# Trin 6.2 - Simulér kampen 1000 gange:
  for (j in 1:1000) {

# Simulering med Post_xG
    brondby_maal <- sum(rbinom(length(brondby_xg), 1, brondby_xg))
    modstander_maal <- sum(rbinom(length(modstander_xg), 1, modstander_xg))
    
    brondby_maal_sim <- c(brondby_maal_sim, brondby_maal)
    modstander_maal_sim <- c(modstander_maal_sim, modstander_maal)
  }
  
  # Trin 6.3 - Beregn sandsynligheder og xP for logistisk xG
  p_sejr <- mean(brondby_maal_sim > modstander_maal_sim)
  p_uafgjort <- mean(brondby_maal_sim == modstander_maal_sim)
  p_nederlag <- mean(brondby_maal_sim < modstander_maal_sim)
  
  # Trin 6.4 - Beregn xP for Post_xG
  xP <- 3 * p_sejr + 1 * p_uafgjort
  
  # Trin 6.5 - Beregn faktiske antal mål i kampen
  faktiske_brondby_maal <- kamp_data %>%
    filter(TEAM_NAME == "Brøndby") %>%
    summarise(sum(maal)) %>%
    pull()
  
  faktiske_modstander_maal <- kamp_data %>%
    filter(TEAM_NAME != "Brøndby") %>%
    summarise(sum(maal)) %>%
    pull()
  
  # Trin 6.6 - Beregn faktiske point
  faktiske_point <- ifelse(faktiske_brondby_maal > faktiske_modstander_maal, 3,
                           ifelse(faktiske_brondby_maal == faktiske_modstander_maal, 1, 0))
  
  # Trin 6.7 - Gem resultat for Post_xG
  xp_post_resultater[[i]] <- data.frame(
    MATCH_WYID = kamp_id,
    modstander = modstander[1],
    brondby_maal = faktiske_brondby_maal,
    modstander_maal = faktiske_modstander_maal,
    faktiske_point = faktiske_point,
    sandsynlighed_sejr = p_sejr,
    sandsynlighed_uafgjort = p_uafgjort,
    sandsynlighed_nederlag = p_nederlag,
    xP = xP
  )
}

# Trin 7 - Saml alle kampe i en tabel:
xp_post_kampe_df <- bind_rows(xp_post_resultater)

# Trin 8 - Afrund tallene:
xp_post_kampe_df <- xp_post_kampe_df %>%
  mutate(
    sandsynlighed_sejr = round(sandsynlighed_sejr, 3),
    sandsynlighed_uafgjort = round(sandsynlighed_uafgjort, 3),
    sandsynlighed_nederlag = round(sandsynlighed_nederlag, 3),
    xP = round(xP, 2)
  )

# Trin 9 - Resultater pr kamp
xp_post_kampe_df

# Trin 10 - Samlet overblik
xp_post_overblik <- xp_post_kampe_df %>%
  summarise(
    antal_kampe = n(),
    samlet_xP = round(sum(xP), 2),
    samlede_faktiske_point = sum(faktiske_point)
  )

xp_post_overblik

# ---- Graf over de 3 forskellige metoder af beregning af Xp ----

# Trin 1 - Hent dato fra original data:
kamp_dato <- master_shots %>%
  select(MATCH_WYID, MATCH_DATE_UTC) %>%
  distinct()

# Trin 2 - Merge kamp_datosættet med de 3 datasæt:
xp_kampe <- xp_kampe %>%
  left_join(kamp_dato, by = "MATCH_WYID")

xp_kampe_logit_opg2 <- xp_kampe_logit_opg2 %>%
  left_join(kamp_dato, by = "MATCH_WYID")

xp_post_kampe_df <- xp_post_kampe_df %>%
  left_join(kamp_dato, by = "MATCH_WYID")

# Trin 3 - Graf over ShotXg fra datasætte wyscout:
ggplot(xp_kampe, aes(x = MATCH_DATE_UTC)) +
  # xP
  geom_line(aes(y = xP), color = "steelblue", size = 1.2) +
  # Faktiske point
  geom_line(aes(y = faktiske_point), color = "red", linetype = "dashed") +
  geom_point(aes(y = faktiske_point), color = "red") +
  labs(
    title = "SHOTXG: En del afvigelser mellem forventede og faktiske point i enkelte kampe",
    subtitle = "Blå = xP | Rød = Faktiske point",
    x = "Dato",
    y = "Point",
    caption = "Kilde: Wyscount SHOTXG") +
  theme_minimal() +
theme(
  plot.caption = element_text(hjust = 0)
)

# Trin 4 - Graf over vores eget beregnet Xg:
ggplot(xp_kampe_logit_opg2, aes(x = MATCH_DATE_UTC)) +
  geom_line(aes(y = xP), color = "steelblue", size = 1.2) +
  geom_line(aes(y = faktiske_point), color = "red", linetype = "dashed") +
  geom_point(aes(y = faktiske_point), color = "red") +
  labs(
    title ="EGEN XG: Stabil sammenhæng mellem forventede og faktiske point",
    subtitle = "Blå = xP | Rød = Faktiske point",
    x = "Dato",
    y = "Point",
    caption = "Kilde: Wyscout og vores egen beregning (Logistisk regression)") +
  theme_minimal() +
  theme(
  plot.caption = element_text(hjust = 0)
)

# Trin 5 Graf over Post_shot_Xg: 
ggplot(xp_post_kampe_df, aes(x = MATCH_DATE_UTC)) +
  geom_line(aes(y = xP), color = "steelblue", size = 1.2) +
  geom_line(aes(y = faktiske_point), color = "red", linetype = "dashed") +
  geom_point(aes(y = faktiske_point), color = "red") +
  labs(
    title = "POST_SHOT_XG: Resultater påvirkes stærkt af afslutningskvalitet",
    subtitle = "Blå = xP | Rød = Faktiske point",
    x = "Dato",
    y = "Point",
    caption = "Kilde: Wyscout (Post-shot xG)") +
  theme_minimal() +
  theme(
  plot.caption = element_text(hjust = 0)
)



bar_data <- data.frame(
  Model = c("SHOTXG", "Logistisk xG", "Post-shot xG"),
  xP = c(
    xp_overblik$samlet_xP[1],
    xp_overblik_logit_opg2$samlet_xP[1],
    xp_post_overblik$samlet_xP[1]
  )
)

# Faktiske point
faktiske_point <- xp_overblik$samlede_faktiske_point[1]

ggplot(bar_data, aes(x = Model, y = xP, fill = Model)) +
  
  # Søjler (modeller)
  geom_col(width = 0.6) +
  
  # Vandret linje (faktiske point)
  geom_hline(yintercept = faktiske_point,
             linetype = "dashed",
             color = "red",
             size = 1.2) +
  
  # Tekst på søjler
  geom_text(aes(label = round(xP, 2)), vjust = -0.5) +
  
  # Tekst på linjen
  annotate("text",
           x = 2,
           y = faktiske_point + 1,
           label = paste("Faktiske point:", faktiske_point),
           color = "red") +
  
  labs(
    title = "Forventede point vs. faktiske point",
    x = "Model",
    y = "Point",
    caption = "Kilde: Wyscout og egne beregninger"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )
