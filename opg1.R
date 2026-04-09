#
# Opgave 1 – Expected Goals Model (xG model)
#
#
# ---- Opgave 1.1 – Opdeling i trænings- og testdata for skud ----
#
# Indlæs filerne for skud i kampene i Superligaen for sæsonen 2024/2025 og opdel jeres data i træning og test.
# Forklar jeres overvejelser i forhold til opdelingen i træning og test givet data for skud i kampene.

# Trin 0 - Hent pakker:
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Trin 1 - Skab en connection til MySQL databasen:
con <- dbConnect(MariaDB(),
                 host="www.talmedos.com",
                 dbname="superliga2",
                 user="dalremote",
                 password="OttoRehagel123456789Long2026!")

# Trin 2 - Opret dataframe i R, direkte fra dataen i MySQL.
# Trin 2.1 - Opret en vektor der indeholder MySQL koden, der skal bruges til at oprette en dataframe for superligaen (24/25):
sql_super <- "SELECT
  m.COMPETITION_WYID,
  m.SEASON_WYID,
  s.MATCH_WYID,
  m.DATEUTC AS MATCH_DATE_UTC,

  s.EVENT_WYID,

  e.PRIMARYTYPE,
  e.MATCHPERIOD,
  e.MINUTE,
  e.SECOND,

  e.TEAM_WYID,
  t.TEAMNAME AS TEAM_NAME,

  e.PLAYER_WYID,
  CONCAT(pl.FIRSTNAME, ' ', pl.LASTNAME) AS PLAYER_NAME,
  pl.ROLENAME AS PLAYER_ROLE,

  -- SIKKER mål-variabel:
  -- kun hvis eventet er et skud + goal-tag ligger i secondarytype 1/2
    CASE
    WHEN e.PRIMARYTYPE = 'shot'
     AND 'goal' IN (
       st.SECONDARYTYPE1, st.SECONDARYTYPE2, st.SECONDARYTYPE3, st.SECONDARYTYPE4,
       st.SECONDARYTYPE5, st.SECONDARYTYPE6, st.SECONDARYTYPE7, st.SECONDARYTYPE8)
    THEN 1
    ELSE 0
  END AS maal,

  -- behold originalen til sammenligning
  s.SHOTISGOAL,

  s.SHOTONTARGET,
  s.SHOTGOALZONE,
  s.SHOTBODYPART,
  s.SHOTXG,
  s.SHOTPOSTSHOTXG,

  e.LOCATIONX,
  e.LOCATIONY

FROM wyscout_matchevents_shots s

JOIN wyscout_matches m
  ON m.MATCH_WYID = s.MATCH_WYID
 AND m.COMPETITION_WYID = s.COMPETITION_WYID

LEFT JOIN wyscout_matchevents_common e
  ON e.MATCH_WYID = s.MATCH_WYID
 AND e.EVENT_WYID = s.EVENT_WYID
 AND e.COMPETITION_WYID = s.COMPETITION_WYID
 AND e.SEASON_WYID = m.SEASON_WYID

LEFT JOIN wyscout_matchevents_secondarytype st
  ON st.MATCH_WYID = s.MATCH_WYID
 AND st.EVENT_WYID = s.EVENT_WYID
 AND st.COMPETITION_WYID = s.COMPETITION_WYID

LEFT JOIN wyscout_players pl
  ON pl.PLAYER_WYID = e.PLAYER_WYID
 AND pl.SEASON_WYID = m.SEASON_WYID
 AND pl.COMPETITION_WYID = m.COMPETITION_WYID

LEFT JOIN wyscout_teams t
  ON t.TEAM_WYID = e.TEAM_WYID
 AND t.SEASON_WYID = m.SEASON_WYID
 AND t.COMPETITION_WYID = m.COMPETITION_WYID

WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = 189918;
"

# Trin 2.2 - Lav mastertabellen for Superligaen:
master_shots <- dbGetQuery(con, sql_super)

# Trin 2.3 - Luk forbindelsen:
dbDisconnect(con)

# Trin 2.4 - Fjern dubletter:
master_shots <- master_shots %>%
  distinct(MATCH_WYID, EVENT_WYID, .keep_all = TRUE)

# Trin 3 - Fjern alt der ikke rent skud (åbent spil), da disse kan påvirke træningsdataen:
master_shots <- master_shots %>%
  filter(PRIMARYTYPE == "shot")

# -- Opdeling ved hjælp af K cross validation --

# Trin 4 - Installere og indlæs pakken:
library(caret)

# Trin 5 - Lav K-fold på kampniveau (MATCH_WYID) for at undgå leakage:
set.seed(123)

K <- 5

# Trin 5.1 – Identificér unikke kampe (MATCH_WYID):
# Formål: Vi vil lave folds på kampniveau, ikke skudniveau.
matches <- unique(master_shots$MATCH_WYID)


# Trin 5.2 – Lav folds ud fra kampene:
# Formål: Hver fold indeholder hele kampe (ingen kamp deles).
match_folds <- createFolds(matches, k = K, list = TRUE)

# Trin 5.3 – Opret train/test datasæt for hver fold:
# Formål: Alle skud fra en kamp skal enten være i train eller test.
fold_data <- vector("list", K)

for (i in 1:K) {
  
  # Test-kampe i fold i
  test_matches <- matches[match_folds[[i]]]
  
  # Resten bliver train
  train_matches <- matches[-match_folds[[i]]]
  
  # Lav train og test datasæt
  train_data <- master_shots %>%
    filter(MATCH_WYID %in% train_matches)
  
  test_data <- master_shots %>%
    filter(MATCH_WYID %in% test_matches)
  
  # Gem dem i en liste
  fold_data[[i]] <- list(
    train = train_data,
    test = test_data
  )
}

# Trin 5.4 – Tjek størrelse på test og train-data i hver fold (kontrol af leakage og målrate):
# Formål: Tjek at der ikke er overlap i kampe og at målprocenten ligner hinanden.
for (i in 1:K) {
  cat("Fold", i, "\n")
  cat("Train shots:", nrow(fold_data[[i]]$train), "\n")
  cat("Test shots:", nrow(fold_data[[i]]$test), "\n")
  cat("Train kampe:", length(unique(fold_data[[i]]$train$MATCH_WYID)), "\n")
  cat("Test kampe:", length(unique(fold_data[[i]]$test$MATCH_WYID)), "\n\n")
}


#
# ---- Opgave 1.2 – Forklarende variable, forklaring og grafiske illustrationer beskrivende statistik ----
#
# Giv en kort beskrivelse af de forklarende variable som I ønsker at benytte i en xG model.
# Derudover skal I lave grafiske illustrationer af de ønskede forklarende variable, samt beskrivende statistik.

#
# -- Procent opdeling på mål/ikke-mål --
#

# Trin 1 - Tilføje mål-status:
  master_shots <- master_shots %>%
  mutate(
    status = ifelse(maal == 1, "Mål", "Ikke mål")
  )

# Trin 2 - Lav en dataframe med procenter for mål vs ikke mål:
  skud_data <- master_shots %>%
  count(status, name = "antal") %>%
  mutate(procent = 100 * antal / sum(antal))

# Trin 3 - Lav graf over procent for mål og ikke mål:
ggplot(skud_data,
       aes(x = status,
           y = procent,
           fill = status)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(procent,1), " %")),
            vjust = -0.5) +
  scale_fill_manual(values = c("Ikke mål" = "firebrick",
                               "Mål" = "darkgreen")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Langt størstedelen af skud bliver ikke til mål",
    subtitle = "Superligaen, 2024/2025",
    x = "",
    y = "Procent (%)",
    caption = "Kilde: Wyscout, 2024/25") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

#
# -- Afstand til målet, opdeling i målprocent pr. afstandsinterval --
#

# Trin 4 - Beregn x- og y-koordinater til meter og afstanden til målet:
  master_shots <- master_shots %>%
    mutate(
      x_meter = LOCATIONX * 105 / 100,
      y_meter = LOCATIONY * 68 / 100,
      afstand_meter = sqrt((105 - x_meter)^2 + (34 - y_meter)^2)
    )

# Trin 5 - Lav afstandsintervaller på 1 meter:
afstand_data <- master_shots %>%
  mutate(
    afstand_bin = floor(afstand_meter)
  )

# Trin 6 - Beregn målprocent for hvert afstandsinterval:
afstand_opdelt <- afstand_data %>%
  group_by(afstand_bin) %>%
  summarise(
    antal_skud = n(),
    antal_maal = sum(maal == 1),
    maalprocent = 100 * antal_maal / antal_skud,
    .groups = "drop") %>%
  filter(antal_skud >= 10)

# Trin 7 - Lav linjegraf over målprocent pr. afstand:
ggplot(afstand_opdelt,
       aes(x = afstand_bin,
           y = maalprocent)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Afstand til målet har en tydelig negativ sammenhæng med målprocent",
    subtitle = "Superligaen, 2024/2025",
    x = "Afstand til målet (meter)",
    y = "Målprocent (%)",
    caption = "Kilde: Wyscout, 2024/25") +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0)
  )

#
# -- Vinkel opdeling for mål og ikke mål --
#

# Trin 7 - Fastlæggelse af målets position:
  goal_width_y <- 7.32 / 68 * 100
  left_post_y  <- 50 - goal_width_y / 2
  right_post_y <- 50 + goal_width_y / 2
  goal_x <- 100
  
# Trin 8 - Vinkel til målet på skuddene:
  master_shots <- master_shots %>%
    mutate(
      dx = goal_x - LOCATIONX,
      dy_left  = left_post_y  - LOCATIONY,
      dy_right = right_post_y - LOCATIONY,
      angle_left  = atan2(dy_left,  dx),
      angle_right = atan2(dy_right, dx),
      vinkel_maal_rad  = abs(angle_right - angle_left),
      vinkel_maal_grad = vinkel_maal_rad * 180 / pi) %>%
    select(-dx, -dy_left, -dy_right, -angle_left, -angle_right, -vinkel_maal_rad)

# Trin 9 – Gns. vinkel ved mål vs ikke-mål:
  vinkel_opdelt <- master_shots %>%
  group_by(status) %>%
  summarise(
    gennemsnit_vinkel = mean(vinkel_maal_grad, na.rm = TRUE),
    .groups = "drop")
  
# Trin 10 - Søjlediagram over skudvinkel:
  ggplot(vinkel_opdelt,
         aes(x = status,
             y = gennemsnit_vinkel,
             fill = status)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(gennemsnit_vinkel, 2)),
            vjust = -0.5) +
  scale_fill_manual(
    values = c("Mål" = "darkgreen",
                "Ikke mål" = "firebrick")) +
   labs(
     title = "Større skudvinkel øger sandsynligheden for mål",
     subtitle = "Superligaen, 2024/2025",
     x = "",
     y = "Gennemsnitlig skudvinkel (grader)",
     caption = "Kilde: Wyscout, 2024/25") +
   theme_minimal() +
   theme(legend.position = "none",
     plot.caption = element_text(hjust = 0)
   )

#
# -- Beregn målprocent pr kropsdel --
#

# Trin 11 - Beregn målprocent opdelt efter kropsdel:
bodypart_stats <- master_shots %>%
  mutate(
    kropsdel = case_when(
      !is.na(SHOTBODYPART) & grepl("head", SHOTBODYPART, ignore.case = TRUE) ~ "Hovedstød",
      TRUE ~ "Andet") ) %>%
  group_by(kropsdel) %>%
  summarise(
    antal_skud = n(),
    antal_maal = sum(maal == 1),
    maalprocent = 100 * antal_maal / antal_skud,
    .groups = "drop")

# Trin 12 - Lav graf over andel af kropsdele der scorer mål:
ggplot(bodypart_stats, aes(x = kropsdel, y = maalprocent, fill = kropsdel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(maalprocent, 1), " %")),
            vjust = -0.5) +
  labs(
    title = "Lavere målprocent ved hovedstød end ved øvrige afslutninger",
     subtitle = "Superligaen, 2024/2025",
    x = "Kropsdel",
    y = "Målprocent",
    caption = "Kilde: Wyscout, 2024/25") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

#
# -- Målprocent pr. kampperiode --
#

# Trin 13 - Beregn målprocent opdelt efter kampperiode:
matchperiod_stats <- master_shots %>%
  mutate(
    kampperiode = case_when(
      MATCHPERIOD == "1H" ~ "1. halvleg",
      MATCHPERIOD == "2H" ~ "2. halvleg",
      TRUE ~ as.character(MATCHPERIOD) ) ) %>%
  group_by(kampperiode) %>%
  summarise(
    antal_skud = n(),
    antal_maal = sum(maal == 1),
    maalprocent = 100 * antal_maal / antal_skud,
    .groups = "drop")

# Trin 14 - Kombineret graf: antal mål + målprocent:
ggplot(matchperiod_stats, aes(x = kampperiode)) +
  # Søjler = antal mål
  geom_col(aes(y = antal_maal, fill = kampperiode), alpha = 0.7) +
  # Linje = målprocent (skaleret)
  geom_line(aes(y = maalprocent * max(antal_maal) / 100, group = 1)) +
  geom_point(aes(y = maalprocent * max(antal_maal) / 100)) +
  # Labels for målprocent
  geom_text(aes(y = maalprocent * max(antal_maal) / 100,
                label = paste0(round(maalprocent,1), " %")),
            vjust = -1) +
  # Labels for antal mål
  geom_text(aes(y = antal_maal, label = antal_maal),
            vjust = 1.5, color = "white") +
  # Dual axis
  scale_y_continuous(
    name = "Antal mål",
    sec.axis = sec_axis(~ . * 100 / max(matchperiod_stats$antal_maal),
                        name = "Målprocent (%)") ) +
  labs(
    title = "Flere mål i 2. halvleg – men højere effektivitet i 1. halvleg",
    subtitle = "Superligaen, 2024/2025",
    x = "Kampperiode",
    caption = "Kilde: Wyscout, 2024/25") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )

#
# ---- Opgave 1.3 – Forklarende variable og effekt på om et skud bliver til mål ----
#
# Hvilke af jeres valgte variable vil I forvente har en effekt på om et skud bliver til et mål i Superligaen 2024/2025?
#

# Trin 1 - Lav et model-datasæt med de variable, vi vil bruge:
master_shots_model <- master_shots %>%
  mutate(
    kropsdel = case_when(
      !is.na(SHOTBODYPART) & grepl("head", SHOTBODYPART, ignore.case = TRUE) ~ "Hovedstød",
      TRUE ~ "Andet"),
    kropsdel = factor(kropsdel),
    MATCHPERIOD = factor(MATCHPERIOD),
    maal = as.integer(maal),
    afstand_meter = as.numeric(afstand_meter),
    vinkel_maal_grad = as.numeric(vinkel_maal_grad)
  )

# Trin 2 - Sæt Andet som referencekategori:
master_shots_model$kropsdel <- relevel(master_shots_model$kropsdel, ref = "Andet")

# Trin 3 - Lav en logistisk regression på de udvalgte variable:
model <- glm(
  maal ~ afstand_meter + vinkel_maal_grad + kropsdel + MATCHPERIOD,
  data = master_shots_model,
  family = binomial)

# Trin 4 - Se den logistiske regressionsmodel:
summary(model)


#
# ---- Opgave 1.4 – Forudsige om et givent skud bliver til et mål ----
#
# Opstil en klassificeringsmodel til forudsigelse af om et givent skud bliver til et mål.
# I skal benytte trænings- og testdata til at udvælge jeres model.
#

# Trin 1 - Hent ekstra pakker:
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

# Trin 2 - Lav et model-datasæt med de variable, vi vil bruge:
model_data <- master_shots %>%
  mutate(
    kropsdel = case_when(
      grepl("head", SHOTBODYPART, ignore.case = TRUE) ~ "Hovedstød",
      TRUE ~ "Andet"),
    kropsdel = factor(kropsdel),
    maal = as.integer(maal),
    afstand_meter = as.numeric(afstand_meter),
    vinkel_maal_grad = as.numeric(vinkel_maal_grad),
    status = factor(
      ifelse(maal == 1, "Mål", "Ikke mål"),
      levels = c("Ikke mål", "Mål") ) ) %>%
  select(MATCH_WYID, maal, status, afstand_meter, vinkel_maal_grad, kropsdel)

# Trin 3 - Lav nyt K-fold split på kampniveau ud fra model-datasættet:
set.seed(123)

K <- 5
matches_model <- unique(model_data$MATCH_WYID)
match_folds_model <- createFolds(matches_model, k = K, list = TRUE)

fold_data_model <- vector("list", K)

for (i in 1:K) {
  
  test_matches <- matches_model[match_folds_model[[i]]]
  train_matches <- setdiff(matches_model, test_matches)
  
  fold_data_model[[i]] <- list(
    train = model_data %>% filter(MATCH_WYID %in% train_matches),
    test  = model_data %>% filter(MATCH_WYID %in% test_matches)
  )
}

#
# -- Logistisk regression --
#

# Trin 5 - Opret objekter til at gemme accuracy og predictioner:
results_glm <- numeric(K)
all_pred_glm <- c()
all_true_glm <- c()

# Trin 6 - Træn og test modellen via K-fold cross validation:
for (i in 1:K) {
  
  # Opdel i train og test data:
  train_data <- fold_data_model[[i]]$train
  test_data  <- fold_data_model[[i]]$test
  
  # Estimér logistisk regressionsmodel:
  model_glm <- glm(
    maal ~ afstand_meter + vinkel_maal_grad + kropsdel,
    data = train_data,
    family = binomial)
  
  # Beregn sandsynligheder (prediction):
  pred_prob_glm <- predict(model_glm, newdata = test_data, type = "response")
  
  # Klassificér til mål / ikke mål:
  pred_glm <- ifelse(pred_prob_glm >= 0.5, "Mål", "Ikke mål")
  pred_glm <- factor(pred_glm, levels = c("Ikke mål", "Mål"))
  
  # Beregn accuracy:
  results_glm[i] <- mean(pred_glm == test_data$status)
  
  # Gem predictioner og faktiske værdier:
  all_pred_glm <- c(all_pred_glm, as.character(pred_glm))
  all_true_glm <- c(all_true_glm, as.character(test_data$status))
}

# Trin 7 - Beregn gennemsnitlig accuracy:
mean_accuracy_glm <- mean(results_glm)

mean_accuracy_glm

# Trin 8 - Lav confusion matrix:
conf_glm <- confusionMatrix(
  factor(all_pred_glm, levels = c("Ikke mål", "Mål")),
  factor(all_true_glm, levels = c("Ikke mål", "Mål"))
)

conf_glm

summary(model_glm)

#
# -- Beslutningstræ (klassifikationsmodel) --
#

# Trin 9 - Opret objekter til at gemme accuracy og predictioner:
results_tree <- numeric(K)
all_pred_tree <- c()
all_true_tree <- c()

# Trin 10 - Træn og test modellen via K-fold cross validation:
for (i in 1:K) {
  
  # Opdel i train og test data:
  train_data <- fold_data_model[[i]]$train
  test_data  <- fold_data_model[[i]]$test
  
  # Estimér beslutningstræ:
  model_tree <- rpart(
    status ~ afstand_meter + vinkel_maal_grad + kropsdel,
    data = train_data,
    method = "class",
    control = rpart.control(cp = 0.01)
  )
  
  # Lav prediction:
  pred_tree <- predict(model_tree, newdata = test_data, type = "class")
  
  # Beregn accuracy:
  results_tree[i] <- mean(pred_tree == test_data$status)
  
  # Gem predictioner og faktiske værdier:
  all_pred_tree <- c(all_pred_tree, as.character(pred_tree))
  all_true_tree <- c(all_true_tree, as.character(test_data$status))
}

# Trin 11 - Beregn gennemsnitlig accuracy:
mean_accuracy_tree <- mean(results_tree)

mean_accuracy_tree

# Trin 12 - Lav confusion matrix:
conf_tree <- confusionMatrix(
  factor(all_pred_tree, levels = c("Ikke mål", "Mål")),
  factor(all_true_tree, levels = c("Ikke mål", "Mål"))
)

conf_tree

# Trin 13 - Visualiser beslutningstræet
# Trin 13.1 - Estimér beslutningstræ baseret på træningsdata:
tree_plot_model <- rpart(
  status ~ afstand_meter + vinkel_maal_grad + kropsdel,
  data = fold_data_model[[1]]$train,
  method = "class",
  control = rpart.control(cp = 0.01)
)

# Trin 13.2 - Plot beslutningstræet:
rpart.plot(
  tree_plot_model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE
)

# Trin 13.3 - Tilføj titel og kilde til figuren:
title("Beslutningstræ for forudsigelse af mål", line = 3)
mtext("Superligaen, 2024/2025", side = 3, line = 2, cex = 0.9)
mtext("Kilde: Egen tilvirkning baseret på Wyscout", side = 1, line = 4, adj = 0, cex = 0.8)

#
# -- Random forest (klassifikationsmodel) --
#

# Trin 13 - Opret objekter til at gemme accuracy og predictioner:
results_rf <- numeric(K)
all_pred_rf <- c()
all_true_rf <- c()

# Trin 14 - Træn og test modellen via K-fold cross validation:
for (i in 1:K) {
  
  # Trin Opdel i train og test data:
  train_data <- fold_data_model[[i]]$train
  test_data  <- fold_data_model[[i]]$test
  
  # Trin Estimér random forest:
  model_rf <- randomForest(
    status ~ afstand_meter + vinkel_maal_grad + kropsdel,
    data = train_data,
    ntree = 300,
    importance = TRUE)
  
  # Trin Lav prediction:
  pred_rf <- predict(model_rf, newdata = test_data, type = "class")
  
  # Trin Beregn accuracy:
  results_rf[i] <- mean(pred_rf == test_data$status)
  
  # Trin Gem predictioner og faktiske værdier:
  all_pred_rf <- c(all_pred_rf, as.character(pred_rf))
  all_true_rf <- c(all_true_rf, as.character(test_data$status))
}

# Trin 15 - Beregn gennemsnitlig accuracy:
mean_accuracy_rf <- mean(results_rf)

mean_accuracy_rf

# Trin 16 - Lav confusion matrix:
conf_rf <- confusionMatrix(
  factor(all_pred_rf, levels = c("Ikke mål", "Mål")),
  factor(all_true_rf, levels = c("Ikke mål", "Mål"))
)

conf_rf

# Trin 17 - Visualiser variable importance i random forest:
rf_plot_model <- randomForest(
  status ~ afstand_meter + vinkel_maal_grad + kropsdel,
  data = model_data,
  ntree = 300,
  importance = TRUE)

varImpPlot(rf_plot_model, main = "Variable importance i random forest")

#
# -- Sammenligning af klassifikationsmodeller --
#

# Trin 18 - Saml modellernes gennemsnitlige accuracy i en tabel:
model_results <- data.frame(
  Model = c("Logistisk regression", "Beslutningstræ", "Random forest"),
  Mean_Accuracy = c(mean_accuracy_glm, mean_accuracy_tree, mean_accuracy_rf)
)

# Trin 19 - Vis resultattabellen:
model_results


#
# ---- Opgave 1.6 – I virkeligheden ----
#
# Hvad kan Brøndby IF bruge jeres model og resultater til? (Hint: Test jeres model på data for den indeværende sæson.)
#

# Trin 1 - Lav en sql kode:
sql_nuv <- "SELECT
  m.COMPETITION_WYID,
  m.SEASON_WYID,
  s.MATCH_WYID,
  m.DATEUTC AS MATCH_DATE_UTC,

  s.EVENT_WYID,

  e.PRIMARYTYPE,
  e.MATCHPERIOD,
  e.MINUTE,
  e.SECOND,

  e.TEAM_WYID,
  t.TEAMNAME AS TEAM_NAME,

  e.PLAYER_WYID,
  CONCAT(pl.FIRSTNAME, ' ', pl.LASTNAME) AS PLAYER_NAME,
  pl.ROLENAME AS PLAYER_ROLE,

  -- SIKKER mål-variabel:
  -- kun hvis eventet er et skud + goal-tag ligger i secondarytype 1/2
    CASE
    WHEN e.PRIMARYTYPE = 'shot'
     AND 'goal' IN (
       st.SECONDARYTYPE1, st.SECONDARYTYPE2, st.SECONDARYTYPE3, st.SECONDARYTYPE4,
       st.SECONDARYTYPE5, st.SECONDARYTYPE6, st.SECONDARYTYPE7, st.SECONDARYTYPE8)
    THEN 1
    ELSE 0
  END AS maal,

  -- behold originalen til sammenligning
  s.SHOTISGOAL,

  s.SHOTONTARGET,
  s.SHOTGOALZONE,
  s.SHOTBODYPART,
  s.SHOTXG,
  s.SHOTPOSTSHOTXG,

  e.LOCATIONX,
  e.LOCATIONY

FROM wyscout_matchevents_shots s

JOIN wyscout_matches m
  ON m.MATCH_WYID = s.MATCH_WYID
 AND m.COMPETITION_WYID = s.COMPETITION_WYID

LEFT JOIN wyscout_matchevents_common e
  ON e.MATCH_WYID = s.MATCH_WYID
 AND e.EVENT_WYID = s.EVENT_WYID
 AND e.COMPETITION_WYID = s.COMPETITION_WYID
 AND e.SEASON_WYID = m.SEASON_WYID

LEFT JOIN wyscout_matchevents_secondarytype st
  ON st.MATCH_WYID = s.MATCH_WYID
 AND st.EVENT_WYID = s.EVENT_WYID
 AND st.COMPETITION_WYID = s.COMPETITION_WYID

LEFT JOIN wyscout_players pl
  ON pl.PLAYER_WYID = e.PLAYER_WYID
 AND pl.SEASON_WYID = m.SEASON_WYID
 AND pl.COMPETITION_WYID = m.COMPETITION_WYID

LEFT JOIN wyscout_teams t
  ON t.TEAM_WYID = e.TEAM_WYID
 AND t.SEASON_WYID = m.SEASON_WYID
 AND t.COMPETITION_WYID = m.COMPETITION_WYID

WHERE m.COMPETITION_WYID = 335
  AND m.SEASON_WYID = 191611;
"

# Trin 2 - Hent et nyt dataframe ned for den indeværende sæson:
master_nuv <- dbGetQuery(con, sql_nuv)

# Trin 3 - Fjern dubletter:
master_nuv <- master_nuv %>%
  distinct(MATCH_WYID, EVENT_WYID, .keep_all = TRUE)

# Trin 4 - Fjern alt der ikke rent skud (åbent spil), da disse kan påvirke træningsdataen:
master_nuv <- master_nuv %>%
  filter(PRIMARYTYPE == "shot")

# Trin 5 - Lav alle de variabler vi skal bruge til modellen:
master_nuv <- master_nuv %>%
  mutate(
    x_meter = LOCATIONX * 105 / 100,
    y_meter = LOCATIONY * 68 / 100,
    afstand_meter = sqrt((105 - x_meter)^2 + (34 - y_meter)^2),
    
    dx = 100 - LOCATIONX,
    goal_width_y <- 7.32 / 68 * 100,
    left_post_y  <- 50 - goal_width_y / 2,
    right_post_y <- 50 + goal_width_y / 2,
    
    dy_left  = left_post_y  - LOCATIONY,
    dy_right = right_post_y - LOCATIONY,
    
    angle_left  = atan2(dy_left, dx),
    angle_right = atan2(dy_right, dx),
    
    vinkel_maal_grad = abs(angle_right - angle_left) * 180 / pi,
    
    kropsdel = case_when(
      grepl("head", SHOTBODYPART, ignore.case = TRUE) ~ "Hovedstød",
      TRUE ~ "Andet"),
    kropsdel = factor(kropsdel),
    status = factor(ifelse(maal == 1, "Mål", "Ikke mål"),
                    levels = c("Ikke mål", "Mål"))
  )

# Trin 6 - Træn "final model" på det gamle datasæt:
final_model <- glm(
  status ~ afstand_meter + vinkel_maal_grad + kropsdel,
  data = model_data,
  family = binomial)

# Trin 7 - Beregn xG på nuværende sæson:
master_nuv$xG <- predict(
  final_model,
  newdata = master_nuv,
  type = "response")

# Trin 8 - Filtrer til kun Brøndby:
brondby_nuv <- master_nuv %>%
  filter(TEAM_NAME == "Brøndby")

# Trin 9 - Sammenlign xG og mål:
brondby_summary <- brondby_nuv %>%
  summarise(
    skud = n(),
    maal = sum(maal),
    xG = sum(xG) )
