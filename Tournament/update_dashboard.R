library(jsonlite)
library(dplyr)
library(stringr)

run_models_and_clean <- function(raw_df) {
  # 1. Clean data, calculate metrics, and compute Simulated Elo safely
  processed_df <- raw_df %>% 
    mutate(
      kdr = Kills / pmax(Death, 1),
      dodge_ratio = ifelse(is.nan((Evaded / total_attacks_against) * 100) | total_attacks_against == 0, 0, (Evaded / total_attacks_against) * 100),
      crit_ratio = ifelse(is.nan((Crit / total_attacks_made) * 100) | total_attacks_made == 0, 0, (Crit / total_attacks_made) * 100),
      accuracy = ifelse(is.na(Acc.), 0, Acc.*.01),
      wins = Win
    ) %>%
    mutate(
      simulated_elo = round(
        (kdr * 100) + 
          ifelse(is.na(dodge_ratio), 0, dodge_ratio) + 
          ifelse(is.na(crit_ratio), 0, crit_ratio) + 
          (accuracy * 100) + 
          (wins * 100)
      )
    )
  
  # 2. Build Top 3 Performers by Stage using Simulated Elo
  stages_list <- processed_df %>%
    group_by(Stage) %>%
    slice_max(order_by = simulated_elo, n = 3, with_ties = FALSE) %>%
    group_split() %>%
    lapply(function(df) {
      list(
        stage = unique(df$Stage),
        top_performers = df %>% select(Unit, kdr, dodge = dodge_ratio, crit = crit_ratio, simulated_elo) %>% as.list()
      )
    })
  
  # 3. Softmax Composite Score Model
  processed_df <- processed_df %>%
    mutate(
      scaled_kdr = as.numeric(scale(kdr)),
      scaled_acc = as.numeric(scale(accuracy)),
      scaled_crit = as.numeric(scale(crit_ratio)),
      scaled_dodge = as.numeric(scale(dodge_ratio)),
      scaled_rating = as.numeric(scale(Rating))
    )
  
  processed_df$raw_score <- (1.5 * processed_df$scaled_kdr) + 
    (0.8 * processed_df$scaled_acc) + 
    (0.6 * processed_df$scaled_crit) + 
    (0.5 * processed_df$scaled_dodge) - 
    (0.2 * processed_df$scaled_rating)
  
  max_score <- max(processed_df$raw_score, na.rm = TRUE)
  exp_scores <- exp(processed_df$raw_score - max_score)
  processed_df$softmax_win_prob <- (exp_scores / sum(exp_scores, na.rm = TRUE)) * 100
  
  # 4. Fit Logistic Regression model safely
  logistic_model <- tryCatch({
    glm(Win ~ kdr + accuracy + crit_ratio + dodge_ratio + Rating, data = processed_df, family = binomial)
  }, error = function(e) {
    message("Logistic regression failed: ", e$message)
    NULL
  })
  
  if (!is.null(logistic_model)) {
    pred_probs <- predict(logistic_model, newdata = processed_df, type = "response") * 100
    processed_df$logistic_win_prob <- ifelse(is.na(pred_probs), 0, pred_probs)
  } else {
    processed_df$logistic_win_prob <- 0
  }
  
  # 5. Compile predictions data structure for JSON (including simulated_elo)
  predictions_list <- processed_df %>%
    select(Unit, kdr, accuracy, crit_ratio, dodge_ratio, Rating, simulated_elo, softmax_win_prob, logistic_win_prob) %>%
    as.list()
  
  # 6. Final list structure for export
  final_data <- list(
    stages = stages_list,
    scatter_data = processed_df,
    predictions = predictions_list 
  )
  
  return(final_data)
}

# --- Execution Block ---
Unit <- c("Myrna", "Dodger", "Fluffy", "Nessi", "Pepper",
          "Rudeas", "Fiddle", "Boppington", "HDaddyHugh",
          "Eclaire", "Zianna", "Spensa", "Bramble", "Barrett",
          "Esrever", "Evan", "Fusion", "Zephyr", "Foley",
          "Lala-Licious", "Mikaela", "Rosey", "Ben",
          "Supremacy", "Claus", "Buraq", "Ashe",
          "Alcalai", "Healena", "L'Archella", "Hirame", "Neil",
          "TinythMighty", "Zomii", "Manager", "Edge",
          "Ceclix", "Toman", "Austin", "Annie",
          "Halley", "Heavy", "Tina", "Procbot1",
          "Slipshade", "Cosse", "Eve", "Creaky")
Rating <- 1:48
personal_ratings <- data.frame(Unit = Unit, Rating = Rating)

path_data <- "D:/Videos_creation/CMU_project/final_showdown/Q1FullData.csv"
raw_data <- read.csv(path_data)

merged_data <- left_join(raw_data, personal_ratings, by = "Unit")
merged_data <- merged_data %>%
  mutate(
    Acc.= as.numeric(str_remove_all(Acc., "[%'\"]"))
    )

final_dashboard_data <- run_models_and_clean(merged_data)

write_json(final_dashboard_data, 
           path = "data.json", 
           auto_unbox = TRUE)

# 3. Automatically push changes to GitHub using system commands

#system("git add path/to/my-web-app/data.json")
#system("git commit -m 'Automated data update'")
#system("git push origin main")