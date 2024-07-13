library(nflfastR)
library(dplyr)
library(xgboost)
library(caret)
library(nflplotR)
library(gt)
library(gtExtras)

pbp <- load_pbp(2014:2023) %>% filter(play_type == "pass" | play_type == "run", season_type == "REG")
pbp$home_coach[which(pbp$season == 2023 & pbp$home_team == "LAC" & pbp$week %in% c(16, 17, 18))] <- "Giff Smith"
pbp$away_coach[which(pbp$season == 2023 & pbp$away_team == "LAC" & pbp$week %in% c(16, 17, 18))] <- "Giff Smith"

player_stats <- calculate_player_stats(pbp, weekly = TRUE) %>%
  filter(season_type == "REG") %>%
  group_by(season, recent_team, week) %>%
  summarize(attempts = sum(attempts, na.rm = TRUE), carries = sum(carries, na.rm = TRUE), pass_epa = sum(passing_epa, na.rm = TRUE), rush_epa = sum(rushing_epa, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(season, recent_team) %>%
  mutate(attempts = cumsum(attempts) - attempts, carries = cumsum(carries) - carries, pass_epa = cumsum(pass_epa) - pass_epa, rush_epa = cumsum(rush_epa) - rush_epa) %>%
  ungroup() %>%
  group_by(season, week) %>%
  mutate(pass_quality = scale(pass_epa/attempts), rush_quality = scale(rush_epa/carries)) %>%
  ungroup() %>%
  select(-attempts, -carries, -pass_epa, -rush_epa)

player_stats$pass_quality[which(is.nan(player_stats$pass_quality))] <- 0
player_stats$rush_quality[which(is.nan(player_stats$rush_quality))] <- 0

data <- pbp %>%
  mutate(is_pass = ifelse(play_type == "pass", 1, 0), timeouts = ifelse(posteam_type == "home", home_timeouts_remaining, away_timeouts_remaining), coach = ifelse(posteam_type == "home", home_coach, away_coach), ) %>%
  left_join(player_stats, by = c("season", "posteam"="recent_team", "week")) %>%
  select(season, posteam, coach, is_pass, yardline_100, posteam_type, half_seconds_remaining, down, ydstogo, score_differential, timeouts, wp, pass_quality, rush_quality)

data$posteam_type <- as.factor(data$posteam_type)
data$down <- as.factor(data$down)
data$timeouts <- as.factor(data$timeouts)

extras <- data %>% select(posteam, coach)
data <- data %>% select(-posteam, -coach)
dummy <- dummyVars(" ~ .", data = data)
final_data <- data.frame(predict(dummy, newdata = data))
final_data <- final_data %>% cbind(extras) %>% select(season, posteam, coach, everything())

xgboost_train <- final_data %>%
  filter(season < 2021)

xgboost_test <- final_data %>%
  filter(season >= 2021)

labels_train <- as.matrix(xgboost_train[, 4]) 
xgboost_trainfinal <- as.matrix(xgboost_train[, c(5:21)]) 
xgboost_testfinal <- as.matrix(xgboost_test[, c(5:21)])

proe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3) 

xpass_preds <- as.data.frame(
  matrix(predict(proe_model, as.matrix(final_data[,c(5:21)])))
)

all_stats <- cbind(final_data, xpass_preds) %>%
  select(season, team = posteam, coach, is_pass, xpass = V1) %>%
  group_by(season, team, coach) %>%
  summarize(pass_rate = mean(is_pass), xpass_rate = mean(xpass), proe = pass_rate - xpass_rate)

stats_2023 <- all_stats %>% filter(season == 2023) %>% arrange(-proe) %>% mutate(pass_rate = 100 * round(pass_rate, 3), xpass_rate = 100 * round(xpass_rate, 3), proe = 100 * round(proe, 3)) %>% ungroup() %>% select(-season)
stats_2023 <- stats_2023 %>% mutate(wordmark = team) %>% select(team, wordmark, everything())

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>nflverse</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

table <- stats_2023 %>% gt() %>%
  gt_nfl_logos(columns = gt::starts_with("team")) %>%
  gt_nfl_wordmarks(columns = gt::starts_with("wordmark")) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_hulk_col_numeric(c(pass_rate, xpass_rate, proe)) %>%
  cols_label(
    team = md(""),
    wordmark = md("**Team**"),
    coach = md("**Coach**"),
    pass_rate = md("**Pass %**"),
    xpass_rate = md("**xPass %**"),
    proe = md("**PROE**")
  ) %>%
  tab_header(
    title = "2023 NFL Pass Rate Over Expected Leaders",
    subtitle = md("*Ranked by **Coach** | Factored In **Pass** & **Rush** Quality*")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything()
    )
  )


gtsave(table, "table.png", vwidth = 2500, vheight = 2000)