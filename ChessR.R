library(chessR)
library(dplyr)
library(ggplot2)
#################
games_all <- get_raw_chessdotcom(usernames = "ChessPoppy15")

game_data <- get_game_data("ChessPoppy15")
head(game_data)
colnames(game_data)

game_data <- game_data %>% select(
  Date, time_class, White, Black, Moves, UserResult, GameEnding,  UserELO, OpponentELO, Opening
)
#################
ggplot(game_data, aes(x = Date, y = UserELO)) +
  geom_line() +
  facet_wrap(~ time_class) +
  labs(title = "ELO Rating Over Time", y = "User ELO")
#################
Rapid <- game_data %>% filter(time_class == 'rapid')
table(Rapid$UserResult)
Rapid_openings <- Rapid %>%
  group_by(Opening) %>%
  summarise(
    total_games = n(),
    wins = sum(UserResult == "Win"),
    win_percentage = round(100 * wins / total_games, 1)
  ) %>%
  arrange(desc(total_games))
#################
Blitz <-game_data %>% filter(time_class == 'blitz')
table(Blitz$UserResult)
Blitz_openings <- Blitz %>%
  group_by(Opening) %>%
  summarise(
    total_games = n(),
    wins = sum(UserResult == "Win"),
    win_percentage = round(100 * wins / total_games, 1)
  ) %>%
  arrange(desc(total_games))
#################
Bullet <- game_data %>% filter(time_class == 'bullet')
table(Bullet$UserResult)
Bullet_openings <- Bullet %>%
  group_by(Opening) %>%
  summarise(
    total_games = n(),
    wins = sum(UserResult == "Win"),
    win_percentage = round(100 * wins / total_games, 1)
  ) %>%
  arrange(desc(total_games))
#################
game_data %>%
  mutate(Color = ifelse(White == "ChessPoppy15", "White", "Black")) %>%
  group_by(Color, UserResult) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = UserResult, values_from = n, values_fill = 0)
#################
all_openings <- game_data %>%
  group_by(Opening) %>%
  summarise(
    total = n(),
    wins = sum(UserResult == "Win"),
    losses = sum(UserResult == "Loss"),
    draws = sum(UserResult == "Draw"),
    win_rate = round(100 * wins / total, 1)
  ) %>%
  filter(total >= 5) %>%  
  arrange(win_rate)
head(all_openings, 10)  
#################
game_data %>%
  group_by(GameEnding, UserResult) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = UserResult, values_from = n, values_fill = 0)
#################
game_data %>%
  mutate(ELO_diff = UserELO - OpponentELO,
         OpponentStronger = ifelse(ELO_diff < 0, "Stronger", "Weaker or Equal")) %>%
  group_by(OpponentStronger, UserResult) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = UserResult, values_from = n, values_fill = 0)
#################
game_data %>%
  mutate(move_count = sapply(strsplit(Moves, " "), length)) %>%
  mutate(game_length = case_when(
    move_count <= 20 ~ "Short",
    move_count <= 40 ~ "Medium",
    TRUE ~ "Long"
  )) %>%
  group_by(game_length, UserResult) %>%
  summarise(n = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = UserResult, values_from = n, values_fill = 0)
#################


#################


#################


