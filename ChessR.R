install.packages("devtools")
devtools::install_github("JaseZiv/chessr")

library(chessR)
library(dplyr)
library(ggplot2)

games_all <- get_raw_chessdotcom(usernames = "ChessPoppy15")

game_data <- get_game_data("ChessPoppy15")
head(game_data)
colnames(game_data)

game_data <- game_data %>% select(
  Date, time_class, White, Black, Moves, UserResult, GameEnding,  UserELO, OpponentELO, Opening
)

ggplot(game_data, aes(x = Date, y = UserELO)) +
  geom_line() +
  facet_wrap(~ time_class) +
  labs(title = "ELO Rating Over Time", y = "User ELO")

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


