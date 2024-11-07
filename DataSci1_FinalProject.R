library(readr)

set_2021_2022_NBA_Player_Stats_Regular <- read_delim("2021-2022 NBA Player Stats - Regular.csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

data_2022_2023_NBA_Player_Stats_Regular <- read_delim("2022-2023 NBA Player Stats - Regular.csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(tidyverse)

#Position vs 3 Point Percentage 2022-2023

pos_vs_3pt <- data_2022_2023_NBA_Player_Stats_Regular %>% select(Pos, `3P%`)

table(pos_vs_3pt)

as_tibble(data_2022_2023_NBA_Player_Stats_Regular)

as_tibble(set_2021_2022_NBA_Player_Stats_Regular)

ggplot(data = data_2022_2023_NBA_Player_Stats_Regular, aes(x = Pos, y = `3P%`)) +
  geom_point() +  
  geom_boxplot() +  
  labs(x = "Position", y = "Three-Point Percentage", title = "Three-Point Percentage by Position")

ggplot(data = data_2022_2023_NBA_Player_Stats_Regular, aes(x = Pos, y = `3P%`)) +
  geom_boxplot() +  
  labs(x = "Position", y = "Three-Point Percentage", title = "Three-Point Percentage by Position") +
  facet_wrap(~Pos, scales = "free")  

ggplot(data = data_2022_2023_NBA_Player_Stats_Regular, aes(x = Pos, y = `3P%`)) +
  geom_point() +  
  labs(x = "Position", y = "Three-Point Percentage", title = "Three-Point Percentage by Position") +
  facet_wrap(~ Pos, scales = "free")  

mean_pos <- aggregate(`3P%` ~ Pos, data = data_2022_2023_NBA_Player_Stats_Regular, FUN = mean, na.rm = TRUE)

print(mean_pos)

#Field Goal Percentage vs Free Throw Percentage 2021-2022

high_ft_fg <- set_2021_2022_NBA_Player_Stats_Regular %>% filter(`FG%` >= 0.48 & `FT%` >= 0.79 & FTA >= 1)
two_high_ft_vs_fg <- data_2022_2023_NBA_Player_Stats_Regular %>% filter(`FG%` >= 0.48 & `FT%` >= 0.79 & FTA >= 1)

ggplot(data = high_ft_fg, aes(x = `FG%`, y = `FT%`, label = Player)) +
  geom_point() +  
  geom_text(vjust = 1.5, hjust = -0.5, size = 3) + 
  labs(x = "Field Goal Percentage (FG%)", y = "Free Throw Percentage (FT%)", title = "FG% vs FT% per Player") + 
  theme_minimal()

ggplot(data = two_high_ft_vs_fg, aes(x = `FG%`, y = `FT%`, label = Player)) +
  geom_point() +  
  geom_text(vjust = 1.5, hjust = -0.5, size = 3) + 
  labs(x = "Field Goal Percentage (FG%)", y = "Free Throw Percentage (FT%)", title = "FG% vs FT% per Player") + 
  theme_minimal()


#Points Per Game vs Age 2022-2023

ppg_vs_age <- data_2022_2023_NBA_Player_Stats_Regular %>% filter(PTS >= 10)

ggplot(data = data_2022_2023_NBA_Player_Stats_Regular, aes(x = Age, y = PTS)) +
  geom_point() +  
  labs(x = "AGE", y = "PPG", title = "PPG BY AGE")

ggplot(data = ppg_vs_age, aes(x = Age, y = PTS)) +
  geom_point() +  
  labs(x = "AGE", y = "PPG", title = "AT LEAST 10 PPG BY AGE")




#PREDICTIONS



