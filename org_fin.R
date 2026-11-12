library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

# Download Data

mets <- read_excel('mets2026_2025.xlsx')
order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

mets <- mets |> mutate(Season = factor(Season), Day_of_week = factor(Day_of_week, levels = order) )

mets_2025 <- subset(mets, Season == 2025)
mets_2026 <- subset(mets, Season == 2026)

# CHARTS
# 2025 Attendance Per Game

ggplot(mets_2025, aes(x = Date_formatted, y = Attendance)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = mean(mets_2025$Attendance), color = 'orange', size = 1) +
  labs(
  title = 'Mets 2025 Season Attendance Per Game',
  x = 'Month',
  y = 'Attendance',
  subtitle = 'Average 2025 Season Attendance in Orange') + theme_minimal()

# 2026 Attendance Per Game

ggplot(mets_2026, aes(x = Date_formatted, y = Attendance)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = mean(mets_2026$Attendance), color = 'orange', size = 1) +
  labs(
    title = 'Mets 2026 Season Attendance Per Game',
    x = 'Month',
    y = 'Attendance',
    subtitle = 'Average 2026 Season Attendance in Orange') + theme_minimal()

# 2025 vs 2026 Win % vs. Attendance

ggplot(mets, aes(x = Win_Pct_Pre, y = Attendance, color = Season)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  scale_color_manual(values = c("2025" = "blue", "2026" = "orange")) +
  labs(
    title = "Mets Win % vs. Attendance",
    subtitle = "2025 vs 2026 Seasons",
    x = "Win % Going Into Game",
    y = "Attendance"
  ) + theme_cowplot()

## REMOVING FIRST 10 GAMES
remove <- which(mets$`Gm#` < 10)
mets_filter <- mets[-remove, ]

ggplot(mets_filter, aes(x = Win_Pct_Pre, y = Attendance, color = Season)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  scale_color_manual(values = c("2025" = "blue", "2026" = "orange")) +
  labs(
    title = "Mets Win % vs. Attendance",
    subtitle = "2025 vs 2026 Seasons",
    x = "Win % Going Into Game",
    y = "Attendance"
  ) + theme_cowplot()

# 2025 vs 2026 Avg Attendance by Day of Week

day_avg <- mets |> group_by(Season, Day_of_week) |> summarise(avg_attendance = mean(Attendance))
ggplot(mets, aes(x = Day_of_week, y = Attendance, fill = Season)) +
  geom_bar(stat = 'summary', position = 'dodge') +
  scale_fill_manual(values = c("2025" = "blue", "2026" = "orange"))+
  labs(
    title = 'Mets Average Attendance by Day of Week',
    x = 'Day',
    y = 'Avg Attendance',
    subtitle = '2025 vs 2026 Seasons') + theme_cowplot()


# 2025 vs 2026 Avg Attendance on Theme Nights
theme_avg <- mets |> group_by(Season, theme_night) |> summarise(avg_attendance = mean(Attendance))
ggplot(mets, aes(x = theme_night, y = Attendance, fill = Season)) +
  geom_bar(stat = 'summary', position = 'dodge') +
  scale_fill_manual(values = c("2025" = "blue", "2026" = "orange"))+
  scale_x_discrete(
    labels = c("FALSE" = "No Theme", "TRUE" = "Theme Night")
  )+
  labs(
    title = 'Mets Average Attendance by Theme Night',
    x = NULL,
    y = 'Avg Attendance',
    subtitle = '2025 vs 2026 Seasons') + theme_cowplot()

# 2025 vs 2026 Avg Attendance on Giveaway Nights

giveaway_avg <- mets |> group_by(Season, Giveaway) |> summarise(avg_attendance = mean(Attendance))
ggplot(mets, aes(x = Giveaway, y = Attendance, fill = Season)) +
  geom_bar(stat = 'summary', position = 'dodge') +
  scale_fill_manual(values = c("2025" = "blue", "2026" = "orange"))+
  scale_x_discrete(
    labels = c("FALSE" = "No Giveaway", "TRUE" = "Giveaway Night")
  )+
  labs(
    title = 'Mets Average Attendance by Giveaway Night',
    x = NULL,
    y = 'Avg Attendance',
    subtitle = '2025 vs 2026 Seasons') + theme_cowplot()

# 2025 vs 2026 Avg Attendance on Rival Team Nights

rival_avg <- mets |> group_by(Season, is_Rival) |> summarise(avg_attendance = mean(Attendance))
ggplot(mets, aes(x = is_Rival, y = Attendance, fill = Season)) +
  geom_bar(stat = 'summary', position = 'dodge') +
  scale_fill_manual(values = c("2025" = "blue", "2026" = "orange"))+
  scale_x_discrete(
    labels = c("FALSE" = "Not a Rival Team", "TRUE" = "Rival Team")
  )+
  labs(
    title = 'Mets Average Attendance by Rival Team Opponent',
    x = NULL,
    y = 'Avg Attendance',
    subtitle = '2025 vs 2026 Seasons') + theme_cowplot()

# By Specific Rival
mets |>
  filter(is_Rival == TRUE) |>
  ggplot(aes(x = Opp, y = Attendance, fill = Season)) +
  geom_bar(stat = 'summary', fun = 'mean', position = 'dodge') +
  scale_fill_manual(values = c("2025" = "blue", "2026" = "orange")) +
  labs(
    title = 'Mets Average Attendance for Specific Rival Opponents',
    x = 'Opponent',
    y = 'Avg Attendance',
    subtitle = '2025 vs 2026 Seasons') + theme_cowplot()


# 2025 vs 2026 Avg Attendance based on Time of Day (Day v Night Game)

tod_avg <- mets |> group_by(Season, `D/N`) |> summarise(avg_attendance = mean(Attendance))
ggplot(mets, aes(x = `D/N`, y = Attendance, fill = Season)) +
  geom_bar(stat = 'summary', position = 'dodge') +
  scale_fill_manual(values = c("2025" = "blue", "2026" = "orange"))+
  scale_x_discrete(
    labels = c("D" = "Day Game", "N" = "Night Game")
  )+
  labs(
    title = 'Mets Average Attendance by Time of Day',
    x = NULL,
    y = 'Avg Attendance',
    subtitle = '2025 vs 2026 Seasons') + theme_cowplot()


# HYPOTHESES
# Null Hypothesis #1: Win % Doesn't Affect Attendance (correlation)

# Null Hypothesis #2: Day of Week Doesn't Affect Attendance (ANOVA)

# Null Hypothesis #3: Theme Nights Don't Affect Attendance (t-test)

# Null Hypothesis #4: Giveaway Nights Don't Affect Attendance (t-test)

# Null Hypothesis #5: Playing a Rival Team Doesn't Affect Attendance (t-test, ANOVA for specific team)

# Null Hypothesis #6: Time of Day Doesn't Affect Attendance (t-test)
