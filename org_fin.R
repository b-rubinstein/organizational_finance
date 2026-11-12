library(readxl)
library(dplyr)
library(ggplot2)

# Download Data

mets <- read_excel('mets2026_2025.xlsx')
order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

mets <- mets |> mutate(Season = factor(Season), Day_of_week = factor(Day_of_week, levels = order) )
mets


# CHARTS
# 2025 Attendance Per Game

# 2026 Attendance Per Game


# 2025 vs 2026 Average Attendance by Day of Week


# 2025 vs 2026 Win % vs. Attendance



# 2025 vs 2026 Avg Attendance on Theme Nights


# 2025 vs 2026 Avg Attendance on Giveaway Nights


# 2025 vs 2026 Avg Attendance on Rival Team Nights



# HYPOTHESES
# Null Hypothesis #1: Win % Doesn't Affect Attendance (correlation)

# Null Hypothesis #2: Day of Week Doesn't Affect Attendance (ANOVA)

# Null Hypothesis #3: Theme Nights Don't Affect Attendance (t-test)

# Null Hypothesis #4: Giveaway Nights Don't Affect Attendance (t-test)

# Null Hypothesis #5: Playing a Rival Team Doesn't Affect Attendance (t-test)
