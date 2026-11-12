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

# HYPOTHESES
# Null Hypothesis #1: Win % Doesn't Affect Attendance (correlation)

# Null Hypothesis #2: Day of Week Doesn't Affect Attendance (ANOVA)

# Null Hypothesis #3: Theme Nights Don't Affect Attendance (t-test)

# Null Hypothesis #4: Giveaway Nights Don't Affect Attendance (t-test)

# Null Hypothesis #5: Playing a Rival Team Doesn't Affect Attendance (t-test, ANOVA for specific team)

# Null Hypothesis #6: Time of Day Doesn't Affect Attendance (t-test)

