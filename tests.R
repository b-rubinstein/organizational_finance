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

cor.test(mets$Win_Pct_Pre, mets$Attendance)
cor.test(mets_2025$Win_Pct_Pre, mets_2025$Attendance)
cor.test(mets_2026$Win_Pct_Pre, mets_2026$Attendance)

# Null Hypothesis #2: Day of Week Doesn't Affect Attendance (ANOVA)

model <- aov(Attendance ~ Day_of_week, data = mets_2025)
summary(model)

model26 <- aov(Attendance ~ Day_of_week, data = mets_2026)
summary(model26)

modelcomb <- aov(Attendance ~ Season + Day_of_week, data = mets)
summary(modelcomb)

# Tuesday vs other weeknights (t-test)

weeknights <- c('Monday', 'Wednesday', 'Thursday')

t.test(mets_2025$Attendance[mets_2025$Day_of_week == "Tuesday"],
       mets_2025$Attendance[mets_2025$Day_of_week %in% weeknights])

t.test(mets_2026$Attendance[mets_2026$Day_of_week == "Tuesday"],
       mets_2026$Attendance[mets_2026$Day_of_week %in% weeknights])


# Null Hypothesis #3: Theme Nights Don't Affect Attendance (t-test)

t.test(Attendance ~ theme_night, data = mets_2025)
t.test(Attendance ~ theme_night, data = mets_2026)

# Null Hypothesis #4: Giveaway Nights Don't Affect Attendance (t-test)

t.test(Attendance ~ Giveaway, data = mets_2025)
t.test(Attendance ~ Giveaway, data = mets_2026)

# Null Hypothesis #5: Playing a Rival Team Doesn't Affect Attendance (t-test, ANOVA for specific team)

t.test(Attendance ~ is_Rival, data = mets_2025)
t.test(Attendance ~ is_Rival, data = mets_2026)

rivals25 <- subset(mets_2025, is_Rival == TRUE)
rivals25model <- aov(Attendance ~ Opp, data = rivals25)
summary(rivals25model)

rivals26 <- subset(mets_2026, is_Rival == TRUE)
rivals26model <- aov(Attendance ~ Opp, data = rivals26)
summary(rivals26model)

# Null Hypothesis #6: Time of Day Doesn't Affect Attendance (t-test)

t.test(Attendance ~ `D/N`, data = mets_2025)
t.test(Attendance ~ `D/N`, data = mets_2026)
