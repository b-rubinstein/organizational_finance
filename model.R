library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidymodels)
library(modelsummary)

# Download Data

mets <- read_excel('mets2026_2025.xlsx')
order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
order_month <- c("March", "April", "May", "June", "July", "August", "September")


mets <- mets |> mutate(Season = factor(Season), Day_of_week = factor(Day_of_week, levels = order),
                       Month  = factor(format(Date_formatted, "%B"), levels = order_month))

remove <- which(mets$Month == 'March')
mets <- mets[-remove, ] |> droplevels()

model1 <- lm(Attendance ~ Season + Month + Day_of_week + Win_Pct_Pre +
               Giveaway + theme_night + is_Rival + `D/N`, data = mets)
summary(model1)

model2 <- lm(Attendance ~ Season * Month + Day_of_week + Win_Pct_Pre +
               Giveaway + theme_night + is_Rival + `D/N`, data = mets)
summary(model2)

mets_cv <- mets |> rename(DN = `D/N`)
lm_mod <- linear_reg() |> set_engine("lm") |> set_mode("regression")

model1_lm <- workflow() |> add_model(lm_mod) |> add_formula(
  Attendance ~ Season + Month + Day_of_week + Win_Pct_Pre +
    Giveaway + theme_night + is_Rival + DN
)

model2_lm <- workflow() |> add_model(lm_mod) |> add_formula(
  Attendance ~ Season * Month + Day_of_week + Win_Pct_Pre +
    Giveaway + theme_night + is_Rival + DN
)


set.seed(345)
folds <- vfold_cv(mets_cv, v = 10)

model1_fit <- model1_lm |> fit_resamples(folds)
model2_fit <- model2_lm |> fit_resamples(folds)

collect_metrics(model1_fit)
collect_metrics(model2_fit)

mets$Predicted <- predict(model2)
ggplot(mets, aes(x= Predicted, y = Attendance, color = Season)) +
  geom_point() + scale_color_manual(values = c("2025" = "blue", "2026" = "orange"))+
  geom_abline(slope = 1, intercept = 0) +
  labs(
  title = "Predicted vs. Actual Attendance",
  subtitle = "Model 2",
  x = "Predicted Attendance",
  y = "Actual Attendance") + theme_cowplot()


modelsummary(
  list("Model 1" = model1, "Model 2" = model2),
  stars = TRUE,
  gof_map = c("nobs", "adj.r.squared", "rmse"),
  output = "model_table.png"
)
