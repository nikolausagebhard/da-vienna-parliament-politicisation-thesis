# Load libraries
library(stats)
library(stargazer)
library(car)
library(dotwhisker)
library(broom)
library(dplyr)
library(purrr)

speakeranimosity <- read_excel("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach/FinalResultsCollapsed.xls")

# Clean data: remove missing or invalid party entries
speakeranimosity_clean <- speakeranimosity %>%
  filter(!is.na(speaker_party),
         speaker_party != "-", speaker_minister=="notMinister")

# Ensure data is filtered for Left/Right blocks
regression_df <- subset(speakeranimosity_clean, speaker_party %in% c("SPÖ", "GRÜNE", "JETZT", "ÖVP", "FPÖ", "BZÖ", "STRONACH"))
regression_df$ideology <- ifelse(regression_df$speaker_party %in% c("SPÖ", "GRÜNE", "JETZT"), "Left", "Right")

##Run a regression for each period
# Store models in a list
models_by_period <- list()

# Fit restricted model (ideology only) for each period
for (period in unique_periods) {
  period_data <- subset(regression_df, legislative_period == period)
  model <- lm(LLM_score ~ factor(ideology), data = period_data)
  models_by_period[[as.character(period)]] <- model
}
models_by_period_full <- list()
# Fit full model (ideology+government) for each period
for (period in unique_periods) {
  period_data <- subset(regression_df, legislative_period == period)
  model <- lm(LLM_score ~ factor(party_status) + factor(ideology), data = period_data)
  models_by_period_full[[as.character(period)]] <- model
}
# Display all models side-by-side using stargazer
stargazer(models_by_period,
          type = "text",
          column.labels = paste("Period", unique_periods),
          keep.stat = c("n", "rsq", "adj.rsq", "f"))
stargazer(models_by_period_full,
          type = "text",
          column.labels = paste("Period", unique_periods),
          keep.stat = c("n", "rsq", "adj.rsq", "f"))
##Visualization
# 1. Bind all models into a single tidy dataframe
all_models_df <- map_dfr(models_by_period_full, tidy, conf.int = TRUE, .id = "Period") %>%
  filter(term != "(Intercept)") # Usually excluded to keep the scale readable

# 2. Plot with facets for each legislative period
ggplot(all_models_df, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ Period, labeller = labeller(Period = function(x) paste("Period", x))) +
  theme_minimal() +
  labs(
    title = "Determinants of LLM Score by Legislative Period",
    subtitle = "Points represent OLS estimates with 95% confidence intervals",
    x = "Coefficient Estimate",
    y = ""
  )
