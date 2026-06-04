library(dplyr)
library(readxl)

# Load data
speakeranimosity <- read_excel("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach/FinalResultsCollapsed.xls")

# Clean data: remove missing or invalid party entries
speakeranimosity_clean <- speakeranimosity %>%
  filter(!is.na(speaker_party),
         speaker_party != "-", speaker_minister=="notMinister")

# Compute party-period average animosity
party_period_avg <- speakeranimosity_clean %>%
  group_by(speaker_party, legislative_period) %>%
  summarise(
    avg_animosity = mean(LLM_score, na.rm = TRUE),
    .groups = "drop"
  )

# Join averages back and compute deviation per speech
speakeranimosity_with_dev <- speakeranimosity_clean %>%
  left_join(party_period_avg,
            by = c("speaker_party", "legislative_period")) %>%
  mutate(
    deviation = abs(LLM_score - avg_animosity)
  )

# Compute maximum deviation per party-period
max_dev <- speakeranimosity_with_dev %>%
  group_by(speaker_party, legislative_period) %>%
  summarise(
    max_deviation = max(deviation, na.rm = TRUE),
    .groups = "drop"
  )

# Combine results into final table
final_results <- party_period_avg %>%
  left_join(max_dev,
            by = c("speaker_party", "legislative_period"))

# View results
print(final_results)

library(ggplot2)
##Average Animosity along the legislative Period all Parties
ggplot(final_results, aes(x = factor(legislative_period),
                          y = avg_animosity,
                          color = speaker_party,
                          group = speaker_party)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_discrete(labels = c("20\n(1996-1999)",
                              "21\n(1999-2002)",
                              "22\n(2002-2006)",
                              "23\n(2006-2008)",
                              "24\n(2008-2013)",
                              "25\n(2013-2017)",
                              "26\n(2017-2019)",
                              "27\n(2019-2022)"))+
  scale_color_manual(values = party_colors) +
  labs(
    title = "Average Animosity by Party over Time",
    x = "Legislative Period",
    y = "Average Animosity",
    color = "Party"
  ) +
  theme_minimal()

###Individual Maximum Deviation
library(ggplot2)
library(dplyr)

ggplot(speakeranimosity_clean, aes(x = factor(legislative_period),
                     y = LLM_score,
                     fill = speaker_party)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = party_colors) +
  facet_wrap(~ speaker_party) +
  labs(
    title = "Distribution of Animosity Scores by Party and Legislative Period",
    x = "Legislative Period",
    y = "Animosity Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

