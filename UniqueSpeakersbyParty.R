library(dplyr)

collapsed_results <- read_excel("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach/FinalResultsCollapsed.xls")
collapsed_results <- collapsed_results %>% filter(speaker_minister!="minister")
speaker_counts <- collapsed_results %>%
  group_by(legislative_period, speaker_party) %>%
  summarise(
    n_unique_speakers = n_distinct(speaker_id),
    .groups = "drop"
  )


seats_long <- PartySeatsthroughout %>%
  pivot_longer(
    cols = -legislative_period,
    names_to = "speaker_party",
    values_to = "seats"
  )
speaker_counts <- speaker_counts %>%
  left_join(seats_long, by = c("legislative_period", "speaker_party"))
speaker_counts <- speaker_counts %>%
  mutate(
    speakers_per_seat = n_unique_speakers / seats
  )

plot_data <- speaker_counts %>%
  filter(speaker_party %in% c("FPÖ", "GRÜNE"))

WriteXLS::WriteXLS(speaker_counts,"/Users/nikolausgebhard/Library/Mobile Documents/com~apple~CloudDocs/DA/Master Thesis Politicasition/SpeakerCounts.xls")
library(ggplot2)

ggplot(speaker_counts, aes(
  x = factor(legislative_period),
  y = speakers_per_seat,
  color = speaker_party,
  group = speaker_party
)) +
  geom_line() +
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
    title = "Speakers per Seat: All Parties",
    x = "Legislative Period",
    y = "Speakers per Seat",
    color = "Party"
  ) +
  theme_minimal()

write_csv(collapsed_results,"FinalResultsCollapsed.csv")
