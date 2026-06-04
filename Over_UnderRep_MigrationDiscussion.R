#Function to Create the Comparision List
create_comparison_list <- function(dates, data, seat_data) {
  
  library(dplyr)
  
  results <- lapply(dates, function(d) {
    
    data %>%
      filter(date == as.Date(d)) %>%
      group_by(speaker_party) %>%
      summarise(
        total_words = sum(word_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        word_share = total_words / sum(total_words),
        percentage = word_share * 100
      ) %>%
      left_join(seat_data, by = "speaker_party") %>%
      mutate(
        diff_share = word_share - seat_share
      )
  })
  
  names(results) <- format(as.Date(dates), "%Y-%m-%d")
  
  return(results)
}
#Period20
{
  dates_period20 <- immigration_high_salience %>%
    filter(legislative_period == 20) %>%
    pull(date)
  
  results_listperiod20 <- create_comparison_list(
    dates_period20,
    MigrationDiscussionPeriod_20,
    seat_distribution_20
  )
  library(dplyr)
  
  results_df_period20 <- bind_rows(
    lapply(names(results_listperiod20), function(d) {
      results_listperiod20[[d]] %>%
        mutate(date = as.Date(d))
    })
  )
}
#Visialization Period 20
{
  ggplot(results_df_period20, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 20th Legislative Period",
      x = "Date",
      y = "Difference (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}
#Period21
{
dates_period21 <- immigration_high_salience %>%
  filter(legislative_period == 21) %>%
  pull(date)

results_listperiod21 <- create_comparison_list(
  dates_period21,
  MigrationDiscussionPeriod_21,
  seat_distribution_21
)
library(dplyr)

results_df_period21 <- bind_rows(
  lapply(names(results_listperiod21), function(d) {
    results_listperiod21[[d]] %>%
      mutate(date = as.Date(d))
  })
)
}
#Visialization Period 21

  {
    ggplot(results_df_period21, aes(x = date, y = diff_share*100, color = speaker_party)) +
      geom_line(size = 1) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_color_manual(values = party_colors) +
      labs(
        title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 21st Legislative Period",
        x = "Date",
        y = "Difference % (Word Share - Seat Share)",
        color = "Party"
      ) +
      theme_minimal()
  }
#Period22
{
dates_period22 <- immigration_high_salience %>%
  filter(legislative_period == 22) %>%
  pull(date)

results_listperiod22 <- create_comparison_list(
  dates_period22,
  MigrationDiscussionPeriod_22,
  seat_distribution_22
)
library(dplyr)

results_df_period22 <- bind_rows(
  lapply(names(results_listperiod22), function(d) {
    results_listperiod22[[d]] %>%
      mutate(date = as.Date(d))
  })
)
}
#Visualization Period 22
{
ggplot(results_df_period22, aes(x = date, y = diff_share*100, color = speaker_party)) +
  geom_line(size = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = party_colors) +
  labs(
    title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 22nd Legislative Period",
    x = "Date",
    y = "Difference % (Word Share - Seat Share)",
    color = "Party"
  ) +
  theme_minimal()
}
#Period 23
{
  dates_period23 <- immigration_high_salience %>%
    filter(legislative_period == 23) %>%
    pull(date)
  
  results_listperiod23 <- create_comparison_list(
    dates_period23,
    MigrationDiscussionPeriod_23,
    seat_distribution_23
  )
  library(dplyr)
  
  results_df_period23 <- bind_rows(
    lapply(names(results_listperiod23), function(d) {
      results_listperiod23[[d]] %>%
        mutate(date = as.Date(d))
    })
  )
}
#Visualization Period 23
{
  ggplot(results_df_period23, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 23rd Legislative Period",
      x = "Date",
      y = "Difference % (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}
#Period 24
{
  dates_period24 <- immigration_high_salience %>%
    filter(legislative_period == 24) %>%
    pull(date)
  
  results_listperiod24 <- create_comparison_list(
    dates_period24,
    MigrationDiscussionPeriod_24,
    seat_distribution_24
  )
  library(dplyr)
  
  results_df_period24 <- bind_rows(
    lapply(names(results_listperiod24), function(d) {
      results_listperiod24[[d]] %>%
        mutate(date = as.Date(d))
    })
  )
}
#Visualization Period 24
{
  ggplot(results_df_period24, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 24th Legislative Period",
      x = "Date",
      y = "Difference % (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}
#Period 25
{
  dates_period25 <- immigration_high_salience %>%
    filter(legislative_period == 25) %>%
    pull(date)
  
  results_listperiod25 <- create_comparison_list(
    dates_period25,
    MigrationDiscussionPeriod_25,
    seat_distribution_25
  )
  library(dplyr)
  
  results_df_period25 <- bind_rows(
    lapply(names(results_listperiod25), function(d) {
      results_listperiod25[[d]] %>%
        mutate(date = as.Date(d))
    })
  )
}
#Visualization Period 25
{
  ggplot(results_df_period25, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 25th Legislative Period",
      x = "Date",
      y = "Difference % (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}
#Period 26
##No Green Party left in Parliament
##Manual Adjustment Needed
#MP Efgani Dönmez changed from Greens to ÖVP to without Affiliation
WriteXLS::WriteXLS(MigrationDiscussionPeriod_26,"/Users/nikolausgebhard/Masterthesis Politication/Issue Competition/MigrationDebate26.xls")
MigrationDiscussionPeriod_26 <- readxl::read_xls("/Users/nikolausgebhard/Masterthesis Politication/Issue Competition/MigrationDebate26.xls")
{
  dates_period26 <- immigration_high_salience %>%
    filter(legislative_period == 26) %>%
    pull(date)
  
  results_listperiod26 <- create_comparison_list(
    dates_period26,
    MigrationDiscussionPeriod_26,
    seat_distribution_26
  )
  library(dplyr)
  
  results_df_period26 <- bind_rows(
    lapply(names(results_listperiod26), function(d) {
      results_listperiod26[[d]] %>%
        mutate(date = as.Date(d))
    })
  )
}
#Visualization Period 26
{
  ggplot(results_df_period26, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 26th Legislative Period",
      x = "Date",
      y = "Difference % (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}
#Period 27
{
  dates_period27 <- immigration_high_salience %>%
    filter(legislative_period == 27) %>%
    pull(date)
  
  results_listperiod27 <- create_comparison_list(
    dates_period27,
    MigrationDiscussionPeriod_27,
    seat_distribution_27
  )
  library(dplyr)
  
  results_df_period27 <- bind_rows(
    lapply(names(results_listperiod27), function(d) {
      results_listperiod27[[d]] %>%
        mutate(date = as.Date(d))
    })
  )
}
#Visualization Period 27
{
  ggplot(results_df_period27, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in the 27th Legislative Period",
      x = "Date",
      y = "Difference % (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}

#Combine Periods back to data_frame


results_df_allperiods <- bind_rows(
  results_df_period20,
  results_df_period21,
  results_df_period22,
  results_df_period23,
  results_df_period24,
  results_df_period25,
  results_df_period26,
  results_df_period27
) %>%
  arrange(date)%>%
  mutate(diff_share_pct = diff_share * 100)
WriteXLS::WriteXLS(results_df_allperiods, "/Users/nikolausgebhard/Masterthesis Politication/Issue Competition/Over-Underrep.xls")
# Calculate the average over-/underrepresentation per legislative period
df_period_averages <- results_df_allperiods %>%
  group_by(legislative_period, speaker_party) %>%
  summarise(
    avg_diff_pct = mean(diff_share_pct, na.rm = TRUE),
    .groups = "drop"
  )

{
  ggplot(results_df_allperiods, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation in Immigration Debates 
    in all Legislative Period",
      x = "Date",
      y = "Difference (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
  }

ggplot(results_df_allperiods, aes(x = speaker_party, y = diff_share*100, fill = speaker_party)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Distribution of Party Overrepresentation across Immigration Debates",
    x = "Party",
    y = "Difference (Word Share - Seat Share)"
  ) +
  theme_minimal()
results_df_gruene <- results_df_allperiods%>%filter(speaker_party=="GRÜNE"|speaker_party=="FPÖ")
{
  ggplot(results_df_gruene, aes(x = date, y = diff_share*100, color = speaker_party)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = party_colors) +
    labs(
      title = "Party Over-/Underrepresentation FPÖ & GRÜNE in Immigration Debates 
    in all Legislative Period",
      x = "Date",
      y = "Difference (Word Share - Seat Share)",
      color = "Party"
    ) +
    theme_minimal()
}
ggplot(df_period_averages, aes(
  x = factor(legislative_period), # 1. Converted to factor here
  y = avg_diff_pct, 
  color = speaker_party, 
  group = speaker_party           # 2. Added grouping for geom_line
)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = party_colors) +
  scale_x_discrete(labels = c("20\n(1996-1999)",
                              "21\n(1999-2002)",
                              "22\n(2002-2006)",
                              "23\n(2006-2008)",
                              "24\n(2008-2013)",
                              "25\n(2013-2017)",
                              "26\n(2017-2019)",
                              "27\n(2019-2022)")) + 
  labs(
    title = "Distribution of Party Overrepresentation across Legislative Periods",
    x = "Legislative Period",
    y = "Average Difference % (Word Share - Seat Share)",
    color = "Party"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )
