library(dplyr)
library(stringr)
library(ggplot2)

ParliamentaryData_noText <- read.table("/Users/nikolausgebhard/Masterthesis Politication/cda1055_dat_ParlaCAP-AT_speeches.tsv",
                                       header = TRUE,
                                       sep = "\t",
                                       stringsAsFactors = FALSE,
)
ParliamentaryData_Sentences <- read.table("/Users/nikolausgebhard/Masterthesis Politication/cda1055_dat_ParlaCAP-AT_sentences.tsv",
                                          header = TRUE,
                                          sep = "\t",
                                          stringsAsFactors = FALSE,)
ParliamentaryData_Sentences$date <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2}).*", "\\1", ParliamentaryData_Sentences$id))

#Avoid Difference in Processing with "GRÜNE" and "GRÜNE"
ParliamentaryData_noText <- ParliamentaryData_noText %>%
  mutate(speaker_party = toupper(speaker_party))


ParliamentaryData_noText$date <- as.Date(ParliamentaryData_noText$date)
ParliamentaryData_noText$lang <- NULL
ParliamentaryData_noText$speaker_party_name <- NULL
ParliamentaryData_noText$partyfacts_id <- NULL
ParliamentaryData_noText$sent_logit <- NULL
ParliamentaryData_noText$vdem_country_id <-NULL
ParliamentaryData_noText$speaker_birth <- NULL
ParliamentaryData_noText$text_en <- NULL

#Adds Legislative Period Column
ParliamentaryData_noText <- ParliamentaryData_noText %>%
  mutate(legislative_period = as.numeric(
    str_extract(id, "(?<=\\d{4}-\\d{2}-\\d{2}-)[0-9]+")
  ))

#Splitting into Nationalratsperioden
{
  AllDiscussionPeriod_20 <- ParliamentaryData_noText %>% filter(legislative_period == 20)
  AllDiscussionPeriod_21 <- ParliamentaryData_noText %>% filter(legislative_period == 21)
  AllDiscussionPeriod_22 <- ParliamentaryData_noText %>% filter(legislative_period == 22)
  AllDiscussionPeriod_23 <- ParliamentaryData_noText %>% filter(legislative_period == 23)
  AllDiscussionPeriod_24 <- ParliamentaryData_noText %>% filter(legislative_period == 24)
  AllDiscussionPeriod_25 <- ParliamentaryData_noText %>% filter(legislative_period == 25)
  AllDiscussionPeriod_26 <- ParliamentaryData_noText %>% filter(legislative_period == 26)
  AllDiscussionPeriod_27 <- ParliamentaryData_noText %>% filter(legislative_period == 27)
  }
#Seat Distribution According to the Data of NR
create_seat_distribution <- function(parties, seats, period = NULL) {
  
  # Check: total seatsmust equal 183
  if (sum(seats) != 183) {
    stop(paste0(
      "Error: Seat total is ", sum(seats), 
      ", but must be exactly 183."
    ))
  }
  
  df <- data.frame(
    speaker_party = parties,
    seats= seats
  ) %>%
    dplyr::mutate(
      seat_share = seats/ sum(seats)
    )
  
  # Optional: add legislative period
  if (!is.null(period)) {
    df$legislative_period <- period
  }
  
  return(df)
}
#Input Seat Distribution
{
  seat_distribution_20 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(41, 52, 71, 0, 9,0,0,0,10),
    period = 20
  )
  seat_distribution_21 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(52, 52, 65, 0, 14,0,0,0,0),
    period = 21
  )
  seat_distribution_22 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(18, 79, 69, 0, 17,0,0,0,0),
    period = 22
  )
  seat_distribution_23 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(21, 66, 68, 0, 21,0,0,7,0),
    period = 23
  )
  seat_distribution_24 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(34, 51, 57, 0, 20,0,0,21,0),
    period = 24
  )
  seat_distribution_25 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(40, 47, 52, 9, 24,0,11,0,0),
    period = 25
  )
  seat_distribution_26 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(51, 62, 52, 10, 0,8,0,0,0),
    period = 26
  )
  seat_distribution_27 <- create_seat_distribution(
    parties = c("FPÖ", "ÖVP", "SPÖ","NEOS", "GRÜNE", "JETZT","STRONACH","BZÖ","LIF" ),
    seats= c(31, 71, 40, 15, 26,0,0,0,0),
    period = 27
  ) }

#One Dataframe
create_seat_matrix <- function(...) {
  
  library(dplyr)
  library(tidyr)
  
  # 1. Combine all input dataframes
  df_all <- bind_rows(...)
  
  # 2. Keep only relevant columns
  df_all <- df_all %>%
    select(legislative_period, speaker_party, seats)
  
  # 3. Reshape: periods = rows, parties = columns
  df_wide <- df_all %>%
    pivot_wider(
      names_from = speaker_party,
      values_from = seats,
      values_fill = 0
    ) %>%
    arrange(legislative_period)
  
  return(df_wide)
}
PartySeatsthroughout <- create_seat_matrix(
  seat_distribution_20,
  seat_distribution_21,
  seat_distribution_22,
  seat_distribution_23,
  seat_distribution_24,
  seat_distribution_25,
  seat_distribution_26,
  seat_distribution_27
)

#Color Coding Parties
{
  party_colors <- c(
    "ÖVP" = "#000000",
    "SPOE" = "#E3000F",
    "SPÖ" = "#E3000F",
    "FPOE" = "#0050A0",
    "FPÖ" = "#0050A0",
    "NEOS" = "#E2007A",
    "Grüne" = "#6AB023",
    "GRÜNE" = "#6AB023",
    "LIF" = "#fbf315",
    "BZÖ" = "#EE7F00",
    "STRONACH" = "#FFFF00",
    "JETZT" = "#3D665E"
  )
}
#Function for automated Processing of Topics in the Legislative Periods
process_legislative_period <- function(df, period = NULL) {
  
  
  # 1. Topic distribution per debate
  topic_by_debate <- df %>%
    group_by(date, CAP_category) %>%
    summarise(
      total_words = sum(word_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(date) %>%
    mutate(
      debate_total = sum(total_words),
      share = total_words / debate_total
    ) %>%
    ungroup()
  
  
  # 4. Optional: add period identifier
  if (!is.null(period)) {
    topic_by_debate$legislative_period <- period
  }
  
  # 5. Return all outputs as a list
  return(
    topic_by_debate
  )
}

topic_by_debateperiod20 <- process_legislative_period(AllDiscussionPeriod_20, period = 20)
topic_by_debateperiod21 <- process_legislative_period(AllDiscussionPeriod_21, period = 21)
topic_by_debateperiod22 <- process_legislative_period(AllDiscussionPeriod_22, period = 22)
topic_by_debateperiod23 <- process_legislative_period(AllDiscussionPeriod_23, period = 23)
topic_by_debateperiod24 <- process_legislative_period(AllDiscussionPeriod_24, period = 24)
topic_by_debateperiod25 <- process_legislative_period(AllDiscussionPeriod_25, period = 25)
topic_by_debateperiod26 <- process_legislative_period(AllDiscussionPeriod_26, period = 26)
topic_by_debateperiod27 <- process_legislative_period(AllDiscussionPeriod_27, period = 27)


topic_by_debate_all <- bind_rows(
  topic_by_debateperiod20,
  topic_by_debateperiod21,
  topic_by_debateperiod22,
  topic_by_debateperiod23,
  topic_by_debateperiod24,
  topic_by_debateperiod25,
  topic_by_debateperiod26,
  topic_by_debateperiod27
)
rm(topic_by_debateperiod20,
   topic_by_debateperiod21,
   topic_by_debateperiod22,
   topic_by_debateperiod23,
   topic_by_debateperiod24,
   topic_by_debateperiod25,
   topic_by_debateperiod26,
   topic_by_debateperiod27
) #removes collapsed Dataframes
immigration_all <- topic_by_debate_all %>% filter(
  CAP_category == "Immigration"
)%>%
  select(
    date,
    legislative_period,
    total_words,
    debate_total,
    share
  ) %>%
  arrange(date)



immigration_high_salience <- topic_by_debate_all %>%
  filter(
    CAP_category == "Immigration",
    (total_words >= 10000 | share >= 0.10)
  ) %>%
  select(
    date,
    legislative_period,
    total_words,
    debate_total,
    share
  ) %>%
  arrange(date)
WriteXLS::WriteXLS(immigration_high_salience,"/Users/nikolausgebhard/Masterthesis Politication/Immigration_High_Salience2.xls")
write.csv(immigration_high_salience,"/Users/nikolausgebhard/Masterthesis Politication/Immigration_High_Salience.csv")
print(MissedDatesMigrationHighSalience <- anti_join(immigration_high_salience2,immigration_high_salience, by="date"))

#Migration Discussion needed for Over-UnderRep_MigrationDiscussion
{
  MigrationDiscussionPeriod_20 <- ParliamentaryData_noText %>% filter(legislative_period == 20,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_21 <- ParliamentaryData_noText %>% filter(legislative_period == 21,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_22 <- ParliamentaryData_noText %>% filter(legislative_period == 22,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_23 <- ParliamentaryData_noText %>% filter(legislative_period == 23,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_24 <- ParliamentaryData_noText %>% filter(legislative_period == 24,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_25 <- ParliamentaryData_noText %>% filter(legislative_period == 25,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_26 <- ParliamentaryData_noText %>% filter(legislative_period == 26,CAP_category=="Immigration",word_count>=100)
  MigrationDiscussionPeriod_27 <- ParliamentaryData_noText %>% filter(legislative_period == 27,CAP_category=="Immigration",word_count>=100)
}
significant_dates_by_period <- immigration_high_salience %>%
  group_by(legislative_period) %>%
  summarise(
    n_dates = n_distinct(date),
    .groups = "drop"
  ) %>%
  arrange(legislative_period) %>%
  mutate(legislative_period = factor(legislative_period, levels = sort(unique(legislative_period))))

period_labels <- data.frame(
  legislative_period = c(20,21,22,23,24,25,26,27),
  label = c(
    "20\n(1996-1999)",
    "21\n(1999-2002)",
    "22\n(2002-2006)",
    "23\n(2006-2008)",
    "24\n(2008-2013)",
    "25\n(2013-2017)",
    "26\n(2017-2019)",
    "27\n(2019-2022)"
  )
)

ggplot(significant_dates_by_period, aes (x=legislative_period, y=n_dates,group=1))+
  geom_line()+
  geom_point()+
  scale_x_discrete(labels = c("20\n(1996-1999)",
                              "21\n(1999-2002)",
                              "22\n(2002-2006)",
                              "23\n(2006-2008)",
                              "24\n(2008-2013)",
                              "25\n(2013-2017)",
                              "26\n(2017-2019)",
                              "27\n(2019-2022)"))+
  labs(
    title = "Number of High-Salience Immigration Debates",
    x="Legislative Period (Year Range)",
    y = "Number of Significant Dates >10.000 Words or 10% of 
    Debate on Immigration"
  ) + 
  theme_minimal()

View(immigration_high_salience)
