library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

process_parliamentary_debate_full <- function(target_date_str) {
  
  # 1. Setup and Filtering
  target_date <- as.Date(target_date_str)
  date_label <- str_replace_all(target_date_str, "-", "_")
  
  message(paste("Processing debate for date:", target_date_str))
  
  # Filter main datasets
  debate_data <- ParliamentaryData_noText %>% filter(date == target_date)
  debate_sentences <- ParliamentaryData_Sentences %>% filter(date == target_date)
  
  # 2. Cleaning Columns
  debate_sentences$text_en <- NULL
  debate_data$lang_code <- NULL
  debate_data <- debate_data %>% mutate(id = as.character(id))
  debate_data$text <- NULL
  
  # 3. Extract Full Text (Collapse Sentences)
  sentences_collapsed <- debate_sentences %>%
    mutate(sent_id = str_remove(sent_id, "\\.s\\d+$")) %>%
    group_by(id, sent_id) %>%
    summarise(
      full_text = str_c(text, collapse = " "),
      n_rows = n(),
      .groups = "drop"
    )
  
  debate_final <- debate_data %>%
    left_join(sentences_collapsed, by = "id") 
  
  # 4. Word Count Check
  check_df <- debate_final %>%
    mutate(actual_word_count = str_count(str_squish(full_text), "\\S+")) %>%
    group_by(id) %>%
    summarise(
      word_count = first(word_count),
      actual_word_count = sum(actual_word_count, na.rm = TRUE),
      match = word_count == actual_word_count,
      .groups = "drop"
    ) %>%
    filter(!match)
  
  print("Word count mismatch check:")
  print(check_df)
  print(paste("Total mismatched words:", sum(check_df$word_count)))
  
  # 5. Non-affiliated Member Checks
  print("Unique speaker parties:")
  print(unique(debate_final$speaker_party))
  
  non_affiliated <- debate_final %>% 
    filter(speaker_party == "-", speaker_minister != "Minister") %>% 
    select(speaker_name)
  print("Non-affiliated members (non-ministers):")
  print(non_affiliated)
  
  # 6. Save Initial CSV
  base_path <- "/Users/nikolausgebhard/Masterthesis Politication/Full Debates Creation/Full Debates/"
  write.csv(debate_final, paste0(base_path, "Debate", date_label, ".csv"), row.names = FALSE)
  
  # 7. Topic Filtering
  
  debate_immigration <- debate_final %>% 
    filter(CAP_category == "Immigration")
  
  # 8. Party Share Analysis
  party_share <- debate_immigration %>%
    group_by(speaker_party) %>%
    summarise(
      total_words = sum(word_count, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      word_share = total_words / sum(total_words),
      percentage = word_share * 100
    )
  
 
  
  
  
  # 11. Clean Up Discussion Data and Save
  debate_cleaned <- debate_immigration %>% 
    filter(word_count >= 100, speaker_role != "Chairperson")
  
  write.csv(debate_cleaned, paste0(base_path, "Debate", date_label, "_cleaned.csv"), row.names = FALSE)
  
  debate_collapsed <- debate_cleaned %>%
    mutate(row_order = row_number()) %>%
    group_by(id) %>%
    arrange(row_order, .by_group = TRUE) %>%
    summarise(
      speaker_name = first(speaker_name),
      speaker_party = first(speaker_party),
      word_count = first(word_count),
      speaker_id = first(speaker_id),
      legislative_period = first(legislative_period),
      text = paste(full_text, collapse = " "),
      n_segments = n(),
      .groups = "drop"
    )
  
  write.csv(debate_collapsed, paste0("/Users/nikolausgebhard/Masterthesis Politication/Full Debates Creation/WF Prep/All Periods/", "Debate", date_label, "_WFprep.csv"), row.names = FALSE)
  
  message("Processing complete.")
  return(debate_collapsed)
  return(debate_cleaned)# Returns the final dataframe for the environment
}

#Automated Workflow
# 1. Define your list of dates as strings
debate_dates_all <- immigration_high_salience %>%
  pull(date) %>%
  as.character()

# 2. Create a list to store the results if you want to keep them in R
all_debate_results <- list()

# 3. Run the loop
for (current_date in debate_dates_all) {
  
  # Optional: Print a separator in the console to track progress
  cat("\n--- Starting processing for:", current_date, "---\n")
  
  # Run the function and store the 'WFprep' dataframe in our list
  # We use the date as the name for the list element
  all_debate_results[[current_date]] <- process_parliamentary_debate_full(current_date)
  
}

# 4. (Optional) Check how many debates were successfully processed
message(paste("Successfully processed", length(all_debate_results), "debates."))

