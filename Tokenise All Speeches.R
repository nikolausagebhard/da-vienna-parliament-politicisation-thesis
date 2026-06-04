# Load necessary libraries
library(dplyr)
library(stringr)
library(tidytext)
library(purrr)
library(readr)

# 1. Define the phrases to be removed (Noise reduction)
# We use a case-insensitive regex to capture variations
phrases_to_remove <- c(
  "Meine Damen und Herren!", 
  "Hohes Haus!", 
  "Sehr geehrte Damen und Herren!", 
  "Herr Präsident",
  "Frau Präsidentin"
)
pattern <- paste0(phrases_to_remove, collapse = "|")
setwd("/Users/nikolausgebhard/Masterthesis Politication/Full Debates Creation/WF Prep/All Periods/")
# 2. Batch process your CSV files
file_list <- list.files(pattern = "Debate.*\\.csv")

processed_data <- file_list %>%
  map_df(~read_csv(.x)) %>%
  # Remove often used parliamentary filler phrases
  mutate(text_clean = str_remove_all(text, regex(pattern, ignore_case = TRUE))) %>%
  # 3. Tokenize into individual sentences
  # unnest_tokens uses the 'tokenizers' package logic to handle abbreviations (Dr., Nr., etc.)
  unnest_tokens(sentence, text_clean, token = "sentences") %>%
  # 4. Group into 5-sentence chunks within each speech (id)
  group_by(id) %>%
  mutate(sentence_num = row_number()) %>%
  mutate(chunk_id = (sentence_num - 1) %/% 5) %>%
  # 5. Re-collapse sentences into chunks
  group_by(id, chunk_id, speaker_name, speaker_party, speaker_id) %>%
  summarize(
    text_segment = paste(sentence, collapse = " "),
    original_word_count = n(), # number of sentences in this chunk
    .groups = "drop"
  )
processed_data <- processed_data %>% mutate(row_id = row_number())
# View the result
summary(processed_data)

setwd("/Users/nikolausgebhard/Masterthesis Politication/Full Debates Creation/WF Prep")
WriteXLS::WriteXLS(processed_data, "Debates All Legislative Period 5 Sentences.xls")
write.csv(processed_data, "Debates All Legislative Period 5 Sentences.csv")

