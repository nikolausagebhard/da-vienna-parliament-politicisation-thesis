library(httr2)
library(jsonlite)
library(tidyverse)
library(glue)
library(caret)
# CONFIGURATION --------------------------------------------------------

GEMINI_API_KEY <- "PERSONALAPIKEY"  # ← Your personal API key from Google AI Studio
#   Never share this!

GEMINI_MODEL   <- "gemini-2.5-flash"   # Which model to use.
#   "flash" = faster and cheaper than "pro"

###End of Setup--------------------------------

classify_animosity_0S <- function(text_segment) {
  
  prompt <- glue("
You are a classifier for political speech.

Task:
Classify whether the following text contains animosity.

Definition:
Animosity = any expression of hostility, contempt, derogation, or strong negative evaluation towards another political actor, party, or political
group.

Ignore any negative comments about the EU and other European Countries in General
Rules:
- Negative evaluation towards other political group or person or hostile tone → 1
- Neutral, factual, or respectful language → 0
- Mere disagreement without negative evaluation → 0

Labels:
0 = no animosity
1 = animosity present

Output format (STRICT):
Start your answer with either:
0 / or 1 /

Then provide a short explanation.

Example:
1 / This sentence contains negative evaluation.

Text:
{text_segment}
")
  
  req <- request(glue("https://generativelanguage.googleapis.com/v1beta/models/{GEMINI_MODEL}:generateContent?key={GEMINI_API_KEY}")) %>%
    req_body_json(list(
      contents = list(list(parts = list(list(text = prompt)))),
      generationConfig = list(
        temperature = 0,
        topP = 1,
        topK = 1,
        maxOutputTokens = 300
      )
    )) %>%
    req_retry(max_tries = 3)
  
  resp <- req_perform(req)
  content <- resp_body_json(resp)
  
  raw_text <- paste(
    sapply(content$candidates[[1]]$content$parts, function(p) p$text),
    collapse = ""
  )
  
  cat(raw_text)  # debugging (optional)
  
  # Extract label (only first character)
  label <- str_extract(raw_text, "^[01]")
  
  if (is.na(label)) {
    return(list(
      label = NA,
      reason = raw_text
    ))
  }
  
  label <- as.integer(label)
  
  # Extract explanation
  reason <- str_remove(raw_text, "^[01]\\s*/\\s*")
  
  return(list(
    label = label,
    reason = reason
  ))
}
##Data Import
setwd("/Users/nikolausgebhard/Masterthesis Politication/Full Debates Creation/WF Prep")
data_llm <- read.csv("Debates All Legislative Period 5 Sentences.csv")
setwd("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach")
training_standard_sample_LLM <- readxl::read_xls("training_standard_Manually Classified.xls")
data_llm <- data_llm %>% mutate(row_id = row_number())

data_0S <- anti_join(data_llm, training_standard_sample_LLM, by = "X")
results_LLM_list <- list()

for(i in 1:nrow(NotClassified)) {
  
  message(glue("Processing row {i} of {nrow(NotClassified)}"))
  
  tryCatch({
    
    res <- classify_animosity_0S(NotClassified$text_segment[i])
    
    results_LLM_list[[i]] <- tibble(
      X = NotClassified$X[i],
      id = NotClassified$id[i],
      speaker_id = NotClassified$speaker_id[i],
      text_segment = NotClassified$text_segment[i],
      llm_label = res$label,
      llm_reason = res$reason
    )
    
  }, error = function(e) {
    message(glue("Error at row {i}: {e$message}"))
    
    results_LLM_list[[i]] <- tibble(
      X = NotClassified$X[i],
      id = NotClassified$id[i],
      speaker_id = NotClassified$speaker_id[i],
      text_segment = NotClassified$text_segment[i],
      llm_label = NA,
      llm_reason = "API error"
    )
  })
  
  Sys.sleep(0.5)  # rate limit safety
}

final_results_LLM <- bind_rows(results_LLM_list)

setwd("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach")
WriteXLS::WriteXLS(final_results_LLM, "animosity_validation_with error_results.xls")
##Manually Grade the skipped over Parts from the LLM
final_classification_LLM <- readxl::read_xls("animosity_validation_results.xls")
nrow(final_classification_LLM) #check how many Rows were already classified
nrow(data_0S) ##10729 rows

##Identify Errors and not processed Lines from the AI
NotClassified <- anti_join(data_0S,final_classification_LLM,by="X")
##Read in formerly not classified errors
NotClassifiedPostAI <- readxl::read_xls("animosity_validation_with error_results.xls")
final_classification_LLM <- rbind(final_classification_LLM,NotClassifiedPostAI)
nrow(final_classification_LLM)
##end training data back in
Training_Sample_AI <- readxl::read_xls("animosity_validation_results_OS.xls")
head(Training_Sample_AI)
end_result_AI_classified <- rbind(final_classification_LLM,Training_Sample_AI)
nrow(end_result_AI_classified) ##11.129 rows

WriteXLS::WriteXLS(end_result_AI_classified,"Final Classified Speeches.xls")
