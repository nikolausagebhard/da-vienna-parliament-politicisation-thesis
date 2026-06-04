library(httr2)
library(jsonlite)
library(tidyverse)
library(glue)
library(caret)

# CONFIGURATION --------------------------------------------------------

GEMINI_API_KEY <- "PERSONALKEY"  # ← Your personal API key from Google AI Studio
#   Never share this!

GEMINI_MODEL   <- "gemini-2.5-flash"   # Which model to use.
#   "flash" = faster and cheaper than "pro"

###DATA PREPARATION ALREADY DONE

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
        maxOutputTokens = 400
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

# 5. EXECUTION: RUN ON VALIDATION SAMPLE ------------------------------------
# We use a loop or map function to process the sample
# Note: For large samples, consider using purrr::map_dfr with a delay
results_LLM_list <- list()

for(i in 1:nrow(training_standard_sample_LLM)) {
  
  message(glue("Processing row {i} of {nrow(training_standard_sample_LLM)}"))
  
  tryCatch({
    
    res <- classify_animosity_0S(training_standard_sample_LLM$text_segment[i])
    
    results_LLM_list[[i]] <- tibble(
      X = training_standard_sample_LLM$X[i],
      id = training_standard_sample_LLM$id[i],
      speaker_id = training_standard_sample_LLM$speaker_id[i],
      text_segment = training_standard_sample_LLM$text_segment[i],
      llm_label = res$label,
      llm_reason = res$reason
    )
    
  }, error = function(e) {
    message(glue("Error at row {i}: {e$message}"))
    
    results_LLM_list[[i]] <- tibble(
      X = training_standard_sample_LLM$X[i],
      id = training_standard_sample_LLM$id[i],
      speaker_id = training_standard_sample_LLM$speaker_id[i],
      text_segment = training_standard_sample_LLM$text_segment[i],
      llm_label = NA,
      llm_reason = "API error"
    )
  })
  
  Sys.sleep(1)  # rate limit safety
}

final_results_LLM <- bind_rows(results_LLM_list)

# 6. EXPORT FOR YOUR SELF-CLASSIFICATION ------------------------------------
setwd("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach")
WriteXLS::WriteXLS(final_results_LLM, "training_standrad_AI_Classified_0S.xls")   

###Robustness against Manually Classification
manual <- readxl::read_xls("training_standard Manually Classified.xls")
final_results_LLM_0S <- readxl::read_xls("training_standrad_AI_Classified_0S.xls")

merged_training_data_0S<- manual %>%
  left_join(
    final_results_LLM_0S %>%
      select(X, llm_label, llm_reason),
    by = "X"
  )

confusionMatrix(
  factor(merged_training_data_0S$llm_label),
  factor(merged_training_data_0S$true_label),
  positive = "1"
)

WriteXLS::WriteXLS(merged_training_data_0S,"Training Data 0S with Manual.xls")
