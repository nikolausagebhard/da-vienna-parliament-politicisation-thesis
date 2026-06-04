##Find Examples for Few-shot Classification
setwd("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach")
training_standard_sample_manually_coded <- readxl::read_xls("training_standard Manually Classified.xls")
training_sample_pos <- training_standard_sample_manually_coded %>% filter(true_label == 1)
training_sample_neg <- training_standard_sample_manually_coded %>% filter(true_label == 0)

##Sample Candidates for Review
set.seed(123)

pos_candidates <- training_sample_pos %>% slice_sample(n = 30)
neg_candidates <- training_sample_neg %>% slice_sample(n = 30)
Candidates_Few_Show <- rbind(pos_candidates,neg_candidates)
WriteXLS::WriteXLS(Candidates_Few_Show,"Candidates for Few-Shot.xls")

##Manually Working through the Code in Excel and Identifying best Candidates for Few Shot
##Include some falsely labeles Examples in the Few-Shot Training

print(merged_training_data_0S %>%
        filter(llm_label != true_label) %>% sample_n(2) )

few_shot_ids <- c(5703, 8906,11315, 4207 ,399,  #Positive Animosity
                  11415,1822,2238,2941,9656,
                  9630,1470)
few_shot_examples <- training_standard_sample_manually_coded %>%
  filter(X %in% few_shot_ids) %>% select(X,text_segment,true_label)




training_standard_sample_LLM_FS <- anti_join(training_standard_sample_LLM,few_shot_examples, by="X")
nrow(training_standard_sample_LLM) #400 Lines
nrow(training_standard_sample_LLM_FS) ##388 lines correct as 12 will be used for the few shot classification

###################################################################
##Initialising for Few Shot
format_example <- function(text, label) {
  glue("Text: \"{text}\"\nLabel: {label}")
}

few_shot_block <- paste(
  apply(few_shot_examples, 1, function(row) {
    format_example(row["text_segment"], row["true_label"])
  }),
  collapse = "\n\n"
) ## For Prompt
print(few_shot_block)

classify_animosity_FS <- function(text_segment) {
  
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

Examples:

{few_shot_block}

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

# 5. EXECUTION: RUN ON VALIDATION SAMPLE ------------------------------------
# We use a loop or map function to process the sample
# Note: For large samples, consider using purrr::map_dfr with a delay
results_LLM_list <- list()

for(i in 1:nrow(training_standard_sample_LLM_FS)) {
  
  message(glue("Processing row {i} of {nrow(training_standard_sample_LLM_FS)}"))
  
  tryCatch({
    
    res <- classify_animosity_0S(training_standard_sample_LLM_FS$text_segment[i])
    
    results_LLM_list[[i]] <- tibble(
      X = training_standard_sample_LLM_FS$X[i],
      id = training_standard_sample_LLM_FS$id[i],
      speaker_id = training_standard_sample_LLM_FS$speaker_id[i],
      text_segment = training_standard_sample_LLM_FS$text_segment[i],
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
  
  Sys.sleep(0.5)  # rate limit safety
}

final_results_LLM <- bind_rows(results_LLM_list)

# 6. EXPORT FOR YOUR SELF-CLASSIFICATION ------------------------------------
setwd("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach")
WriteXLS::WriteXLS(final_results_LLM, "training_standrad_AI_Classified_FS.xls") 
##F1 Score Calculation
manual <- readxl::read_xls("training_standard Manually Classified.xls")
final_results_LLM_FS <- readxl::read_xls("training_standrad_AI_Classified_FS.xls")
merged_training_data_fewShot <- manual %>%
  left_join(
    final_results_LLM_FS %>%
      select(X, llm_label, llm_reason),
    by = "X"
  )

compute_metrics <- function(df, model_name) {
  
  
  
  tp <- sum(df$llm_label == 1 & df$true_label == 1, na.rm = TRUE)
  
  fp <- sum(df$llm_label == 1 & df$true_label == 0, na.rm = TRUE)
  
  fn <- sum(df$llm_label == 0 & df$true_label == 1, na.rm = TRUE)
  
  tn <- sum(df$llm_label == 0 & df$true_label == 0, na.rm = TRUE)
  
  
  
  precision <- tp / (tp + fp)
  
  recall    <- tp / (tp + fn)
  
  accuracy  <- (tp + tn) / (tp + tn + fp + fn)
  
  
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(data.frame(
    
    Model = model_name,
    
    Precision = round(precision, 3),
    
    Recall = round(recall, 3),
    
    F1 = round(f1, 3),
    
    Accuracy = round(accuracy, 3)
    
  ))
  
}
results_zero <- compute_metrics(merged_training_data_0S, "Zero-shot")
results_few  <- compute_metrics(merged_training_data_fewShot,  "Few-shot")
comparison_table <- bind_rows(results_zero, results_few)

comparison_table
