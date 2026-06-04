library(dplyr)
library(readr)
library(stringr)
#Create Speaker Metadata
speaker_lookup <- ParliamentaryData_noText %>%
  select(speaker_id, speaker_name, speaker_party,party_status,speaker_minister,legislative_period) %>%
  distinct(speaker_id, .keep_all = TRUE)
setwd("/Users/nikolausgebhard/Masterthesis Politication/LLM Approach")
##Import classified ddtaset
end_result_AI_classified <- read_delim("Full AI Results Gemini.csv", 
                                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(end_result_AI_classified)
end_result_AI_classified_with_Metadata <- end_result_AI_classified %>% left_join(speaker_lookup, by="speaker_id"|"legislative period") 
end_result_AI_classified_with_Metadata <- end_result_AI_classified_with_Metadata %>%
  mutate(legislative_period = as.numeric(
    str_extract(id, "(?<=\\d{4}-\\d{2}-\\d{2}-)[0-9]+")
  ))


WriteXLS::WriteXLS(
  
  end_result_AI_classified_with_Metadata,
  
  "End Results with Meta Data.xls",
  
  SheetNames = "Results"
  
)

collapsed_results <- end_result_AI_classified_with_Metadata %>%
  group_by(id) %>%
  summarise(
    LLM_score = mean(as.numeric(llm_label), na.rm = TRUE),
    across(-llm_label, first),
    .groups = "drop"
  ) %>% mutate(speaker_party = toupper(speaker_party))
collapsed_results$llm_reason=NULL
collapsed_results$X=NULL
collapsed_results$chunk_id=NULL
#Add if Government or Coalition
library(dplyr)


collapsed_results <- collapsed_results %>%
  mutate(
    party_status = case_when(
      
      # 20th period (1995–1999): SPÖ–ÖVP
      legislative_period == 20 & speaker_party %in% c("SPÖ","ÖVP") ~ "government",
      
      # 21st period (1999–2002): ÖVP–FPÖ
      legislative_period == 21 & speaker_party %in% c("ÖVP","FPÖ") ~ "government",
      
      # 22nd period (2002–2006): ÖVP–FPÖ (+ BZÖ later)
      legislative_period == 22 & speaker_party %in% c("ÖVP","FPÖ","BZÖ") ~ "government",
      
      # 23–25: Grand coalitions
      legislative_period %in% c(23,24,25) & speaker_party %in% c("SPÖ","ÖVP") ~ "government",
      
      # 26: ÖVP–FPÖ
      legislative_period == 26 & speaker_party %in% c("ÖVP","FPÖ") ~ "government",
      
      # 27: ÖVP–Greens
      legislative_period == 27 & speaker_party %in% c("ÖVP","GRÜNE") ~ "government",
      
      # Everything else
      TRUE ~ "opposition"
    )
  )

WriteXLS::WriteXLS(collapsed_results,"FinalResultsCollapsed.xls")
write.csv(collapsed_results,"FinalResultsCollapsed.csv")

                   