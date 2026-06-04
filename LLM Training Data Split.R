# Load necessary library
library(dplyr)
#######Create Training Sample######
# Set seed for reproducibility
set.seed(123)

# Load your data
setwd("/Users/nikolausgebhard/Masterthesis Politication/Full Debates Creation/WF Prep")
data <- read.csv("Debates All Legislative Period 5 Sentences.csv")

# Create an index for the split
# 80% of 11,129 is approx 8,903


training_standard_sample <- data %>% sample_n(400) %>% select(X, id, chunk_id, speaker_id, text_segment)

write.csv(training_standard_sample, "/Users/nikolausgebhard/Masterthesis Politication/LLM Approach/training_standard_to_classify.csv", row.names = FALSE)
WriteXLS::WriteXLS(training_standard_sample, "/Users/nikolausgebhard/Masterthesis Politication/LLM Approach/training_standard_to_classify.xls", row.names = FALSE)

training_standard_sample_LLM <- training_standard_sample %>% select(X, id, chunk_id, speaker_id, text_segment)
