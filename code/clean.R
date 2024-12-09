if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(stringr)
library(dplyr)

file_path <- "data.csv"
podcast_data <- read.csv(file_path, stringsAsFactors = FALSE)

cat("data:\n")
print(str(podcast_data))

podcast_data$name <- sapply(podcast_data$name, function(x) {
  cleaned_description <- str_replace_all(x, "[^A-Za-z0-9 .,!?()\\[\\]'\"-]", " ")
  cleaned_description <- str_squish(cleaned_description)
  return(cleaned_description)
})

head(podcast_data$name)

output_file <- file.path(dirname(file_path), "cleaned_podcast_data_with_spaces.csv")
write.csv(podcast_data, output_file, row.names = FALSE)

cat("Cleaned data saved to:", output_file, "\n")
