if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(stringr)
library(dplyr)

file_path <- "cleaned_podcast_data.csv"  # 替换为你的文件路径
podcast_data <- read.csv(file_path, stringsAsFactors = FALSE)

cat("data:\n")
print(str(podcast_data))


podcast_data$description <- sapply(podcast_data$description, function(x) {
  cleaned_description <- str_replace_all(x, "[^A-Za-z0-9 .,!?()\\[\\]'\"-]", " ")
  cleaned_description <- str_squish(cleaned_description)
  return(cleaned_description)
})

head(podcast_data$description)

write.csv(podcast_data, "cleaned_podcast_data_with_spaces.csv", row.names = FALSE)
cat("to 'cleaned_podcast_data_with_spaces.csv'")
