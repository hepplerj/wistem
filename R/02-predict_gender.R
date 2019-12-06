# Predict gender using the `gender` package.
# Write out three datasets
# 1. Predicted gender
# 2. Journal articles
# 3. Conference proceedings

library(gender)
library(tidyverse)

# Gender prediction -------------------------------------------------------
# We're attempting to predict the gender of the authors based on their first
# names. The scripts below do a few things:
# 1. We separate the authors into their own rows based on the ";" delimiter in
#    the author column.
# 2. We separate names into individual first_name and last_name columns
# 3. We create a temporary year 2012 for the `gender` package to use
# 4. We predict gender based on first name using `gender_df`

# Separate out authors into their own rows
data_clean <- separate_rows(data, author, sep = ";")

# The below code is designed to clean up names and prep them to predict gender
# Separate out first and last names
data_clean <- separate(data_clean, c("lname", "firstn"), col = "author", sep = ",")

# Remove middle initial abbreviations
data_clean$fname <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", data_clean$firstn)

# Trim any possible whitespace
data_clean$fname <- str_trim(data_clean$fname)

# Set a tmpyear to 2012 for `gender` to work
data_clean <- data_clean %>%
  select(lname, fname, pubtitle, date) %>%
  mutate(tmpyear = 2012)

# Add a unique ID to each row
data_clean <- mutate(data_clean, id = rownames(data_clean))

# Predict gender of author using `gender_df`
gender_predicted <- gender_df(data_clean, name_col = "fname",
                              year_col = "tmpyear", method = "ssa")

# Rename fname to name to prepare for merge
gender_predicted$fname <- gender_predicted$name

# Merge the predicted gender with the original dataset
gender_predicted <- merge(gender_predicted, data_clean, all = TRUE)
gender_predicted$author <- paste(gender_predicted$lname,
                                       gender_predicted$fname, sep = ", ")

gender_predicted <- gender_predicted %>%
  select(author, fname, lname, gender, pubtitle, date, id)

gender_predicted$author <- str_trim(gender_predicted$author)

rm(data_clean)

# Separate out journal articles from conference proceedings.
# NB: Proceedings from the Natl. Academy is not a conference proceeding, so we 
# remove it and replace it in the journals dataframe.
conferences <- data %>%
  filter(str_detect(pubtitle, "Conference|Proceedings")) %>% 
  filter(pubtitle != "Proceedings of the National Academy of Sciences of the United States of America")

pnasus <- data %>% 
  filter(pubtitle == "Proceedings of the National Academy of Sciences of the United States of America")

journals <- data %>%
  filter(!str_detect(pubtitle, "Conference|Proceedings")) %>% 
  bind_rows(pnasus)

rm(pnasus)
  
conferences_solo_authors <- gender_predicted %>%
  filter(str_detect(pubtitle, "Conferences|Proceedings"))

journals_solo_authors <- gender_predicted %>%
  filter(!str_detect(pubtitle, "Conferences|Proceedings"))

# Create a dataset that includes the publication type (journal vs. conference)
journal_mutate <- journals %>% mutate(pubtype = "journal")
journal_solo_mutate <- journals_solo_authors %>% mutate(pubtype = "journal")
conf_mutate <- conferences %>% mutate(pubtype = "conference")
conf_solo_mutate <- conferences_solo_authors %>% mutate(pubtype = "conference")
conf_journal_merged <- bind_rows(journal_solo_mutate, conf_solo_mutate)

rm(journal_mutate)
rm(conf_mutate)
rm(conf_solo_mutate)
rm(journal_solo_mutate)

# Save our data -- analysis will be run on these sets
write_csv(gender_predicted, "analysis/data/derived_data/gender_predicted.csv")
write_csv(conferences, "analysis/data/derived_data/conferences.csv")
write_csv(journals, "analysis/data/derived_data/journals.csv")
write_csv(conf_journal_merged, "analysis/data/derived_data/conferences_journals_labeled.csv")

