# General statistics

# Count genders
gender_predicted %>%
  group_by(gender) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

# Top journals
journals %>%
  group_by(pubtitle) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

# Top conferences
conferences %>%
  group_by(pubtitle) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

# Top publications (all)
data %>%
  group_by(pubtitle) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

# Top publication years
data %>%
  group_by(date) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

# Total number of authors
count(gender_predicted, author, sort = TRUE)
