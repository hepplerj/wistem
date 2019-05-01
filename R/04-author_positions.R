# Authorship order -- determining first, second, ... n author positions
# across journal articles.

# Separate lists of names into columns according to the order they appear in
# the comma-separated list
author_order <- mutate(data, id = row_number())
author_order <- author_order %>%
  select(author) %>%
  transmute(
    id = row_number(),
    author = strsplit(author, ";")
  ) %>%
  unnest() %>%
  group_by(id) %>%
  mutate(
    position = row_number(),
    author = trimws(author)
  ) %>%
  ungroup()

# Clean up data and remove special characters
author_order$author <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", author_order$author)

# Merge name order with analysis dataset
author_order <- merge(author_order, gender_predicted, by = "author")

# Data output
write_csv(author_order, "analysis/data/derived_data/gender_position.csv")
