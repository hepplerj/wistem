# General statistics

# Count genders
gender_predicted %>%
  group_by(gender) %>%
  tally

# Total percentage of female authors among all authors
(gender_predicted %>% filter(gender == "female") %>% tally) / (nrow(gender_predicted)) * 100 

# Find any study where women are author
female_authors_all <- gender_predicted %>% 
  filter(gender == "female") %>% 
  select(author)

female_authors_all <- unique(female_authors_all$author)
female_authors_all <- female_authors_all %>% as.data.frame()
names(female_authors_all) <- "author"

female_authors_collapsed <- female_authors_all %>% 
  rowwise %>% 
  ungroup %>%
  { paste(.$author, collapse = "|") }

list_authors_female <- data %>% filter(grepl(female_authors_collapsed, author))
nrow(list_authors_female) / nrow(data) * 100

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
