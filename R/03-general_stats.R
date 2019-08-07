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

# tests ------------------
data %>%
  rownames_to_column('rn') %>% 
  separate_rows(author, sep=";\\s*") %>%
  inner_join(female_authors_all)%>% 
  group_by(rn, pubtitle) %>% 
  summarise(author = str_c(author, collapse = "; ")) %>%
  ungroup %>%
  select(names(data))

data2 <- data %>%
  # If you want to keep the original names duplicate column first
  mutate(author_sep = author) %>%
  # Take each delimited author and give them their own row (tidy data)
  tidyr::separate_rows(author_sep,sep = ";") %>%
  # Filter to only keep rows where the individual author is the other vector
  filter(author_sep %in% female_authors_all$author) %>%
  # Remove that extra column we created
  select(-author_sep) %>%
  # Remove duplicate rows in case more than one author is the delimited list was female
  distinct()
# tests --------------------

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
