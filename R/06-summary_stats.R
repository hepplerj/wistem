# Summary statistics

pub_counts <- count(data, pubtitle, sort = TRUE)

# % of journals that accounted for % of the references
# (i.e. 5% of the journals were responsible for 60% of the citations)

pub_counts$pct <- pub_counts$n / sum(pub_counts$n)
pub_counts

# Number of journals (X%)

count(journals, pubtitle, sort = TRUE)

# Number of conference proceedings (X%)

count(conferences, pubtitle, sort = TRUE)

# NAME HERE had the greatest number of publications at XX,
# followed by NAME HERE with X, and NAME HERE with XX

count(data, pubtitle, sort = TRUE)

## % of authors who write with a co-author
# Find semicolons to indicate co-authorship and assign TRUE or FALSE
# to indicate coauthorship (TRUE) or solo authorship (FALSE).
authors_coauthor <- data %>%
  select(author) %>%
  rowwise %>%
  mutate(solo = ifelse(str_detect(author, ";"), TRUE, FALSE))

authors_coauthor %>%
  group_by(solo) %>%
  tally()
# 522 coauthored articles, 125 solo authors

coauthors_list <- authors_coauthor %>%
  filter(solo == "TRUE") %>%
  separate_rows(author, sep = ";") %>%
  select(author) %>%
  distinct()

coauthors_list$author <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", coauthors_list$author)
coauthors_list$author <- str_trim(coauthors_list$author)
coauthors_list <- coauthors_list %>% distinct()

total_authors <- data %>%
  separate_rows(author, sep = ";") %>%
  select(author) %>%
  distinct()

nrow(coauthors_list) / nrow(total_authors) * 100
# 88.84% of the work is co-authored

## X% of authors who are female who write with co-authors
# ---------------------------------------------------------------------------------------------------------

# merge authors_coauthor with gender_predicted_final to find this data
# then subset by female

author_gender_to_match <- gender_predicted %>%
  select(author, gender) %>%
  distinct()

authors_coauthor_gender <- coauthors_list %>%
  left_join(author_gender_to_match, by = "author")

coauthors_female <- authors_coauthor_gender %>% filter(gender == "female")
nrow(coauthors_female) / nrow(total_authors) * 100
# 51.99%

## X% of authors who are male who write with co-authors
# ---------------------------------------------------------------------------------------------------------
coauthors_male <- authors_coauthor_gender %>% filter(gender == "male")
nrow(coauthors_male) / nrow(total_authors) * 100
# 20.47%

coauthors_unknown <- authors_coauthor_gender %>% filter(is.na(gender))
nrow(coauthors_unknown) / nrow(total_authors) * 100
# 16.36%

## X% of first authors who are male/female
# ---------------------------------------------------------------------------------------------------------
first_author_female <- author_order %>% filter(position == 1, gender == "female")
nrow(first_author_female) / nrow(author_order %>% filter(position == 1)) * 100
# 60.72%
nrow(unique(first_author_female %>% select(author)))
# 357

first_author_male <- author_order %>% filter(position == 1, gender == "male")
nrow(first_author_male) / nrow(author_order %>% filter(position == 1)) * 100
# Answer: 18%
nrow(unique(first_author_male %>% select(author)))
# 110

first_author_unknown <- author_order %>% filter(position == 1, is.na(gender))
nrow(first_author_unknown) / nrow(author_order %>% filter(position == 1)) * 100
# Answer: 21.27%
nrow(unique(first_author_unknown %>% select(author)))
# 88

nrow(first_author_female %>% filter(date == 2008))
nrow(first_author_female %>% filter(date == 2018))
((113/3)^(1/11) - 1) * 100
# 39.08%

nrow(first_author_male %>% filter(date == 2008))
nrow(first_author_male %>% filter(date == 2018))
((23/6)^(1/11) - 1) * 100
# 12.99%

## X% of last authors who are male/female
# ---------------------------------------------------------------------------------------------------------

# Assuming here that "last" author could be second author, but falls anywhere
# between 2 and 33.
author_order %>%
  group_by(position) %>%
  tally()

last_author_female <- author_order %>% filter(gender == "female", position %in% c(2:33))
last_author_male <- author_order %>% filter(gender == "male", position %in% c(2:33))
last_author_unknown <- author_order %>% filter(is.na(gender), position %in% c(2:33))

nrow(last_author_female) / nrow(author_order) * 100
# Answer: 38.48%

nrow(last_author_male) / nrow(author_order) * 100
# Answer: 14.54%

nrow(last_author_unknown) / nrow(author_order) * 100
# Answer: 10.94% 

## X% of first authors who are female who co-author
# ---------------------------------------------------------------------------------------------------------

positions_to_match <- author_order %>% select(author, position)

first_authors_coauthor_exerpt <- coauthors_list %>%
  left_join(positions_to_match, by = "author") %>%
  left_join(author_gender_to_match, by = "author")

first_author_female_coauthor <- first_authors_coauthor_exerpt %>% filter(gender == "female", position == 1)

nrow(first_author_female_coauthor) / nrow(first_authors_coauthor_exerpt) * 100
# Answer: 20.25%

## X% of last authors who are female who co-author
# ---------------------------------------------------------------------------------------------------------

last_author_female_coauthor <- first_authors_coauthor_exerpt %>% filter(gender == "female", position %in% c(2:33))
nrow(last_author_female_coauthor) / nrow(first_authors_coauthor_exerpt) * 100
# Answer: 39.72%

## Where are first authors publishing (what journals supporting female first authors)
# ---------------------------------------------------------------------------------------------------------
female_journalfirst_author <- author_order %>% 
  select(author, gender, position, pubtitle) %>% 
  filter(gender == "female", position == 1)

pubtitle_count <- female_journalfirst_author %>% 
  select(pubtitle) %>% group_by(pubtitle) %>% tally()
female_journalfirst_author <- female_journalfirst_author %>% left_join(pubtitle_count, by = "pubtitle")
# Answer: There are 69 senior female scholars in the dataset.

## Who are the senior female authors (last authors in pieces)
# ---------------------------------------------------------------------------------------------------------
female_senior_authors <- sapply(gsub("^.*;", "", journals$author), tail, 1) %>% as.data.frame()
colnames(female_senior_authors) <- "author"
female_senior_authors$author <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", female_senior_authors$author)
female_senior_authors <- left_join(female_senior_authors, gender_predicted, by = "author")
female_senior_authors <- female_senior_authors %>% filter(gender == "female") %>% select(author) %>% distinct()

## %X articles by females 
# ---------------------------------------------------------------------------------------------------------

# Women that are solo authors or first author
female_first_author_unique <- unique(first_author_female$author) %>% as.data.frame()
colnames(female_first_author_unique) <- "author"

female_solo_author_unique <- authors_coauthor %>% 
  filter(solo == "FALSE") %>% 
  select(author) %>% 
  distinct()

female_solo_author_unique$author <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", female_solo_author_unique$author)
female_solo_author_unique$author <- str_trim(female_solo_author_unique$author)
female_first_or_solo <- full_join(female_solo_author_unique, female_first_author_unique)
female_first_or_solo_vector <- female_first_or_solo %>% as.vector()

nrow(female_first_or_solo) / nrow(conf_journal_merged %>% filter(gender == "female")) * 100
# Answer: 33.93% of all female authors are first or solo author 

nrow(female_first_or_solo) / nrow(conf_journal_merged %>% select(author) %>% distinct()) * 100
# Answer: 24.39% of all authors are first or solo author

## Top ten journals that saw growth
# ---------------------------------------------------------------------------------------------------------
top_three_journals <- c("Journal of Women and Minorities in Science and Engineering", 
                      "Sex Roles", "Social Sciences")

top_ten_journals <- c("Journal of Women and Minorities in Science and Engineering", 
                      "Sex Roles", "Social Sciences", "Frontiers in Psychology", 
                      "Journal of Science Education and Technology", 
                      "Psychology of Women Quarterly", "Research in Higher Education", 
                      "Journal of vocational behavior", "PLoS ONE", 
                      "Journal of Diversity in Higher Education")

top_ten_publishers <- c("Sage Publications, Inc.",
                        "Springer New York LLC",
                        "Elsevier Inc.",
                        "Routledge",
                        "American Society of Engineering Education",
                        "Wiley-Blackwell Publishing, Inc.",
                        "American Psychological Association",
                        "Begell House, Inc.",
                        "Frontiers Research Foundation",
                        "M D P I AG")

top_ten_conferences <- c("ASEE Annual Conference and Exposition, Conference Proceedings",
                         "Proceedings of the National Academy of Sciences of the United States of America",
                         "2007 37th Annual Frontiers In Education Conference - Global Engineering: Knowledge Without Borders, Opportunities Without Passports",
                         "2014 IEEE Frontiers in Education Conference (FIE) Proceedings",
                         "7th IEEE GCC Conference and Exhibition (GCC)",
                         "ACM Conference on Innovation and Technology in Computer Science Education",
                         "ACM International Conference on Measurement and Modeling of Computer Systems",
                         "AIP Conference Proceedings",
                         "2016 International Conference on Computational Science and Computational Intelligence, CSCI 2016",
                         "Proceedings of the 7th IEEE Integrated STEM Education Conference")

social_science_journals <- c("Sex Roles", "Social Sciences", "Frontiers in Psychology",
                             "Psychology of Women Quarterly", "Research in Higher Education",
                             "Journal of vocational behavior", "Journal of Diversity in HIgher Education")

journals_top_three <- journals %>% 
  filter(pubtitle %in% top_three_journals) %>% as.data.frame()
journals_top_ten <- journals %>% 
  filter(pubtitle %in% top_ten_journals) %>% as.data.frame()
conference_top_ten_abbr <- conferences %>% 
  filter(pubtitle %in% top_ten_conferences) %>% as.data.frame()
publishers_top_ten_abbr <- journals %>% 
  filter(publisher %in% top_ten_publishers) %>% as.data.frame()
journals_top_ten_abbr <- journals %>% 
  filter(pubtitle %in% top_ten_journals) %>% as.data.frame()

journals_social_science <- journals_top_ten %>% 
  filter(pubtitle %in% social_science_journals) %>% as.data.frame()

## Top publisher with topic three journal names (this might be trickier)
pub_ownership_titles_count <- data %>% 
  group_by(pubtitle) %>% summarize(pub_title_count = n())

pub_ownership_publishers_count <- data  %>% 
  group_by(publisher) %>% summarize(pub_publisher_count = n())

pub_ownership <- data %>% 
  select(publisher, pubtitle) %>% 
  left_join(pub_ownership_publishers_count, by = "publisher")
pub_ownership <- pub_ownership %>% 
  left_join(pub_ownership_titles_count, by ="pubtitle")

pub_top_three_journals <- pub_ownership %>% 
            arrange(desc(pub_publisher_count), desc(pub_title_count)) %>% 
                distinct(pubtitle, .keep_all=TRUE) %>% 
                group_by(publisher) %>% slice(1:3)
write_csv(pub_top_three_journals, "analysis/data/derived_data/topthree.csv")

## Social science journals authors

### % of authors in soc sci journals women
female_social_science_authors <- gender_predicted %>% 
  filter(gender == "female", pubtitle %in% social_science_journals) %>% 
  distinct(author)

all_social_science_authors <- gender_predicted %>% 
  filter(pubtitle %in% social_science_journals) %>% 
  distinct(author)

nrow(female_social_science_authors) / nrow(all_social_science_authors)
# Answer: 63.12%

### % of women as first authors in soc sci journals
female_social_science_authors_solo <- gender_predicted %>% 
  filter(pubtitle %in% social_science_journals, author %in% female_first_or_solo$author) %>% 
  distinct(author)

nrow(female_social_science_authors_solo) / nrow(all_social_science_authors)
# Answer: 25.1%
