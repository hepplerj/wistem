# Analyzing gender in STEM publications
#
# Heidi Blackburn and Jason Heppler
# UNO Libraries, University of Nebraska at Omaha
# Updated: 2019-03-18

library(tidyverse)
library(gender)
library(tidygraph)
library(ggraph)
library(network)
library(jahMisc)

# Read data
data <- readxl::read_xlsx("data-raw/Heidi_gender_data4.xlsx")

# Data cleanup -----------------------------------------------------------------

# Rename columns to make it easier for writing scripts.
data <- data %>% 
  rename(author = "Author") %>% 
  rename(title = "Title") %>% 
  rename(pubtitle = "Publication Title") %>% 
  rename(publisher = "Publisher") %>% 
  rename(date = "Date") 

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
data_predict <- separate(data_clean, c("lastn", "firstn"), col = "author", sep = ",")
data_predict$fname <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", data_predict$firstn)
data_predict$fname <- str_trim(data_predict$fname)
data_predict <- data_predict %>% 
  select(lastn, fname, pubtitle, date) %>% 
  mutate(tmpyear = 2012)
data_predict <- mutate(data_predict, id = rownames(data_predict))

# Predict gender of author using `gender_df`
gender_predicted <- gender_df(data_predict, name_col = "fname", year_col = "tmpyear", method = "ssa")
gender_predicted$fname <- gender_predicted$name

gender_predicted_final <- merge(gender_predicted, data_predict, all = TRUE)
gender_predicted_final$author <- paste(gender_predicted_final$lastn, gender_predicted_final$fname, sep = ", ")
gender_predicted_final <- gender_predicted_final %>% 
  select(author, fname, lastn, gender, pubtitle, date, id)
gender_predicted_final$author <- str_trim(gender_predicted_final$author)

rm(data_clean)
rm(data_predict)
write_csv(gender_predicted_final, "/data/gender_data.csv")

# Conferences and journals -----------------------------------------------------
# Separate out journal articles from conference proceedings.

conferences <- gender_predicted_final %>% 
  filter(str_detect(pubtitle, "Conference|Proceedings"))
journals <- gender_predicted_final %>% 
  filter(!str_detect(pubtitle, "Conference|Proceedings"))

write_csv(conferences, "/data/conference_data.csv")
write_csv(journals, "/data/journal_data.csv")

# Data analytics ----------------------------------------------------------

# Count genders
gender_final %>% 
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

# Create a dataset that includes the publication type (journal vs. conference)
journal_mutate <- journals %>% mutate(pubtype = "journal") 
conf_mutate <- conferences %>% mutate(pubtype = "conference") 
conf_journal_merged <- bind_rows(journal_mutate, conf_mutate)

rm(journal_mutate)
rm(conf_mutate)
write_csv(conf_journal_merged, "data/total_data.csv")
  
# Top publications (all)
gender_predicted_final %>% 
  group_by(pubtitle) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

# Top publication years
gender_predicted_final %>% 
  group_by(date) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

# Total number of articles
count(data, pubtitle, sort = TRUE)

# Total number of authors
count(data, author, sort = TRUE)


# Authorship order --------------------------------------------------------
# NB: Use data from WiSTEM Data9.xlsx for lists of authors
data_name_listed <- readxl::read_xlsx("data-raw/WiSTEM Data9.xlsx")

# Rename columns for easier manipulation
data_name_listed <- data_name_listed %>% 
  rename(author = "Author") %>% 
  rename(title = "Title") %>% 
  rename(pubtitle = "Publication Title") %>% 
  rename(publisher = "Publisher") %>% 
  rename(date = "Date") 

# Separate lists of names into columns according to the order they appear in 
# the comma-separated list
data_name_listed <- mutate(data_name_listed, id = row_number())
data_name_order <- data_name_listed %>%
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
data_name_order$author <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", data_name_order$author)

# Merge name order with analysis dataset
data_name_order <- merge(data_name_order, gender_predicted_final, by = "author")

write_csv(data_name_order, "data/gender_position.csv")

# Data plots -------------------------------------------------------------------

# Plot the number of publications per year
ggplot(gender_predicted_final, aes(x = date)) +
  geom_bar() +
  theme_minimal()

# Plot the number of articles per publication
ggplot(gender_predicted_final, aes(x = pubtitle)) +
  geom_bar() +
  theme_minimal()

# Scatterplot of name and position
# Not a particularly useful view into the data.
ggplot(data_name_order, aes(x = author, y = position)) +
  geom_point(aes(color=gender))

# Faceted barchart of gender by publication
# Not a particularly useful view into the data.
ggplot(data_name_order, aes(position, fill = gender)) +
  geom_bar() +
  facet_grid(pubtitle ~ .) 

# Authorship by position
# Useful chart.
ggplot(data_name_order, aes(position)) +
  geom_bar() +
  facet_grid(. ~ gender) +
  labs(title = "Authorship position by gender") +
  theme_jah()

# Stacked barchart of gender by year
# Useful chart.
ggplot(data_name_order, aes(x = date, fill = gender)) +
  geom_bar() +
  theme_minimal()

# Gender by journal vs. conference
# Useful chart.
ggplot(conf_journal_merged, aes(x = date, fill = gender)) +
  geom_bar() +
  facet_grid(~ pubtype) +
  theme_light()

# Counting the number of authors by position
data_name_order %>% 
  count(position) %>% 
  ggplot(aes(x = position, y = n)) + geom_bar(stat = "identity") +
  theme_minimal()

# Network analysis ------------------------------------
# We want to look at two networks:
# 1. Where do people publish most often (bimodal)
# 2. Who publishes with each other

# Separate out data to a 'source'
sources <- gender_final %>% 
  distinct(author, gender) %>% 
  dplyr::rename(label = author) %>% 
  mutate(type = "author")

# Separate out data to a 'target'
destinations <- gender_final %>% 
  distinct(pubtitle) %>% 
  dplyr::rename(label = pubtitle) %>% 
  mutate(type = "publication")

# Join source and target together and add a unique ID
nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column('id')
nodes[is.na(nodes)] = ''
nodes <- nodes %>% unite(type, c(type.x, type.y), remove = FALSE, sep='')
nodes <- nodes %>% select(id, label, gender, type)

# Generate a nodes (author) set
nodes_gender <- nodes %>% filter(type == "author")

# Generate a nodes (publication) set
publications <- gender_final %>% 
  dplyr::group_by(author, pubtitle) %>% 
  dplyr::summarise(weight = n()) %>% 
  dplyr::ungroup()

# Generate edges for publications
edges <- publications %>% 
  left_join(nodes, by = c("author" = "label")) %>% 
  dplyr::rename(from = id)

# Generate edges for authors
edges <- edges %>% 
  left_join(nodes, by = c("pubtitle" = "label")) %>% 
  dplyr::rename(to = id)

# Final edges
edges <- select(edges, from, to, weight)
edges_gender <- publications %>% 
  left_join(nodes_gender, by = c("author" = "label")) %>% 
  dplyr::rename(from = id)
edges_gender <- edges_gender %>%
  left_join(nodes_gender, by = c("author" = "label")) %>% 
  dplyr::rename(to = id)

# Final edges by gender
edges_gender <- select(edges_gender, from, to, weight)

# Prepare network data: network (publication types) and network (gender)
network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
network_gender <- network(edges_gender, vertex.attr = nodes_gender, matrix.type = "edgelist", ignore.eval = FALSE)

nodes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
graph_tidy <- nodes_tidy %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  mutate(degree = centrality_degree()) %>% 
  activate(edges) %>% 
  arrange(desc(weight))

gender_nodes_tidy <- tbl_graph(nodes = nodes_gender, edges = edges_gender, directed = FALSE)
gender_tidy <- gender_nodes_tidy %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  mutate(degree = centrality_degree()) %>% 
  activate(edges) %>% 
  arrange(desc(weight))

edges <- edges %>% dplyr::rename(source = from) %>% dplyr::rename(target = to)

# Write final network data for use in Gephi, etc.
write_csv(nodes, "/data/nodes.csv")
write_csv(edges, "/data/edges.csv")

# Graph: Authors and publications bimodal network colored by 
# author and publication
ggraph(graph_tidy, layout = "nicely") +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  geom_node_point(aes(color = type)) +
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "No. of publications",
       title = "Author Publications",
       caption = "Heidi Blackburn and Jason Heppler") +
  theme_jah() +
  theme_graph()

# Graph: Authors and co-authors colored by gender
ggraph(gender_tidy) +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  geom_node_point(aes(color = gender)) +
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "No. of publications",
       title = "Journal Publications",
       subtitle = "Authors corresponding journals.",
       caption = "Heidi Blackburn and Jason Heppler") +
  theme_graph() 

# Summary statistics -----------------------------------------------------------
# % of journals that accounted for % of the references (i.e. 5% of the journals were responsible for 60% of the citations)
# # of journals (X%)
# # of conference proceedings (X%)
# NAME HERE had the greatest number of publications at XX, followed by NAME HERE with X, and NAME HERE with XX
# top twenty journals that publish articles on women in STEM in higher education and the ranking of publisher by number of articles (table)
# # of authors identified
# # of authors who have published X% of the works
# X% of authors who write with a co-author
# # of publishers and % of journals they own (within sample)
# Top publisher with topic three journal names (this might be trickier)
# % increase in journals published between 2007 and 2018
# % increase in journal articles from top publishing journals (if true)
# % increase in conference proceeding publications
# % of studies published outside traditional STEM journals
