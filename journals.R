# Analyzing women in STEM publications
#
# Heidi Blackburn and Jason Heppler
# UNO Libraries, University of Nebraska at Omaha
# 2018-12-03

library(tidyverse)
library(gender)
library(tidygraph)
library(ggraph)
library(network)
library(jahMisc)

data <- readxl::read_xlsx("data-raw/WiSTEM Data9.xlsx")

# Cleanup -----------------------------------------------------------------

#names(data)
data <- data %>% 
  rename(author = "Author") %>% 
  rename(title = "Title") %>% 
  rename(pubtitle = "Publication Title") %>% 
  rename(publisher = "Publisher") %>% 
  rename(date = "Date") 

# Gender prediction -------------------------------------------------------

# Separate out authors into their own rows
data_clean <- separate_rows(data, author, sep = ";")

# The below code is designed to clean up names and prep them to predict gender
data_predict <- separate(data_clean, c("lastn", "firstn"), col = "author", sep = ",")
data_predict$fname <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", data_predict$firstn)
data_predict$fname <- str_trim(data_predict$fname)
data_predict <- data_predict %>% 
  select(lastn, fname, pubtitle, date)
data_predict <- data_predict %>% mutate(tmpyear = 2012)
data_predict <- mutate(data_predict, id = rownames(data_predict))

# Predict gender of author
gender_predicted <- gender_df(data_predict, name_col = "fname", year_col = "tmpyear", method = "ssa")
gender_predicted$fname <- gender_predicted$name

gender_final <- merge(gender_predicted, data_predict, all = TRUE)
gender_final$author <- paste(gender_final$lastn, gender_final$fname, sep = ", ")
gender_final <- gender_final %>% 
  select(author, fname, lastn, gender, pubtitle, date, id)
gender_final$author <- str_trim(gender_final$author)

write_csv(gender_final, "data/gender_data.csv")

# Separate out conferences from journals
conferences <- gender_final %>% 
  filter(str_detect(pubtitle, "Conference|Proceedings"))
journals <- gender_final %>% 
  filter(!str_detect(pubtitle, "Conference|Proceedings"))

# Data analytics ----------------------------------------------------------

gender_final %>% 
  group_by(gender) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

gender_final %>% 
  group_by(pubtitle) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

gender_final %>% 
  group_by(date) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(gender_final, aes(x = date)) +
  geom_bar() +
  theme_minimal()

ggplot(gender_final, aes(x = pubtitle)) +
  geom_bar() +
  theme_minimal()

# Authorship order --------------------------------------------------------
data_name_order <- data %>% select(author)
data_name_order$author <- str_trim(data_name_order$author)
data_name_order <- data_name_order %>% 
  separate(col = author, into = c("1","2","3","4","5","6","7","8","9","10","11",
                                  "12","13","14","15", "16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29",
                                  "30","31","32","33","34","35"), sep = ";")

data_name_order <- data_name_order %>% 
  gather(position, name)
data_name_order$name <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", data_name_order$name)
data_name_order <- data_name_order %>% drop_na()
data_name_order$position <- as.numeric(data_name_order$position)
data_name_order$name <- str_trim(data_name_order$name)
data_name_order %>%
  group_by(name) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

data_name_order <- merge(data_name_order, gender_final, by.x = "name", by.y = "author", all = TRUE)

write_csv(data_name_order, "data/gender_position.csv")

# Viz: 
ggplot(data_name_order, aes(x = name, y = position)) +
  geom_point(aes(color=gender))

ggplot(data_name_order, aes(x = date, fill = gender)) +
  geom_bar() +
  theme_minimal()

ggplot(subset(data_name_order, !is.na(date)), aes(x = date, fill = gender)) +
  geom_bar(position = position_fill()) +
  coord_polar(theta = "y") +
  facet_wrap(~ date) +
  theme_jah() +
  labs(title = "Authorship by Gender") +
  theme(axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
 
# Viz: Counting the number of authors by position
data_name_order %>% 
  count(position) %>% 
  ggplot(aes(x = position, y = n)) + geom_bar(stat = "identity") +
  theme_minimal()

# Viz: number of articles per author by year


# Viz: number of articles per journal by year

# Preparing network analysis ------------------------------------
sources <- gender_final %>% 
  distinct(author) %>% 
  dplyr::rename(label = author) %>% 
  mutate(type = "author")

destinations <- gender_final %>% 
  distinct(pubtitle) %>% 
  dplyr::rename(label = pubtitle) %>% 
  mutate(type = "publication")

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column('id')
nodes <- nodes %>% unite(type, c(type.x, type.y), remove = FALSE)
nodes <- nodes %>% select(id, label, type)

publications <- data_clean %>% 
  dplyr::group_by(author, pubtitle) %>% 
  dplyr::summarise(weight = n()) %>% 
  dplyr::ungroup()

publications <- publications %>% drop_na()

edges <- publications %>% 
  left_join(nodes, by = c("author" = "label")) %>% 
  dplyr::rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("pubtitle" = "label")) %>% 
  dplyr::rename(to = id)

edges <- select(edges, from, to, weight)

# Prepare network data
network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
graph_tidy <- routes_tidy %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  mutate(degree = centrality_degree()) %>% 
  activate(edges) %>% 
  arrange(desc(weight))

edges <- edges %>% dplyr::rename(source = from) %>% dplyr::rename(target = to)

write_csv(nodes, "nodes.csv")
write_csv(edges, "edges.csv")

ggraph(graph_tidy, layout = 'graphopt') +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  geom_node_point(aes(color = type)) +
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "No. of publications",
       title = "Journal Publications",
       subtitle = "Authors corresponding journals.",
       caption = "Heidi Blackburn and Jason Heppler") +
  theme_graph() 
