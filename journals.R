# Analyzing women in STEM publications
#
# Heidi Blackburn and Jason Heppler
# UNO Libraries, University of Nebraska at Omaha
# 2018-12-03

library(tidyverse)

#data <- readxl::read_xlsx("data-raw/WiSTEM.xlsx")
data <- readxl::read_xlsx("~/Downloads/WiSTEM Data9.xlsx")

#names(data)
data <- data %>% 
  rename(author = "Author") %>% 
  rename(title = "Title") %>% 
  rename(pubtitle = "Publication Title") %>% 
  rename(publisher = "Publisher") %>% 
  rename(date = "Date") 

data_name_order <- data %>% select(author)
data_name_order <- data_name_order %>% 
  separate(col = author, into = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15", "16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35"), sep = ";")

data_name_order <- data_name_order %>% 
  gather(key = position, value = name)
data_name_order_final <- data_name_order %>% drop_na()
data_name_order_final$position <- as.numeric(data_name_order_final$position)

# Counting the number of authors by position
data_name_order_final %>% 
  count(position) %>% 
  ggplot(aes(x = position, y = n)) + geom_bar(stat = "identity")


# Viz: number of articles per author by year


# Viz: number of articles per journal by year


# Separate out authors into their own rows
data_clean <- separate_rows(data, author, sep = ";")

# The below code is designed to predict gender
data_predict <- data_clean #%>% drop_na()
data_predict$fname <- str_trim(data_predict$fname)

data_predict <- separate(data_clean, c("lastn", "firstn"), col = "author", sep = ",")
data_predict$fname <- gsub("(.*)\\s+[A-Z]\\.?$", "\\1", data_predict$firstn)
data_predict <- data_predict %>% 
  select(lastn, fname, pubtitle, date)
data_predict <- data_predict %>% mutate(tmpyear = 2012)

# Predict gender of author
library(gender)
gender_predicted <- data_predict %>%
  distinct(fname, tmpyear) %>% 
  group_by(tmpyear) %>% 
  do(results = gender(.$fname, method = "ssa")) %>% 
  do(bind_rows(.$results))

gender_predicted <- gender_df(data_predict, name_col = "fname", year_col = "year", method = "ssa")

# Because the gender package can't handle all names, lets match names in the 
# `names` dataset to create a new DF, predict those names, and recombine into
# a new dataframe with completed and missing data. 
library(genderdata)


#write_csv(data_clean, "~/Desktop/data.csv")

# Minor data analytics ---------------------------------------
data_clean %>% 
  group_by(author) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

data_clean %>% 
  group_by(pubtitle) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

data_clean %>% 
  group_by(date) %>% 
  tally %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data_clean, aes(x = date)) +
  geom_bar()

ggplot(data_clean, aes(x = publisher)) +
  geom_bar()

# Preparing network analysis ------------------------------------
sources <- data_clean %>% 
  distinct(author) %>% 
  rename(label = author) %>% 
  mutate(type = "author")

destinations <- data_clean %>% 
  distinct(pubtitle) %>% 
  rename(label = pubtitle) %>% 
  mutate(type = "publication")

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column('id')
nodes <- nodes %>% unite(type, c(type.x, type.y), remove = FALSE)
nodes <- nodes %>% select(id, label, type)

publications <- data_clean %>% 
  group_by(author, pubtitle) %>% 
  summarise(weight = n()) %>% 
  ungroup()

publications <- publications %>% drop_na()

edges <- publications %>% 
  left_join(nodes, by = c("author" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("pubtitle" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

library(network)
network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
graph_tidy <- routes_tidy %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  mutate(degree = centrality_degree()) %>% 
  activate(edges) %>% 
  arrange(desc(weight))

edges <- edges %>% rename(source = from) %>% rename(target = to)

write_csv(nodes, "nodes.csv")
write_csv(edges, "edges.csv")

library(gephi)
library(igraph)
gephi_write_edges(routes_tidy, 'edges.csv')
gephi_write_nodes(routes_tidy, 'nodes.csv')

ggraph(graph_tidy, layout = 'graphopt') +
  geom_edge_link(aes(width = weight), alpha = 0.8) +
  geom_node_point(aes(color = type)) +
  scale_edge_width(range = c(0.2, 2)) +
  labs(edge_width = "No. of publications",
       title = "Journal Publications",
       subtitle = "Authors corresponding journals.",
       caption = "Heidi Blackburn and Jason Heppler") +
  theme_graph() 
