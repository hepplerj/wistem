# Data visualizations

# Plot the number of publications per year
ggplot(data, aes(x = date)) +
  geom_bar() +
  theme_minimal()

# Plot the number of articles per publication
ggplot(data, aes(x = pubtitle)) +
  geom_bar() +
  theme_minimal()

# Scatterplot of name and position
# Not a particularly useful view into the data.
ggplot(author_order, aes(x = author, y = position)) +
  geom_point(aes(color=gender))

# Faceted barchart of gender by publication
# Not a particularly useful view into the data.
ggplot(author_order, aes(position, fill = gender)) +
  geom_bar() +
  facet_grid(pubtitle ~ .)

# Authorship by position
# Useful chart.
ggplot(author_order, aes(position)) +
  geom_bar() +
  facet_grid(. ~ gender) +
  labs(title = "Authorship position by gender") +
  theme_jah()

# Stacked barchart of gender by year
# Useful chart.
ggplot(author_order, aes(x = date, fill = gender)) +
  geom_bar() +
  theme_minimal()

# Gender by journal vs. conference
# Useful chart.
ggplot(conf_journal_merged, aes(x = date, fill = gender)) +
  geom_bar() +
  facet_grid(~ pubtype) +
  theme_light()

# Counting the number of authors by position
author_order %>%
  count(position) %>%
  ggplot(aes(x = position, y = n)) + geom_bar(stat = "identity") +
  theme_minimal()

# Network analysis ------------------------------------
# We want to look at two networks:
# 1. Where do people publish most often (bimodal)
# 2. Who publishes with each other

library(tidygraph)
library(ggraph)
library(network)

# Separate out data to a 'source'
sources <- gender_predicted %>%
  distinct(author, gender) %>%
  dplyr::rename(label = author) %>%
  mutate(type = "author")

# Separate out data to a 'target'
targets <- gender_predicted %>%
  distinct(pubtitle) %>%
  dplyr::rename(label = pubtitle) %>%
  mutate(type = "publication")

# Join source and target together and add a unique ID
nodes <- full_join(sources, targets, by = "label")
nodes <- nodes %>% rowid_to_column('id')
nodes[is.na(nodes)] = ''
nodes <- nodes %>% unite(type, c(type.x, type.y), remove = FALSE, sep='')
nodes <- nodes %>% select(id, label, gender, type)

# Generate a nodes (author) set
nodes_gender <- nodes %>% filter(type == "author")

# Generate a nodes (publication) set
publications <- gender_predicted %>%
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
write_csv(nodes, "analysis/data/derived_data/nodes.csv")
write_csv(edges, "analysis/data/derived_data/edges.csv")

rm(edges)
rm(nodes)
rm(gender_nodes_tidy)
rm(nodes_tidy)
rm(network)
rm(edges_gender)
rm(sources)
rm(targets)
rm(network_gender)
rm(nodes_gender)

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
