# Network analysis ------------------------------------------------------------

# We want to look at two networks:
# 1. Where do people publish most often (bimodal)
# 2. Who publishes with each other

library(tidygraph)
library(ggraph)
library(network)
library(igraph)

# Make a copy of master data file since we're altering data
network_data <- data
network_data$id <- 1:nrow(network_data)

coauthors <- network_data %>%
  select(author, title, pubtitle, date, id) %>% 
  unnest(author = str_split(author, ";"))

coauthors$author <- str_trim(coauthors$author)

coauthors <- coauthors %>% 
  left_join(gender_predicted %>% select(author, gender), by = "author")

coauthors <- coauthors %>% 
  left_join(data %>% select(title, publisher), by = "title")

edges <- coauthors %>% 
  select(author, id) %>% 
  dplyr::rename(to = id)

nodes <- coauthors %>% 
  distinct(author) %>% 
  rowid_to_column('id')

nodes <- nodes %>% 
  left_join(coauthors, by = "author")

edges <- edges %>% 
  left_join(nodes, by = "author") %>% 
  select(to, id.x)
edges <- edges %>% dplyr::rename(from = id.x)

nodes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  
graph_tidy <- nodes_tidy %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(),
         group = group_infomap())

network_elsevier <- graph_tidy %>% filter(publisher == "Elsevier Inc.")
network_sage <- graph_tidy %>% filter(publisher == "Sage Publications, Inc.")
network_springer <- graph_tidy %>% filter(publisher == "Springer New York LLC")

network_graphic <- ggraph(graph_tidy) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = gender), alpha = 0.8) +
  labs(title = "Co-authorship Network",
       subtitle = "Nodes are colored by gender",
       caption = "CC-BY | Heidi Blackburn and Jason Heppler") +
  scale_color_brewer(palette = "Set1", na.value = "#D5DCF5") +
  theme_graph()
ggsave(filename = "analysis/figures/network_all.jpg", plot = network_graphic, dpi = 300)

network_graphic_filtered <- ggraph(graph_tidy %>% activate(nodes) %>% filter(!node_is_isolated())) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = gender), alpha = 0.8) +
  labs(title = "Co-authorship Network",
       subtitle = "Nodes are colored by gender",
       caption = "CC-BY | Heidi Blackburn and Jason Heppler") +
  scale_color_brewer(palette = "Set1", na.value = "#F5F5F5") +
  theme_graph()
ggsave(filename = "analysis/figures/network_filtered.jpg", plot = network_graphic_filtered, dpi = 300)

elsevier_graphic <- ggraph(network_elsevier) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = gender), alpha = 0.8) +
  labs(title = "Co-authorship Network - Elsevier",
       subtitle = "Nodes are colored by gender",
       caption = "CC-BY | Heidi Blackburn and Jason Heppler") +
  scale_color_brewer(palette = "Set1", na.value = "#D5DCF5") +
  theme_graph()
ggsave(filename = "analysis/figures/network_elsevier.jpg", plot = elsevier_graphic, dpi = 300)

sage_graphic <- ggraph(network_sage) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = gender), alpha = 0.8) +
  labs(title = "Co-authorship Network - Sage",
       subtitle = "Nodes are colored by gender",
       caption = "CC-BY | Heidi Blackburn and Jason Heppler") +
  scale_color_brewer(palette = "Set1", na.value = "#D5DCF5") +
  theme_graph()
ggsave(filename = "analysis/figures/network_sage.jpg", plot = sage_graphic, dpi = 300)

springer_graphic <- ggraph(network_springer) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = gender), alpha = 0.8) +
  labs(title = "Co-authorship Network - Springer",
       subtitle = "Nodes are colored by gender",
       caption = "CC-BY | Heidi Blackburn and Jason Heppler") +
  scale_color_brewer(palette = "Set1", na.value = "#D5DCF5") +
  theme_graph()
ggsave(filename = "analysis/figures/network_springer.jpg", plot = springer_graphic, dpi = 300)

# Write final network data for use in Gephi, etc.
write_csv(nodes, "analysis/data/derived_data/nodes.csv")
write_csv(edges, "analysis/data/derived_data/edges.csv")
