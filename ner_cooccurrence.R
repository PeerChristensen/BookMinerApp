# NER co-occurrence

# co-occurence

library(tidyverse)
library(tidytext)
library(udpipe)
library(epubr)
library(spacyr)
library(igraph)
library(ggraph)


df <- read_csv("DaVinciCode.csv")

df <- df %>%
  unnest_tokens(output = sentences,
                input = text,token = "sentences",
                to_lower = F) %>%
  mutate(sentences = tm::removePunctuation(sentences))

anno <- spacy_parse(df$sentences) %>%
  mutate(row = row_number()) %>%
  mutate(is_chunk = if_else(row %% 500 == 0,1,0)) %>%
  mutate(doc_id = cumsum(is_chunk) + 1)

ents <- entity_extract(anno) %>%
  # GPE, NORP, EVENT, PERSON, ORG,     "LANGUAGE" ,    "WORK"    
  ##  [7] "PERSON"   "FAC"      "PRODUCT"  "LOC"      "LAW"
  filter(entity_type %in% c("GPE","FAC","NORP","PERSON")) %>%
  mutate(entity = str_replace_all(entity,"_"," ")) 

pairs <- ents %>%
  widyr::pairwise_count(entity, doc_id, sort = TRUE)

pairs %>%
  top_frac(.1) %>%
  filter(n >= 2)  %>%
  top_n(80,n) %>%
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = log(n), edge_width = log(n)), edge_colour = "red") +
  geom_node_point(size = 5,colour="snow") +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines"),colour = "snow") +
  theme_void() +
  theme(plot.background  = element_rect(fill="#272B30"),
        panel.background = element_rect(fill="#272B30",
                                        color = "#272B30", size = 0))

d3 <- pairs %>%
  top_frac(.1) %>%
  #filter(n > 1)  %>%
  graph_from_data_frame() %>% 
  igraph_to_networkD3()

ents2 <- ents %>%
  group_by(entity_type) %>%
  count(entity) %>%
  group_by(entity) %>%
  arrange(desc(n)) %>%
  top_n(1,n)

d3$nodes <- d3$nodes %>%
  left_join(ents2, by = c("name" = "entity"))

my_color <- 'd3.scaleOrdinal() .domain(["PERSON", "NORP","GPE", "FAC"]) .range(["#C41A24", "blue" , "green", ""])'

forceNetwork(Links = d3$links, Nodes = d3$nodes, 
               Source = 'source', Target = 'target', 
               NodeID = 'name', Group = 'entity_type',
               Value='value', Nodesize = 'n',fontSize=10,
               colourScale = my_color,zoom=T,
               linkColour = "snow",
             linkDistance = networkD3::JS("function(d) { return 10*d.value; }"))
