# NER
library(tidyverse)
library(tidytext)
library(entity)

df <- read_csv("DaVinciCode.csv")

df <- df %>%
  unnest_tokens(output = sentences, input = text,token = "sentences",to_lower = F,
) %>%
  mutate(sentences = tm::removePunctuation(sentences))


# people
tic()
people <- person_entity(df$sentences) %>%
  unlist() %>%
  as_tibble() %>%
  count(value) %>% 
  arrange(desc(n)) %>% 
  top_n(20,n)
toc()

# locations
tic()
locations <- location_entity(df$text) %>%
  unlist() %>%
  as_tibble() %>%
  count(value) %>% 
  arrange(desc(n)) %>% 
  top_n(20,n)
toc()

# organizations
tic()
organizations <- organization_entity(df$text) %>%
  unlist() %>%
  as_tibble() %>%
  count(value) %>% 
  arrange(desc(n)) %>% 
  top_n(20,n)
toc()

# map

# spaCy
# Packages for manipulating data

library(stringr)
library(lubridate)
# Packages for NLP
library(NLP)
# install.packages("openNLPmodels.en",repos = "http://datacube.wu.ac.at/", type = "source")
library(openNLP)
library(cleanNLP)
#cnlp_download_corenlp() # install the coreNLP Java back end 'CoreNLP' <http://stanfordnlp.github.io/CoreNLP/>
# Packages for Python interface
# Packages for Python
library(reticulate)
use_virtualenv("r-reticulate")
use_python("/Users/peerchristensen/anaconda/bin/python")


# cnlp_init_spacy()
# 
# d <- paste(df$sentences,collapse = " ")
# anno <- cnlp_annotate(d)
# 
# 
# anno$entity %>%
#   filter(entity_type == "GPE") %>%
#   group_by(entity) %>%
#   count() %>%
#   arrange(desc(n)) 

tic()
anno2 <- spacy_parse(df$sentences)

ents <- entity_extract(anno2) %>%
  # GPE, NORP, EVENT, PERSON, ORG,     "LANGUAGE" ,    "WORK"    
  ##  [7] "PERSON"   "FAC"      "PRODUCT"  "LOC"      "LAW"
  filter(entity_type %in% c("GPE","FAC","NORP","PERSON"))

ents_plot <- ents %>% 
  group_by(entity_type) %>%
  count(entity) %>%
  top_n(10,n) %>%
  ungroup()     %>%
  arrange(entity_type, -n) %>%
  filter(n>=2) %>%
  mutate(order = rev(row_number()))
  
ents_plot %>%
  ggplot(aes(order,n)) +
  geom_col() +
  scale_x_continuous(
    breaks = ents_plot$order,
    labels = ents_plot$entity,
    expand = c(0,0)) +
  facet_wrap(~entity_type,scales="free") +
  coord_flip() 


