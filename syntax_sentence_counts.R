

library(tidyverse)
#library(tidytext)
#library(entity)
library(epubr)
library(spacyr)

df <- epub("DaVinciCode.epub")
df <- df$data[[1]]

spacy_initialize(model="en_core_web_lg")

# dependency
anno <- spacy_parse(df$text,dependency = T)

anno <- anno %>%
  mutate(u_sent = paste(str_trunc(doc_id,10,"left"),sentence_id)) %>%
  select(u_sent, pos)
  
anno %>%
  filter(! pos %in% c("SPACE","PUNCT")) %>%
  group_by(u_sent) %>%
  summarise(struct = paste(pos,collapse='-')) %>%
  group_by(struct) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n>10,str_detect(struct,"VERB")) %>%
  ggplot(aes(reorder(struct,n),n)) +
  geom_col() +
  coord_flip()
