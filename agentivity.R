
# NER
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
  filter(token %in% c("Robert","Sophie","Langdon"))

anno %>%
  group_by(token,dep_rel) %>%
  count()

library(udpipe)
mod <- udpipe::udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

anno2 <- udpipe(df$text,mod)

anno2 <- anno2 %>%
  filter(token %in% c("Robert","Sophie","Langdon"))

anno2 %>%
  group_by(token,dep_rel) %>%
  count() %>%
  ungroup() %>%
  #top_n(10,n) %>%
  arrange(desc(n)) %>%
  pivot_wider(names_from=dep_rel,values_from=n) %>%
  #select(-conj,-root,-appos,-compound,-nmod,-case,-flat, -`nmod:poss`,-`nsubj:pass`) %>%
  replace(is.na(.),0) %>%
  mutate(agentivity = nsubj / (nsubj + obl + obj + iobj)) %>%
  select(token, agentivity)

#####################################################################
# verbs associated with characters

anno <- spacy_parse(df$text)

top_verbs <- anno %>%
  mutate(lead = lead(pos), 
         term = lead(lemma)) %>%
  filter(pos == "PROPN", lead == "VERB",
         str_detect(entity,"PERSON"),
         token %in% c("Langdon","Sophie","Fache","Silas")) %>%
  count(token,term) %>%
  group_by(token) %>%
  arrange(desc(n)) %>%
  top_n(10,n) %>%
  ungroup() %>%
  arrange(token, n) %>%
  # 3. Add order column of row numbers
  mutate(order = row_number())

top_verbs %>%
  ggplot(aes(order,n)) +
  geom_col() +
  facet_wrap(~token,scales="free") +
  scale_x_continuous(
    breaks = top_verbs$order,
    labels = top_verbs$term,
    expand = c(0,0)) +
  coord_flip()
