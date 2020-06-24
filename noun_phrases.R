# noun phrases

library(epubr)
library(tidyverse)
library(spacyr)

spacy_finalize()

df <- epub("DaVinciCode.epub")
df <- epub("kvinden_i_buret.epub")

spacy_finalize()

spacy_initialize(model="en_core_web_lg")
spacy_initialize(model="da_core_news_lg")

df <- df$data[[1]]

np <- spacy_extract_nounphrases(df$text)

np %>%
  filter(length>1) %>%
  group_by(text) %>%
  count() %>%
  arrange(desc(n))
