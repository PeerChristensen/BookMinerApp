# Preprocess epub

library(epubr)
library(tidytext)
library(tidyverse)

#file <- system.file("dracula.epub", package = "epubr")
#(x <- epub(file))

file <- "DaVinciCode.epub"

df <- epub(file)

# Meta data

#Title
df$title

#Author
df$creator

#ISBN (join theme & more)
df$identifier

# Get text
text <- df$data[[1]] %>%
  select(section,text)

write_csv(text,"DaVinciCode.csv")


