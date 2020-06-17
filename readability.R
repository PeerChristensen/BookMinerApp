# readability

library(tidyverse)
library(quanteda)

df <- read_csv("DaVinciCode.csv")

# single row
df <- tibble(text = paste(df$text,collapse = ","))

dfCorpus <- quanteda::corpus(df,  text_field = "text")

readability <- quanteda::textstat_readability(dfCorpus,
                     measure = c("Flesch")) %>%
  as_tibble() %>%
  select(Flesch) %>%
  mutate(level = case_when(Flesch > 90 ~ "very easy",
                           Flesch > 80 ~ "easy",
                           Flesch > 70 ~ "fairly easy",
                           Flesch > 60 ~ "medium difficulty",
                           Flesch > 50 ~ "fairly difficult",
                           Flesch > 30 ~ "difficult",
                           Flesch > 0  ~ "very difficult"),
         row="a")

readability %>%
  ggplot(aes(row,max)) +
  #geom_col(width=.3,fill="snow",colour="grey",size=1.5) +
  geom_col(width=.4, fill = "#1a9850",aes(row,100),colour="lightgrey") +
  geom_col(width=.4, fill = "#a6d96a",aes(row,90),colour="lightgrey") +
  geom_col(width=.4, fill = "#d9ef8b",aes(row,80),colour="lightgrey") +
  geom_col(width=.4, fill = "#fee08b",aes(row,70),colour="lightgrey") +
  geom_col(width=.4, fill = "#fdae61",aes(row,60),colour="lightgrey") +
  geom_col(width=.4, fill = "#f46d43",aes(row,50),colour="lightgrey") +
  geom_col(width=.4, fill = "#d73027",aes(row,30),colour="lightgrey") +
  geom_hline(yintercept = readability$Flesch, size = 9,colour="lightgrey") +
  geom_hline(yintercept = readability$Flesch, size = 7,colour="snow") +
  ggplot2::annotate("text",size=9,colour="snow", family = "Roboto Condensed",
                    x=0.6,y=readability$Flesch + 8, label = glue::glue("{readability$level}")) +
  coord_flip() +
  theme(plot.margin = margin(12, .1, 12, .1, "cm"),
        plot.background = element_rect(fill= "#272B30"),
        panel.background = element_rect(fill="#272B30"),
        panel.grid = element_blank(),
        axis.title = element_blank())

# lexical diversity
df_dfm <- dfm(dfCorpus)

textstat_lexdiv(df_dfm,measure = c("MTLD"))

df <- tibble(val = 80,max = 100,row="a")
