

library(epubr)
library(tidyverse)
library(spacyr)
library(stm)

df <- epub("DaVinciCode.epub")

df <- df$data[[1]]

anno <- spacy_parse(df$text)

anno <- anno %>%
  filter(pos == "NOUN")


d <- anno %>%
  select(doc_id,lemma)

# ---------------------------------
# BUILD STM MODELS

df_sparse <- d %>%
  count(doc_id, lemma) %>%
  filter(n>5) %>%
  cast_sparse(doc_id, lemma, n)



n_topics = seq(2,12,2)

models <- tibble(K = n_topics) %>%
  mutate(topic_model = map(K, ~stm(df_sparse, K = ., verbose = T)))


heldout <- make.heldout(df_sparse)

k_result <- models %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, df_sparse),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, df_sparse),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics")

ggsave("plots/sotu_stm_eval.png")

# ---------------------------------
# EXCLUSIVITY 

#BOXPLOT

k_result$exclusivity                   %>%
  unlist()                             %>% 
  enframe()                            %>% 
  mutate(K=rep(k_result$K,k_result$K)) %>%
  group_by(K)                          %>%
  ggplot(aes(x = factor(K), y = value)) +
  geom_boxplot(fill=NA,colour = "darkorange") +
  geom_jitter(width=.2, alpha = .7,colour = "steelblue") +
  labs(x = "Number of topics",
       y = "Exclusivity",
       title = "Exclusivity by number of topics")

ggsave("plots/sotu_stm_exclusivity_boxplot.png")

# LINE PLOT

k_result$exclusivity %>%
  unlist() %>% 
  enframe() %>% 
  mutate(K=rep(k_result$K,k_result$K)) %>%
  group_by(K) %>%
  summarise(m = mean(value)) %>%
  ggplot(aes(x = K, y = m)) +
  geom_line() +
  labs(x = "Number of topics",
       y = "Exclusivity",
       title = "Exclusivity by number of topics")

ggsave("plots/sotu_stm_exclusivity_lineplot.png")

# ---------------------------------
# COMPARE EXCLUSIVITY AND SEMANTIC COHERENCE

k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  filter(K %in% seq(4,10,2)) %>%
  unnest()                                         %>%
  mutate(K = as.factor(K))                         %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity") +
  scale_color_viridis_d()

ggsave("plots/sotu_stm_excl_sem_plot.png")

# ---------------------------------
# ANIMATE EXCLUSIVITY AND SEM. COHERENCE

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = 10, fps = 0.5)

# ---------------------------------
# SELECT STM MODELS

topic_model_stm_small <- k_result %>% 
  filter(K ==4)             %>% 
  pull(topic_model)         %>% 
  .[[1]]


topic_model_stm_big <- k_result %>% 
  filter(K == 8)             %>% 
  pull(topic_model)         %>% 
  .[[1]]



# ---------------------------------
# EXPLORE STM MODELS

# small model
td_beta_small <- tidy(topic_model_stm_small)

top_terms <- td_beta_small  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma_small <- tidy(topic_model_stm_small, matrix = "gamma",
                       document_names = rownames(df_sparse))

gamma_terms_small <- td_gamma_small  %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot_small <- gamma_terms_small %>%
  top_n(10, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,size=0) +
  geom_text(hjust = -.05, vjust=0, size = 5, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(gamma_terms_small$gamma)+.3),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot_small

# big model

td_beta_big <- tidy(topic_model_stm_big)

top_terms <- td_beta_big  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma_big <- tidy(topic_model_stm_big, matrix = "gamma",
                     document_names = rownames(df_sparse))

gamma_terms_big <- td_gamma_big  %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot_big <- gamma_terms_big %>%
  top_n(10, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,size=0) +
  geom_text(hjust = -.05, vjust=0, size = 5, family = "Lato") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(gamma_terms_big$gamma)+.3),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot_big

