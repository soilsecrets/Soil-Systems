# https://www.r-bloggers.com/2020/10/sequence-mining-my-browsing-history-with-arulessequences/


setwd("B:/modelNC")


traverses <- read.csv("B:/Classified/df_catena_comp_percent_w_elevations.csv")
head(oh_traverses)

sixteentraverses <- traverses[traverses$Class==16,]

sixteentraverses <- sixteentraverses[order(sixteentraverses$Catena_ID, sixteentraverses$Component),]


oh_traverses <- sixteentraverses[,-c(1:6)]
oh_traverses <- oh_traverses > 0

elevation_oh_traverses <- cbind(traverses[,c(1:6)],oh_traverses)

sub_oh_traverses <- oh_traverses

library(arulesSequences)
library(dplyr)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(ggtext)
library(forcats)
library(tidyr)
library(stringr)

transaction_traverses <-  as(sub_oh_traverses, "transactions")

head(inspect(transaction_traverses))
transactionInfo(transaction_traverses)$sequenceID <- sixteentraverses$Catena_ID
transactionInfo(transaction_traverses)$eventID <- sixteentraverses$Component
head(inspect(transaction_traverses))


itemsets <- cspade(transaction_traverses, 
                   parameter = list(maxsize=1, maxlen=2, support = 0.001), 
                   control = list(verbose = FALSE))

inspect(itemsets)

#Convert Back to DS
itemsets_df <- as(itemsets, "data.frame") %>% as_tibble()

#Top 10 Frequent Item Sets
itemsets_df %>%
  slice_max(support, n = 10) %>% 
  ggplot(aes(x = fct_reorder(sequence, support),
             y = support,
             fill = sequence)) + 
  geom_col() + 
  geom_label(aes(label = support %>% scales::percent()), hjust = 0.5) + 
  labs(x = "Site", y = "Support", title = "Most Frequently Visited Item Sets",
       caption = "**Support** is the percent of segments the contain the item set") + 
  scale_fill_discrete(guide = F) +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, .1))) + 
  coord_flip() + 
  cowplot::theme_cowplot() + 
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    plot.title.position = 'plot'
  )
    

rules <- ruleInduction(itemsets, 
                       confidence = 0.01, 
                       control = list(verbose = FALSE))
inspect(head(rules, 200))                 

rules_cleaned <- rules[!is.redundant(rules)]


inspect(rules_cleaned)      

rules_df <- as(rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)

rules_df %>% 
  arrange(-confidence) %>% 
  select(lhs, rhs, support, confidence, lift) %>% 
  head() %>% 
  knitr::kable()


rules_df %>% 
  #Remove All Rules that Rock.outcrop
  filter(!str_detect(rule, '\\{Rock.outcrop\\/\\}')) %>% 
  #Keep only Rule, Confidence, and Lift - 1
  transmute(rule, confidence, lift = lift - 1) %>% 
  #Pivot Lift and confidence into a single column
  pivot_longer(cols = c('confidence','lift'),
               names_to = "metric", 
               values_to = "value") %>% 
  group_by(metric) %>% 
  #Keep only the Top 10 Rules for Each Metric
  top_n(10, value) %>% 
  ungroup() %>% 
  # Reorder so that order is independent for each metrics
  ggplot(aes(x = tidytext::reorder_within(rule, value, metric),
             y = value,
             fill = rule)) + 
  geom_col() + 
  geom_label(aes(label = value %>% scales::percent()), 
             hjust = 0) +
  scale_fill_discrete(guide = F) + 
  tidytext::scale_x_reordered() + 
  scale_y_continuous(label = scales::percent, 
                     limits = c(0, 1),
                     expand = expansion(mult = c(0, .1))) + 
  labs(x = "Rule", 
       y = "", 
       title = "Top Rules by Confidence and Lift",
       caption = "**Confidence** is the probability RHS occurs 
         given LHS occurs <br>
         **Lift** is the increased liklihood of seeing LHS & RHS together vs. independent") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )


collapsed_history_graph_dt <- elevation_oh_traverses %>% 
  group_by(Catena_ID) %>% 
  #transmute(Component, source = base_url) %>% 
  mutate(destination = lead(source)) %>% 
  ungroup() %>%
  filter(!is.na(destination)) %>% 
  select(source, destination, segment_id) %>% 
  count(source, destination, name = 'instances') 


elevation_oh_traverses_graph_dt <- elevation_oh_traverses %>% 
  group_by(Component) %>% 
  mutate(destination = lead(source)) %>% 
  ungroup() %>%
  filter(!is.na(destination)) %>% 
  select(source, destination, Component) %>% 
  count(source, destination, name = 'instances') 
