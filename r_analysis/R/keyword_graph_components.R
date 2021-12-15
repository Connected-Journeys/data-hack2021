library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(janitor)
library(openxlsx)
library(tidygraph)
library(igraph)
library(ggraph)
library(networkD3)
library(ggplot2)
library(here)

# read data
theme_tidy <- function(theme){
    str_split(theme, pattern = " |,")[[1]][1]
}

keywords_enriched <- read.xlsx("../data/summary_lemmatization_v2.xlsx", 
                               sheet="Sheet1") %>% 
  clean_names() %>% 
  mutate(new_theme = map(theme, theme_tidy)) %>% 
  unnest(cols = c(new_theme))

# 3 keywords per paper
paper_keywords <- keywords_enriched %>% 
  filter(!new_label %in% c("New Zealand",
                                     "roads", 
                                     "transport", 
                                     "road")) %>%
  group_by(report_note_number) %>% 
  summarise(keyword_1 = new_label[1],
            keyword_2 = new_label[2],
            keyword_3 = new_label[3])

## paper graphs
papers_raw <- keywords_enriched %>% 
  select(report_note_number, new_label) %>% 
  rename(paper_1 = report_note_number)

# create data frame with 2 columns for paper ID
papers_df <- papers_raw %>% 
  inner_join(papers_raw %>% 
               rename(paper_2 = paper_1)) %>% 
  filter(paper_1 != paper_2)


# pairwise count of papers
# code to also remove reverse duplicates
# taken from https://stackoverflow.com/a/56193682
# joining theme 
papers_pairs <- papers_df %>% 
  filter(!new_label %in% c("New Zealand",
                                     "roads", 
                                     "transport", 
                                     "road")) %>%
  group_by(paper_1, paper_2) %>% 
  summarise(shared_keywords = n_distinct(new_label)) %>% 
  group_by(grp = paste(pmax(paper_1, paper_2), 
                       pmin(paper_1, paper_2),
                       sep = "_")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp) %>% 
  left_join(keywords_enriched %>% 
              select(report_note_number, new_theme) %>% 
              distinct(),
            by = c("paper_1" = "report_note_number")) %>% 
  rename(new_theme1 = new_theme)%>% 
  left_join(keywords_enriched %>% 
              select(report_note_number, new_theme) %>% 
              distinct(),
            by = c("paper_2" = "report_note_number")) %>% 
  rename(new_theme2 = new_theme)  


# create graph structure 
# filter appropriately..
papers_pairs_graph <- tidygraph::as_tbl_graph(
  papers_pairs %>%
    filter(shared_keywords >= 3) %>% 
    filter(new_theme1 == "Safety" | new_theme2 == "Safety")
  ) %>% 
  activate("nodes") %>% 
  mutate(community = as.factor(group_infomap())) %>% 
  left_join(keywords_enriched %>% select(report_note_number, 
                                published_date, 
                                researcher,
                                available_at,
                                new_theme,
                                research_report_title) %>% distinct(), 
            by = c("name" = "report_note_number")) %>% 
  left_join(paper_keywords, 
            by = c("name" = "report_note_number"))

# simple viz
ggraph(papers_pairs_graph, layout="fr") + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality_degree(), 
                      colour = community))

# interactive viz
nodes <- papers_pairs_graph %>%
  activate("nodes") %>%
  as.data.frame() %>% mutate(id = row_number() - 1)

edges <- papers_pairs_graph %>%
  activate("edges") %>%
  as.data.frame() %>% mutate(from = from -1 , to = to -1)


forceNetwork(Links = edges, Nodes = nodes,
             Source = "from", Target = "to",
             Value = "shared_keywords", NodeID = "research_report_title",
             Group = "community", opacity = 0.8, zoom=TRUE)  %>%
  saveNetwork(file = here::here("results", "safety_papers.html"))


# export papers nodes
papers_pairs_graph %>%
  activate("nodes") %>%
  as_tibble() %>%
  mutate(Id = row_number()) %>% 
  write.csv(here::here("results", "safety_papers_nodes.csv"), 
            row.names = FALSE)

# export papers edges
papers_pairs_graph %>%
  activate("edges") %>%
  as_tibble() %>%
  rename(Source = from,
         Target = to) %>%
  write.csv(here::here("results", "safety_papers_edges.csv"))


#########
## OLD ## 
#########
# keyword_paper_links <- keywords_enriched %>% 
#   select(report_note_number, new_label, available_at)
# 
# # create keyword pairs

# ## paper graphs
# keywords_raw <- keywords_enriched %>% 
#   select(report_note_number, new_label) %>% 
#   rename(keyword_1 = new_label)
# 
# # create data frame with 2 columns for paper ID
# keywords_df <- keywords_raw %>% 
#   inner_join(papers_raw %>% 
#                rename(keyword_2 = keyword_1)) %>% 
#   filter(keyword_1 != keyword_2)
# 
# # pairwise count of keywords
# # code to also remove reverse duplicates
# # taken from https://stackoverflow.com/a/56193682
# keywords_pairs <- keywords_df %>% 
#   filter(!new_label %in% c("New Zealand",
#                                      "roads", 
#                                      "transport", 
#                                      "road")) %>%
#   group_by(keyword_1, keyword_2) %>% 
#   summarise(n = n_distinct(report_note_number)) %>% 
#   group_by(grp = paste(pmax(keyword_1, keyword_2), 
#                        pmin(keyword_1, keyword_2),
#                        sep = "_")) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(-grp)
# 
# # create graph structure 
# # filter appropriately..
# # - i.e. keywords connected by only one paper
# keywords_pairs_graph <- tidygraph::as_tbl_graph(
#   keywords_pairs %>%
#     filter(n > 3)) %>% 
#   activate("nodes") %>% 
#   mutate(community = as.factor(group_infomap()))
# 
# # simple viz
# ggraph(keywords_pairs_graph, layout="fr") + 
#   geom_edge_link() + 
#   geom_node_point(aes(size = centrality_degree(), 
#                       colour = community))
# 
# 
# # export keyword nodes
# keywords_pairs_graph %>% 
#   activate("nodes") %>% 
#   as_tibble() %>%
#   write.csv(here::here("results", "keywords_nodes.csv"))
# 
# # export keyword edges
# keywords_pairs_graph %>% 
#   activate("edges") %>% 
#   as_tibble() %>%
#   write.csv(here::here("results", "keywords_edges.csv"))
# 
