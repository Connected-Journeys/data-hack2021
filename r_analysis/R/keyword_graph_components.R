library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(widyr)
library(janitor)
library(openxlsx)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(here)

# read data
# expects excel file in data folder of project
keywords <- openxlsx::read.xlsx(
  here::here("data", "published-research-reports.xlsx"),
  sheet = "Keywords") %>% 
  janitor::clean_names()

# split out keywords
extract_keywords <- function(string){
  vector = str_split(string, pattern = ", ")[[1]]
  return(vector)
}
  

keywords_enriched <- keywords %>% 
  mutate(individual_keywords = map(.x = key_words, 
                                   .f = extract_keywords)) %>% 
  tidyr::unnest(cols = c(individual_keywords)) 

keyword_paper_links <- keywords_enriched %>% 
  select(report_note_number, individual_keywords, available_at)

# create keyword pairs
# quickest way to do this was using widyr package
# but could re-write
keywords_pairs <- keywords_enriched %>% 
  filter(!individual_keywords %in% c("New Zealand", "roads")) %>%
  widyr::pairwise_count(individual_keywords, report_note_number)


# create graph structure 
# filter appropriately..
# - i.e. keywords connected by only one paper
keywords_pairs_graph <- tidygraph::as_tbl_graph(
  keywords_pairs %>%
    filter(n > 3)) %>% 
  activate("nodes") %>% 
  mutate(community = as.factor(group_infomap()))
  
communities_papers <- keywords_pairs_graph %>% 
  activate("nodes") %>% 
  as_tibble() %>% 
  left_join(keyword_paper_links, by = c("name" = "individual_keywords"))


# simple viz
ggraph(keywords_pairs_graph, layout="fr") + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality_degree(), 
                      colour = community))


# export keyword nodes
keywords_pairs_graph %>% 
  activate("nodes") %>% 
  as_tibble() %>%
  write.csv(here::here("results", "keywords_nodes.csv"))

# export keyword edges
keywords_pairs_graph %>% 
  activate("edges") %>% 
  as_tibble() %>%
  write.csv(here::here("results", "keywords_edges.csv"))
  