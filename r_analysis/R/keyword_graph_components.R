library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(janitor)
library(openxlsx)
library(tidygraph)
library(igraph)
library(ggplot2)
library(here)


keywords <- openxlsx::read.xlsx(
  here::here("data", "published-research-reports.xlsx"),
  sheet = "Keywords") %>% 
  janitor::clean_names()

extract_keywords <- function(string){
  vector = str_split(string, pattern = ", ")[[1]]
  return(vector)
}
  
keywords_enriched <- keywords %>% 
  mutate(individual_keywords = map(.x = key_words, 
                                   .f = extract_keywords)) %>% 
  tidyr::unnest(cols = c(individual_keywords)) %>% 
  mutate(keyword_pair = individual_keywords)


keywords_enriched %>% 
  group_by(individual_keywords, keyword_pair) %>% 
  summarise(papers = n_distinct(report_note_number))