

cluster_by_labels <- function(data, elements, labels){
  
  library(tidygraph)
  
  data <- data %>% distinct_(elements, labels)
  data <- distinct_(data, elements) %>% 
    mutate(vid = 1:n()) %>% 
    left_join(data)
  
  vertices <- pull(distinct(data, vid))
  
  edges <- expand.grid(vertices, vertices) %>% 
    rename(v1 = Var1, v2 = Var2) %>% 
    filter(v1 >= v2)
  
  edges <- edges %>% 
    left_join(data %>% select(-id), by = c('v1' = 'vid')) %>% 
    left_join(data %>% select(-id), by = c('v2' = 'vid')) %>% 
    filter(label.x == label.y) %>% 
    select(v1, v2) %>% 
    distinct
 
  clusters <- edges %>% 
    as_tbl_graph(directed = FALSE) %>% 
    activate(nodes) %>% 
    mutate(cluster_id = group_components()) %>% 
    as_tibble %>% 
    transmute(vid = as.integer(name), cluster_id)
  
  data <- data %>%
    group_by(id, vid) %>%
    summarize(labels = list(as.character(label))) %>%
    left_join(clusters) %>%
    group_by(cluster_id) %>% 
    mutate(cluster_labels = list(as.character(sort(unique(unlist(labels)))))) %>%
    select(id, labels, cluster_id, cluster_labels) %>% 
    ungroup 
  
  ordered_ids <- 
  select(data, cluster_id) %>% 
    unique %>% 
    mutate(new_id = 1:n())
  
  data %>% 
    left_join(ordered_ids, by = 'cluster_id') %>% 
    select(-cluster_id) %>% 
    select(id, cluster_id = new_id, labels, cluster_labels)
  
}

test <- function(){
  n = 100
  data <- data.frame(
    id = sample(letters, size = n, replace = T),
    label = sample(LETTERS, size = n, replace = T)
    )
  cluster_by_labels(data, 'id', 'label')  
}

test() %>% View
