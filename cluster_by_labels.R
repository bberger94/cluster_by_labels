

cluster_by_labels <- 
  function(data, 
           elements, 
           labels, 
           return_labels = TRUE
           ){
  
  library(dplyr)
  library(tidygraph)
  
  left_join <- function(...) suppressMessages(dplyr::left_join(...))
    
  elements <- enquo(elements)
  labels <- enquo(labels)
  
  # Keep unique rows of data
  data <- data %>% distinct(!! elements, !! labels)
  
  # Enumerate the elements (vertices)
  data <- distinct(data, !! elements) %>%
    mutate(vid = 1:n()) %>%
    left_join(data)

  # Select all pairs of vertices 
  vertices <- pull(distinct(data, vid))

  # Take the cartesian product of vertices for a list of all possible edges
  edges <- expand.grid(vertices, vertices) %>%
    rename(v1 = Var1, v2 = Var2) %>%
    filter(v1 >= v2)
  
  # Filter down to edges for elements/vertices that share a label
  edges <- edges %>%
    left_join(data %>% select(vid, !!labels) %>% rename(x_label = !!labels), by = c('v1' = 'vid')) %>%
    left_join(data %>% select(vid, !!labels) %>% rename(y_label = !!labels), by = c('v2' = 'vid')) %>%
    filter(x_label == y_label) %>%
    select(v1, v2) %>%
    distinct
  
  # Select connected components as our clusters
  clusters <- edges %>%
    as_tbl_graph(directed = FALSE) %>%
    activate(nodes) %>%
    mutate(cluster_id = group_components()) %>%
    as_tibble %>%
    transmute(vid = as.integer(name), cluster_id)

  # Merge labels into data
  data <- data %>%
    group_by(!! elements, vid) %>%
    summarize(labels = list(as.character(!! labels))) %>%
    left_join(clusters) %>%
    group_by(cluster_id) %>%
    mutate(cluster_labels = list(as.character(sort(unique(unlist(labels)))))) %>%
    select(!! elements, labels, cluster_id, cluster_labels) %>%
    ungroup
  
  # Renumber cluster ids for a more natural ordering
  ordered_ids <-
  select(data, cluster_id) %>%
    unique %>%
    mutate(new_id = 1:n())
  data <- data %>%
    left_join(ordered_ids, by = 'cluster_id') %>%
    select(-cluster_id) %>%
    select(!!elements, cluster_id = new_id, labels, cluster_labels)
 
  # Drop labels if requested
  if(return_labels == FALSE) data <- data %>% select(-labels, -cluster_labels)
  
  return(data)
}





test <- function(){
  
  # Test 1
  data <- data.frame(
    id = c(1,2,2),
    label = c(1,1,1)
  )
  cluster_by_labels(data, id, label)  %>% print
  
  # Test 2
  data <- data.frame(
    id = c(1,2,2),
    label = c(1,1,1)
  )
  cluster_by_labels(data, id, label, return_labels = F)  %>% print
  
  # Test 3
  n = 30
  data <- data.frame(
    id = sample(letters, size = n, replace = T),
    label = sample(LETTERS, size = n, replace = T)
    )
  cluster_by_labels(data, id, label) %>% print
  
  # Test 4
  n = 100
  data <- data_frame(
    fryD = sample(letters, size = n, replace = T),
    shlabel = sample(LETTERS, size = n, replace = T)
  )
  cluster_by_labels(data, fryD, shlabel) %>% print
  
  # Test 5
  n = 10
  data <- data_frame(
    date = as.Date(sample(0:9, n, replace = T), origin = '2010-01-01'),
    label = sample(c(LETTERS, letters), size = n, replace = T)
  )
  cluster_by_labels(data, date, label) %>% print
  
}

test() 
