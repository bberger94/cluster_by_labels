library(testthat)
library(dumbcluster)

test_check("dumbcluster")



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
