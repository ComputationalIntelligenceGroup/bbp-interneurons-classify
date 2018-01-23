 #' Make a hybrid data set.
#' currently ignoring the parameters
get_hybrid_db <- function(.task, usw.rate, sw.rate) { 
  rm(usw.rate)
  rm(sw.rate)
  usw.rate <- max(0.5, min(3 / get_times_less(.task$env$data, 'class'), 1))  
  .task = undersample(.task, rate = usw.rate)   
  sw.rate <- min(get_times_less(.task$env$data), 3)
  smote(.task, rate = sw.rate)
}
get_times_less <- function(data, target) { 
  tbl <- table(data[['class']]) 
  max(tbl) / min(tbl)
} 