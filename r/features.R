get_kruskal_pvals <- function(dataset) {
  task <- mlr::makeClassifTask(id = 'test', data = dataset, target = 'class')
  i <- generateFilterValuesData(task, method = 'kruskal.pval')
  pval <- -i$data$kruskal.pval
  names(pval) <- i$data$name
  pval <- sort(pval, na.last = TRUE) 
  data.frame(variable = names(pval), pvalue = unname(pval)) 
}
get_kruskal_selected <- function(dataset) {
  pvals <- get_kruskal_pvals(dataset)  
  pvals <- subset(pvals, pvalue < 0.05)
  as.character(pvals$variable)
}
get_kruskal_db <- function(db) {
  cols <- get_kruskal_selected(db) 
  db[ , c(cols, 'class'), drop = FALSE]
} 
#' 1st kruskal, then rf ranking 
get_vi <- function(class, dataset, ntree = 20000, type = 'harmonic-mean') { 
  set.seed(0, "L'Ecuyer")
  class <- class_groups[[class]]
  db <- convert2binary(class, dataset)    
  # db <- get_kruskal_db(db)
  vis <- rforest_ranking(ntree, db, class = type) 
  dplyr::arrange(data.frame(variable=rownames(vis), importance=vis[,1]), desc(importance))
} 
#' Gets the rforest ranking of VIs 
rforest_ranking <- function(ntree, dataset, class) { 
  cart <- mods[['RF']]
  cart <- setHyperPars(cart, importance = TRUE)
  cart <- setHyperPars(cart, ntree = ntree)
  n <- ncol(dataset) - 1
  mtry <- floor(sqrt(n)) 
  # 2 * sqrt(n)
  # mtry <- min(round(3 * sqrt(n)), round( .95 * n))
  # mtry <- min(round(3 * sqrt(n)), round( .95 * n))
  # mtry <- n
  cart <- setHyperPars(cart, mtry = mtry)  
  # cart <- setHyperPars(cart, sampsize = round(nrow(basket) / 2))
  # cart <- setHyperPars(cart, replace = FALSE)
  task <- mlr::makeClassifTask(id = 'test', data = dataset, target = 'class')
  ct <- train(cart, task)
  model <- ct$learner.model 
  a <- randomForest::importance(model, scale = FALSE) 
  # inds <- which(colnames(a) %in% c('NBC', 'MC', 'BA')) 
  # stopifnot(length(inds) > 0)
  # a[, 'MeanDecreaseAccuracy'] <- a[ , inds]
  # if (!is.null(class) && class == 'harmonic_mean') {
  x <- a[, 1]
  y <- a[, 2]
  # means <- 2 * x * y / (x + y)
  # means <- mapply(mean, x, y)
  means <- (x + y)  / 2
  a <- data.frame( MeanDecreaseAccuracy=unname(means))
  rownames(a) <- names(means)
  #   # geometric mean 
  #   means <- sqrt(exp(log(x) + log(y)))
  #   a[, 'MeanDecreaseAccuracy'] <- means 
  #   class <- NULL
  #   # (log of harmonic mean) of a 2-element vector 
  #   # lx <- log(x)
  #   # y1 <- log(length(x)) - logSumExp(-lx)
  #   # print(y1) ## [1] -1.600885        
  # }
  # column <- ifelse(is.null(class), 'MeanDecreaseAccuracy', class)
  column <- 'MeanDecreaseAccuracy'
  a <- a[order(a[,column], decreasing = TRUE), column, drop = FALSE]
  a
} 
get_lasso_coefs_mod <- function(dataset, lambda) {
  dataset <- standardize(dataset)
  mod <- learn_model(dataset, model, classifiers)
  get_lasso_coefs(mod, lambda)
}
#' Get a glmnet models 
# get_model <- function(dataset) { 
#   lambdas <- exp(seq(-1, -20, by = -0.03))
#   glmnet(as.matrix(dataset[, -ncol(dataset)]), dataset$class, family = 'binomial', lambda = lambdas) 
# }
get_lars_lasso <- function(db) {
 y <-  as.numeric(db$class == 'BA')
 lars(as.matrix(db[, -ncol(db)]), y, type = 'lasso')
}
get_lars_lasso_betas <- function(model) { 
 coefs <- coef(model) 
 # mid <- trunc(3 * nrow(coefs) / 4)
 mid <- nrow(coefs) - 10
 coefs <- coefs[mid, ]
 coefs <- coefs[abs(coefs) > 0] 
 coefs 
}
#' Return the betas for this model
get_betas <- function(g, s = exp(-18)) {
    default_mlr <- coef(g, s = s)
    default_mlr <- as.matrix(default_mlr)
    inds <- abs(default_mlr) > 0
    default_mlr <- default_mlr[inds, ]
    lasso_vars <- data.frame(variable = as.character(names(default_mlr )), beta = default_mlr)
    # Remove intercept
    lasso_vars <- lasso_vars[-1, ]
    lasso_vars <- dplyr::arrange(lasso_vars, desc(abs(beta)))
    lasso_vars$variable <- as.character(lasso_vars$variable) 
    lasso_vars
}
#' Get bolasso intersection feature subset for a number of repetitions 
bolasso <- function(db, reps = 8, size = 0.01) { 
  dbcs <-  lapply(1:reps, function(i) bnclassify:::bootstrap_ss(db, 1)) 
  lasso <- lapply(dbcs, function(db) get_mlr_model(mods[['RMLR']], db))
  lasso <-  lapply(lasso, get_betas, size) 
  # lasso <- lapply(dbcs, get_lasso_coefs_mod, 'RMLR', mods, 7.280603e-05) 
  # lasso <- lapply(dbcs, get_model)  
  # lasso <-  lapply(lasso, get_betas)  
  laske <- Reduce(function(x, y) dplyr::inner_join(x, y, by = "variable"), lasso)
  # laske <- Reduce(function(x, y) dplyr::full_join(x, y, by = "variable"), lasso)
  # laske[is.na(laske)] <- 0 
  combine_lasso(laske)  
}
combine_lasso <- function(lasso) {
  lasso <- na.omit(lasso)
  lasso <- data.frame(var = lasso[, 1], beta = rowMeans(lasso[ , -1]), 
                      abs_beta_sd = matrixStats::rowSds(abs(as.matrix(lasso[ , -1]))))
  dplyr::arrange(lasso, desc(abs(beta)))
}
#' Returns all KW RMLR betas for the dataset
get_kruskal_betas <- function(dataset) { 
  dataset <- get_kruskal_db(dataset)
  dataset <- standardize(dataset)         
  # lasso <- get_model(dataset)
  lasso <- get_mlr_model(mods[['RMLR']], dataset)
  get_betas(lasso, s = 0.01)  
}
do_pca <- function(db) {
  pcadb <- db
  pcadb$class <- NULL
  prcomp(pcadb, center = TRUE, scale = TRUE)  
} 
get_neurostr_features <- function(features) {
  neurostr_funs <- c("centrifugal_order", "height", "length", "N_bifurcations", "euclidean_dist", "path_dist", "N_stems", "partition_asymmetry", 
                     "remote_bifurcation_angle", "remote_tilt_angle", "remote_torque_angle", "terminal_degree", 
                     "tortuosity", "total_length", "vertex_ratio", "vertex_type", "width", "tree_length")                        
  neurostr_funs <- paste0('^', neurostr_funs)
  inds <- unlist(sapply(neurostr_funs, grep, features))
  inds <- unique(inds)
  features[inds]
}