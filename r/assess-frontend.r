get_rdesc <- function(class_group_name, dataset, task) { 
  # rdesc <- mlr::makeResampleDesc("LOO") # TODO
  folds <- 10
  if (class_group_name == 'chc') folds <- 7 
  class_freq <- table(dataset$class)
  min_count <-  class_freq[task$task.desc$positive]
  folds <- min(folds, min_count)
  class_freq <- class_freq[class_freq > 0]
  min_class <- min(class_freq)
  stratify <- min_class >=  folds
  
  rdesc <- mlr::makeResampleDesc("CV", iters = folds, stratify = stratify)
  rdesc 
}
evaluate_class_group <- function(class_group_name, class_groups, classifiers, dataset, reps, keep.pred = FALSE) { 
 dataset <- convert2binary(class_groups[[class_group_name]], dataset)               
 measures <- get_mlr_measures() 
 rdesc <- get_rdesc(class_group_name, dataset, taskify(dataset))
 basic <- lapply(seq_len(reps), function(i) assess2(dataset, standardize = TRUE, classifiers = classifiers, keep.pred = keep.pred, measures = measures, rdesc = rdesc, loocv = TRUE))
 basic <- lapply(basic, as.data.frame) 
 basic 
} 
add_filters <- function(classifiers) { 
 kruskal <-  lapply(classifiers, make_filtered, method = 'kruskal')
 rf_bvi <-  lapply(classifiers, make_filtered, method = 'rf_bvi')
 c(classifiers, kruskal, rf_bvi)
}
assess2 <- function(dataset, loocv = FALSE, standardize = TRUE, classifiers = NULL, 
                   keep.pred = FALSE, models = FALSE, measures = mlr::acc, rdesc = NULL) {
  # stopifnot(is.null(folds))
  if (standardize) {
    dataset <- standardize(dataset)
  }
  t <- taskify(dataset)  
  res <- mlr::benchmark(classifiers, t, rdesc, show.info = FALSE, measures = measures,  
                        keep.pred = keep.pred, models = models)
  res
}    
eval_a_group <- function(group, class_groups, dataset, nonhybrid, hybrid) {
  parallelStartMulticore(4, level = "mlr.benchmark") 
  set.seed(0, "L'Ecuyer")
  configureMlr(on.learner.error='warn') 
  vanilla <- evaluate_class_group(group, class_groups = class_groups, classifiers = nonhybrid, dataset = dataset, reps = 1)   
  set.seed(0, "L'Ecuyer")
  sampled <- evaluate_class_group(group, class_groups = class_groups, classifiers = hybrid, dataset = dataset, reps = 10)   
  res <- append(vanilla, sampled)
  filename  <- get_filename(group)
  save(res, file = filename) 
  parallelStop()   
} 
format_results <- function(res) {
   lrnid <- res$learner.id
   fss_method <- gsub('.*\\.filtered\\-(.*)$', '\\1', lrnid)
   fss_method <- gsub('^kruskal$', 'kw', fss_method ) 
   wsampling <- gsub('\\.filtered\\-.*$', '', lrnid)
   lrn <- gsub('\\..*$', '', wsampling)
   sampling <- gsub('^.*?\\.', '', wsampling) 
   sampling[sampling == lrn] <- '' 
   ind_fss <- grep('\\.filtered-', lrnid)
   fss <- rep('', length(lrnid))
   fss[ind_fss]  <- fss_method[ind_fss]
   res$fss <- fss
   res$learner.id <- lrn
   res$sampling <- sampling 
   res
}
get_results <- function(class_group_name) {
  filename  <- get_filename(class_group_name) 
  load(filename)  
  lapply(res, format_results)
} 
#' Gets the results for the specified class group 
get_aggr_results <- function(class_group_name) { 
  fresults <- get_results(class_group_name) 
  a <- merge_runs_metrics(fresults ) 
  aggregate_results(a) 
}
#' Gets results from runs 
#' Merges them and adds metrics 
#' Returns a single table containing all runs of CV 
merge_runs_metrics <- function(fresults) {
  pr <- lapply(fresults, process_cv)
  pr <- Reduce(rbind, pr) 
  pr$p <- pr$tp + pr$fn 
  pr$n <- pr$tn + pr$fp 
  pr$N <- pr$n + pr$p 
  pr
} 
#' Aggregate class results accross repetitions
aggregate_results <- function(class_results) {  
  class_results <- dplyr::group_by(class_results, learner.id, sampling, fss)
  class_results <- dplyr::summarise(class_results,  acc = mean(acc), f1 = mean(f1), tp = mean(tp), tn = mean(tn), n = mean(n), p = mean(p))  
  dplyr::arrange(class_results, desc(f1))
}  
process_cv <- function(res) {
  grouped <- dplyr::group_by(res, learner.id, sampling, fss)
  # sumar acc es para loo-cv
  # grouped <- dplyr::summarize(grouped, acc = sum(acc), fp = sum(fp), tp = sum(tp), fn = sum(fn), tn = sum(tn))
  grouped <- dplyr::summarize(grouped, acc = mean(acc), fp = sum(fp), tp = sum(tp), fn = sum(fn), tn = sum(tn))
  grouped <- dplyr::mutate(grouped, f1 = 2 * tp /(2 * tp + fp + fn))
  dplyr::arrange(grouped, desc(f1)) 
}
get_filename <- function(class_group_name) {
  paste0('rdata/results-', class_group_name, '.RData') 
}     
get_best_all <- function(summarized) {
  summarized$fss_method <- summarized$fss != '' 
  summarized <- dplyr::group_by(summarized, fss_method, sampling)  
  best <- slice(summarized, which.max(f1)) 
  best$fss_method <- NULL 
  as.data.frame(best)
}
get_learners <- function(results) {
  lrns <- get_mlr_models_full(results$learner.id, results$sampling, results$fss) 
  trained <- mapply(function(model, group) {   
    set.seed(0, "L'Ecuyer")
    train_group(model, dataset, group)
    }, lrns, results$class, SIMPLIFY = FALSE)   
  trained 
}
get_fss <- function(results) {
  lrnfss <- rep('', length(results$learner.id))
  ind <- (results$learner.id %in% c('CART', 'RMLR') & results$sampling == '')
  lrnfss[ind]  <- results$learner.id[ind] 
  results$thefss <- paste0(results$fss, ' + ', lrnfss) 
  results$thefss <- trimws(toupper(clean_plus(results$thefss)))
  results
} 
count_final_features <- function(model) {
  lrn <- model$learner.model
  if ('glmnet' %in% class(lrn)) {
    return (nrow(get_betas(lrn, s = 0.01)))
  }  
  else if ('rpart' %in% class(lrn)) {
    vars <- unique(lrn$frame$var)
    # return (length(lrn$variable.importance))
    return (length(vars) - 1)
  }
  length(model$features)
}
get_final <- function(ekudum) {
 while("WrappedModel" %in% class(ekudum$learner.model))  {
   ekudum <- ekudum$learner.model$next.model
 } 
 ekudum 
} 
get_misclassified_ids <- function(results, dataset) { 
  preds <- results$result$test[[1]]$pred$data  
  ids <- preds$id[preds$truth != preds$response]
  rownames(dataset)[ids]
}
get_classifier_errors <- function(classifier, group, reps) {
  set.seed(0, "L'Ecuyer")
  configureMlr(on.learner.error='warn') 
  dukele <- convert2binary(class_groups[[group]], dataset)               
  measures <- get_mlr_measures() 
  rdesc <- get_rdesc(group, dukele, taskify(dukele))
  basic <- lapply(seq_len(reps), function(i) assess2(dukele, standardize = TRUE, classifiers = classifier, keep.pred = TRUE, measures = measures, rdesc = rdesc, loocv = TRUE))  
  miski <- lapply(basic, get_misclassified_ids, dataset)
  # Reduce(intersect, miski)
  miski 
}  
get_common <- function(errors, threshold) {
  errors <- unlist(errors)
  tbl <- table(errors) 
  names(tbl[tbl >= threshold])
} 