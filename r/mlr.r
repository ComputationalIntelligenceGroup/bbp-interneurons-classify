taskify <- function(dataset) {
  tbl <- table(dataset$class)
  positive <- names(tbl)[which.min(tbl)] 
  if (all(c('BA', 'Non-BA') %in% names(tbl))) {
   positive <- 'BA' 
  } 
  mlr::makeClassifTask(id = 'test', data = dataset, target = 'class', positive = positive)
}
#' Handles aggregation of measures 
get_mlr_measures <- function() { 
 measures <- list(mlr::acc,  mlr::tp, mlr::tn, mlr::fp, mlr::fn)
 measures <- setNames(measures, c('acc',  'tp', 'tn', 'fp', 'fn'))
 measures[c('tp', 'tn', 'fp', 'fn')] <- lapply(measures[c('tp', 'tn', 'fp', 'fn')], mlr::setAggregation, mlr::test.sum)
 measures
} 
#' Set threshold for Kruskal Wallis 
make_filtered <- function(classifier, method) {
  kruskal <- list(fw.method = 'kruskal.pval', fw.threshold = -0.05)
  rf_bvi <- list(fw.method = 'rf_bvi', fw.threshold = 0.01)
  s2n <- list(fw.method = 't.test', fw.abs = 40)
  fss <- list(kruskal = kruskal, s2n = s2n, rf_bvi = rf_bvi) 
  fs <- fss[[method]] 
  lrn <- makeFilterWrapper(learner = classifier, fw.method = fs$fw.method, fw.perc = fs$fw.perc, fw.threshold = fs$fw.threshold, fw.abs = fs$fw.abs)
  rep <- paste0('\\1.\\2-', method)
  lrn$id <- gsub('^(.*)\\.(filtered)$', rep, lrn$id) 
  lrn
}  

# ====  ====  ====  ====  ====  ====  ====  ====  ====  ====  ====  #
test_make_wrapper <- function(learner, usw.rate = 1, smote.rate = 1, usw.cl = NULL)  {
  learner = mlr:::checkLearner(learner, "classif")
  pv = list()
  if (!missing(usw.rate)) {
    checkmate::assertNumber(usw.rate, lower = 0, upper = 1)
    pv$usw.rate = usw.rate
  } 
  if (!missing(smote.rate)) {
    pv$smote.rate = smote.rate
  }
  if (!is.null(usw.cl)) {
    checkmate::assertString(usw.cl)
    pv$usw.cl = usw.cl
  }
  id = paste0(learner$id, ".hybrid", usw.rate, "-", smote.rate)
  ps = ParamHelpers::makeParamSet(makeNumericLearnerParam(id = "usw.rate",  lower = 0, upper = 1), 
                                  makeNumericLearnerParam(id = "smote.rate",  lower = 0, upper = Inf), 
                                  makeUntypedLearnerParam(id = "usw.cl",  default = NULL, tunable = FALSE))
  mlr:::makeBaseWrapper(id, "classif", learner, package = "mlr", 
                  par.set = ps, par.vals = pv, learner.subclass = "BojanWrapper", 
                  model.subclass = "UndersampleModel")
}
trainLearner.BojanWrapper <- function(.learner, .task, .subset, .weights = NULL, usw.rate = 1, smote.rate = 3, usw.cl = NULL, ...) {  
  .task = subsetTask(.task, .subset)
  .task <- get_hybrid_db(.task, usw.rate = usw.rate, sw.rate = smote.rate) 
  m = train(.learner$next.learner, .task, weights = .weights)
  m$train.task = .task
  mlr:::makeChainModel(next.model = m, cl = "UndersampleModel")
}
get_mlr_models_full <- function(learner, sampling, fss) {
 models <- mods[learner]  
 ind_sample <- sampling != ''
 models[ind_sample] <- get_hybrid_classifiers(models[ind_sample], under_rate = 0.6, smote_rate = Inf) 
 fss <- gsub('^kw$', 'kruskal', fss)
 ind_fss <- fss != ''
 models[ind_fss] <- mapply(make_filtered, models[ind_fss], method = fss[ind_fss], SIMPLIFY = FALSE)
 models
}
# ====  ====  ====  ====  ====  ====  ====  ====  ====  ====  ====  #      
learn_model <- function(dataset, model, classifiers) {
  task <- taskify(dataset)
  glm <- classifiers[[model]]   
  train(glm, task)
}
get_mlr_model <- function(model, dataset) { 
  task <- taskify(dataset)
  train(model, task)$learner.model
}
get_lasso_coefs <- function(mlr_model, lambda) {
  # glm <- setHyperPars(glm, par.vals = list(alpha = 0))
  # coef <- coef(model)  
  model <- mlr_model$learner.model
  default_mlr <- coef(model, s = lambda)
  default_mlr <- as.matrix(default_mlr)
  inds <- abs(default_mlr) > 0
  default_mlr <- default_mlr[inds, ]
  lasso_vars <- data.frame(variable = names(default_mlr ), beta = default_mlr)
  # Remove intercept
  lasso_vars <- lasso_vars[-1, ]
  dplyr::arrange(lasso_vars, desc(abs(beta)))
}   
#' Determine the SMOTE rate in order to reach identical levels of both classes
smote_train <-  function(data, target, args = list()) { 
  rate <- get_smote_rate(data, target) 
  if (rate > 0) { 
    t <- mlr::makeClassifTask('1', data, target = target)
    t <- mlr::smote(t, rate = rate)
    data <- t$env$data
  }
  return(list(data = data, control = control))
}
#' Determine the SMOTE rate in order to reach identical levels of both classes
smote_pred <- function(data, target, args, control) {
  cols <- control[[1]]
  data <- data[ , cols, drop = FALSE]
  return(data)
}
make_preproc_SMOTE <- function(learner, smote_rate_max) {
  makePreprocWrapper(
    learner,
    train = smote_train,
    predict = smote_pred, par.set = makeParamSet(makeNumericParam('smote_rate_max')), par.vals = list(smote_rate_max = smote_rate_max)
  )
}
make_preproc_rescale <- function(learner) { 
  train <-  function(data, target, args = list()) {
    data <- mlearn::multiclass_rescale(data)
    return(list(data = data, control = list()))
  }
  pred <- function(data, target, args, control) {
    return(data)
  }
  makePreprocWrapper(
    learner,
    train = train,
    predict = pred
  )
}
#' Preprocess that removes constant features. 
make_preproc_constant <- function(learner) {
  train <-  function(data, target, args = list()) {
    data <- mlearn::remove_constant(data)
    return(list(data = data, control = list()))
  }
  pred <- function(data, target, args, control) {
    return(data)
  }
  makePreprocWrapper(
    learner,
    train = train,
    predict = pred
  )
}  
get_under_classifiers <- function(use_classifiers, under_rate) { 
  classifiers <- lapply(use_classifiers, mlr::makeUndersampleWrapper, usw.rate = under_rate)
  # classifiers <- mapply(function(new, old) {  
  #   new$id <- old$id  
  #   new
  # }, classifiers, use_classifiers, SIMPLIFY = FALSE)
  classifiers 
}  
get_smote_classifiers <- function(use_classifiers, smote_rate) { 
  classifiers <- lapply(use_classifiers, mlr::makeSMOTEWrapper, sw.rate = smote_rate)
  # classifiers <- mapply(function(new, old) {  
  #   new$id <- old$id  
  #   new
  #   }, classifiers, use_classifiers, SIMPLIFY = FALSE)
  classifiers 
} 
evaluate_training <- function(vars, model, dataset) {   
  dataset <- dataset[ , c(vars, 'class'), drop = FALSE] 
  task <- taskify(dataset) 
  model <- train(model, task)
  pred <- predict(model, taskify(dataset))
  mlr::performance(pred, list(mlr::acc, mlr::f1))
}