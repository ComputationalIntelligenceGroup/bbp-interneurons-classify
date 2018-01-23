list_selected_classifiers <- function() { 
  classifiers <- read.csv('csv/classifiers.csv')   
  classifiers  <- subset(classifiers, is.na(included))
  as.character(classifiers$Abbreviation)
}
base_classifiers <- function() { 
  rf <- mlr::makeLearner("classif.randomForest", id = "RF", par.vals = list(ntree = 2000), predict.type = 'prob')
  # I suppose that min split is enough to limit, no need for the cp parameter
  cart <- mlr::makeLearner('classif.rpart', id = 'CART', par.vals = list(minbucket = 5, minsplit = 10, cp = 0), predict.type = 'prob')
  knn <- mlr::makeLearner("classif.kknn", par.vals = list (k = 5, kernel = "rectangular"), id = "kNN", predict.type = 'prob') 
  nb <- mlr::makeLearner("classif.naiveBayes", id = "NB", predict.type = 'prob') 
  svm <- mlr::makeLearner("classif.svm", id = "SVM", predict.type = 'prob')    
  glmnet <- mlr::makeLearner('classif.glmnet', id = 'RMLR', predict.type = 'prob') 
  glmnet_ridge <- mlr::makeLearner('classif.glmnet', id = 'RMLR_RIDGE', par.vals = list(alpha = 0), predict.type = 'prob')   
  lr <- mlr::makeLearner('classif.binomial', id = 'LR', predict.type = 'prob')   
  lda <- mlr::makeLearner("classif.lda", id = 'LDA', predict.type = 'prob')    
  svm_lin <- mlr::makeLearner("classif.svm", id = "SVM_LIN", predict.type = 'prob', par.vals = list(kernel = 'linear'))    
  
  classifiers <- list( rf = rf, cart = cart,  knn = knn,  nb = nb, 
                       svm = svm, rmlr = glmnet, glmnet_ridge = glmnet_ridge, 
                       lda = lda, lr = lr, svm_lin = svm_lin  )  
  names(classifiers) <- toupper(names(classifiers))
  names(classifiers) <- gsub('KNN', 'kNN', names(classifiers))
  classifiers  
}   
get_hybrid_classifiers <- function(use_classifiers, under_rate, smote_rate) { 
  classifiers <- lapply(use_classifiers, test_make_wrapper, usw.rate = under_rate, smote.rate = smote_rate) 
  classifiers 
} 