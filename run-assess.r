source('r/init.R')    
registerS3method("trainLearner", "BojanWrapper", trainLearner.BojanWrapper)

# use this to get results quickly
# dataset <- dataset[ , c(1:30, ncol(dataset))]
# source('r/quick-eval.r')  
# system('cp rdata/results-* rdata/final/')

classifs <- mods
hybrid <- get_hybrid_classifiers(classifs, under_rate = 0.6, smote_rate = Inf)

classifs <- add_filters(classifs)
hybrid <- add_filters(hybrid) 
lapply(groups, eval_a_group, class_groups, dataset = dataset, classifs, hybrid) 
