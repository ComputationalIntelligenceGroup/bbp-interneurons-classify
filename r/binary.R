get_binary_kruskal <- function(class_group_name, dataset) { 
  dataset <- convert2binary(class_groups[[class_group_name]], dataset)      
  get_kruskal_pvals(dataset) 
} 
train_group <- function(model, dataset, group) {
  group <- tolower(group) 
  dataset <- convert2binary(class_groups[[group]], dataset)       
  task <- taskify(dataset)
  train(model, task)
}
assess_class_group <- function(class_groups, class_group_name, dataset) {   
 dataset <- convert2binary(class_groups[[class_group_name]], dataset)             
 classifiers <- get_final_classifiers(dataset) 
 filtered <-  lapply(classifiers, make_filtered, method = 'kruskal')
 # filtered <-  lapply(classifiers, filtered_classifiers, method = 'kruskal')
 classifiers <- c(classifiers, filtered)
 if (class_group_name == 'chc' ) {
    folds <- 7
 }
 else {
   folds <- 5
 }
 measures <- get_mlr_measures()
 # basic <-  assess(dataset, classifiers = classifiers, keep.pred = FALSE, 
 basic <-  assess(dataset, classifiers = classifiers, keep.pred = TRUE,
                  measures = measures, folds = folds, loocv = TRUE)
 as.data.frame(basic)  
}
get_binary_kruskal_learner <- function(class, learner, dataset) {
  mcdb <- convert2binary(to_join = class, dataset) 
  mcdb <- get_kruskal_db(mcdb)
  mctask <- taskify(mcdb)
  train(mods[[learner]], mctask) 
}