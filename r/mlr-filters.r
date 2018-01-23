# ==========
# RF ranking
# ==========
rf_bvi <- function(task, nselect, decreasing = TRUE, ntree = 20000, ...) { 
 data <- getTaskData(task)
 ranking <- rforest_ranking(ntree = ntree, dataset = data, class = getTaskTargetNames(task)) 
 setNames(ranking[, 1], rownames(ranking)) 
}
makeFilter(
  name = "rf_bvi",
  desc = "rf_bvi",
  pkg = "mlr",
  supported.tasks = c("classif"),
  supported.features = c("numerics"),
  fun = rf_bvi
)  
# ==========
# S2N
# ==========
ttest_select<- function (task, nselect, ...)  {
  data = getTaskData(task)
  s2n <- sapply(getTaskFeatureNames(task), s2n, getTaskTargetNames(task), data)
  # pvals <- p.adjust(pvals, method = 'fdr')
  # return negative because it assumes the higher the better.  
  abs(s2n)
} 
s2n <- function(feat.name, class, dataset) { 
  db <- dataset[ , c(feat.name, 'class')]
  db <- dplyr::group_by(db, class)
  db <- dplyr::summarise_each(db, dplyr::funs(mean, var, sd))
  means <- as.numeric(unlist(db[, 2]))
  vars <- as.numeric(unlist(db[, 3]))
  sds <- as.numeric(unlist(db[, 4]))
  # (means[1] - means[2]) / (sum(vars / means)) ^ 0.5
  (means[1] - means[2]) / sum(sds)
}
makeFilter(
  name = "t.test",
  desc = "t.test",
  pkg = "FSelector",
  supported.tasks = c("classif"),
  supported.features = c("numerics"),
  fun = ttest_select
)  
# ==========
# Kruskal
# ==========
kruskal_pval <- function(feat.name, class, data) {
  f = as.formula(paste0(feat.name, "~", class))
  t <- kruskal.test(f, data = data)
  # t <- wilcox.test(f, data = data, exact = TRUE, conf.int = FALSE)
  unname(t$p.value)
}
kruskal_fun <- function (task, nselect, ...)  {
  data = getTaskData(task)
  pvals <- sapply(getTaskFeatureNames(task), kruskal_pval, getTaskTargetNames(task), data)
  pvals <- p.adjust(pvals, method = 'fdr')
  # return negative because it assumes the higher the better. 
  -pvals
} 
makeFilter(
  name = "kruskal.pval",
  desc = "kruskal.pval",
  # pkg = "FSelector",
  pkg = "mlr",
  supported.tasks = c("classif"),
  supported.features = c("numerics"),
  fun = kruskal_fun 
)   