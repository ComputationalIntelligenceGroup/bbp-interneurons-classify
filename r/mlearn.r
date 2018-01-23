is_2D <-
function(x) {
  length(dim(x)) == 2
}
subset_by_colnames <-
function(colnames, data) {
  stopifnot(is.character(colnames), is_2D(data), length(colnames) == nrow(data))
  ind_cols <- match(colnames, colnames(data))
  ind_matrix <- cbind(seq_along(ind_cols), ind_cols)
  data[ind_matrix]
}
standardize <-
function(db) {
  if (ncol(db) < 2) return(db)
  # Class is also a factor and will be included in ind_ignore
  ind_ignore <- which(vapply(db, is.factor, FUN.VALUE = logical(1)))
  db_preds <- db[, -ind_ignore, drop = FALSE]
  # avoid division by 0
  safe_sd <- function(x) {
    sd <- sd(x)
    if (sd == 0) sd <- 1
    sd
  }
  db_preds <- apply(db_preds, 2, function(x) (x - mean(x)) / safe_sd(x))
  stopifnot(all.equal(unname(apply(db_preds, 2, mean)), rep(0, ncol(db_preds))))
  db[, -ind_ignore] <- db_preds
  db
}
convert2binary <-
function(to_join, data) {
  classes <- levels(data$class)
  others <- setdiff(classes, to_join)
  stopifnot(is.character(to_join),
            length(classes) > 2,
            all(to_join %in% classes),
            length(others) > 0)
  ind_to_join <- classes %in% to_join
  new_classes <- classes
  to_join_label <- paste0(to_join, collapse = '')
  if (to_join_label == 'LBCNBCSBC') to_join_label = 'BA'
  new_classes[ind_to_join] <- to_join_label
  # others_label <- paste0(others, collapse = '')
  others_label  <- paste0('Non-', to_join_label, collapse = '')
  new_classes[!ind_to_join] <- others_label
  levels(data$class) <- new_classes
  data
} 
convert_ternary <- function(data, a, b) {  
  data <- droplevels(data)
  classes <- levels(data$class)
  keep <- c(a, b)
  others <- setdiff(classes, keep )
  stopifnot(is.character(keep),
            length(classes) > 2,
            all(keep %in% classes),
            length(others) > 0)
  values <- as.character(data$class)
  ind_change <- !values %in% keep
  values[ind_change ] <- 'Other'
  data$class <- factor(values)
  
  
  values <- as.character(data$class)
  ind_change <- values %in% a
  values[ind_change ] <- 'BA' 
  data$class <- factor(values)
  data
}
remove_na_cols <-
function(x) {
  cols <- find_na_cols(x)
  remove_cols(cols, x)
}
remove_cols <-
function(cols, x) {
  stopifnot(is.character(cols), is.character(colnames(x)),
            all(cols %in% colnames(x)))
  keep <- setdiff(colnames(x), cols)
  x[ , keep, drop = FALSE]
}
remove_constant <-
function(x) {
  cols <- find_constant_cols(x)
  remove_cols(cols, x)
}
find_na_cols <-
function(x) {
  ind_na <- apply(x, 2, anyNA)
  colnames(x)[ind_na]
}
find_constant_cols <-
function(x) {
  sds <- apply(x, 2, is_constant)
  colnames(x)[sds]
}
is_constant <-
function(x) {
  # if (!is.numeric(x)) return(NA)
  # sd(x) == 0
  !anyNA(x) & all(x == x[1])
}
