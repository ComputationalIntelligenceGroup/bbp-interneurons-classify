make_prefix <- function(db) {
    prefixes <- get_prefix(rownames(db))
    db$class <- as.factor(prefixes)
    cones <- subset(db, class == 'C')
    no_cones <- subset(db, class != 'C')
    no_cones$class <- 'NC'
    db <- rbind(cones, no_cones)
    db$class <- droplevels(db$class)
    db 
}
get_prefix <- function(ids) {
  gsub('(.*?)\\d.*', '\\1', ids)
}
