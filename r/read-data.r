read_metadata <- function(unclear_class = FALSE) {
  m <- read.csv('~/code/bbp-data/csv/metadata.csv', row.names = 1, quote = "\"")
  layers <- unique(as.character(m$layer))
  m$layer <- factor(m$layer, levels = sort(layers))
  if (!unclear_class) {
    m <- m[!is.na(m$class), ]
  }
  m <- droplevels(m)
  m
}