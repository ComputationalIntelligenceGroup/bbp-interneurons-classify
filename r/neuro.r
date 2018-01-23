plot_neuron_file <-
function(path) {
  neu <- nat:::read.neuron(path)
  plot_neuron(neu)
}
seglist2lines <-
function(seglist) {
  unlist(sapply(seglist, function(x) c(x, NA)))
}
get_soma_inds <-
function(neuron) {
  subset(neuron$d, Label == 1)$PointNo
}
plot_neuron <-
function(neuron) {
  set_grid(neuron)
  naxon <- subset(neuron, Label == 2)
  plot_axon(naxon)
  if (sum(neuron$d$Label == 3) > 0) {
    ndend <- subset(neuron, Label == 3)
    plot_dendrites(ndend)
  }
  else {
    warning('No dendrites')
  }
  plot_soma(neuron)
}
plot_axon <-
function(neuron, ...) {
  # stopifnot(neuron$nTrees == 1)
  if (neuron$nTrees == 1) {
    lines <- seglist2lines(neuron$SegList)
    plot_trees(neuron, lines, col = 'blue', ...)
  }
  else {
    warning('Multiple axon trees')
    lines <- lapply(neuron$SubTrees, seglist2lines)
    lines <- unlist(lines, use.names = FALSE)
    plot_trees(neuron, lines, col = 'blue', ...)
  }
}
plot_dendrites <-
function(neuron, ...) {
  lines <- lapply(neuron$SubTrees, seglist2lines)
  lines <- unlist(lines, use.names = FALSE)
  plot_trees(neuron, lines, col = 'red', ...)
}
plot_trees <-
function(neuron, lines, ...) {
  omar <- par('mar')
  par(mar = c(0, 0, 0, 0))
  lines(neuron$d[lines, c('X', 'Y')], lwd = 0.5, ...)
  par(mar = omar)
}
plot_soma <-
function(neuron) {
  soma_inds <- get_soma_inds(neuron)
  soma_limits <- subset(neuron$d, Parent %in% soma_inds)
  polygon(soma_limits$X, soma_limits$Y, col = 'red', border = 'red')
  # Dont know if putting these together is a good idea.
  # sx <- c(soma_limits$X, soma$X)
  # sy <- c(soma_limits$Y, soma$Y)
}
set_grid <-
function(neuron) {
  omar <- par()$mar
  par(mar = c(0, 0, 0, 0))
  plot(1, type="n", axes=F, xlab="", ylab="")
  bbox <- get_plot_bbox(neuron)
  xlim <- bbox$xlim
  ylim <- bbox$ylim
  par(usr=c(xlim, ylim))
  abline(v = seq(xlim[1], xlim[2], by = 100), h = seq(ylim[1], ylim[2], by = 100), col = 'grey60')
  column_col <- gray(0.2, alpha = 0.05)
  soma_inds <- get_soma_inds(neuron)
  xsoma <- neuron$d[soma_inds, 'X']
  column_xlim <- mean(xsoma) + c(-150, 150)
  rect(column_xlim[1], ylim[1], column_xlim[2], ylim[2], col = column_col, border = NA)
  par(mar=omar)
}
get_plot_bbox <-
function(neuron) {
  # cell extemes +- 50
  xlim <- round_hundred(range(neuron$d[, 'X']) + c(-50, 50))
  ylim <- round_hundred(range(neuron$d[, 'Y']) + c(-50, 50))
  list(xlim = xlim, ylim = ylim)
}
round_hundred <-
function(x) {
  fract <- x / 100
  ifelse(x >= 0, ceiling(fract), floor(fract)) * 100
}
