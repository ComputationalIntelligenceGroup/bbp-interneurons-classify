layer_to_l1 <-
function(cell_layer, thicknesses) {
  stopifnot(identical(names(thicknesses), c('1', '23', '4','5', '6')))
  ind <- match(cell_layer, c('23','4','5', '6'))
  inds <- seq_len(ind)[-1]
  thicknesses[1] / 2 + sum(thicknesses[inds]) + thicknesses[ind + 1] / 2
}
layer_to_l1_sd <-
function(cell_layer, sds) {
  stopifnot(identical(names(sds), c('1', '23', '4','5', '6')))
  ind <- match(cell_layer, c('1', '23','4','5', '6'))
  stopifnot(ind > 1)
  var <- sds ^ 2
  inds <- seq_len(ind)[-c(1, ind)]
  var <- (var[1]  / 4)  +  sum(var[inds])  + ( var[ind]  / 4)
  sqrt(var)
}
layer_to_l1_devs <-
function(cell_layer, sds) {
  stopifnot(identical(names(sds), c('1', '23', '4','5', '6')))
  ind <- match(cell_layer, c('23','4','5', '6'))
  inds <- seq_len(ind)[-1]
  above <- sds[1] / 2 + sum(sds[inds]) + sds[ind + 1] / 2
  above
}
