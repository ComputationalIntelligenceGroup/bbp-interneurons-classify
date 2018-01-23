ind_vert <- grep('vertical', colnames(dataset))
stopifnot(length(ind_vert) == 4)
colnames(dataset) <- gsub('^vertical', 'radial', colnames(dataset))
colnames(dataset) <- gsub('\\.vertical', '.radial', colnames(dataset))
dataset$l1_cy <- NULL
dataset$l1_mean <- NULL

colnames(dataset) <- gsub('_cx', '_gx', colnames(dataset))
colnames(dataset) <- gsub('_gxo', '_gxa', colnames(dataset))
colnames(dataset) <- gsub('origin_init', 'axon_origin', colnames(dataset))
colnames(dataset) <- gsub('origin_above_below', 'axon_above_below', colnames(dataset))
colnames(dataset) <- gsub('short_vertical', 'short_vertical_terminals', colnames(dataset))

dataset[ ,grep('x_std_mean', colnames(dataset))] <- NULL
dataset[ ,grep('_min', colnames(dataset))] <- NULL
dataset[ ,grep('_max', colnames(dataset))] <- NULL
dataset[ ,grep('type_one', colnames(dataset))] <- NULL
dataset[ ,grep('type_two', colnames(dataset))] <- NULL
dataset[ ,grep('com\\.dist', colnames(dataset))] <- NULL
# colnames(dataset)[grep('_max', colnames(dataset))]

grid_density <- dataset$grid_coarse.area / dataset$grid_area

grid <- colnames(dataset)[grep('grid', colnames(dataset))]
grid <- setdiff(grid, c('grid_area', 'd.grid_area', 'grid_mean', 'd.grid_mean'))
dataset[, grid] <- NULL
dataset$grid_density <- grid_density 
# This is the number of features specified in the paper 