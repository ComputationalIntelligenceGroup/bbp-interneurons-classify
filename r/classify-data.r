class_groups <- list(ba = c('LBC', 'NBC', 'SBC'), compact = c('ChC', 'NBC', 'SBC'), dendritic = c('BTC', 'DBC'),
                     mc = 'MC', chc = 'ChC', dbc = 'DBC', nbc = 'NBC', btc = 'BTC', lbc = 'LBC', sbc = 'SBC')
# Order groups by scarcity
groups <- c( 'chc', 'btc', 'dbc', 'sbc', 'nbc', 'mc', 'lbc', 'ba')
groups <- setNames(groups, groups)
mods <- base_classifiers()[list_selected_classifiers()]

# ============================================================================ #
# mods <- mods[c('RMLR', 'SVM', 'RF', 'CART', 'kNN')]
# mods <- mods[c('RF')]
# ============================================================================ #

# ============================================================================ #
# Clean this up
# ============================================================================ #
dataset <- read.csv('data/final-db.csv', row.names = 1)
# Move this below
dataset <- droplevels(dataset)
source('r/features-remove-rename-add.R')  
stopifnot(ncol(dataset) == 103)

# ============================================================================ #  
# dataset <- read.csv('~/code/neuro-intermorpho/data/custom-axon.csv', row.names = 1)
m <- read_metadata()
ku <- subset(m, id %in% rownames(dataset))
rownames(ku) <- ku$id
ku <- ku[rownames(dataset), ]
dataset$class <- ku$class

# ============================================================================ #
# Select instances
# ============================================================================ # 
load('data/old-dble.rdata')
dataset <- dataset[rownames(dble), ]
load('data/old-dbco.rdata')
load('data/old-dbfu.rdata')

# ============================================================================ #
# Full db
# ============================================================================ #
# Clean this up
dataset_full <- read.csv('data/full-db.csv', row.names = 1)
dataset_full <- dataset_full[rownames(dataset), ]

# ============================================================================ #
# Add old features
# ============================================================================ #
dble <- dble[rownames(dataset), ]
dbco <- dbco[rownames(dataset), ]
# ku <- sapply(lapply(dble, unique), length)
# #  This removes class
# dble <- dble[, ku > 20]
# dataset <- cbind(dble, dataset)
# dataset <- mlearn::remove_na_cols(dataset)

# ============================================================================ #
# Checks
# ============================================================================ #
stopifnot(identical(rownames(dataset_full), rownames(dbfu)))
stopifnot(identical(rownames(dataset), rownames(dbfu)))
