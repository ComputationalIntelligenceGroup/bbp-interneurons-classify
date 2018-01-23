source('r/init-paper.R') 
set_xtable_options()          

# ====================================================================================================================================
# Data checks
# ====================================================================================================================================  

pdf('paper/prefix-classes-depth.pdf', width = 7, height = 7)
dataset_prefixplot <- dataset_prefix
dataset_prefixplot$layer <- name_layers(dataset_prefixplot$layer)
ggplot(data = dataset_prefixplot, aes(x = type, y = tortuosity.avg)) + geom_boxplot() + facet_grid( layer ~ class ) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = 'type') 
dev.off() 

dataset_prefix$type <- NULL
dataset_prefix$layer <- NULL
dataset_prefix <- remove_na_cols(dataset_prefix)
dataset_prefix <- remove_constant(dataset_prefix)
tables <- get_kruskal_pvals(dataset_prefix)  
tables <- subset(tables, pvalue < 0.05)   
tables <- tables[grep('^t\\.', tables$variable, invert = TRUE), ]
# tables <- tables[grep('^d\\.', tables$variable, invert = TRUE), ] 
tables 
rownames(tables) <- tables$variable
tables$variable <- NULL

vars <- rownames(tables)
funs <- gsub('^[td]\\.', '', vars)
funs <- unique(funs)
funs  <- sort(funs)
# group by: type and name. 
axons <- funs %in% vars
dends <- paste0('d.', funs) %in% vars
tbl <- data.frame(axon = axons, dendrite = dends)
tbl$axon <- ifelse(tbl$axon, tables[funs, ], 0) 
tbl$dendrite <- ifelse(tbl$dendrite, tables[paste0('d.', funs), ], 0) 
tbl <- cbind(variable = funs, tbl)   
tbl[tbl == 0] <- NA
tbl <- dplyr::arrange(tbl, axon)
tbl 

different <- tbl
colnames(different) <- gsub('variable', 'morphometric', colnames(different)) 
different <- verbize(different, 1)       
different[is.na(different)] <- ''
different[ -1] <- sapply(different[ -1], function(x) paste0('\\num[round-precision=2,round-mode=figures,scientific-notation=true]{', x, '}'))  
caption <- 'Morphometrics that differed between the cells whose id begins with a C and the rest, according to a Kruskal-Wallis test at $\\alpha = 0.05$, \\
with the p-value corrected for multiple testing \\
with the false discovery rate procedure \\citep{Benjamini1995}.'
label <- 'tbl:morphometrics-different'
xtable_print(different, label = label, caption = caption,
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/morphometrics-different.tex', floating = TRUE)
             # add.to.row = addtorow, hline.after = c(0,nrow(table)))     

# ====================================================================================================================================
# Duplicated reconstructions
# ====================================================================================================================================     
pdf('paper/OG061201A1-8_IDA.pdf', width = 7, height = 7)
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', 'OG061201A1-8_IDA', '.swc')) 
dev.off()
pdf('paper/OG061201A3_CH1_IN_H_ZK_60X_1.pdf', width = 7, height = 7)
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', 'OG061201A3_CH1_IN_H_ZK_60X_1', '.swc')) 
dev.off() 
pdf('paper/OG061201A1-8_IDE.pdf', width = 7, height = 7)
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', 'OG061201A1-8_IDE', '.swc')) 
dev.off() 
pdf('paper/OG061201A6_CH5_BC_H_ZK_60X_1.pdf', width = 7, height = 7)
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', 'OG061201A6_CH5_BC_H_ZK_60X_1', '.swc')) 
dev.off()

# ====================================================================================================================================
# Morphometrics table
# ==================================================================================================================================== 
vars <- colnames(dataset[ , -ncol(dataset)])  
# maybe meaningfull names: n bifs , n stems instead of uppercase 
# along with an explanation 

funs <- gsub('^[td]\\.', '', vars)
funs <- unique(funs)
funs  <- sort(funs)
# group by: type and name. 
axons <- funs %in% vars
terms <- paste0('t.', funs) %in% vars
dends <- paste0('d.', funs) %in% vars
tbl <- data.frame(axon = axons, terminal = terms, dendrite = dends)
tbl[] <- lapply(tbl, as.integer)
tbl <- cbind(variable = funs, tbl)   

neurostr <- subset(tbl, variable %in% get_neurostr_features(tbl$variable))
rownames(neurostr) <- seq_len(nrow(neurostr))  
nneurostr <- sum(neurostr[, -1])

colnames(neurostr) <- gsub('variable', 'morphometric', colnames(neurostr)) 
neurostr <- verbize(neurostr, 1)      
neurostr[neurostr == 0] <- ''
neurostr[neurostr == 1] <- '\\checkmark' 
caption <- 'NeuroSTR morphometrics. For part-of-tree morphometrics, suffixes avg, med, sd, and max denote the mean, median, standard deviation, and maximum, respectively. \
Detailed documentation for NeuroSTR features is available online: \\url{https://computationalintelligencegroup.github.io/neurostr/doc/measures/prebuilt.html}.' 
label <- 'tbl:morphometrics-neurostr'
xtable_print(neurostr, label = label, caption = caption,
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/morphometrics-neurostr.tex', floating = TRUE)
             # add.to.row = addtorow, hline.after = c(0,nrow(table)))  

custom <- subset(tbl, !variable %in% get_neurostr_features(tbl$variable)) 
rownames(custom) <- seq_len(nrow(custom))     
ncustom <- sum(custom[, -1])
stopifnot(nneurostr + ncustom == 103)

custom_desc <- read.csv('csv/custom-features.csv')
custom <- dplyr::right_join(custom_desc, custom, by = 'variable')
custom <- dplyr::arrange(custom, type)

custom$terminal <- NULL
custom[custom == 0] <- ''
custom[custom == 1] <- '\\checkmark' 
colnames(custom) <- gsub('variable', 'morphometric', colnames(custom)) 
custom <- verbize(custom, 2)      
caption <- 'Custom morphometrics.'
label <- 'tbl:morphometrics-custom'
xtable_print(custom, label = label, caption = caption,
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/morphometrics-custom.tex', floating = TRUE)

# ================================================================================================================================================================================ #
# Layer thickness
# ================================================================================================================================================================================ # 
thickness <- get_layers_thickness()

t <- get_layers_thickness()
thick <- setNames(t$thickness, t$layer)
sds <- setNames(t$thickness_sd, t$layer)
dist <- sapply(c('23', '4', '5', '6'), layer_to_l1, thick)
dist_sd <- sapply(c('23', '4', '5', '6'), layer_to_l1_sd, sds)  
dist_sd <- round(dist_sd, 1)

thickness$dist <- c(NA, dist) 
thickness$dist_sd <- c(NA, dist_sd)  

# thickness$dist_sd <- paste0(' $\\pm$ ', thickness$dist_sd ) 
# thickness$dist_sd[1] <- NA
# thickness 
# sprintf('%6.1f', thickness$dist)

thickness$dist <- sprintf('%6.1f', thickness$dist)
thickness <- tidyr::unite(thickness, distance_to_L1, dist, dist_sd, sep = ' $\\pm$ ')  
thickness$distance_to_L1 <- gsub('^ ', '\\\\;\\\\:', thickness$distance_to_L1)
thickness$distance_to_L1[1] <- '' 
# thickness$distance_to_L1[2:3] <- paste0('~', thickness$distance_to_L1[2:3])
thickness <- tidyr::unite(thickness, thickness, thickness, thickness_sd, sep = ' $\\pm$ ')
colnames(thickness)[3]  <- gsub('_', ' ', colnames(thickness)[3]  ) 
colnames(thickness)[3] <- paste0(colnames(thickness)[3], ' ($md_l \\pm sd_l$)' ) 

thickness$layer <- gsub('23', '2/3', thickness$layer) 

caption <- 'Layer thickness data from \\cite{Markram2015} and the estimated distance from the layer\'s center to the center of L1.'
label <- 'tbl:layer-thickness'
xtable_print(thickness, label = label, caption = caption, sanitize.colnames.function = sanitize_colnames_math,
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/layer-thickness.tex', floating = TRUE)

# ================================================================================================================================================================================ #
# Univariate feature selection
# ================================================================================================================================================================================ # 

# When was KW better or worse? 
# b <- lapply(groups, get_aggr_results ) 
# b <- lapply(b, get_best_f1, 1, fss = TRUE)
# b <- Reduce(rbind, b)
# better <- b[seq(1, 15, by = 2), ]
# worse <- b[-seq(1, 15, by = 2), ] 

# MC, 0.8333513 vs 0.8065673.
# **NBC**: 0.8553191 vs 0.7630702
# SBC: 0.7123810 vs 0.7077869
# BTC: 0.3392461 vs 0.4000000

# Better
# ChC DBC lbc Basket   
# BTC - problem is it is too stringent, and keeps too few examples. 
# For DBC, SBC, NBC and BASKET, dendritic were around 30% of the selected vars. For LBC and MC, however, they less than 20%. For Chc and BTC there were none (thus, none was capturing the bipolar dendritic nature)

# Table   

tables <- lapply(groups, get_binary_kruskal, dataset) 
tables <- lapply(tables, subset, pvalue < 0.05)  
tbs <- make_table_univarate(tables)      

caption <- "Morphometrics that differed most between the given class and the remaining classes joined together, according to the Kruskal-Wallis test. Empty entries mean that the p-value was above 0.05.\
Morphometrics that were significant for most classes are shown in the upper rows. "  
xtable_print(tbs, caption = caption,  label = 'tbl:kruskal-univariate-all', 
             include.rownames = TRUE,  size = 'tiny',  file = 'paper/kruskal-univariate-all.tex', tabular.environment = 'longtable', 
             floating = FALSE ) 

dendi <- lapply(tables, function(x) grep('^d\\.', x$variable)  )
dendi <- sapply(dendi, length)
total <- sapply(tables, nrow)
dendi   / total 

feats <- apply(tbs[, -1], 2, function(x) sum(x > 0, na.rm = TRUE))
feats 
# chc    btc    dbc    sbc    nbc     mc    lbc basket 
# 8      6     49     30     32     43     23     50

# dend <- tbs[grep('^d\\.', tbs$morphometric), ]
# dfeats <- apply(dend[, -1], 2, function(x) sum(x > 0, na.rm = TRUE))
# dfeats
# dfeats / feats
# 
# dend[dend > 1e-7] <- NA 

# Count how many times useful
allkws <- Reduce(rbind, tables)
kwreps <- table(allkws$variable)
stopifnot(length(kwreps) == 103)
a <- as.vector(unname(kwreps))
kwreps <- data.frame(morphometric = names(kwreps ), useful = a)
kwreps <- dplyr::arrange(kwreps, useful)
kwreps 
kwreps[grep('total', kwreps$morphometric), ]

useless <- subset(kwreps, useful ==  0)
useless 
uselessd <- useless[grep('^d\\.', useless$morphometric), ]
dim(uselessd)
useless[grep('^t\\.', useless$morphometric), ]
kwreps <- verbize(kwreps, 1) 
xtable_print(kwreps, caption = caption,  label = 'tbl:kruskal-usefulness', 
             include.rownames = TRUE,  size = 'tiny',  file = 'paper/kruskal-usefulness.tex', tabular.environment = 'longtable', 
             floating = FALSE ) 
  

# Unique for ba?
varsba <- as.character(tables$ba$variable)
setdiff(colnames(dataset), varsba) 
# Useless always
tbs <- Reduce(function(a, b) dplyr::full_join(a, b, by = 'variable'), tables)  
setdiff(colnames(dataset), as.character(tbs$variable))
#  intesections 
find_unique <- function(type, tables) { 
  varsba <- as.character(tables[[type]]$variable)
  ruke <- tables
  ruke[[type]] <- NULL 
  varsother <- lapply(ruke, function(x) as.character(x$variable))
  varsdiff <- lapply(varsother, function(x) setdiff(varsba, x))
  Reduce(intersect, varsdiff) 
}
find_unique('ba', tables) 
find_unique('mc', tables) 
find_unique('sbc', tables) 
find_unique('nbc', tables) 
find_unique('dbc', tables)  

varika <- as.character(tables[['mc']]$variable)
setdiff(colnames(dataset), varika )

ggplot(data = dataset, aes(x = axon_origin)) + geom_histogram() + facet_grid( class ~ .) 

# ================================================================================================================================================================================ #
# RF BVI Univariate feature selection
# ================================================================================================================================================================================ #  
grps <- groups
grps <- setNames(grps, grps)
vi <- lapply(grps, get_vi, dataset, ntree = 20000, type = NULL)
viki <- lapply(vi, subset, importance >= 0.01)

tbs <- make_table_univarate(viki, scientific = FALSE) 

caption <- "Morphometrics that differed between the given class and the remaining classes joined together, according to the RF BVI ranking. Empty entries mean that the RF BVI for that class was above 0.01.\
Morphometrics that were relevant to most classes are shown in the upper rows."  
xtable_print(tbs, caption = caption,  label = 'tbl:rfbvi-univariate-all', 
             include.rownames = TRUE,  size = 'footnotesize',  file = 'paper/rfbvi-univariate-all.tex', tabular.environment = 'longtable', 
             floating = FALSE )  

# ================================================================================================================================================================================ #
# MC RMLR
# ================================================================================================================================================================================ #  
load('rdata/full_rmlr_table.rdata')
table <- full_rmlr_table  
addtorow <- list()
addtorow$pos <- list(-1)
# addtorow$command <- '\\hline \\multicolumn{2}{c}{MC} & \\multicolumn{2}{c}{BA} & \\multicolumn{2}{c}{BA (RF BVI)} \\\\ \n'    
addtorow$command <- '\\hline \\multicolumn{2}{c}{MC} \\\\ \n'
table[, 2 + 1:4] <- NULL
table <- table[1:22, ]
label <- paste0('tbl:mc-betas')
file <- paste0('paper/mc-betas.tex') 
# logistic regression model for the BA type.'   
caption <- 'The logistic regression model for MC.\
The $\\coef$ were estimated from the standardized data set, after feature selection with KW.'
xtable_print(table, label = label,  caption = caption,              
             include.rownames = FALSE, size = 'footnotesize', file = file, sanitize.column.names = NULL,  
             hline.after = c(0, nrow(table)))  

# ================================================================================================================================================================================ #
# Detailed classification results
# ================================================================================================================================================================================ #  
fresults <- get_results('chc') 
a <- merge_runs_metrics(fresults ) 
a <- subset(a, learner.id == 'SVM' & fss == 'rf_bvi')[-1, ]
range(a$f1) 

ps <- lapply(groups, get_results_plot)
files <- paste0('paper/results-', groups, '.pdf') 
mapply(ggsave, files,  ps, MoreArgs = list(width = 8, height = 4, dpi = 1200))


# ================================================================================================================================================================================ #
# KW superset of RF BVI?
# ================================================================================================================================================================================ #  
tables <- lapply(groups, get_binary_kruskal, dataset) 
tables <- lapply(tables, subset, pvalue < 0.05)  
kwvars <- lapply(tables, '[[', 'variable')
rfvars <- lapply(viki, '[[', 'variable')

mapply(setdiff, rfvars, kwvars)  