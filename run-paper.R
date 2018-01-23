source('r/init-paper.R')         

# ================================================================================================================================================================================ #
# Classification results
# ================================================================================================================================================================================ #  
b <- lapply(groups, get_aggr_results)   
b <- lapply(b, get_best_all)  
all <- format_table_all_results(b, groups, bold = TRUE, preprocessing = FALSE) 
colnames(all)[1] <- 'Cell Type'
all <- all[ ,c(1:2,4,3,6:8,5)]
# uken <- Reduce(rbind, b)
# uken$class <- rep(names(b), each = 4)
# akem <- get_learners(uken)
# save(akem, file = 'rdata/trained-best.rdata') 
load(file = 'rdata/trained-best.rdata')
models <- lapply(akem, get_final )
features <- sapply(models, count_final_features)
all$features <- as.integer(features)
colnames(all)[ncol(all)] <- 'Morphom.'
# tabular.environment = 'longtable', 
# floating = FALSE
label <- paste0('tbl:binary-performance')   
caption <- 'F-measure one-versus-all classification. The table shows, for each type, the best F-measure in all four learning settings: with and without sampling, and with and without feature selection. \
TPR: true positive rate; TNR: true negative rate; the minority class is always the positive one, except for BA; Acc: classification accuracy; Morphom.: the number of morphometrics in the model. \
Types are sorted from least to most frequent\ (e.g., ChC, with only seven examples, is shown uppermost). The best F-measure for each type is typeset in bold. \
Types with their best F-measure $\\geq$ 0.75 are shown in green; those with the F-measure $\\geq$ 0.60 in orange; and the rest in red.'
xtable_print(all, label = label,  caption  = caption, include.rownames = TRUE, size = 'normalsize', file = 'paper/binary-performance.tex')          

# ================================================================================================================================================================================ #
# Results: sampling vs non-sampling
# ================================================================================================================================================================================ #  
# acc <- get_results_group('sbc')
# # Was it generally better? Show by different class as facet. In same horizontal line.  
# p <- ggplot(acc, aes_string("Classifier", 'f1', colour = 'Classifier'))
# p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0))
# # p <- p + ylab('Sampling')  + xlab('Feature selection')
# p <- p + geom_violin() + stat_summary(fun.ymin = mean,  fun.ymax = mean, fun.y = mean, geom = "crossbar") 
# p <- p + facet_grid( ~ sampling)
# p 

# ================================================================================================================================================================================ #
# Detailed results graphics
# ================================================================================================================================================================================ #  
p <- get_results_plot('ba') 
stopifnot(file.remove('paper/results-ba.pdf'))
ggsave( 'paper/results-ba.pdf',  p,  width = 8, height = 4, dpi = 1200) 

# ================================================================================================================================================================================ #
# Univariate feature selection
# ================================================================================================================================================================================ # 
grps <- c('nbc', 'mc', 'ba') 
grps <- setNames(grps, grps)
tables <- lapply(groups, get_binary_kruskal, dataset) 
tables <- lapply(tables, subset, pvalue < 0.05)   
# tables <- tables[c('dbc', 'nbc', 'mc', 'ba')]  
tables <- tables[grps]  

counts <- sapply(tables, nrow)
dends <- sapply(tables, function(x) length(grep('^d\\.', x$variable)))
termis <- sapply(tables, function(x) length(grep('^t\\.', x$variable))) 
termis <- unlist(lapply(lapply(termis, c, 'Terminal'), rev))
dends <- unlist(lapply(lapply(dends, c, 'Dendritic'), rev))
counts <- unlist(lapply(lapply(counts, c, 'Total'), rev))

# denddies <- sapply(tables, function(x) x[grep('^d\\.', x$variable), ])
# dplyr::arrange(denddies$ba, pvalue)

tables <- lapply(tables, '[', 1:10, TRUE, drop = FALSE)      
table <- paste_columns(tables)
inds <- grep('pvalue', colnames(table))
table[, inds] <- apply(table[, inds], 2, function(x) paste0('\\num[round-precision=2,round-mode=figures,scientific-notation=true]{', x, '}')) 
colnames(table) <- gsub('pvalue', 'p-value', colnames(table))    

table <- format_vars_tbl(table)
table <- rbind(table, dends)
table <- rbind(table, counts)
table  

addtorow <- list() 
addtorow$pos <- list(-1)
addtorow$command <- '\\hline \\multicolumn{2}{c}{NBC} & \\multicolumn{2}{c}{MC} & \\multicolumn{2}{c}{BA} \\\\ \n'
label <- paste0('tbl:kruskal-features')
file <- paste0('paper/kruskal-features.tex')  
caption <- 'Morphometrics that differed most between the given class (NBC, MC, BA) and the remaining classes joined together, \
according to the Kruskal-Wallis test. Total shows the number of relevant morphometrics at the $\\alpha = 0.05$ significance level. \
'
xtable_print(table, label = label,  include.rownames = FALSE, caption = caption,
             size = 'footnotesize', add.to.row = addtorow, file = file, floating = TRUE, 
             hline.after = c(0,nrow(table) - 2, nrow(table))) # , tabular.environment = 'longtable')    
# Total morphometrics for classes not shown here: ChC: 13; BTC: 7; SBC: 65; NBC: 44; LBC: 36.'

# dbs <- lapply(class_groups[grps], convert2binary, dataset)
# #  Training set performance  
# vars <- lapply(tables, '[[', 'variable') 
# vars <- lapply(vars, as.character) 
# perf <- mapply(function(vars, db) evaluate_training(vars, mods[['RMLR']], db), vars, dbs)
# perf 
# 
# vars <- c("y_mean_abs", "t.remote_bifurcation_angle.avg", "d.N_stems")
# evaluate_training(vars, mods[['RMLR']], dbs[[3]]) 
# vars <- c("y_mean_abs", "t.remote_bifurcation_angle.avg")
# evaluate_training(vars, mods[['RMLR']], dbs[[3]])

grps <- c('nbc', 'lbc', 'sbc') 
grps <- setNames(grps, grps)
tables <- lapply(groups, get_binary_kruskal, dataset) 
tables <- lapply(tables, subset, pvalue < 0.05)   
# tables <- tables[c('dbc', 'nbc', 'mc', 'ba')]  
tables <- tables[grps]  

counts <- sapply(tables, nrow)
dends <- sapply(tables, function(x) length(grep('^d\\.', x$variable)))
dends <- unlist(lapply(lapply(dends, c, 'Dendritic'), rev))
counts <- unlist(lapply(lapply(counts, c, 'Total'), rev))

tables <- lapply(tables, '[', 1:5, TRUE, drop = FALSE)      
table <- paste_columns(tables)
inds <- grep('pvalue', colnames(table))
table[, inds] <- apply(table[, inds], 2, function(x) paste0('\\num[round-precision=2,round-mode=figures,scientific-notation=true]{', x, '}')) 
colnames(table) <- gsub('pvalue', 'p-value', colnames(table))    

table <- format_vars_tbl(table)
table <- rbind(table, dends)
table <- rbind(table, counts)
table 

addtorow <- list() 
addtorow$pos <- list(-1)
addtorow$command <- '\\hline \\multicolumn{2}{c}{NBC} & \\multicolumn{2}{c}{LBC} & \\multicolumn{2}{c}{SBC} \\\\ \n'
label <- paste0('tbl:kruskal-features2')
file <- paste0('paper/kruskal-features2.tex')  
caption <- 'Morphometrics that differed most between the given class (NBC, LBC, and SBC) and the remaining classes joined together, \
according to the Kruskal-Wallis test. Total shows the number of relevant morphometrics at the $\\alpha = 0.05$ significance level. \
' 
xtable_print(table, label = label,  include.rownames = FALSE, caption = caption,
             size = 'footnotesize', add.to.row = addtorow, file = file, floating = TRUE, 
             hline.after = c(0,nrow(table) - 2, nrow(table))) 
# , tabular.environment = 'longtable')      
# Total morphometrics for classes not shown here: ChC: 13; BTC: 7; SBC: 65; NBC: 44; LBC: 36.' 

# ================================================================================================================================================================================ #
# FSS vs no-fss
# ================================================================================================================================================================================ #   
rabbabb <- lapply(groups, get_aggr_results ) 
# rabbabb <- lapply(rabbabb, get_fss)   
# b <- lapply(b, subset, (thefss != ''))  
allk <- join_results(rabbabb, groups) 
allk <- allk %>% dplyr::group_by(class, fss)  
bestaga <- top_n(allk, 1, f1)   
bestaga <- subset(bestaga, fss != 'kw')
# bestaga <- subset(bestaga, fss != 'rf_bvi')
bestaga <- bestaga[ ,c('class', 'fss', 'f1')] 
bestaga <- dplyr::arrange(bestaga, class, fss) 
bestaga 
fone <- bestaga$f1 
inds <- seq(2, nrow(bestaga), by = 2)
diffif <- fone[inds] - fone[inds - 1] 
typessss <- bestaga$class[!duplicated(bestaga$class)]
# good <- typessss[diffif  > -0.03]
good <- typessss[diffif  >= 0.0]
good 
# setdiff(typessss, good )  

# ================================================================================================================================================================================ #
# Interpretable results 
# ================================================================================================================================================================================ #  
# b <- lapply(b, subset, learner.id %in% c('CART', 'RF', 'RMLR') & sampling == '' & f1 > 0.65)  
# b <- lapply(b, subset, learner.id %in% c('CART', 'RF', 'RMLR') & sampling == '' & fss == 'fss' & f1 > 0.65)  
# b <- lapply(b, subset, learner.id %in% c('CART', 'RMLR') & sampling == '' & fss != '' & f1 >= 0.65)
# b <- lapply(b, subset, learner.id %in% c('CART', 'RMLR') & sampling == '' &  f1 >= 0.65)

b <- lapply(groups, get_aggr_results ) 
b <- lapply(b, subset, (learner.id %in% c('CART', 'RMLR') & sampling == '') | fss != '')    
b <- lapply(b, get_fss)   
b <- lapply(b, subset, (thefss != ''))  
all <- join_results(b, groups) 
all <- all %>% dplyr::group_by(class, thefss)  
best <- top_n(all, 1, f1)   
all_best_fsel <- best
# best  <- subset(best, f1 >= 0.65 & f1 < 0.75) 
best  <- subset(best, f1 >= 0.75)
best  

# KW; RF BVI; KW + RMLR; 
all <- best 
# TODO: Format table
# !!!!!!!!!!!!!!!!!
# all <- format_table_all_results(b, groups)    
# all <- as.data.frame(all)

label <- paste0('tbl:binary-interpretable') 
caption = 'F-measure and accuracy of simple and interpretable classifiers (CART, and RMLR) not sampling the training data and after performing feature selection. Only showing classifiers with F-measure $\\geq 0.65$. TPR: true positive rate; TNR: true negative rate. The minority class is always the positive one. Classes sorted by frequency.'
xtable_print(all, label = label,  caption = caption,             
             include.rownames = FALSE, size = 'normalsize',  file = 'paper/binary-interpreted-performance.tex')     

# ================================================================================================================================================================================ #
# Feature selection results
# ================================================================================================================================================================================ #       
b <- lapply(groups, get_aggr_results )  
b <- lapply(b, get_fss)   
b <- lapply(b, subset, (thefss != ''))  
all <- join_results(b, groups) 
all <- all %>% dplyr::group_by(class, thefss)  
best <- top_n(all, 1, f1)   

print('If any changes to variables or rows, re run feature subset sizes table!!!!!')
# trained <- get_learners(best)
# save(trained, file = 'rdata/trained.rdata')
load(file = 'rdata/trained.rdata')
models <- lapply(trained, get_final )  
features <- sapply(models, count_final_features)          

final <-  cbind(as.data.frame(best), data.frame(features = features))
# final <-  cbind(best, features = features)
final <-  as.data.frame(final)
final <-  final[ , c('class', 'thefss', 'features', 'f1')]
final$class <- factor(final$class, levels = unique(final$class))
feats <- dcast(final, class ~ thefss, value.var = 'features')
fone <- dcast(final, class ~ thefss, value.var = 'f1')
fone <- fone[, -1, drop = FALSE] 
fone[, ] <- color_fone(as.vector(as.matrix(fone)))
fone[fone == ''] <- 'red'
feats[is.na(feats)] <- ''
feats[, -1] <- add_color(fone, feats[, -1])
# feats[, 2] <- NULL 
cols <- colnames(feats) 
cols <- cols[c(1, 3:8, 2, 9)]
feats  <- feats[ , cols]
feats   

label <- paste0('tbl:selection-overview') 
caption = 'Number of selected morphometrics with the different methods. The color indicates the best F-measure obtained with the corresponding feature selection method.\ 
Best F-measure $\\geq$ 0.75 are shown in green; best F-measure $\\geq$ 0.60 in orange; and the rest in red. \
CART and RMLR refer to the embedded feature selection performed by those models. Filter feature selection followed by embedded selection is denoted with a +, e.g.,\
KW followed by CART is denoted with KW + CART.\ CART and RMLR are only considered in absence of prior sampling. There are no entries for RF BVI + RMLR for the BTC\ 
as RMLR could not be fit due to too few features being selected by the RF BVI.'
xtable_print(feats, label = label,  caption = caption,             
             include.rownames = FALSE, size = 'normalsize',  file = 'paper/selection-overview.tex')           

# Find best with KW and without 
b <- lapply(groups, get_aggr_results )  
all <- join_results(b, groups) 
allkw <- subset(all, fss == 'kw')
allkw <- allkw %>% dplyr::group_by(class)  
bestkw <- top_n(allkw, 1, f1)  

allnkw <- subset(all, fss != 'kw')  
allnkw <- allnkw %>% dplyr::group_by(class)  
bestnkw <- top_n(allnkw, 1, f1) 

diff <- bestkw$f1  - bestnkw$f1  
names(diff) <- groups
ordi <- order(-diff, decreasing = TRUE)
diff <- diff[ordi ]
diff[diff < -0.03] 
diff

# ================================================================================================================================================================================ #
# RF VIs 
# ================================================================================================================================================================================ #   
grps <- c('nbc', 'ba') 
grps <- setNames(grps, grps)
vi <- lapply(grps, get_vi, dataset, ntree = 20000, type = NULL)
viki <- lapply(vi, subset, importance >= 0.01)  
counts <- sapply(viki, nrow) 
table <- paste_columns(viki)  
viki_store <- viki

addtorow <- list()
addtorow$pos <- list(-1)
addtorow$command <- '\\hline \\multicolumn{2}{c}{NBC} & \\multicolumn{2}{c}{BA} \\\\ \n'
label <- 'tbl:rf-vi'
table <- format_vars_tbl(table) 

counts <- unlist(lapply(lapply(counts, c, 'Total'), rev))
dim(counts) <- c(1, length(counts))
counts <- as.data.frame(counts)
colnames(counts) <- colnames(table)
counts[c(2,4)] <- apply(counts[c(2,4)], 2, as.integer)

table[, c(2,4)] <- round(table[, c(2,4)], 2)
table[, c(2)] <- sprintf('%.2f', table[, c(2)])
table[, c(4)] <- sprintf('%.2f', table[, c(4)])
table[table == 'NA'] <- NA
table <- rbind(table, counts)  

colnames(table) <- gsub('importance', 'BVI', colnames(table))
# colnames(table) <- gsub('variable', 'morphometric', colnames(table))
# ind_morpho <- grep('morphometric', colnames(table))
# table <- verbize(table, ind_morpho)
# caption <- 'Minority class RF variable importance. Shows only the morphometrics with VI $\\geq$ 0.01. The VI can loosely be interpreted as the morphometric\'s effect on specificity, i.e., the accuracy of the minority (positive) class; e.g., randomly permuting \\texttt{y\\_sd\\_abs} decreases the accuracy of NBC cells by 7 percent.'
# , whereas for BASKET shows features kept with the \\textbf{XYZ} method
# caption <- 'RF variable importance. Shows only the morphometrics with VI $\\geq$ 0.01. The VI can loosely be interpreted as the morphometric\'s effect on accuracy, e.g., randomly permuting \\texttt{node root dist.max} decreases accuracy by 3\\% when classifying NBC cells versus rest.' 
caption <- 'Morphometrics with RF BVI $\\geq$ 0.01 for the NBC and BA types.'
xtable_print(table, label = label, caption = caption,
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/rf-vi.tex', floating = TRUE,
             add.to.row = addtorow, hline.after = c(0,nrow(table) - 1, nrow(table)))
rfbva_table <- table
# But class-specific is problematic: higher values for types with less instances!!!

# RF is not so much affected by mtry; the values are bigger though 

grps <- c('dbc', 'sbc', 'lbc') 
grps <- setNames(grps, grps)
# vi <- lapply(grps , get_kruskal_vi, dataset, ntree = 20000)
vi <- lapply(grps, get_vi, dataset, ntree = 20000, type = NULL)
viki <- lapply(vi, subset, importance >= 0.01)
# viki[[1]] <- NULL 
counts <- sapply(viki, nrow) 
table <- paste_columns(viki) 
viki_store2 <- viki  

counts <- unlist(lapply(lapply(counts, c, 'Total'), rev))
dim(counts) <- c(1, length(counts))
counts <- as.data.frame(counts)
colnames(counts) <- colnames(table)
counts[c(2,4)] <- apply(counts[c(2,4)], 2, as.integer)

table[, c(2,4,6)] <- round(table[, c(2,4,6)], 2)
table[, c(2)] <- sprintf('%.2f', table[, c(2)])
table[, c(4)] <- sprintf('%.2f', table[, c(4)])
table[, c(6)] <- sprintf('%.2f', table[, c(6)])
table[table == 'NA'] <- NA
table <- rbind(table, counts)  

addtorow <- list()
addtorow$pos <- list(-1) 
addtorow$command <- '\\hline \\multicolumn{2}{c}{DBC} & \\multicolumn{2}{c}{SBC} & \\multicolumn{2}{c}{LBC} \\\\ \n'
label <- 'tbl:rf-vi2'
table <- format_vars_tbl(table)
colnames(table) <- gsub('importance', 'BVI', colnames(table))
# colnames(table) <- gsub('variable', 'morphometric', colnames(table))
# ind_morpho <- grep('morphometric', colnames(table))
# table <- verbize(table, ind_morpho)
# caption <- 'Minority class RF variable importance. Shows only the morphometrics with VI $\\geq$ 0.01. The VI can loosely be interpreted as the morphometric\'s effect on specificity, i.e., the accuracy of the minority (positive) class; e.g., randomly permuting \\texttt{y\\_sd\\_abs} decreases the accuracy of NBC cells by 7 percent.'
# , whereas for BASKET shows features kept with the \\textbf{XYZ} method
# caption <- 'RF variable importance. Shows only the morphometrics with VI $\\geq$ 0.01. The VI can loosely be interpreted as the morphometric\'s effect on accuracy, e.g., randomly permuting \\texttt{node root dist.max} decreases accuracy by 3\\% when classifying NBC cells versus rest.'

caption <- 'Morphometrics with RF BVI $\\geq$ 0.01 for the DBC, SBC, and LBC types.'
xtable_print(table, label = label, caption = caption,
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/rf-vi2.tex', floating = TRUE,
             add.to.row = addtorow, hline.after = c(0,nrow(table) - 1,nrow(table)))
# But class-specific is problematic: higher values for types with less instances!!!

# ================================================================================================================================================================================ #
# Betas
# ================================================================================================================================================================================ #  
# gure <- train(trained[[41]]$learner, taskify(dbs[[2]])) 
# gure <- train(trained[[41]]$learner, taskify(standardize(dbs[[2]]))) 
# gure$learner.model$next.model$features
# gure$learner.model$next.model$learner.model
# get_betas(gure$learner.model$next.model$learner.model, s = 0.01) 

grps <- c('mc', 'ba') 
grps <- setNames(grps, grps)
dbs <- lapply(class_groups[grps], convert2binary, dataset)
betas <- lapply(dbs, get_kruskal_betas)

# Get BA RF BVI + RMLR 
vars <- c(as.character(viki_store[['ba']]$variable), 'class') 
mod <- get_mlr_model(mods[['RMLR']], convert2binary(class_groups[['ba']], dataset[, vars]))    
betas$ba_rfbi <- get_betas(mod, s = 0.01)  

keep_betas <- betas
keep_betas$ba <- NULL 
counts <- sapply(betas, nrow)
# betas <- lapply(betas, '[', 1:8, TRUE)  

table <- paste_columns(betas)         
colnames(table) <- gsub('variable', 'morphometric', colnames(table))
colnames(table) <- gsub('beta', '$\\\\beta$', colnames(table))
ind_morpho <- grep('morphometric', colnames(table)) 
table <- verbize(table, ind_morpho)     
counts <- unlist(lapply(lapply(counts, c, 'Total'), rev))
dim(counts) <- c(1, length(counts))
counts <- as.data.frame(counts)
colnames(counts) <- colnames(table)
counts[c(2,4,6)] <- apply(counts[c(2,4,6)], 2, as.integer)
table[, c(2)] <- sprintf('%.2f', table[, c(2)])
table[, c(4)] <- sprintf('%.2f', table[, c(4)])
table[is.na(table)] <- ''
table[, 6] <- paste0('\\num[round-precision=2,round-mode=figures,scientific-notation=true]{', table[, 6], '}')
# table <- rbind(table, counts)
# table[table == 'NA'] <- '' 
full_rmlr_table <- table
save(full_rmlr_table, file = 'rdata/full_rmlr_table.rdata' )

addtorow <- list()
addtorow$pos <- list(-1)
# addtorow$command <- '\\hline \\multicolumn{2}{c}{MC} & \\multicolumn{2}{c}{BA} & \\multicolumn{2}{c}{BA (RF BVI)} \\\\ \n'    
addtorow$command <- '\\hline \\multicolumn{2}{c}{BA} \\\\ \n'
table[, 1:4] <- NULL
table <- table[1:5, ]
label <- paste0('tbl:bolasso-betas')
file <- paste0('paper/bolasso-betas.tex')
# caption <- 'First and second column: eight morphometrics with highest magnitude of $\\coef$ coefficients for the KW + RMLR. \
# Third column: the full RMLR model for BA after RF BVI feature selection.
# The $\\coef$ were estimated from the standardized data set, after feature selection with KW (RF BVI for the third column). Note that the rightmost column contains a complete \\
# logistic regression model for the BA type.'   
caption <- 'The logistic regression model for BA.\
The $\\coef$ were estimated from the standardized data set, after feature selection with RF BVI. Interpretation is straightforward; for example, according to the model, \ 
a $7.33^{\\circ}$ increase in the average bifurcation angle of a cell reduce the log-odds of BA by 0.21 \
($7.33^{\\circ}$ was the standard deviation of \\texttt{remote\\_bifurcation\\_angle.avg}).' 
xtable_print(table, label = label,  caption = caption,              
             include.rownames = FALSE, size = 'footnotesize', file = file, sanitize.column.names = NULL,  
             add.to.row = addtorow, hline.after = c(0, nrow(table)))    

rfbva_table  <- rfbva_table[1:6 , 3:4] 
table <- rbind(table, c('', ''))
# tableba <- cbind(rfbva_table, table)

# xtable_print(tableba, label = 'tbl:ba-rfbvi',  caption = '',              
#              include.rownames = FALSE, size = 'footnotesize', file = 'paper/ba-rfbvi.tex', sanitize.column.names = NULL, 
#              hline.after = c(0, nrow(tableba)), floating = FALSE )      

xtable_print(table, label = 'tbl:ba-rfbvi',  caption = '',
             include.rownames = FALSE, size = 'footnotesize', file = 'paper/ba-rfbvi.tex', sanitize.column.names = NULL,
             hline.after = c(0, nrow(table)), floating = FALSE )

#  Training set performance  
# vars <- lapply(betas, '[[', 'variable') 
# perf <- mapply(function(vars, db) evaluate_training(vars, mods[['RMLR']], db), vars, dbs)
# perf      

# ================================================================================================================================================================================ #
# Basket features plotting 
# ================================================================================================================================================================================ #  
db <- convert2binary(class_groups[['ba']], dataset)
varsrf <- viki_store$ba[, 'variable']
varsrf <- droplevels(varsrf)
varsrf <- as.character(varsrf)
varsrmlr <- as.character(keep_betas$ba_rfbi$variable)
# a <- plot_2d(c("path_dist.avg", "remote_bifurcation_angle.avg"), db) 
rf_pca <- plot_pca(varsrf, db, legend = FALSE, omit_vars = "path_dist.sd" )  
# rmlr_pca <- plot_pca(varsrmlr[1:10], db, legend = FALSE)
# d <- plot_2d(c("l1_width", "tips_max"), db, legend = FALSE) 
# d <- plot_2d(c("l1_width", "length.sd"), db, legend = FALSE)
# b <- b + theme(plot.margin = unit(c(0,0,0,0), "cm")) 
# d <- plot_2d(c('remote_bifurcation_angle.avg', 'remote_bifurcation_angle.sd'), db, legend = FALSE)  
# a <- plot_2d(c("uw_width", "remote_bifurcation_angle.avg"), db, legend = FALSE) 
dbasket <- get_binary_kruskal('ba', dataset) 
dbasket <- subset(dbasket, pvalue < 0.05 )  

varsall <- as.character(dbasket$variable)
vars <- get_non_redundant(varsall, dataset)    
# length(varsall)
# length(vars)
kw_ten_pca <- plot_pca(vars[1:10], db, legend = TRUE)  
# ymean_dend <- plot_2d(c('y_mean_abs', 'd.N_bifurcations'), db, legend = TRUE)
ymean_dend <- plot_2d(c('remote_bifurcation_angle.avg', 'path_dist.avg'), db, legend = FALSE)
ymean_dend <- ymean_dend + xlab('remote_bifurcation_angle.avg (deg.)' ) + ylab(expression(paste('path_dist.avg (', mu,"m)", sep = ''))) 
rf_two <- plot_2d(varsrf[1:2], db, legend = FALSE)

rfbvival <- viki_store$ba
rfbvival$importance <-  round(rfbvival$importance, digits = 2)
bp_rmlr <- plot_box_kw(varsrf, db, rfbvival, scientific = FALSE) 
bp_rmlr <- bp_rmlr + theme(legend.position = "none") 

# First filter; then learn...
# plotLearnerPrediction(learner = mods[['RMLR']] , task = taskify(db), measures = mlr::acc, features = c('remote_bifurcation_angle.avg', 'path_dist.avg')).
# ymean_dend  + stat_function(fun=function(x){(-Intercept-Beta1*x)/Beta2},xlim=c(0,100))   

bp <- plot_box_kw(vars, db, dbasket) 
viki_store$ba$variable <- factor(viki_store$ba$variable, levels = rev(as.character(viki_store$ba$variable))) 
dplotrf <- plot_dotp(viki_store$ba) 
# babetas <- keep_betas$ba_rfbi
# colnames(babetas )[2] <- 'importance'
# plot_dotp(babetas) 

# require(gridExtra)
# lay <- cbind(c(2, 3),
#              c(1, 1))
# gs <- list(bp+ theme(axis.title =  element_blank()), dplotrf+ theme(axis.title =  element_blank()), ymean_dend + theme(axis.title =  element_blank(), legend.position = 'none'))
# grid.arrange(grobs = gs, layout_matrix = lay)

uke <- db[ ,c(vars[1:10], 'class')]  
rfplot <- plot_rfbvi(viki_store$ba)

subset(dbasket, variable == 'length.sd')
subset(dbasket, variable == 't.tortuosity.avg')
# subset(dbasket, variable == 'length.avg')
# plot_2d(c('length.sd' ,'y_std_mean_abs'), db)

cor(dataset$y_std_mean_abs, dataset$euclidean_dist.avg)
cor(dataset$path_dist.avg, dataset$euclidean_dist.avg)
corsba <- cor(dataset[, -ncol(dataset)])
akemd <- corsba['t.tortuosity.avg', ] 
sort(abs(akemd ))

dplyr::group_by(dataset, class) %>% dplyr::summarise(mean(height))
dplyr::group_by(dataset, class) %>% dplyr::summarise(sd(height))
dplyr::group_by(dataset, class) %>% dplyr::summarise(min(height))
mean(dataset$height)
dplyr::group_by(db, class) %>% dplyr::summarise(mean(height))
nbsb <- convert2binary(c('SBC', 'NBC'), dataset)
dplyr::group_by(nbsb , class) %>% dplyr::summarise(mean(height))
dplyr::group_by(nbsb , class) %>% dplyr::summarise(sd(height))
sd(db$remote_bifurcation_angle.avg)

# y: not much above soma while having lower std of length; since branch length is selected by KW (sd and average are correlated) it may suggest the branches of basket cells are 
# of a more homogeneous length.  
# it suggests that the branches of the basket cells are more homogeneous in length that those of non basket.  

# Maybe final plot the CART selection 
# plot: show exactly how these can be separated. Show with 4 vars in two dimensions. 
# plot_2d(c('y_std_mean_abs', 'd.N_bifurcations'), db)     

graphic <- paste0('paper/features-basket-rf-bvi-boxplots.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  bp_rmlr,  width = 7, height = 7, dpi = 1200) 

graphic <- paste0('paper/features-basket-scatter-vi.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  rf_two,  width = 7, height = 7, dpi = 1200) 

graphic <- paste0('paper/features-basket-scatter-vi-pca.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  rf_pca,  width = 7, height = 7, dpi = 1200)    

graphic <- paste0('paper/features-basket-scatter-kw-pca.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  kw_ten_pca,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-basket-scatter-kw-ymean-dend.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  ymean_dend,  width = 3, height = 3, dpi = 1200)

graphic <- paste0('paper/features-basket-kw-bp.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  bp,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-basket-rfbvi-bar.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  rfplot,  width = 7, height = 3, dpi = 1200)

graphic <- paste0('paper/features-basket-rf-bvi.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  dplotrf,  width = 5, height = 4, dpi = 1200) 

# \includegraphics[width=0.47\textwidth]{figure/features-basket-rf-bvi-boxplots}
# bp_rmlr
# \includegraphics[width=0.47\textwidth]{figure/features-basket-scatter-vi-pca} 
# rf_pca
# \includegraphics[width=0.47\textwidth]{figure/features-basket-kw-bp}
# bp
# \includegraphics[width=0.47\textwidth]{figure/features-basket-scatter-kw-pca}
# kw_ten_pca  
 
pdf( 'paper/features-basket-merged.pdf',  width = 14, height = 14) 
grid.arrange(bp_rmlr, rf_pca, bp, kw_ten_pca, ncol=2, nrow = 2)  
dev.off()
# system('xdg-open paper/features-basket-merged.pdf')


# ================================================================================================================================================================================ #
# MC features plotting 
# ================================================================================================================================================================================ #    
db <- convert2binary(class_groups[['mc']], dataset)
varsrmlr <- as.character(keep_betas$mc$variable) 
ind_dend <- grep('d\\.', varsrmlr)
ind_dend 
length(ind_dend ) 
varsrmlr[ind_dend] 
# rmlr_pca <- plot_pca(varsrmlr, db, legend = FALSE)
rmlr_pca <- plot_pca(varsrmlr[1:10], db, legend = FALSE)
kw_ten_pca <- plot_pca(varsrmlr[11:20], db, legend = FALSE)

dmc <- get_binary_kruskal('mc', dataset) 
dmc <- subset(dmc, pvalue < 0.05 )
kwvars <- as.character(dmc$variable) 
ind_dend <- grep('d\\.', kwvars)
ind_dend 
length(ind_dend ) 
kwvars[ind_dend] 
# kw_ten_pca <- plot_pca(kwvars[1:20], db) 
subset(dmc, variable %in% varsrmlr)

interesting <- c("path_dist.avg",  "l1_width", "y_std_mean", "d.N_stems",  "remote_bifurcation_angle.avg", "d.terminal_degree.avg", "d.insert.radial")
plot_2d(interesting[1:2], db, legend = FALSE)

interesting <-  c("path_dist.avg","y_std_mean", "remote_bifurcation_angle.avg", "l1_width", "translaminar",  "axon_origin", "terminal_degree.avg",
                  'd.displaced', 'd.total_length', 'd.N_stems', "d.centrifugal_order.avg", "d.insert.eccentricity" , "d.insert.radial" )
all(interesting %in% kwvars)
interesting[!interesting %in% varsrmlr] 

uke <- db[ ,c(interesting, 'class')] 
uke <- standardize(uke)
mm <- melt(uke, id = 'class')
bp <- ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Morphometric') + ylab('Standardized value') 
# + theme(legend.position="none")

# plot_2d(interesting[c(4, 8)], db, legend = FALSE)
# plot_2d(c("d.insert.radial", "d.vertex_type_two"), db, legend = FALSE)
# plot_2d(c("d.insert.radial", "d.N_bifurcations"), db, legend = FALSE)
# twod2 <- plot_2d(interesting[c(4, 5)], db, legend = FALSE)

vars <- get_non_redundant(kwvars, dataset)     
# bp <- plot_box_kw(vars, db, dmc) 
# interesting <- setdiff(interesting, varsrmlr[1:10])
vars <- setdiff(vars, varsrmlr[1:10])
# not cool
bp <- plot_box_kw(vars, db, dmc)  

subset(dmc, variable %in% c("axon_origin", "d.insert.radial" ))

vars <- get_non_redundant(varsrmlr, dataset)     
# 2 best RMRL 
# 10 RMLR correlation. Also say which are most relevant. point to supplementary.
# Box plot of KW interesting ones. With p-values. Dont need anything more.
# The KW top 10 actually includes all the ones included in the RMLR model. Should I strictly stick to the RMLR model?
# Order by KW or by RMLR?
# WHat was interesting for MC but not very used for others?
# Maybe show RF BVI for BA with a boxplot as well? Instead of that table. 
# Where do I put the separation with terminal degrees and angles? Use it for MC and BA and rest, three groups.
# Or maybe move some PCA to supplementary

# **Show maybe fig of MC BA and all others on 2 vars. 
# Add dimension to scat plot. 
# Not scientific for beta. 

cor(dataset$y_std_mean, dataset$l1_width)
cor(dataset$y_std_mean, dataset$path_dist.max)
cor(dataset$path_dist.avg, dataset$centrifugal_order.avg)
cor(dataset$d.total_length, dataset$translaminar)
cor(dataset$path_dist.max, dataset$translaminar)
cor(dataset$path_dist.max, dataset$remote_bifurcation_angle.avg)
cor(dataset$path_dist.max, dataset$y_std_mean)
cor(dataset$l1_width, dataset$y_std_mean)
cor <- cor(dataset[, -ncol(dataset)])
sort(abs(cor['axon_origin', ]))
cor(dataset$l1_width, dataset$y_std_mean)

# ymean_dend <- plot_2d(c('remote_bifurcation_angle.avg', 'path_dist.avg'), db, legend = FALSE)
triwise <- convert_ternary(dataset, a = c('LBC', 'SBC', 'NBC'), b = 'MC')
triwise$class <- factor(triwise$class, levels = c("BA",        "Other", "MC") )
twod <- plot_2d(c('remote_bifurcation_angle.avg', 'path_dist.avg'), triwise)
twod <- twod + xlab('remote_bifurcation_angle.avg (deg.)' ) + ylab(expression(paste('path_dist.avg (', mu,"m)", sep = '')))  
twod <- twod  + theme(legend.position = 'bottom', legend.title = element_blank())   

twod <- plot_2d(c('y_std_mean', 'remote_bifurcation_angle.avg'), db)
twod <- twod + theme(legend.position="bottom", legend.title = element_blank())
twod 

rfbvival <- keep_betas$mc
rfbvival$beta <-  round(rfbvival$beta, digits = 2)
bp_rmlr <- plot_box_kw(varsrmlr, db, rfbvival, scientific = FALSE) 

graphic <- paste0('paper/features-mc-scatter-bp-rmlr.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic, bp_rmlr ,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-mc-scatter-rmlr-pca.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  rmlr_pca,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-mc-scatter-2d.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  twod,  width = 7, height = 7, dpi = 1200) 

graphic <- paste0('paper/features-mc-scatter-2d2.pdf')
stopifnot(file.remove(graphic))
# ggsave( graphic,  twod2,  width = 4, height = 4, dpi = 1200)
ggsave( graphic,  bp,  width = 7, height = 7, dpi = 1200)

# \includegraphics[width=0.47\textwidth]{figure/features-mc-scatter-bp-rmlr} 
# bp_rmlr 
# \includegraphics[width=0.47\textwidth]{figure/features-mc-scatter-2d}
# twod


pdf( 'paper/features-mc-merged.pdf',  width = 14, height = 7) 

grid.arrange(bp_rmlr, twod,  ncol=2, nrow = 1)  
dev.off()
# system('xdg-open paper/features-mc-merged.pdf')

# ================================================================================================================================================================================ #
# NBC features plotting 
# ================================================================================================================================================================================ #     
db <- convert2binary(class_groups[['nbc']], dataset) 
varsrf <- viki_store$nbc[, 'variable']
varsrf <- as.character(varsrf)
rf_pca <- plot_pca(varsrf, db, legend = TRUE, omit_vars = c("path_dist.sd", 'euclidean_dist.max', 'euclidean_dist.max', "path_dist.max", "euclidean_dist.sd", 'y_sd' ))
rf_pca  

rfbvival <- viki_store$nbc
rfbvival$importance <-  round(rfbvival$importance, digits = 2)
bp_rmlr <- plot_box_kw(varsrf, db, rfbvival, scientific = FALSE) 
# bp_rmlr <- bp_rmlr + theme(legend.position = 'none') 

dnbc <- get_binary_kruskal('nbc', dataset) 
dmc <- subset(dnbc, pvalue < 0.05 )
kwvars <- as.character(dmc$variable) 
ind_dend <- grep('d\\.', kwvars)
ind_dend 
length(ind_dend) 
kwvars[ind_dend]   
kw_ten_pca <- plot_pca(kwvars[1:15], db, legend = FALSE)

interesting <- subset(dmc, variable %in% varsrf)
interesting <- as.character(interesting$variable)
# interesting <- interesting[c(1:2, 6:9)] 
kwvars_uniq  <- get_non_redundant(kwvars, dataset)
kwvars_uniq  <- setdiff(kwvars_uniq, varsrf)
kwvars_uniq  
# kwvars_uniq  <- kwvars_uniq[-grep('^d\\.', kwvars_uniq  )]
bp <- plot_box_kw(kwvars_uniq, db, dmc) 
# bp <- plot_box_kw(interesting, db, dmc)   

 # "l1_prob",
interesting <- c("terminal_degree.avg", "t.length.med", "d.insert.radial",  "eccentricity", "vertex_ratio")

# uke <- db[ ,c(interesting, 'class')] 
# uke <- standardize(uke)
# mm <- melt(uke, id = 'class')
# bp <- ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Morphometric') + ylab('Standardized value') 
# + theme(legend.position="none")

cor(dataset$terminal_degree.avg, dataset$euclidean_dist.max)
cor(dataset$terminal_degree.avg, dataset$translaminar)
cor(dataset$terminal_degree.avg, dataset$translaminar)
cor(dataset$terminal_degree.avg, dataset$eccentricity)
# cor(dataset$terminal_degree.avg, dataset$N_bifurcations)
plot_2d(c('terminal_degree.avg', 'path_dist.avg'), db)
plot_2d(c('terminal_degree.avg', 'translaminar'), db)
plot_2d(c('terminal_degree.avg', 'path_dist.max'), db)
plot_2d(c('terminal_degree.avg', 'eccentricity'), db)
plot_2d(c('terminal_degree.avg', 'partition_asymmetry.avg'), db)
plot_2d(c('terminal_degree.avg', 'height'), db)


twod <- plot_2d(c('euclidean_dist.max', 'terminal_degree.avg'), db, legend = FALSE)
setdiff(colnames(dataset), kwvars)

graphic <- paste0('paper/features-nbc-scatter-bp-rfbvi.pdf') 
stopifnot(file.remove(graphic))
ggsave( graphic,  bp_rmlr,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-nbc-scatter-rf-bvi.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  rf_pca,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-nbc-scatter-kw-pca.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic, kw_ten_pca ,  width = 7, height = 7, dpi = 1200)  

graphic <- paste0('paper/features-nbc-scatter-2d.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic, twod,  width = 7, height = 7, dpi = 1200)  

graphic <- paste0('paper/features-nbc-scatter-boxplot.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic, bp ,  width = 7, height = 7, dpi = 1200)  


# \includegraphics[width=0.47\textwidth]{figure/features-nbc-scatter-bp-rfbvi}
# bp_rmlr
# \includegraphics[width=0.47\textwidth]{figure/features-nbc-scatter-rf-bvi} 
# rf_pca 

pdf( 'paper/features-nbc-merged.pdf',  width = 14, height = 7) 
grid.arrange(bp_rmlr, rf_pca,  ncol=2, nrow = 1)  
dev.off()  
# system('xdg-open paper/features-nbc-merged.pdf')

# ================================================================================================================================================================================ #
# DBC features plotting 
# ================================================================================================================================================================================ #      
subset(all_best_fsel , class == 'DBC')
# RF BVI 0.72 and KW .70.

class <- 'dbc'
db <- convert2binary(class_groups[[class]], dataset) 
varsrf <- viki_store2$dbc[, 'variable']
varsrf <- as.character(varsrf)
dbc_pca <- plot_pca(varsrf, db, legend = TRUE)

dnbc <- get_binary_kruskal(class, dataset) 
dmc <- subset(dnbc, pvalue < 0.05 )
kwvars <- as.character(dmc$variable) 
ind_dend <- grep('d\\.', kwvars)
ind_dend 
length(ind_dend) 
kwvars[ind_dend]   
kw_ten_pca <- plot_pca(kwvars[1:15], db)

rfbvival <- viki_store2$dbc
rfbvival$importance <-  round(rfbvival$importance, digits = 2)
bp_rmlr <- plot_box_kw(varsrf, db, rfbvival, scientific = FALSE) 
dbc_pca <- bp_rmlr 

uke <- db[ ,c("eccentricity" , "radial", 'd.radial', 'd.insert.radial', 'width', "t.remote_bifurcation_angle.avg", 'centrifugal_order.max', 'class')] 
uke <- standardize(uke)
mm <- melt(uke, id = 'class')
dbc_bp <- ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Morphometric') + ylab('Standardized value') 

uke <- db[ ,c(kwvars[1:15], 'class')] 
uke <- standardize(uke)
mm <- melt(uke, id = 'class')
dbc_bp <- ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Morphometric') + ylab('Standardized value') 

kwvars_uniq <- get_non_redundant(kwvars, dataset)
kwvars_uniq <- setdiff(kwvars_uniq, varsrf ) 
bp <- plot_box_kw(kwvars_uniq, db, dmc) 
dbc_bp  <- bp
# plot_2d(c('d.radial', 'total_length'), db)

# ================================================================================================================================================================================ #
# SBC features plotting 
# ================================================================================================================================================================================ #       
subset(all_best_fsel , class == 'SBC')
# RF BVI 0.73 and KW .67.

class <- 'sbc'
db <- convert2binary(class_groups[[class]], dataset) 
varsrf <- viki_store2$sbc[, 'variable']
varsrf <- as.character(varsrf)
sbc_pca <- plot_pca(varsrf, db, legend = TRUE)

dnbc <- get_binary_kruskal(class, dataset) 
dmc <- subset(dnbc, pvalue < 0.05 )
kwvars <- as.character(dmc$variable) 
ind_dend <- grep('d\\.', kwvars)
ind_dend 
length(ind_dend) 
kwvars[ind_dend]   
kw_ten_pca <- plot_pca(kwvars[1:15], db)

uke <- db[ ,c(kwvars[1:15], 'class')] 
uke <- standardize(uke)
mm <- melt(uke, id = 'class')
sbc_bp <- ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Morphometric') + ylab('Standardized value') 

rfbvival <- viki_store2$sbc
rfbvival$importance <-  round(rfbvival$importance, digits = 2)
bp_rmlr <- plot_box_kw(varsrf, db, rfbvival, scientific = FALSE) 
sbc_pca <- bp_rmlr 

kwvars_uniq <- get_non_redundant(kwvars, dataset)
kwvars_uniq <- setdiff(kwvars_uniq, varsrf ) 
bp <- plot_box_kw(kwvars_uniq, db, dmc) 

# ================================================================================================================================================================================ #
# LBC features plotting 
# ================================================================================================================================================================================ #      7 
subset(all_best_fsel , class == 'LBC')
# RF BVI 0.66, for KW it is 0.62

class <- 'lbc'
db <- convert2binary(class_groups[[class]], dataset) 
varsrf <- viki_store2$lbc[, 'variable']
varsrf <- as.character(varsrf)
lbc_pca <- plot_pca(varsrf, db, legend = TRUE)

rfbvival <- viki_store2$lbc
rfbvival$importance <-  round(rfbvival$importance, digits = 2)
bp_rmlr <- plot_box_kw(varsrf, db, rfbvival, scientific = FALSE) 
lbc_pca <- bp_rmlr  

dnbc <- get_binary_kruskal(class, dataset) 
dmc <- subset(dnbc, pvalue < 0.05 )  

kwvars <- as.character(dmc$variable) 
min(dmc$pvalue)
ind_dend <- grep('d\\.', kwvars)
ind_dend 
length(ind_dend) 
kwvars[ind_dend]   
kw_ten_pca <- plot_pca(kwvars[1:15], db) 

uke <- db[ ,c(kwvars[1:15], 'class')] 
uke <- standardize(uke)
mm <- melt(uke, id = 'class')
lbc_bp <- ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Morphometric') + ylab('Standardized value') 

kwvars_uniq <- get_non_redundant(kwvars, dataset)
kwvars_uniq <- setdiff(kwvars_uniq, varsrf ) 
bp <- plot_box_kw(kwvars_uniq, db, dmc)   

graphic <- paste0('paper/features-dbc-scatter-rf-bvi.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  dbc_pca,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-sbc-scatter-rf-bvi.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  sbc_pca,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-lbc-scatter-rf-bvi.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  lbc_pca,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-dbc-scatter-boxplot.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  dbc_bp,  width = 7, height = 7, dpi = 1200)

graphic <- paste0('paper/features-sbc-scatter-boxplot.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  sbc_bp,  width = 7, height = 7, dpi = 1200) 

graphic <- paste0('paper/features-lbc-scatter-boxplot.pdf')
stopifnot(file.remove(graphic))
ggsave( graphic,  lbc_bp,  width = 7, height = 7, dpi = 1200)

# # \includegraphics[width=0.47\textwidth]{figure/features-dbc-scatter-rf-bvi}
# dbc_pca
# # \includegraphics[width=0.49\textwidth]{figure/features-dbc-scatter-boxplot}
# dbc_bp
# # \includegraphics[width=0.47\textwidth]{figure/features-sbc-scatter-rf-bvi}
# sbc_pca
# # \includegraphics[width=0.47\textwidth]{figure/features-lbc-scatter-rf-bvi}
# lbc_pca  
 

pdf( 'paper/features-rest-merged.pdf',  width = 14, height = 14) 
grid.arrange(dbc_pca, dbc_bp, sbc_pca, lbc_pca,  ncol=2, nrow = 2)  
dev.off()   
# system('xdg-open paper/features-rest-merged.pdf')

# ================================================================================================================================================================================ #
# ChC
# ================================================================================================================================================================================ #    
dnbc <- get_binary_kruskal('chc', dataset) 
dmc <- subset(dnbc, pvalue < 0.05 )
kwvars <- as.character(dmc$variable) 
kwvars 

# ================================================================================================================================================================================ #
# CART models
# ================================================================================================================================================================================ #    
mod <- get_binary_kruskal_learner('MC', 'CART', dataset)
stopifnot(file.remove('paper/model-cart-mc.pdf') )
pdf(file = 'paper/model-cart-mc.pdf', width = 12 * 1.2, height = 6 * 1.5)
rpart.plot::rpart.plot(mod$learner.model, tweak = 1.4, fallen.leaves = FALSE)
dev.off()

vars <- c(as.character(viki_store[['ba']]$variable), 'class') 
mod <- get_mlr_model(mods[['CART']], convert2binary(class_groups[['ba']], dataset[, vars]))   
stopifnot(file.remove('paper/model-cart-ba.pdf'))
pdf(file = 'paper/model-cart-ba.pdf', width = 12 * 1.2, height = 6 * 1.5)
rpart.plot::rpart.plot(mod, tweak = 1.4, fallen.leaves = FALSE)   
dev.off() 

# mod <- get_binary_kruskal_learner('SBC', 'CART', dataset)
# stopifnot(file.remove('paper/model-cart-sbc.pdf'))
# pdf(file = 'paper/model-cart-sbc.pdf', width = 4, height = 2)
# rpart.plot::rpart.plot(mod$learner.model)
# dev.off()      

# ================================================================================================================================================================================ #
# CART models
# ================================================================================================================================================================================ #    
mod <- get_binary_kruskal_learner('MC', 'CART', dataset)
stopifnot(file.remove('paper/model-cart-mc.pdf')  )
pdf(file = 'paper/model-cart-mc.pdf', width = 12 * 1.2, height = 6 * 1.5)
rpart.plot::rpart.plot(mod$learner.model, tweak = 1.4, fallen.leaves = FALSE)
dev.off()  

vars <- c(as.character(viki_store[['ba']]$variable), 'class') 
mod <- get_mlr_model(mods[['CART']], convert2binary(class_groups[['ba']], dataset[, vars]))   
stopifnot(file.remove('paper/model-cart-ba.pdf') )
pdf(file = 'paper/model-cart-ba.pdf', width = 12 * 1.2, height = 6 * 1.5)
rpart.plot::rpart.plot(mod, tweak = 1.4, fallen.leaves = FALSE)   
dev.off() 

mod <- get_binary_kruskal_learner('SBC', 'CART', dataset)
stopifnot(file.remove('paper/model-cart-sbc.pdf'))
pdf(file = 'paper/model-cart-sbc.pdf', width = 4, height = 2)
rpart.plot::rpart.plot(mod$learner.model)
dev.off()      

# ================================================================================================================================================================================ #
# MC misclassified
# ================================================================================================================================================================================ #    
# Too slow
# source('run-mc-validate.R') 

 # common <- c("C050600B2","C260199A-I3","OG060602A2_CH7_MC_N_NB_100X_1", "RP100426-2_IDA","RP110114_L5-1_IDH" )
load(file = 'rdata/common-mc-misclassified.rdata') 
# common <- c(common, "OG060602A2_CH7_MC_N_NB_100X_1",  "TKB060508C1_CH3_MC_N_TB_100X_2")
# common[4:5] <-  common[5:4] 
ids <- common
layers <- subset(m, id %in% ids)$layer
layers <- as.character(layers )
yoffsets <- get_yoffset()[layers] - 900
# yoffsets[4] <- yoffsets[4] - 50
yoffsets[4] <- yoffsets[4] - 70
# yoffsets[6] <- yoffsets[6] - 50
# xoffsets <- c(0, 550, 450, 1300 ) + 440
# xoffsets <- c(-30, 950, 450, 1350,  500 ) + 440
xoffsets <- c(-30, 950, 450, 1700) + 440
neurons <- lapply(get_swc_paths(ids), nat::read.neuron) 
layers <- c('23', '4', '5')
ylayers <- get_yoffset()[layers ] - 900
layers <- c('1', layers) 
ylayers <- c(2000 - 900, ylayers)
tlayers <- name_layers(layers)

# stopifnot(file.remove('paper/mc-mislabelled.pdf'))
file.remove('paper/mc-mislabelled.pdf')
grDevices::pdf('paper/mc-mislabelled.pdf', width = 12)
set_grid_big(xmax = 2650, ymax = 2082 - 900)
borders <- get_layer_borders()
borders <- setdiff(borders, 0) - 900
abline(h =  borders, col = 'green', lty=2) 
# abline(h =  centers, col = 'blue') 
mapply(add_to_plot, neurons, xoffsets, yoffsets) 
text(x = xoffsets + c(0, 0, 0, 0), y = yoffsets + c(320, 550, 450, 200), ids)  
text(x = 2500, y = ylayers, tlayers)
dev.off()

subset(m, id %in% common) 
mismatch  <- get_nrn_common() 
meta_mc <- subset(mismatch, `Cell type` == 'MC' )
which(common %in% meta_mc$ID)
# missing one from L6: "TKB060508C1_CH3_MC_N_TB_100X_2" 
# neuro:::plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', common[1], '.swc'))   

load('rdata/mc-misclassified-rf.rdata')
load('rdata/mc-misclassified-rmlr.rdata')
rf <- unlist(rf_errors)
rf <- rf[rf %in% meta_mc$ID]
tbl_rf <- table(rf)
rf_misc <- data.frame(ID=names(tbl_rf), rf=unname(as.vector(tbl_rf)) )
subset(meta_mc, ID %in% rf)  

rmlr <- unlist(rmlr_errors)
rmlr <- rmlr[rmlr %in% meta_mc$ID] 
tbl_rmlr <- table(rmlr)
rmlr_misc <- data.frame(ID=names(tbl_rmlr), rmlr=unname(as.vector(tbl_rmlr)) ) 
subset(meta_mc, ID %in% rmlr)
misc <- dplyr::left_join(meta_mc, rf_misc, by='ID')
misc <- dplyr::left_join(misc, rmlr_misc, by='ID')
# misc <- subset(misc, !is.na(rf) | !is.na(rmlr))
misc$`Cell type` <- NULL
misc$DF <- NULL
misc <- misc[, c(1:3, 14:15, 4:13)]
misc[is.na(misc)] <- 0
misc$agree <- as.integer(misc$agree)
misc$rf <- as.integer(misc$rf)
misc$rmlr <- as.integer(misc$rmlr)
colnames(misc) <- gsub('^rmlr', 'RMLR', colnames(misc))
colnames(misc) <- gsub('^rf', 'RF', colnames(misc))

unch <- 42 - rowSums(misc[ ,-(1:5)]) 
misc$UN <- as.integer(unch)
misc <- misc[, -(6:15)]
colnames(misc)[3] <- 'MC'
# misc <- misc[, c(1:3, 6, 4:5)]
misc[ ,4:5] <- 30 - misc[ ,4:5]
misc[ ,4:5] <- lapply(misc[ ,4:5], as.integer)
non_mc <- 42 - rowSums(misc[, c(3,6)])
misc[['Non-MC']] <- as.integer(non_mc)
# misc <- misc[, c(1:3, 7, 4:6)] 

ginds <- sapply(c('C050600B2', 'C260199A-I3', 'C230998C-I4'), grep, misc$ID)
misc$ID[ginds] <-  gsub('(.*)', '\\\\textcolor{red}{\\1}', misc$ID[ginds])  

ginds <- sapply(c('C040600B2'), grep, misc$ID)
misc$ID[ginds] <-  gsub('(.*)', '\\\\textcolor{blue}{\\1}', misc$ID[ginds])

colnames(misc)
misc <- misc[, c(1:2, 4:5, 3, 7, 6)]

caption <- 'Classification of MC cells by the neuroscientists in \\cite{DeFelipe2013} and our two most accurate models, RF and RMLR. \
MC is the number of neuroscientists who classified the cell as MC, \
Non-MC the number of those who assigned it to another type, and \
UN the number of those who considered that the axonal morphology reconstruction was not sufficient to distinguish the type.
RF and RMLR show the number of times (out of 30) that RF and RMLR classified the cell as MC. \
Cells that were never classified as MC by both models are marked in red. \ 
Cell C040600B2, which was presented to the neuroscientists rotated upside-down, is marked in blue. \ 
ID can be used to look the neuron up at Neuromorpho.org.'
file <- 'paper/mc-validate.tex'
misc$layer <- name_layers(misc$layer)
xtable_print(misc, label = 'tbl:mc-validate', caption = caption, comment = FALSE, size = 'footnotesize', include.rownames = TRUE, file = file)   

sum("OG060602A2_CH7_MC_N_NB_100X_1" == rmlr)
sum("OG060602A2_CH7_MC_N_NB_100X_1" == rf) 
sum("TKB060508C1_CH3_MC_N_TB_100X_2" == rf)
sum("TKB060508C1_CH3_MC_N_TB_100X_2" == rmlr) 
re <- rf[which(rf %in% uke$ID)]
unique(re)
subset(meta_mc, ID %in% re)   

# C040600B2 was upside down.    

# ================================================================================================================================================================================ #
# MC with L1
# ================================================================================================================================================================================ #    
mcscs <- subset(dataset, class == 'MC')
ind_no <- mcscs$l1_prob < 0.01
# ind_no <- mcscs$l1_prob < 0.05
ind_no
sum(ind_no)
idis <- rownames(mcscs[ind_no, ])
mcsnl1 <- subset(m, id %in% idis)
mcsnl1 <- subset(mcsnl1, layer != 6) 
# idis <- mcsnl1$id
# dataset[idis, 'y_sd', drop = FALSE]
idmc <- idis [7]
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc'))

load(file = 'rdata/mc-misclassified-rf.rdata')
tabu <- table(unlist(rf_errors))
tabu[idmc]
tabu[idis]

load(file = 'rdata/mc-misclassified-rmlr.rdata')
tabu <- table(unlist(rmlr_errors))
tabu[idmc] 

l6id <- subset(m,  class == 'MC' & layer == '6')$id

mcscsl6 <- dataset[l6id , ]
dim(mcscsl6 )
indm <- which.max(mcscsl6$l1_prob ) 
mcl6 <- l6id[indm ]  
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', mcl6, '.swc'))
