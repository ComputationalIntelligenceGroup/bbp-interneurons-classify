source('r/init.R')

# =======================================================================================================================
# Watch out with seed for repetitions; only set before 1st repetition
# =======================================================================================================================   
rfhybrid <- get_hybrid_classifiers(mods['RF'], under_rate = 0.6, smote_rate = Inf)   
rfhybrid <-  lapply(rfhybrid, make_filtered, method = 'kruskal')
# No improvement with sampling
rmlrhybrid <- mods['RMLR']
rmlrhybrid <-  lapply(rmlrhybrid, make_filtered, method = 'kruskal')

rf_errors <- get_classifier_errors(rfhybrid, 'mc', 30) 
rf_erro <- get_common(rf_errors, 8 * 3) 
table(droplevels(dataset[rf_erro, 'class']))

rmlr_errors <- get_classifier_errors(rmlrhybrid, 'mc', 10 * 3)
rmlr_erro <- get_common(rmlr_errors, 8 * 3)
table(droplevels(dataset[rmlr_erro, 'class']))

common <- intersect(rf_erro, rmlr_erro)
# common <- rf_erro
mcids <- rownames(subset(dataset, class == 'MC'))
common <- intersect(mcids, common)
common 
# "C050600B2"         "C230998C-I4"       "C260199A-I3"       "RP110114_L5-1_IDH"

save(rf_errors, file = 'rdata/mc-misclassified-rf.rdata')
save(rmlr_errors, file = 'rdata/mc-misclassified-rmlr.rdata')
save(common, file = 'rdata/common-mc-misclassified.rdata') 