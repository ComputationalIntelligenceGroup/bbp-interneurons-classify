source('r/init-paper.R') 

all_vars <- read.csv('data/full-db.csv', row.names = 1) 

# ======================
# Duplicated 
# ======================
inds <- c('OG061201A1-8_IDA', 'OG061201A3_CH1_IN_H_ZK_60X_1')
inds <- c(inds, "OG061201A1-8_IDE", "OG061201A6_CH5_BC_H_ZK_60X_1")
all_vars[inds, c('length.avg', 'total_length', 'N_bifurcations', 'd.total_length', 'd.height', 'd.width', 'd.N_stems')]     

# OG061201A1-8_IDA and OG061201A3_CH1_IN_H_ZK_60X_1 are virtually the same. 
# "OG061201A1-8_IDE" "OG061201A6_CH5_BC_H_ZK_60X_1"
par(mfrow=c(1, 2))
id <- inds[3]
neuro:::plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', id, '.swc'))  
id <- inds[4]
neuro:::plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', id, '.swc'))  
par(mfrow=c(1, 1))

# ======================
## Distance to soma  
# ======================
# Cell RP110105_L5-3_IDB > 100 um from soma. 
# - **I may affect the mean dendritic coordinate? Or not?**
# neuro:::plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', 'RP110105_L5-3_IDB', '.swc'))  
# neuro:::plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', 'RP110114_L5-1_IDE', '.swc'))  
  

# Cell RP110105_L5-3_IDB RP110114_L5-1_IDE are > 100 um from soma.
# **maybe this is not correct**
# max(all_vars$d.soma_dist)
all_vars['RP110105_L5-3_IDB', 'd.soma_dist']

# ======================
## Short
# ======================
short <- rownames(dataset)[dataset$total_length < 3000]
short 

# apical, neuromorpho
# is depth really under represented in some cases? does it affect total length?
# what if we multiplied mean branch length by the factor of depth thing. 
# or maybe mean z. would it affect? it is probably very important. 
# paper: write on tortuosity. 

load('~/code/bbp-interneurons/data/neurostrr-both.rdata')  
full_neurons <- neurostrr::convert2lm(full_neurons)       
axon <- neurostrr::filter_neurite(full_neurons)

depths <-  plyr::daply(full_neurons, ~ neuron, function(x) diff(range(x[, 'z'])))      
dataset$depth <- depths 

# ======================
# Trifurcations
# ======================
abranches <- neurostrr::filter_branches(axon)
trifs <- neurostrr::filter_trifurcations(abranches, keep = TRUE)
length(unique(trifs$neuron))
ntrifs <- trifs %>% dplyr::group_by(neuron) %>% dplyr::summarize(dplyr::n)
ntrifs <- plyr::daply(trifs, ~ neuron, nrow)
hist(ntrifs)
length(ntrifs) 


# ======================
# Extremes  
# ======================

## Height
max(dataset$height)

## Branches
  # median 27, ~ 1000 less than 3 microns, up to 1200

hist(full_neurons$length)
head(sort(full_neurons$length, decreasing = TRUE))
head(sort(full_neurons$length))
median(full_neurons$length, na.rm = TRUE)
hist(full_neurons$length[full_neurons$length < 20])
hist(axon$length[axon$length < 20])

## Segments
hist(full_neurons$node_length)
summary(full_neurons$node_length)
head(sort(full_neurons$node_length, decreasing = TRUE))

## Angles
summary(full_neurons$remote_bifurcation_angle)
hist(full_neurons$remote_bifurcation_angle)
hist(full_neurons$remote_tilt_angle)
hist(full_neurons$remote_torque_angle)

# hist(full_neurons$)

## Dendritic length
hist(dataset$d.total_length)
head(sort(dataset$d.total_length))


## Depth   
median(dataset$depth)
dimi <- dataset$total_length / dataset$depth
hist(dimi)
median(dimi)
sum(dataset$depth < 100)
zuro <- dimi > 100 &  dataset$depth < 80
which(zuro)
# sort(dimi[zuro], decreasing = TRUE) 
flat <- rownames(dataset)[zuro]
idmc <- flat[2]
neuro:::plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc')) 
# flat <- c("C050896A-I",           "MTC080301C_IDA" ,      "OG061106A1_IDA",       "SM110127A1-3_INT_IDD") 
subset(read_metadata() , id %in% flat)

# gedzo <- dataset
# prefixes <- get_prefix(rownames(gedzo))
# gedzo$class <- as.factor(prefixes)
# zado <- order(dbfu$w.Depth_avg)
# ake <- head(dbfu$w.Depth_avg[zado], n = 20)
# dke <- head(dbfu$w.Branch_pathlength_sum[zado], n = 20)
# plot(ake, dke)

# ggplot(data = gedzo, aes(x = w.Depth_avg, y = w.Branch_pathlength_sum)) + geom_point() + facet_grid( class ~ .) 

# - OG061106A1_IDA is flat and maybe that is why shorter branch length than actually; maybe that is why less length than other ChC
# - OG060921A_4_IDB is really huge but really flat: maybe these are the worst ones

# these are not true depths; they are lmeasure depths