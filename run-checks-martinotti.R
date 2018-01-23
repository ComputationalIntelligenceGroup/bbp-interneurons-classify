source('r/init-paper.R')

ind_no <- mcscs$l1_prob < 0.05
mcscsl6
mcsnl1 

cbind(mcsnl1,  height = dataset[as.character(mcsnl1$id), 'height']) 

idmc <- 'C270999B-I4' 
idmc <- 'VD101020A_INT_IDA'
path <- paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc')
plot_neuron_file(path ) 

# ============================================================
#  How how above soma is the L1?
# ============================================================   

mcscs <- subset(dataset, class == 'MC')  
get_max_y <- function(id ) {
  path <- paste0('~/code/bbp-data/data/BBP_SWC/', id, '.swc')
  # plot_neuron_file(path )
  neu <- nat:::read.neuron(path) 
  max(neu$d$Y)  
} 
ids <- rownames(mcscs)
l1s <- sapply(ids, get_max_y) 
l1s <- l1s * 1.1 + 80 # 80 to match Table 3 in supp, 1.1 to account for shrinakge
muke <- subset(m, id %in% ids)
rownames(muke) <- ids
muke$maxy <- l1s
ggplot(muke, aes(layer, maxy)) + geom_boxplot()

# mean L4 and L5 match the distances perfectly, meaning most are indeed located around the middle of the soma. 
# The L2 ones do not; seems most of them are actually located above, or there is no shrinkage, or who knows what.

# l1s <- sapply(ids, get_max_y)  
# muke$maxy <- l1s
subset(muke, layer == 5 & maxy < 1037 - 525 / 2)
id <- 'C270999B-I4' 
path <- paste0('~/code/bbp-data/data/BBP_SWC/', id, '.swc')
plot_neuron_file(path )  
dataset[id, 'l1_prob'] 

# L1 is just an estimate. It may perfectly have been located above. So, not enough to conclude label is bad.
# Mention that shrinking seems to have taken place. Add that to supplementary.
# Important: many of these have typical MC morphologies. So, it may be that they are correct.
# In which sense are these cells outliers?

# Indeed, the mean distances match the nominal ones. Only difference is L2. 

# L6 are wrong or not? Well, the models do not seem to think so. So the models include them. So, they are OK as MC cells.   
# Also, on some experts all matched them.

# ============================================================
# Are L6 typical MC? Yes, most of them. 
# ============================================================
stopifnot(identical(as.character(m$id) , rownames(db)))
dude <- db 
dude$layer <- m$layer 
interesting <- c("l1_width", "y_std_mean", "d.N_stems",  "remote_bifurcation_angle.avg")
vars <- interesting[c(2,4)]
# vars <- interesting[c(3,4)]
g <- ggplot(dude , aes_string(x = vars[1],  y = vars[2], color = "class", shape = 'layer')) + geom_point() 
g
ake <- subset(dude, class == 'MC' & layer == '6' & d.N_stems > 5)
ake[, 'd.N_stems'] 
id <- 'OG060602A2_CH7_MC_N_NB_100X_1' 
id <- 'TKB061026B1-3_IDC'
path <- paste0('~/code/bbp-data/data/BBP_SWC/', id, '.swc')
plot_neuron_file(path)

# ============================================================
  # I think my l1 prob is a little off.
# ============================================================
# I think e.g., for L4 the probs should grow more with height increase
muke$maxy
muke$id
idismc <- as.character(muke$id)
muke$l1prob <- dataset[idismc, 'l1_prob' ]

ggplot(muke, aes(l1prob, maxy)) + geom_point() + facet_wrap( ~ layer)

# ============================================================
#  Are L4 or L5 cells not reaching L1?
# ============================================================   
Yes, some I think. The ones I show in the graphic.  
If we account for shrinkage, and assume they are on top of layer, they do reach. 
Only two that barely reach, but they look as typical MCs 

mcscs <- subset(dataset, class == 'MC')
ind_no <- mcscs$l1_prob < 0.01
sum(ind_no)
idis <- rownames(mcscs[ind_no, ])
mcsnl1 <- subset(m, id %in% idis)
mcsnl1 <- subset(mcsnl1, layer != 6) 
mcsnl1 
idis <- mcsnl1$id
dataset[idis, c('height', 'l1_prob'), drop = FALSE]
idmc <- idis [4]
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc')) 

dist_l5 <- 1037.0 - 525 / 2
dist_l5 
dist_l4 <- 679.5 - 190 / 2
dist_l4 
subset(muke, layer == 5 & maxy < dist_l5 )
subset(muke, layer == 4 & maxy < dist_l4 )

idmc <- 'C061000A3' # L4
idmc <- 'C270999B-I4' # L5
plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc'))    

load(file = 'rdata/mc-misclassified-rf.rdata')
tabu <- table(unlist(rf_errors))
tabu[idmc]

load(file = 'rdata/mc-misclassified-rmlr.rdata')
tabu <- table(unlist(rmlr_errors))
tabu[idmc]  