source('r/init-paper.R')           

# =================================== 
# Morpho types 
# ===================================    
mcell <- subset(m, id %in% rownames(dataset)) 
lbc <- which(mcell$full_class == 'L6_LBC')[1]
chc <- which(mcell$full_class == 'L23_ChC')[1]
dbc <- which(mcell$full_class == 'L23_DBC')[1]
btc <- which(mcell$full_class == 'L4_BTC')[1]
sbc <- which(mcell$full_class == 'L6_SBC')[1]
nbc <- which(mcell$full_class == 'L5_NBC')[1]
mc <- which(mcell$full_class == 'L6_MC')
mc <- mcell$id[mc]
mc <- intersect(mc, rownames(dataset))
ind_max <- order(dataset[mc, 'height'], decreasing = TRUE)[5]
mc <- mc[ind_max]
mc <- which(mcell$id == mc)
ids <- c(lbc, chc, dbc, btc, mc, sbc, nbc )    

mcell <- mcell[ids, ]
ids <- mcell$id
layers <- as.character(mcell$layer)
yoffsets <- get_yoffset()[layers]
yoffsets[5] <- 700
yoffsets[6] <- yoffsets[6] - 50
xoffsets <- c(0, -160, 300, 1300, 700, 1200, 1500) + 500
neurons <- lapply(get_swc_paths(ids), nat::read.neuron) 
layers <- c('23', '4', '5', '6')
ylayers <- get_yoffset()[layers ]
layers <- c('1', layers)
ylayers <- c(2000, ylayers)
tlayers <- name_layers(layers) 

grDevices::pdf('paper/m-types.pdf')
set_grid_big()
borders <- get_layer_borders()
borders <- setdiff(borders, 0)
abline(h =  borders, col = 'green', lty=2) 
# abline(h =  centers, col = 'blue') 
mapply(add_to_plot, neurons, xoffsets, yoffsets) 
text(x = xoffsets + c(-50, 0, 0, 0, 70, 0, 0), y = yoffsets + c(650, 200, 200, 400, 250, 250, 200), mcell$class) 
text(x = 2400, y = ylayers, tlayers)
dev.off()

# =================================== 
# Data
# ===================================  

classgrps <- class_groups[groups]
classgrps <- unlist(unname(classgrps))
classgrps <- classgrps[1:7]
datam <- droplevels(m)
datam$class <- factor(datam$class, classgrps) 
datam$realclass <- datam$class 
bask <- subset(datam, class %in% c("LBC", "NBC", "SBC"))
bask$class <- 'BA'
datam <- rbind(datam, bask)

graphic <- paste0('paper/data-freqs.pdf')
p <- ggplot(datam, aes(x = class, color = realclass)) + geom_bar(stat = "count") + ylim(0, max(60, max(table(datam$class)))) + labs(x = 'type') + guides(colour = FALSE)
p <- add_text_size(p, axis_title = TRUE)
# p <- p + theme(aspect.ratio = 0.8)
ggsave(graphic,  p, height = 4)
pall <- p
graphic <- paste0('paper/data-freqs-per-layer.pdf')
mplot <-  datam
mplot$layer <- name_layers(mplot$layer)
p <- ggplot(mplot, aes(x = class, color = class)) + geom_bar(stat = "count") + ylim(0, 40) + facet_grid(layer ~ . ) + labs(x = 'type') + guides(colour = FALSE) 
p <- add_text_size(p, axis_title = TRUE) 
p <- p + theme(axis.text.y = element_text(  size = 14) , strip.text.y = element_text(size = 14), axis.title = element_text(size = 14)) 
# p <- p + theme(aspect.ratio = 0.3)
ggsave(graphic, p, height = 4)  

dim <- ggplot2:::plot_dim(c(NA, 4), scale = 1, units = "in",  limitsize = TRUE)
ggsave("paper/data-freqs-merged.pdf", height = 4, width = dim[1] * 2, arrangeGrob(pall, p, ncol = 2))

# =================================== 
# NRN misclassified
# ===================================  
mismatch  <- get_nrn_common() 
unch <- 42 - rowSums(mismatch[ ,-(1:5)]) 
mismatch$UN <- as.integer(unch)
mismatch <- subset(mismatch, agree < 13)  
mismatch$layer <- gsub('23', '2/3', mismatch$layer) 

ginds <- sapply('C040600B2', grep, mismatch$ID)
mismatch$ID <- as.character(mismatch$ID )
mismatch$ID[ginds] <-  gsub('(.*)', '\\\\textcolor{blue}{\\1}', mismatch$ID[ginds])
dfcol <- colnames(mismatch)[-(1:5)]
dfcol 

colns <- function(x) { 
  ind_col <- x %in% dfcol   
  x <- sanitize_colnames(x)
  x[ind_col ] <- paste0(' \\textbf{', x[ind_col], '}') 
  print(x)
  x
}


caption <- 'Disagreement with our class labels by 42 neuroscientists who participated in \\cite{DeFelipe2013}. Cell type is the label in our data, given according to the classification scheme from \\cite{Markram2004} \ 
while DF (standing for DeFelipe) is the majority label chosen by the neuroscientists, according to the scheme from \\cite{DeFelipe2013}. Agree is the number of neuroscientists that coincided with our label, \
while columns to the right show the number of neuroscientists who selected the corresponding DF label (all shown in boldface): AR -  arcade; CB - common basket; CR - Cajal-Retzius; CT - common type; HT - horse-tail; OT - other; \ 
UN - uncharacterized, meaning that the axonal morphology reconstruction was not sufficient to distinguish the type. \
The table shows eight out of the 20 interneurons which were classified as ChC, MC, or NGC ---the three types common to both classification schemes--- in our data yet differently by the majority of neuroscientists (column DF); \
for the remaining twelve interneurons, the neuroscientists\' majority label matched ours. \
Cell C040600B2, which was presented to the neuroscientists rotated upside-down, is marked in blue. \
ID can be used to look the neuron up at Neuromorpho.org.'
file <- 'paper/nrn-disagreement.tex'
xtable_print(mismatch, label = 'tbl:nrn_disagreement', caption = caption, comment = FALSE, size = 'footnotesize', include.rownames = TRUE, file = file,
             sanitize.colnames.function = colns)  

ids <- c('C050600B2', 'C091000D-I3', 'C150600B-I1')
stopifnot(all(ids %in% mismatch$ID))         

plot_disagree_cells <- function() {  
  thick <- sum(get_layers_thickness()$thickness) - 700 + 200
  set_grid_big(1100 + 300, thick) 
  n1 <- nat::read.neuron(paste0('~/code/bbp-data/data/BBP_SWC/', ids[1], '.swc'))
  n2 <- nat::read.neuron(paste0('~/code/bbp-data/data/BBP_SWC/', ids[2], '.swc'))   
  n3 <- nat::read.neuron(paste0('~/code/bbp-data/data/BBP_SWC/', ids[3], '.swc'))   
  neurons <- list(n1, n2, n3)
  xoffsets <- c(300, 560 + 70, 950) + 80 
  yoffsets <- c(950 + 200, 400, 950 + 20 )   
  mapply(add_to_plot, neurons, xoffsets, yoffsets) 
  
  borders <- get_layer_borders() - 700 + 200 
  abline(h =  borders, col = 'green', lty=2)  
  
  ylayers <- get_yoffset()[c('23', '4', '5') ] - 700   + 200
  tlayers <- name_layers(c('23', '4', '5'))
  text(x = 1300, y = ylayers, tlayers, cex = 3.5)
  text(x = 1300, y = 100, 'L6', cex = 3.5)
  text(x = 1300, y = thick - get_layers_thickness()$thickness[1] / 2, 'L1', cex = 3.5) 
  text(x = xoffsets + c(0, 0, -50), y = yoffsets + c(360, 250, 550), ids, cex = 3)  
  # text(x = 500, y = ylayers, tlayers) 
  # abline(h =  borders, col = 'green', lty=2)  
}

pdf('paper/nrn-disagree-cells.pdf', width = 14.0, height = 15.82) 
plot_disagree_cells() 
dev.off()

pdf('paper/reconstruction-differences.pdf', width = 7, height = 7) 
# ggplot(data = dataset_prefix, aes(x = compartment_length.avg, y = diameter.avg, color = class)) + geom_point() + facet_grid( type ~ .) + labs(x = 'compartment length (avg)' , y = 'diameter (avg)')
# g <- ggplot(data = dataset_prefix, aes(x = compartment_length.avg, y = diameter.avg)) + geom_point() + facet_grid( ~ class ) + labs(x = 'compartment length (avg)' , y = 'diameter (avg)', color = 'prefix')
# g <- add_text_size(g, axis_title = TRUE)
# Double size for merged graphic
g <- ggplot(data = dataset_prefix, aes(x = compartment_length.avg, y = diameter.avg)) + geom_point(size = 4) + facet_grid( ~ class ) + labs(x = 'compartment length (avg)' , y = 'diameter (avg)', color = 'prefix')
g <- g + theme(axis.text = element_text(size = 28), strip.text.y = element_text(size = 28), strip.text.x = element_text(size = 28), legend.text=element_text(size=28), axis.title = element_text(size = 28))  
g
dev.off()        


pdf('paper/reconstruction-issues-merged.pdf', width = 28, height = 15.82 )  
par(mfrow=c(1, 2)) 
plot_disagree_cells()   
plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values  
print(g,vp = vp1)        ## suggested by @b  
dev.off()         
# system('xdg-open paper/reconstruction-issues-merged.pdf')

# =================================== 
# Classifiers 
# ===================================  
classifiers <- read.csv('csv/classifiers.csv', stringsAsFactors = FALSE)   
# get caption before filter rows 
caption <- classifiers$caption[[1]]
classifiers$caption <- NULL

classifiers  <- subset(classifiers, Abbreviation %in% list_selected_classifiers()) 
classifiers$included <- NULL  
classifiers$Comment <- NULL  
classifiers$R_Package <- paste0('\\pkg{', classifiers$R_Package, '}')
classifiers$R_Package <- paste0(classifiers$R_Package, ' ',  classifiers$R_Package_reference) 
classifiers$R_Package_reference <- NULL
# classifiers$parameters <- NULL
classifiers$Reference <- NULL 
classifiers$grid_parameters <- NULL 
indpp <- match('prespecified_parameters', colnames(classifiers))
colnames(classifiers)[indpp] <- 'prespecified_Parameters' 
classifiers$Classifier[2] <- '$k$ nearest neighbors' 

xtable_print(classifiers, caption = caption,  label = 'tbl:classifiers', 
             include.rownames = FALSE,  size = 'footnotesize',  file = 'paper/classifiers.tex', 
             # floating = FALSE)  
             floating = TRUE)      

# =================================== 
# Other methods
# =================================== 

fss <- read.csv('csv/other-methods.csv')   
fss  <- subset(fss, is.na(included))
fss$included <- NULL  
fss$R_Package <- paste0('\\pkg{', fss$R_Package, '}')
fss$R_Package <- paste0(fss$R_Package, ' ',  fss$R_Package_reference ) 
fss$Reference <- NULL
fss$R_Package_reference <- NULL
colnames(fss)[4] <- 'Parameters'
fss[[1]] <- as.character(fss[[1]])
fss$Method[1] <- 'KW'
# fss[[5]] <- NULL
fss[[2]] <- NULL
# strg <- capture.output( xtable_print(fss, caption = 'Other parameters.', label = 'tbl:other-parameters', include.rownames = FALSE, size = 'footnotesize',
#               floating = FALSE) )
# # , latex.environments="flushright"
# strg[2] <- gsub('bular\\}\\{', 'bular}[b]{', strg[2]) 
# write(strg, file = file) 

file <- 'paper/other-parameters.tex'
caption <- 'Parameters for feature selection (KW and RF BVI), sampling (SMOTE) and cross-validation (CV). FDR stands for false discovery rate; $r$ is the number of CV repetitions; k the number of folds.\
Learner parameters are the RF parameters used internally for RF BVI.' 
xtable_print(fss, label = 'tbl:other-parameters', caption = caption, include.rownames = FALSE, 
             size = 'footnotesize', floating = TRUE, file = file)   
 
# =================================== 
# Over-under-sampling
# ===================================   
classgrps <- class_groups[groups]
# classgrps$ba <- NULL
binaries <- lapply(classgrps, convert2binary, dataset) 
tbinaries <- lapply(binaries, taskify) 
# sapply(binaries, function(x) table(x$env$data$class)) 
hybrid <- lapply(tbinaries, get_hybrid_db, 0, 1)
hybrid <- lapply(hybrid, function(x) x$env$data)

# ================================================================================================================================================================================ #
# Sampling figure
# ================================================================================================================================================================================ # 
nrows <- sapply(hybrid, nrow) 
added <- mapply(function(a, b) min(table(a$class)) - min(table(b$class)), hybrid, binaries) 
removed <- mapply(function(a, b) nrow(a) - nrow(b), binaries, hybrid) + added   
# classes <- mapply(function(x, count, deleted) {
classes <- mapply(function(x, count) {
  x$class <- as.character(x$class)
  if ('BA' %in% x$class) { 
    x$class[grep('^Non', x$class)][1:count] <- 'Synthetic'
  }
  else { 
    x$class[grep('^Non', x$class, invert = TRUE)][1:count] <- 'Synthetic'
  }
  x$class 
}, hybrid, added)
labels <- classgrps
labels[8] <- 'BA'
freqs <- mapply(function(a, b) data.frame(class = a, distribution =b), labels, classes, SIMPLIFY = FALSE)
freqs <- Reduce(rbind, freqs)
freqs$distribution <- as.character(freqs$distribution)

freqs$distribution <- gsub('Non-.*$', 'Negative', freqs$distribution ) 
freqs$distribution[freqs$distribution == freqs$class] <- 'Positive'

# freqs$distribution <- factor(freqs$distribution, levels = c(classgrps, 'Removed', 'Synthetic', 'Majority')) 
# freqs$distribution <- factor(freqs$distribution, levels = c(labels, 'Synthetic', 'Majority')) 
freqs$distribution <- factor(freqs$distribution, levels = c('Negative', 'Synthetic','Positive' ))
colnames(freqs) <- c('Case', 'Class')  
graphic <- paste0('paper/freqs-sample.pdf')

# ggplot(freqs, aes(x = Case, fill = Class)) + geom_bar()  + scale_colour_manual(values=my_orange) + xlab('Classification task'),
  # + scale_fill_brewer(palette = "Accent") 
sample_plot <-  ggplot(freqs, aes(x = Case, fill = Class)) + geom_bar()  + xlab('Classification task') + geom_hline(yintercept = 217) + 
          # theme(legend.position="bottom", legend.title=element_blank()),
          theme(legend.position="right", legend.title=element_blank())
sample_plot <- add_text_size(sample_plot, axis_title = TRUE )
sample_plot 
ggsave(
  graphic,  
 sample_plot, 
  width = 9,
  height = 4, 
  dpi = 1200)         

# ====================================================================================================================================
# Morphometrics illustration
# ==================================================================================================================================== 
# mc <- subset(dataset, class == 'MC')
fr <- subset(m, layer == '4')
mc <- dataset_full[as.character(fr$id), ]
mc <- subset(mc, vertical > 0.5 & com.dist > 50 & eccentricity > 0.5 & y_std_mean > 0.3 & height < 1000 )
# subset(read_metadata(), id %in% rownames(mc))
# The lower you go, the more vertical they are and eccentric...
# idmc <- 'VD101020A_INT_IDA' # idmc <- 'RP100428-3_IDI' # idmc <- 'RP100428-12_IDC' # idmc <- 'C080998A'
# idmc <- rownames(mc)[1] 
idmc <- "VD100716B_IDA"

symm <- subset(dataset_full, abs(vertical) < 0.3 & com.dist < 100 & eccentricity < 0.2 & abs(pcy) < 0.6 & height < 800)
subset(m, id %in% rownames(symm))
# idnmc <- 'C230300D1' C070600A4
# idnmc <- 'C080300B2' 
# idnmc <- 'C300106A'
# idnmc <- rownames(symm)[1]
idnmc <- 'C290500B-I2'
subset(m, id %in% c(idmc, idnmc))    

rude <- rownames(subset(dataset, grid_mean > 4 & grid_area > 300 & axon_origin < -50))
# plot_neuron(rude[1])
subset(m, id %in% rude)
idgrid <- rude[1]

plot_morpho_illustrate <- function() {  
  draw_dataset <- dataset_full
  draw_dataset <- draw_dataset[c(idmc, idnmc, idgrid), ] 
  set_grid_big(1900, 800) 
  n1 <- nat::read.neuron(paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc'))
  n2 <- nat::read.neuron(paste0('~/code/bbp-data/data/BBP_SWC/', idnmc, '.swc'))   
  n3 <- nat::read.neuron(paste0('~/code/bbp-data/data/BBP_SWC/', idgrid, '.swc'))   
  neurons <- list(n1, n2, n3)
  xoffsets <- c(250, 760 + 70, 1200 + 70 + 70) + 80 
  yoffsets <- c(95, 190 + 251, 190 + 251)  
  # draw axes 
  coms <- draw_dataset[ , c('x_mean', 'y_mean')] 
  coords <- coms + cbind(xoffsets, yoffsets)
  points(coords[, 1], coords[, 2], cex = 2, col = 'orange', pch = 19) 
  vects <- draw_dataset[ ,c('pcx', 'pcy')]
  sds <- draw_dataset[ ,c('sdev1')] 
  lapply(seq_len(sds), function(i) {
    s <- sds[i]
    v <- vects[i, ]
    v <- v * s
    a <- coords[i, ] + v   
    b <- coords[i, ]  - v
    a <- rbind(a, b)
    # points(a[1, 1], a[1, 2], cex = 4)
    # points(a[2, 1], a[2, 2], cex = 4)
    lines(a[ , 1], a[, 2], col = 'orange')
  })     
  mapply(add_to_plot, neurons, xoffsets, yoffsets) 
  
  borders <- get_layer_borders() - 700 - 525
  borders <- borders[1:2] 
  abline(h =  borders, col = 'green', lty=2)  
  
  ylayers <- get_yoffset()[c('23', '4') ] - 700 - 525
  tlayers <- name_layers(c('23', '4')) 
  text(x = 1500 + 320, y = ylayers, tlayers, cex = 1.5)
  text(x = xoffsets, y = 780, c('MC', 'NBC', 'SBC'))
  # text(x = 500, y = ylayers, tlayers) 
  # abline(h =  borders, col = 'green', lty=2)  
}
  
pdf('paper/morpho-illustrate.pdf', width = 10, height = 5)
plot_morpho_illustrate() 
dev.off()
# system('xdg-open paper/morpho-illustrate.pdf')   

dtbl <- dataset[c(idmc, idnmc), ] 
# vars <- c( 'density_bifs',
#           "y_mean",  "y_mean_abs", "y_std_mean", "y_sd", 'ratio_y',
#                      "x_mean_abs", "x_sd", 'ratio_x',
#           'eccentricity', "radial",
#           "l1_prob", "l1_width", 'l1_gxa', 'l1_gx','l1_bifs',
#          'grid_area', 'grid_mean',
#           'axon_origin','translaminar',
#           'd.displaced',
#           "d.insert.eccentricity", "d.insert.radial", "d.eccentricity", 'd.grid_area', 'd.translaminar')

vars <- c(
  'axon_origin',
  # 'axon_above_below', 
  'eccentricity',"radial", 
          "y_mean", 
  # "y_mean_abs", 
  "y_std_mean", 
  # "y_sd", 
  # 'ratio_y',
            'translaminar',
    "l1_prob", "l1_width", 'l1_gxa', 'l1_gx','l1_bifs', 
           # "x_mean_abs", 
  # "x_sd", 
  # 'ratio_x',
  'short_vertical_terminals',
        # 'density_bifs',
         'grid_area', 'grid_mean', 
  # 't.tortuosity.avg',
          'd.displaced',
          "d.insert.eccentricity", "d.insert.radial", "d.eccentricity", 'd.grid_area', 'd.translaminar'
  )


dtbl <- dtbl[ , vars]  
dtbl <- t(dtbl)
dtbl <- data.frame(variable = rownames(dtbl), dtbl)
rownames(dtbl) <- NULL 
colnames(dtbl)[-1] <- c('left', 'right')
colnames(dtbl)[1] <- c('morphometric') 
dtbl <- verbize(dtbl, 1)

caption <- ''
label <- 'tbl:morpho-illustrate'
file <- 'paper/morpho-illustrate.tex'
xtable_print(dtbl, label = label,  include.rownames = FALSE, caption = caption, file = file, floating = FALSE, latex.environments="flushright")    
strg <- capture.output(xtable_print(dtbl, label = label,  include.rownames = FALSE, caption = caption, floating = FALSE, latex.environments="flushright")    )
strg[2] <- gsub('bular\\}\\{', 'bular}[b]{', strg[2]) 
write(strg, file = file)

bedzo <- dataset[, c(vars, 'class')]
colnames(bedzo)[ncol(bedzo)] <- 'type'
bedzo <- standardize(bedzo)
maxi <- apply(bedzo[, -ncol(bedzo)], 2, max)
maxi <- as.data.frame(t(as.data.frame(maxi)))
mini <- apply(bedzo[, -ncol(bedzo)], 2, min)
mini <- as.data.frame(t(as.data.frame(mini)))
bedzo <- bedzo[c(idmc, idnmc, idgrid), ]    
# bedzo <- bedzo[, -(1:10)]

mm <- melt(bedzo, id = 'type')
# mm$variable <- paste(mm$variable,sep="_")  

stopifnot(identical(names(maxi), as.character(unique(mm$variable))))
stopifnot(identical(names(mini), as.character(unique(mm$variable))))
mm$maxi <- unlist(rep(maxi, each = 3))
mm$mini <- unlist(rep(mini, each = 3))

varnames <- colnames(bedzo[-ncol(bedzo)])
varnames <- gsub('^.\\.', '', varnames)
custom_desc <- read.csv('csv/custom-features.csv') 
indvars <- match(varnames, custom_desc$variable ) 
stopifnot(!any(is.na(indvars )))
group <- custom_desc[indvars  , 'type']
group <- as.character(group)  
xy_char <- 'Arbor shape, direction, and density (XY)'
dend_char <- 'Dendritic'
mc_char <- 'Type-specific pattern'
group <- gsub('^Dendri.*$', dend_char , group)
group <- gsub('^XY distribution.*$', xy_char, group)
group <- gsub('^.* arborization pattern$', mc_char , group)
group <- gsub('^Arbor density$', xy_char , group)
group <- rep(group, each = 3) 
group <- factor(group, levels = c( xy_char,  "Laminar", mc_char,dend_char)  ) 

mm$vartype <- group 
# mm[mm$variable == 'ratio_x' , 'vartype'] <- mc_char
# mm[mm$variable == 'ratio_y' , 'vartype'] <- mc_char

g <- ggplot(mm, aes(x=variable, y=value)) + geom_line(aes(group = type, color = type)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + xlab('Morphometric') + 
  ylab('Standardized value') + xlab('') + ylim(-2.5, 2.5)  
g <- g + geom_point(data=mm, aes(x = variable, y=maxi)) + geom_point(data=mm, aes(x = variable, y=mini))
# g <- g + facet_wrap(~vartype, scales="free_x")
g <- g +facet_grid(~vartype, scales="free_x", space = "free_x")
g <- add_text_size(g) 
ggsave( 'paper/morpho-illustrate-values.pdf',  g,  width = 10, height = 4, dpi = 1200) 
# ggplot(mm, aes(x=paste(variable,sep="_"), y=value, fill = class)) + geom_boxplot() 


pdf('paper/morpho-illustrate-merged.pdf', width = 10, height = 5 + 4)
par(mfrow=c(2, 1)) 
plot_morpho_illustrate() 
plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values  
  print(g,vp = vp1)        ## suggested by @b 
dev.off()

# system('xdg-open paper/morpho-illustrate-merged.pdf')   

# dev.off()
 
# png('paper/morpho-illustrate.png', width = 800, height = 800)
# plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', idmc, '.swc')) 
# plot_neuron_file(file) 
# centroid <- get_axon_centroid(swc)
# plot_centroid(centroid, col = 'orange')
# ch <- get_axon_convex_hull(swc)
# plot_polygon(ch)
# ax <- get_axon_convex_hull_wrt_soma(swc)
# plot_polygon(ax)   
# soma <- subset(swc, Label == 1)
# mean(soma$Y)
# thick_half <- get_layers_thickness()[3, 'thickness'] / 2
# abline(thick_half , 0, col =  'green', lty = 2)
# abline(-thick_half , 0, col =  'green', lty = 2)     
