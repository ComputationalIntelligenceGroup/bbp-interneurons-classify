set_xtable_options <- function() {
  options(xtable.caption.placement = 'top')
  options(xtable.comment = FALSE)
  options(xtable.booktabs = FALSE)
  options(xtable.table.placement = 'H')
  options(xtable.size = 'small')
  options(sanitize.text.function = identity) 
} 
# yesify <- function(data) { 
#   stopifnot(is.logical(data))
#  data[!data] <- NA
#  data[data] <- 'Yes' 
#  data
# }
# fix_colnames <- function(db) {
#  colnames(db) <- gsub('_', ' ', colnames(db) )
#  colnames(db) <- gsub("(^[[:alpha:]])", "\\U\\1", colnames(db), perl=TRUE) 
#  db
# }
# sanitize_variables <- function(db) {
#   db
#  # gsub('_', ' ', db )
# } 
sanitize_colnames <- function(db) {
 db <- gsub('_', ' ', db )
 db <- gsub("(^[[:alpha:]])", "\\U\\1", db, perl=TRUE) 
 db <- gsub('P-value', 'p-value', db )
 db 
}  
sanitize_colnames_math <- function(db) {
 db <- gsub("(^[[:alpha:]])", "\\U\\1", db, perl=TRUE) 
 db <- gsub('P-value', 'p-value', db )
 db 
} 
sanitize_text <- function(db) {
 # gsub('_', ' ', db )
  db <- gsub('RF_BVI', 'RF BVI', db )
  db
}   
xtable_print <- function(x, caption, label, san_text = sanitize_text, align = NULL, sanitize.colnames.function = sanitize_colnames, ...) {
  stopifnot(length(caption) > 0 || is.null(caption), length(label) > 0)
  x <- xtable::xtable(x, caption = caption, label = label, align = align)
  xtable::print.xtable(x,  sanitize.text.function = san_text,  sanitize.colnames.function = sanitize.colnames.function,  ...) 
}
color_fone <- function(fone) { 
  # sapply(fone, function(x) if (is.na(x)) return('') else if (x >= 0.75) 'OliveGreen' else if (x < 0.6) 'red' else 'orange')
  sapply(fone, function(x) if (is.na(x)) return('') else if (x >= 0.75) 'OliveGreen' else if (x < 0.6) 'red' else 'YellowOrange')
}
add_color <- function(colors, xs) {
  mapply(function(color, x) paste0('\\color{', color, '}{', x, '}'), colors, xs)  
}
join_results <- function(b, groups) {
  all <- Reduce(rbind, b)
  all <- as.data.frame(all) 
  class <- mapply(function(class, results) {
    a <- rep(class, nrow(results))
    # a[-1] <- NA
    a
  }, groups, b, SIMPLIFY = FALSE)
  class <- unlist(class, use.names = FALSE)
  stopifnot(is.character(class))
  all$class <- class
  ncol <- ncol(all)
  all <- all[ , c(ncol, 1:(ncol - 1))]  
  all$class <- toupper(all$class) 
  all$class <- gsub('CHC', 'ChC', all$class) 
  all$p <- as.integer(all$p)
  all$n <- as.integer(all$n)
  all <- tidyr::unite(all, 'tpr', tp, p, sep = ' / ')
  all <- tidyr::unite(all, 'tnr', tn, n, sep = ' / ')  
  all 
}
clean_plus <- function(x) { 
    x <- gsub('^ \\+', '', x) 
    gsub(' \\+ $', '', x) 
}
#' Produce table with classification results 
#' @param b list of per class results
format_table_all_results <- function(b, groups, bold = FALSE, preprocessing = TRUE) {
  all <- join_results(b, groups)  
  all$sampling[all$sampling != ''] <- 'Sampling'
  # all$fss[all$fss != ''] <- 'Fss'
  all$fss <- toupper(all$fss)
  # colnames(all)[2] <- 'preprocessing'
  colnames(all)[7:8] <- toupper(colnames(all)[7:8])
  colnames(all)[4] <- toupper(colnames(all)[4])
  colnames(all)[6] <- 'F-measure' 
  colnames(all)[2] <- 'Classifier' 
  if (preprocessing) {
    all <- tidyr::unite(all, 'preprocessing', FSS, sampling,  sep = ' + ')  
    # all$preprocessing <- gsub('^ \\+', '', all$preprocessing)
    # all$preprocessing <- gsub(' \\+ $', '', all$preprocessing)  
    all$preprocessing <-  clean_plus(all$preprocessing) 
  } 
  else {
   colnames(all) <- gsub('^sampling$', 'Sampling', colnames(all)) 
   all$Sampling <- gsub('^Sampling$', 'Yes', all$Sampling) 
  }
  
  all$`F-measure` <- round(all$`F-measure`, digits = 2)
  maxs <- tapply(all$`F-measure`, factor(all$class, unique(all$class)), function(x) max(x))
  # colors <- sapply(maxs, function(x) if (x >= 0.8) 'OliveGreen' else if (x < 0.6) 'red' else 'orange') 
  colors <- sapply(maxs, color_fone) 
  colors <- colors[all$class] 
  # all$class <- mapply(function(color, x) paste0('\\color{', color, '}{', x, '}'), colors, names(colors))  
  all$class <- add_color(colors, names(colors))  
  if (bold) { 
    inds <- tapply(all$`F-measure`, factor(all$class, unique(all$class)), function(x) which(x == max(x)))
    inds <- unlist(mapply('+', 0:(length(inds) - 1) * 4, inds, SIMPLIFY = FALSE)) 
    all$`F-measure` <- sprintf("%.2f", all$`F-measure`)
    all$`F-measure`[inds] <- paste0('\\textbf{', all$`F-measure`[inds], '}')
  } 
  
  all$class[duplicated(all$class)]  <- '' 
  all
}
#' Plot violin plots of results 
plot_results <- function(res) {
  # res <- reshape2::melt(res, measure.vars = c('acc', 'gmean', 'prc', 'f1'), value.name = 'Performance')
  
  res <- reshape2::melt(res, measure.vars = c('acc', 'f1'), value.name = 'Performance')
  p <- ggplot(res, aes_string("learner.id", 'Performance', colour = 'learner.id'))
  # p <- ggplot(res, aes_string("learner.id", 'f1', colour = 'learner.id'))
  
  p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -45, 
                                                                            hjust = 0)) + ylab('Performance')
  p <- p + geom_violin() + stat_summary(fun.ymin = mean,  fun.ymax = mean, fun.y = mean, geom = "crossbar") 
  # p <- p +  facet_grid( ~ type ) 
  p <- p + facet_grid(variable ~ sampling)
  p  
} 
#' Paste multiple columns into a table
paste_columns <- function(dbs) {
  nrows <- max(sapply(dbs, nrow))
  dbs[] <- lapply(dbs, function(db) {
    if (nrow(db) < nrows) {
      n <- nrows - nrow(db)
      col <- ncol(db)
      nas <- matrix(NA, nrow = n, ncol = col)
      colnames(nas) <- colnames(db)
      db <- rbind(db, nas) 
    }
    db
  })
  Reduce(cbind, dbs )
}
format_vars_tbl <- function(table) {
  colnames(table) <- gsub('variable', 'morphometric', colnames(table))
  ind_morpho <- grep('morphometric', colnames(table))
  verbize(table, ind_morpho) 
}
verbize <- function(table, ind_morpho) {
  table[, ind_morpho] <- lapply(table[, ind_morpho, drop = FALSE], function(x) {
    x <- as.character(x)
    x[!is.na(x)] <- paste0('\\verb!', x[!is.na(x)], '!')
    x 
  })
  table
}
plot_2d_binary <- function(vars, classes, dataset) {
  plot_2d(vars, convert2binary(classes, dataset))
}
plot_2d <- function(vars, dataset, legend = TRUE) {
 vars <- as.character(vars)
 g <- ggplot(dataset, aes_string(x = vars[1],  y = vars[2], color = "class")) + geom_point() 
 if (!legend) {
   g <- g + theme(legend.position="none")
 }
 g
}
plot_pca <- function(vars, db, legend = TRUE, ...) { 
  pcadb <- db
  pcadb$class <- NULL
  bolasso_pca <- prcomp(pcadb[ ,vars], center = TRUE, scale = TRUE ) 
  # plot(bolasso_pca, type = "l")  
  g <- biplot(bolasso_pca, obs.scale = 1, var.scale = 1,
                groups = db$class, ellipse = TRUE, circle = FALSE, varname.size = 4, ...)
  g <- g + scale_color_discrete(name = '')
  g <- add_text_size(g)
  # g + theme(legend.direction = 'horizontal', legend.position = 'top')  
  if (!legend) {
   g <- g + theme(legend.position="none")
  }
  else { 
   g <- g + theme(legend.position="bottom")
  }
  g
} 
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
# Set a large grid
set_grid_big <- function(xmax = 2500, ymax = 2082) {
  omar <- par()$mar
  par(mar = c(0, 0, 0, 0))
  plot(1, type="n", axes=F, xlab="", ylab="")
  xlim <- c(0, xmax)
  ylim <- c(0, ymax) 
  par(usr=c(xlim, ylim))
  abline(v = seq(xlim[1], xlim[2], by = 100), h = seq(ylim[1], ylim[2], by = 100), col = 'grey60')
  column_col <- gray(0.2, alpha = 0.05)
  par(mar=omar) 
}
add_neuron2plot <- function(neuron, offset) {
  naxon <- subset(neuron, Label == 2)
  lines <- seglist2lines(naxon $SegList)
  plot_trees_offset (naxon, lines, offset = offset, col = 'blue')
  if (sum(neuron$d$Label == 3) > 0) {
    ndend <- subset(neuron, Label == 3)
    lines <- lapply(ndend$SubTrees, seglist2lines)
    lines <- unlist(lines, use.names = FALSE)
    plot_trees_offset(ndend, lines, offset = offset, col = 'red')
  }
  soma_inds <- get_soma_inds(neuron)
  soma_limits <- subset(neuron$d, Parent %in% soma_inds) 
  polygon(soma_limits$X + offset[1], soma_limits$Y + offset[2], col = 'red', border = 'red')
}
plot_trees_offset <- function(neuron, lines, offset = c(0, 0), ...) {
  omar <- par('mar')
  par(mar = c(0, 0, 0, 0))
  offset <- rep(offset, each = length(lines))
  lines(neuron$d[lines, c('X', 'Y')] + offset, lwd = 0.5, ...)
  par(mar = omar)
}
get_yoffset <- function() {
  c('6' = 350, '23' = mean(c(1917 , 1415)), '5' = mean(c(1225  , 700)), '4' = mean(c(1415, 1225)))
}
add_to_plot <- function(neuron, x, y) {
 add_neuron2plot(neuron, c(x, y))  
}  
get_results_group <- function(group) {
  filename  <- get_filename(group) 
  load(filename)   
  fres <- lapply(res, format_results)
  acc <- lapply(fres, process_cv)
  acc <- Reduce(rbind, acc)
  colnames(acc)[1] <- 'Classifier'
  acc$fss <- toupper(acc$fss) 
  acc$sampling[acc$sampling != ''] <- 'Hybrid'
  acc$sampling[acc$sampling == ''] <- 'None'
  acc$fss[acc$fss == ''] <- 'None'
  acc$fss <- gsub('^KRUSKAL$', 'KW', acc$fss)
  acc$fss <- factor(acc$fss, levels = c('None', 'KW', 'RF_BVI') )
  acc 
}
get_results_plot <- function(group) { 
  acc <- get_results_group(group)  
  acc$sampling <- gsub('Hybrid', 'Sampling', acc$sampling)
  # acc$sampling <- gsub('None', 'No sampling', acc$sampling)
  # acc$sampling <- gsub('None', '', acc$sampling)
  acc$sampling <- factor(acc$sampling, levels = c('Sampling', 'None')) 
  # p <- ggplot(acc, aes(learner.id, (tn + tp) / ( tn + tp + fn + fp), colour = 'learner.id')) 
  p <- ggplot(acc, aes_string("Classifier", 'f1', colour = 'Classifier'))
  p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -45, hjust = 0))
  p <- p + ylab('') 
  p <- p + geom_violin() + stat_summary(fun.ymin = mean,  fun.ymax = mean, fun.y = mean, geom = "crossbar") 
  # p <- p + ylim(c(0.6, 1)) + facet_grid(sampling ~ fss )
  p <- p + facet_grid(sampling ~ fss)
  p
}
get_nrn_all <- function() {
  load('../bbp-data/nrn-mismatch-79.rdata')
  # get object. return it.
}
# TODO: use just package bbp; then just import this and dump it
bbp2nrn_labels <- function(labels) {
  labels <- gsub('^DBC$', 'HT', labels)
  labels <- gsub('^NBC$', 'AR', labels)
  labels <- gsub('^LBC$', 'LB', labels)
  labels <- gsub('^MC$', 'MA', labels)
  labels <- gsub('^ChC$', 'CH', labels)
  labels <- gsub('^NGC$', 'NG', labels)
  labels
}
get_bbp_nrn_votes <- function(merged_meta, basket_correct = FALSE) {
  labels <- bbp2nrn_labels(merged_meta$class)
  votes <- subset_by_colnames(labels, merged_meta)
  votes <- as.numeric(votes)
  if (basket_correct) {
    ind_basket <- merged_meta$class %in% c('SBC', 'NBC', 'LBC') 
    basket <- merged_meta[ ,c('CB', 'LB', 'AR')]
    basket <- rowSums(basket)[ind_basket]
    votes[ind_basket] <- basket
  }
  setNames(votes, merged_meta$id)
} 
#' Return the number of NRN votes for BBP cells. 
read_nrn_common <- function() { 
  load('data/nrn-mismatch-79.rdata') 
  common_types_bbp <- merged_meta_cell$class %in% c('NGC', 'MC', 'ChC') 
  common_types_ind <- common_types_bbp 
  common_types <- merged_meta_cell[common_types_ind , ]
  common_types$class <- bbp2nrn_labels(common_types$class)
  votes <- subset_by_colnames(common_types$class, common_types )
  votes <- as.numeric(votes)
  common_types$votes <- votes      
  common_types[['agree']] <- get_bbp_nrn_votes(common_types, basket_correct = TRUE)
  common_types[['agree']] <- as.integer(common_types[['agree']] ) 
  cols <- c("id", "class", "nrn_class", "agree", "NG", "CH", "MA", "CB", "LB", "CT", "HT", "OT", "AR", "CR", "layer")
  common_types <- common_types[, cols]
  common_types <- dplyr::arrange(common_types, agree)
  common_types  
}
get_nrn_common <- function() { 
  mismatch <- read_nrn_common()
  mismatch <- mismatch[ , c(1,15,2:14)]  
  colnames(mismatch) <- strict_nrn2bbp_labels(colnames(mismatch))
  basic <- mismatch[, (1:5)]
  classes <- mismatch[, -(1:5)]
  classes <- classes[, order(colnames(classes))]
  mismatch <- cbind(basic, classes )
  # mismatch[, -(1:5)] <- mismatch[, -(1:5)][, order(colnames(mismatch)[-(1:5)])]
  mismatch
  colnames(mismatch )[1] <- 'ID'
  colnames(mismatch )[3] <- 'Cell type'
  colnames(mismatch )[4] <- 'DF'  
  mismatch$DF <-  strict_nrn2bbp_labels(mismatch$DF)
  mismatch[['Cell type']] <-  strict_nrn2bbp_labels(mismatch[['Cell type']]) 
  mismatch
}                                                                                                                                                                                                                                                                                                  
save_neuron <- function(id) {
  file <- paste0('paper/', id, '.pdf')
  pdf(file, width = 7, height = 7)
  swc <- paste0(id, '.swc')
  plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', swc)) 
  dev.off() 
}
make_table_univarate <- function(tables, scientific = TRUE) {  
  tbs <- Reduce(function(a, b) dplyr::full_join(a, b, by = 'variable'), tables) 
  colnames(tbs)[-1] <- groups
  tbs <- dplyr::arrange(tbs, variable)
  # save(tbs, file = 'rdata/feat-sel-wilcox-exact.rdata')
  uses <- apply(tbs[, -1], 1, function(x) sum(!is.na(x)))
  tbs <- tbs[order(uses, decreasing = TRUE), ]
  # tbs[tbs > 1e-2] <- NA
  # tbs[tbs > 0.05] <- NA
  # tbs[tbs > 1e-7] <- NA
  # tbs <- tbs[order(tbs$variable, decreasing = TRUE), ]
  tbs[is.na(tbs)] <- '' 
  
  dendi <- tbs[grep('^d\\.', tbs$variable), ] 
  dcounts <- apply(dendi, 2, function(x) sum(x != ''))
  dcounts <- as.character(dcounts)
  dcounts[1] <- 'Dendritic' 
    
  tbs <- verbize(tbs, 1) 
  colnames(tbs)[-1] <- toupper(colnames(tbs)[-1])
  colnames(tbs) <- gsub('variable', 'morphometric', colnames(tbs))
  colnames(tbs)[2] <- 'ChC'
  
  counts <- apply(tbs, 2, function(x) sum(x != ''))
  counts <- as.character(counts)
  counts[1] <- 'Total'  
  if (scientific) { 
    tbs[ -1] <- sapply(tbs[ -1], function(x) paste0('\\num[round-precision=2,round-mode=figures,scientific-notation=true]{', x, '}'))  
  }
  else {
    tbs[ -1] <- round(as.numeric(as.matrix(tbs[ , -1])), digits = 2)
  }
  tbs <- rbind(tbs, dcounts)
  tbs <- rbind(tbs, counts)
  rownames(tbs) <- seq_len(nrow(tbs))
  rownames(tbs)[nrow(tbs)]  <- ''
  rownames(tbs)[nrow(tbs) - 1]  <- ' '
  
  tbs[is.na(tbs)] <- '' 
  tbs
}
# plot_neuron <- function(id) {  
#   plot_neuron_file(paste0('~/code/bbp-data/data/BBP_SWC/', id, '.swc')) 
# }
# copied from ggbiplot and adapted
biplot <- function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
          obs.scale = 1 - scale, var.scale = scale, groups = NULL, 
          ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3, 
          alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69, 
          varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE, 
          omit_vars = NULL,
          ...) 
{
  
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  }
  else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  }
  else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), 
               FUN = "*")
    v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
                                                  1]), FUN = "/")
  }
  else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  }
  else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
                              FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + 
    ylab(u.axis.labs[2]) 
  if (var.axes) {
    if (circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
                                                length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * 
                             sin(theta))
      g <- g + geom_path(data = circle, color = muted("white"), 
                         size = 1/2, alpha = 1/3)
    }
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, 
                                           xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2, 
                                                                                                  "picas")), color = muted("red"))
  }
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    }
    else {
      g <- g + geom_point(alpha = alpha)
    }
  }
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, 
                       mu, FUN = "+"), groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  if (var.axes) {
    if (!is.null(omit_vars)) {
        ind_omit <- match(as.character(omit_vars), as.character(df.v$varname))
        df.v$varname[ind_omit ] <- '' 
    }
    g <- g + geom_text(data = df.v, aes(label = varname, 
                                        x = xvar, y = yvar, angle = angle, hjust = hjust), 
                       color = "darkred", size = varname.size)
  }
  return(g)
} 
name_layers <- function(layers) { 
  tlayers <- paste0('L', layers)  
  gsub('23', '2/3', tlayers) 
}
get_non_redundant <- function(vars, dbset) { 
  vars <- as.character(vars)
  cors <- cor(dbset[, vars])
  cors[abs(cors) < 0.90] <- NA
  while (TRUE) { 
    redundant <- sapply(seq_along(vars),  is_redundant, vars, cors)
    if (!any(redundant )) {
      break
    }
    # remove the first
    vars <- vars[-which.max(redundant)]
  }
  # ind_ok_vars <- !sapply(seq_along(vars),  is_redundant, vars, cors) 
  # vars[ind_ok_vars]
  vars 
}  
is_redundant <- function(ind, vars, cors) {
  var <- as.character(vars[ind])
  crs <- cors[var, ]
  correlated <- names(crs[!is.na(crs)])
  any(correlated  %in% vars[seq_len(ind - 1)])
}
plot_rfbvi <- function(rfbvi) {
  rfbvi$variable <-   factor(rfbvi$variable, levels = as.character(rfbvi$variable )) 
  ggplot(rfbvi, aes(x = variable, y = importance)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))  
}
plot_dotp <- function(var_importance) {   
  var_importance$variable <- factor(var_importance$variable, levels = rev(as.character(var_importance$variable)))
  p <- ggplot(var_importance, aes(x=variable, y=importance)) + 
  geom_point(col="tomato2", size=3) +    
  geom_segment(aes(x=variable, 
                   xend=variable, 
                   y=min(importance), 
                   yend=max(importance)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  coord_flip()  + theme(legend.position="bottom")
  p 
}
plot_box_kw <- function(vars, dset, kwvals, scientific = TRUE) { 
  if (length(vars) > 10) vars <- vars[1:10]
  nvars <- length(vars)
  uke <- dset[ ,c(vars[1:nvars], 'class')] 
  uke <- standardize(uke) 
  
  mm <- melt(uke, id = 'class')
  mm$variable <- factor(mm$variable, levels = rev(setdiff(colnames(uke), "class")) )
  colnames(mm)[1] <- 'type'
  kvalue <- subset(kwvals, variable %in% vars[1:nvars])
  kvalue <- kvalue[[2]]
  if (scientific) {  
    kvalue <- format(kvalue, scientific = TRUE, digits = 2)
    kvalue <- sapply(kvalue, as.character)
    kvalue <- gsub('e', '^{', kvalue)
    kvalue <- paste0('$', kvalue, '}$')
  }
  bp <- ggplot(mm, aes(x=variable, y=value, fill = type)) + geom_boxplot()  + ylim(-2.5, 6) + 
   ylab('Standardized value') +   
       annotate('text', x = seq(nvars + 0.3, 1.3, by = -1), y = 5,  label = TeX(kvalue, output='character'), parse=TRUE, size=3, colour = "blue") 
  bp <- bp + coord_flip() + theme_classic() +  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size = 14), 
                                                     axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "bottom" ) 
  bp <- bp + coord_flip() + theme(axis.text.y = element_text(size = 14)) 
  bp <- bp + coord_flip() + theme(axis.text.x = element_text(size = 14, angle = 0)) 
  bp
}
add_text_size <- function(plot, axis_title = FALSE) {
  if  ( axis_title ) {
   plot + theme(axis.text = element_text(size = 14), strip.text.y = element_text(size = 14), strip.text.x = element_text(size = 14), legend.text=element_text(size=14), axis.title = element_text(size = 14))  
  } 
  else { 
   plot + theme(axis.text = element_text(size = 14), strip.text.y = element_text(size = 14), strip.text.x = element_text(size = 14), legend.text=element_text(size=14))  
  }
}