# ============================================================================ # 
# Paper data - maybe move to other file?
# ============================================================================ #  
dataset_prefix <- dataset 
dataset_prefix$diameter.avg <- dbfu$w.Diameter_avg
dataset_prefix$d.diameter.avg <- dbfu$d.Diameter_avg
dataset_prefix$depth <- dbfu$w.Depth_avg
dataset_prefix$d.depth <- dbfu$d.Depth_avg
# dataset_prefix$fractal_dimension.avg <- dataset_full$fractal_dimension.avg
dataset_prefix$compartment_length.avg <- dataset_full$compartment_length.avg
dataset_prefix$d.compartment_length.avg <- dataset_full$d.compartment_length.avg
dataset_prefix$N_nodes <- dataset_full$N_nodes.sum
dataset_prefix$d.N_nodes <- dataset_full$d.N_nodes.sum
dataset_prefix <- make_prefix(dataset_prefix)
dataset_prefix$local_tilt_angle.avg <- dbfu$w.Bif_tilt_local_avg
dataset_prefix$local_bifurcation_angle.avg <- dbfu$w.Bif_ampl_local_avg
dataset_prefix$local_torque_angle.avg <- dbfu$w.Bif_torque_local_avg
dataset_prefix$d.local_tilt_angle.avg <- dbfu$d.Bif_tilt_local_avg
dataset_prefix$d.local_bifurcation_angle.avg <- dbfu$d.Bif_ampl_local_avg
dataset_prefix$d.local_torque_angle.avg <- dbfu$d.Bif_torque_local_avg
dataset_prefix$type <- dataset$class 
mm <- read_metadata()
rownames(mm) <- mm$id
dataset_prefix$layer <- mm[rownames(dataset_prefix), 'layer'] 
dataset_prefix$class <- factor(gsub('^NC$', 'Non-C', dataset_prefix$class))