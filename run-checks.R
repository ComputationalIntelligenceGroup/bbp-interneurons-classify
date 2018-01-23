# effect
plot(dataset_prefix$depth, dataset_prefix$remote_torque_angle.avg)
cor(dataset_prefix$depth, dataset_prefix$remote_torque_angle.avg)
plot(dataset_full$compartment_length.med, dataset_full$tortuosity.avg)
cor(dataset_full$compartment_length.med, dataset_full$tortuosity.avg)

lm(tortuosity.avg ~ compartment_length.med, dataset_full)  

# depth is not only reason for difference in com dist
plot(dataset_prefix$depth, dataset_prefix$com.dist)
plot(dataset_prefix$depth, dataset_prefix$euclidean_dist.avg)
ggplot(dataset_prefix, aes(euclidean_dist.avg, com.dist, color = depth))  + geom_point()

ggplot(dataset_prefix, aes(euclidean_dist.avg, com.dist, color = depth))  + geom_point()
hist(dataset_prefix$depth)

ggplot(dataset_prefix, aes( x = depth))  + geom_histogram() + facet_grid(class ~ .)
# barely any difference in depth. the rest is not important.

duge <- dataset_full
duge$class <- dataset$class
ggplot(duge, aes( x = tortuosity.avg - compartment_length.med * 0.01099))  + geom_histogram() + facet_grid(class ~ .)  

# no effect

plot(dataset_prefix$depth, dataset_prefix$euclidean_dist.avg)
plot(dataset_prefix$depth, dataset_prefix$remote_tilt_angle.avg)


# Dendrites distant
d <- filter_neurite(full_neurons, axon = FALSE)
ake <- plyr::daply(d, ~ neuron, function(x) x[1, 'euclidean_dist'])
max(ake)

ure <- subset(d, neuron == 'RP110105_L5-3_IDB' & nchar(branch) == 1)
dplyr::summarise(dplyr::group_by(ure, neurite), dplyr::first(euclidean_dist))
dplyr::summarise(dplyr::group_by(ure, neurite), dplyr::first(y))   