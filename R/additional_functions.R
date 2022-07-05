# Additional functions
#' @importFrom stats rnorm
#'
get_defaults = function(metrics.vector = NULL){ # This returns data with missing data replaced by zeroes.
	return(c(
		path.length = c(stats::na.omit(metrics.vector["path.length"]), 0)[1],
		max.time.in.cage = c(stats::na.omit(metrics.vector["max.time.in.cage"]), 0)[1],
		mean.time.in.cage = c(stats::na.omit(metrics.vector["mean.time.in.cage"]), 0)[1],
		min.time.in.cage = c(stats::na.omit(metrics.vector["min.time.in.cage"]), 0)[1],
		sd.time.in.cage = c(stats::na.omit(metrics.vector["sd.time.in.cage"]), 0)[1],
		upper.time.in.cage = c(stats::na.omit(metrics.vector["max.time.in.cage"]), 0)[1],
		median.time.in.cage = c(stats::na.omit(metrics.vector["median.time.in.cage"]), 0)[1],
		lower.time.in.cage = c(stats::na.omit(metrics.vector["min.time.in.cage"]), 0)[1],
		time.high.activity = c(stats::na.omit(metrics.vector["time.high.activity"]), 0)[1],
		time.sustained.activity = c(stats::na.omit(metrics.vector["time.sustained.activity"]), 0)[1],
		time.active = c(stats::na.omit(metrics.vector["time.active"]), 0)[1],
		time.inactive = c(stats::na.omit(metrics.vector["time.inactive"]), 0)[1],
		total.activity.burst.time = c(stats::na.omit(metrics.vector["total.activity.burst.time"]), 0)[1],
		max.activity.burst.length = c(stats::na.omit(metrics.vector["max.activity.burst.length"]), 0)[1],
		max.activity.burst.time = c(stats::na.omit(metrics.vector["max.activity.burst.time"]), 0)[1],
		max.cage.use = c(stats::na.omit(metrics.vector["max.cage.use"]), 0)[1],
		mean.cage.use = c(stats::na.omit(metrics.vector["mean.cage.use"]), 0)[1],
		min.cage.use = c(stats::na.omit(metrics.vector["min.cage.use"]), 0)[1],
		sd.cage.use = c(stats::na.omit(metrics.vector["sd.cage.use"]), 0)[1],
		upper.cage.use = c(stats::na.omit(metrics.vector["max.cage.use"]), 0)[1],
		median.cage.use = c(stats::na.omit(metrics.vector["median.cage.use"]), 0)[1],
		lower.cage.use = c(stats::na.omit(metrics.vector["min.cage.use"]), 0)[1],
		cage.time.entropy = c(stats::na.omit(metrics.vector["cage.time.entropy"]), 0)[1],
		max.adj.cage.use = c(stats::na.omit(metrics.vector["max.adj.cage.use"]), 0)[1],
		mean.adj.cage.use = c(stats::na.omit(metrics.vector["mean.adj.cage.use"]), 0)[1],
		min.adj.cage.use = c(stats::na.omit(metrics.vector["min.adj.cage.use"]), 0)[1],
		sd.adj.cage.use = c(stats::na.omit(metrics.vector["sd.adj.cage.use"]), 0)[1],
		upper.adj.cage.use = c(stats::na.omit(metrics.vector["max.adj.cage.use"]), 0)[1],
		median.adj.cage.use = c(stats::na.omit(metrics.vector["median.adj.cage.use"]), 0)[1],
		lower.adj.cage.use = c(stats::na.omit(metrics.vector["min.adj.cage.use"]), 0)[1],
		adjusted.cage.time.entropy = c(stats::na.omit(metrics.vector["adjusted.cage.time.entropy"]), 0)[1],
		max.cage.transitions = c(stats::na.omit(metrics.vector["max.cage.transitions"]), 0)[1],
		mean.cage.transitions = c(stats::na.omit(metrics.vector["mean.cage.transitions"]), 0)[1],
		min.cage.transitions = c(stats::na.omit(metrics.vector["min.cage.transitions"]), 0)[1],
		sd.cage.transitions = c(stats::na.omit(metrics.vector["sd.cage.transitions"]), 0)[1],
		upper.cage.transitions = c(stats::na.omit(metrics.vector["upper.cage.transitions"]), 0)[1],
		median.cage.transitions = c(stats::na.omit(metrics.vector["median.cage.transitions"]), 0)[1],
		lower.cage.transitions = c(stats::na.omit(metrics.vector["lower.cage.transitions"]), 0)[1],
		cage.location.entropy = c(stats::na.omit(metrics.vector["cage.location.entropy"]), 0)[1],
		max.adj.cage.transitions = c(stats::na.omit(metrics.vector["max.adj.cage.transitions"]), 0)[1],
		mean.adj.cage.transitions = c(stats::na.omit(metrics.vector["min.adj.cage.use"]), 0)[1],
		min.adj.cage.transitions = c(stats::na.omit(metrics.vector["min.adj.cage.transitions"]), 0)[1],
		sd.adj.cage.transitions = c(stats::na.omit(metrics.vector["sd.adj.cage.transitions"]), 0)[1],
		upper.adj.cage.transitions = c(stats::na.omit(metrics.vector["upper.adj.cage.transitions"]), 0)[1],
		median.adj.cage.transitions = c(stats::na.omit(metrics.vector["median.adj.cage.transitions"]), 0)[1],
		lower.adj.cage.transitions = c(stats::na.omit(metrics.vector["lower.adj.cage.transitions"]), 0)[1],
		adjusted.cage.location.entropy = c(stats::na.omit(metrics.vector["adjusted.cage.location.entropy"]), 0)[1],
		max.revisit.time = c(stats::na.omit(metrics.vector["max.revisit.time"]), 0)[1],
		mean.revisit.time = c(stats::na.omit(metrics.vector["mean.revisit.time"]), 0)[1],
		min.revisit.time = c(stats::na.omit(metrics.vector["min.revisit.time"]), 0)[1],
		sd.revisit.time = c(stats::na.omit(metrics.vector["sd.revisit.time"]), 0)[1],
		upper.revisit.time = c(stats::na.omit(metrics.vector["max.revisit.time"]), 0)[1],
		median.revisit.time = c(stats::na.omit(metrics.vector["median.revisit.time"]), 0)[1],
		lower.revisit.time = c(stats::na.omit(metrics.vector["lower.revisit.time"]), 0)[1],
		max.revisit.length = c(stats::na.omit(metrics.vector["max.revisit.length"]), 0)[1],
		mean.revisit.length = c(stats::na.omit(metrics.vector["mean.revisit.length"]), 0)[1],
		min.revisit.length = c(stats::na.omit(metrics.vector["min.revisit.length"]), 0)[1],
		sd.revisit.length = c(stats::na.omit(metrics.vector["sd.revisit.length"]), 0)[1],
		upper.revisit.length = c(stats::na.omit(metrics.vector["max.revisit.length"]), 0)[1],
		median.revisit.length = c(stats::na.omit(metrics.vector["median.revisit.length"]), 0)[1],
		lower.revisit.length = c(stats::na.omit(metrics.vector["lower.revisit.length"]), 0)[1],
		peak.inactive.timepoint = c(stats::na.omit(metrics.vector["peak.inactive.timepoint"]), 0)[1],
		peak.active.timepoint = c(stats::na.omit(metrics.vector["peak.active.timepoint"]), 0)[1],
		number.activity.blocks = c(stats::na.omit(metrics.vector["number.activity.blocks"]), 0)[1],
		max.cage.sharing = c(stats::na.omit(metrics.vector["max.cage.sharing"]), 0)[1],
		mean.cage.sharing = c(stats::na.omit(metrics.vector["mean.cage.sharing"]), 0)[1],
		upper.cage.sharing = c(stats::na.omit(metrics.vector["max.cage.sharing"]), 0)[1],
		median.cage.sharing = c(stats::na.omit(metrics.vector["median.cage.sharing"]), 0)[1],
		lower.cage.sharing = c(stats::na.omit(metrics.vector["lower.cage.sharing"]), 0)[1],
		time.alone = c(stats::na.omit(metrics.vector["time.alone"]), 0)[1],
		max.social.interaction.time = c(stats::na.omit(metrics.vector["max.social.interaction.time"]), 0)[1],
		mean.social.interaction.time = c(stats::na.omit(metrics.vector["mean.social.interaction.time"]), 0)[1],
		min.social.interaction.time = c(stats::na.omit(metrics.vector["min.social.interaction.time"]), 0)[1],
		sd.social.interaction.time = c(stats::na.omit(metrics.vector["sd.social.interaction.time"]), 0)[1],
		upper.social.interaction.time = c(stats::na.omit(metrics.vector["max.social.interaction.time"]), 0)[1],
		median.social.interaction.time = c(stats::na.omit(metrics.vector["median.social.interaction.time"]), 0)[1],
		lower.social.interaction.time = c(stats::na.omit(metrics.vector["lower.social.interaction.time"]), 0)[1],
		social.exposure = c(stats::na.omit(metrics.vector["social.exposure"]), 0)[1],
		max.distance.from.all = c(stats::na.omit(metrics.vector["max.distance.from.all"]), 0)[1],
		mean.distance.from.all = c(stats::na.omit(metrics.vector["mean.distance.from.all"]), 0)[1],
		min.distance.from.all = c(stats::na.omit(metrics.vector["min.distance.from.all"]), 0)[1],
		sd.distance.from.all = c(stats::na.omit(metrics.vector["sd.distance.from.all"]), 0)[1],
		upper.distance.from.all = c(stats::na.omit(metrics.vector["max.distance.from.all"]), 0)[1],
		median.distance.from.all = c(stats::na.omit(metrics.vector["median.distance.from.all"]), 0)[1],
		lower.distance.from.all = c(stats::na.omit(metrics.vector["lower.distance.from.all"]), 0)[1],
		max.distance.from.each = c(stats::na.omit(metrics.vector["max.distance.from.each"]), 0)[1],
		#mean.distance.from.each = c(stats::na.omit(metrics.vector["mean.distance.from.each"]), 0)[1],
		min.distance.from.each = c(stats::na.omit(metrics.vector["min.distance.from.each"]), 0)[1],
		sd.distance.from.each = c(stats::na.omit(metrics.vector["sd.distance.from.each"]), 0)[1],
		upper.distance.from.each = c(stats::na.omit(metrics.vector["max.distance.from.each"]), 0)[1],
		median.distance.from.each = c(stats::na.omit(metrics.vector["median.distance.from.each"]), 0)[1],
		lower.distance.from.each = c(stats::na.omit(metrics.vector["lower.distance.from.each"]), 0)[1],
		max.sharing.change = c(stats::na.omit(metrics.vector["max.sharing.change"]), 0)[1],
		mean.sharing.change = c(stats::na.omit(metrics.vector["mean.sharing.change"]), 0)[1],
		min.sharing.change = c(stats::na.omit(metrics.vector["min.sharing.change"]), 0)[1],
		sd.sharing.change = c(stats::na.omit(metrics.vector["sd.sharing.change"]), 0)[1],
		upper.sharing.change = c(stats::na.omit(metrics.vector["max.sharing.change"]), 0)[1],
		median.sharing.change = c(stats::na.omit(metrics.vector["median.sharing.change"]), 0)[1],
		lower.sharing.change = c(stats::na.omit(metrics.vector["lower.sharing.change"]), 0)[1],
		max.influence = c(stats::na.omit(metrics.vector["max.influence"]), 0)[1],
		mean.influence = c(stats::na.omit(metrics.vector["mean.influence"]), 0)[1],
		min.influence = c(stats::na.omit(metrics.vector["min.influence"]), 0)[1],
		sd.influence = c(stats::na.omit(metrics.vector["sd.influence"]), 0)[1],
		upper.influence = c(stats::na.omit(metrics.vector["max.influence"]), 0)[1],
		median.influence = c(stats::na.omit(metrics.vector["median.influence"]), 0)[1],
		lower.influence = c(stats::na.omit(metrics.vector["min.influence"]), 0)[1],
		number.chase.events = c(stats::na.omit(metrics.vector["number.chase.events"]), 0)[1],
		sd.chase.events = c(stats::na.omit(metrics.vector["sd.chase.events"]), 0)[1],
		mean.chase.wins = c(stats::na.omit(metrics.vector["mean.chase.wins"]), 0)[1],
		sd.chase.wins = c(stats::na.omit(metrics.vector["sd.chase.wins"]), 0)[1]
	))
}

interpolate.features = function(features, mode, seed = 1, verbose = FALSE){ # Interpolate missing data.
	set.seed(seed)
	zeroed = character()
	vars = unique(colnames(features))
	subjects = unique(rownames(features))
	for(subject in subjects){
		for(var in vars){
			if(grepl("^l", mode)){ # Longitudinal.
				x = which(colnames(features) == var)
				na = x[is.na(features[subject, x])]
				mean = max(mean(features[subject, x], na.rm = T), 1e-12, na.rm = T)
				sd = max(sd(features[subject, x], na.rm = T), 1e-12, na.rm = T)
				if(length(na) > 0) features[subject, na] = rnorm(length(na), mean, sd) # Random number from distribution of existing values for this subject.
			}else if(grepl("^p", mode)){ # Pseudoreplication.
				y = which(rownames(features) == subject)
				na = y[is.na(features[y, var])]
				mean = mean(features[y, var], na.rm = T)
				sd = c(na.omit(sd(features[y, var], na.rm = T)), 0)[1] # SD = 0 will make rnorm return just the mean
				if(length(na) > 0 & !is.na(mean)){ # Interpolation is needed
					features[na, var] = rnorm(length(na), mean, sd) # Random number from distribution of existing values for this subject.
				}
			}
		}
		if(grepl("^p", mode)){ # Pseudoreplication.
			if(!all(is.na(features[y, ]))){ # If any NAs (if no data at all for this animal, leave as NA and it will be removed in the next step)...
				features[y, ][is.na(features[y, ])] = 0 # ...replace missing data with 0.
				zeroed = c(zeroed, subject)
			}
		}
	}
	features = features[which(rowVars(features, na.rm = T) > 0), ] # Only subjects that are present at some point.
	withMissingVars = which(colVars(features) == 0)
	for(x in withMissingVars){
		features[, x] = features[, x] + rnorm(length(features[, x]), 1e-24, 1e-24) # Jiggle if the columns are invariant.
	}
	if(length(zeroed) > 0 & !verbose) warning(paste0("Some missing data were replaced with zeros (use 'verbose = TRUE' to see which variables were affected)."))
	if(length(zeroed) > 0 & verbose) warning(paste0("Some missing data were replaced with zeros (", colnames(features)[withMissingVars], ")."))
	return(features)
}

get_na_defaults = function(){
	individual = get_defaults()
	individual[!is.na(individual)] = NA
	return(individual)
}

calculate.stacked.actograms = function(data){
	days = rownames(data$info$days)
	setNames(lapply(names(data$data), function(animalid){
		setNames(lapply(1:length(days), function(i){
			this.data = data$data[[animalid]]
			this.day = which(this.data$ZTDay == days[i])
			return(this.data[this.day, "ZT"])
		}), days)
	}), names(data$data))
}

plot.stacked.actogram = function(animalid, days = "all", actograms){
	reads = actograms[[animalid]]
	present = which(sapply(reads, length) > 1)
	if(length(present) < 3) present = -1
	select = range(present) + c(1, -1) # Get range of present and trim off extremes.
	if(days[1] == "all") days = names(reads)
	if(days[1] == "select"){
		if(min(select) > 0) days = names(reads) else days = NULL
	}
	.parprevious = graphics::par(mar = c(5, 8, 4, 2), xpd = FALSE)
	on.exit(par(.parprevious))
	plot(0, type = "n", xlim = c(0, 24), ylim = c(0, length(days)), main = animalid, yaxt = "n", xaxt = "n", ylab = "", xlab = "ZT", bty = "n")
	abline(v = 0, col = "orange", lwd = 1.5)
	abline(v = 12, col = "orange", lwd = 1.5)
	abline(v = 24, col = "orange", lwd = 1.5)
	axis(1, at = c(0, 3, 6, 9, 12, 15, 18, 21, 24), labels = c(0, 3, 6, 9, 12, 15, 18, 21, 24), las = 1)
	axis(1, at = 0:24, labels = FALSE, tcl = -0.25)
	axis(2, at = seq_along(days), labels = rev(days), las = 1)
	for(i in seq_along(days)){
		ploti = (length(days):1)[i]
		ZT = reads[[days[i]]]
		if(length(ZT) > 0) segments(ZT, ploti - 1, ZT, ploti, col = "#000000FF", lwd = 2)
	}
}

