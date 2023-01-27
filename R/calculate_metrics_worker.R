# Worker functions
#' @importFrom utils read.delim head
#' @importFrom stats na.omit sd median setNames density
#' @importFrom Rfast colsums rowsums
#' @importFrom fastmatch fmatch
#' @importFrom IRanges IRanges findOverlaps from to
#'
calculate_metrics_worker = function(window.data, log){
	time.bounds = as.numeric(window.data$window.definition[2:3])

	matrix.unmap = function(row, col, dim.m){
		outofbounds = col < 1 | col > dim.m[2] | row < 1 | row > dim.m[1]
		n = ((col - 1) *  dim.m[1]) + row
		n[outofbounds] = NA
		return(n)
	}

	# Calculate the cage presence matrix.
	## Get list of _all_ antenna contacts and work out which animals were in which cage.
	cage.presence.matrix.timestamp = sort(Reduce(union, lapply(window.data$data, "[[", "Timestamp")))
	cage.presence.matrix = sapply(seq_along(window.data$data), function(i){
		indices = cumsum(!is.na(fastmatch::fmatch(cage.presence.matrix.timestamp, window.data$data[[i]]$Timestamp)))
		indices[indices == 0] = NA
		window.data$data[[i]]$Cage[indices]
	})
	if(is.null(ncol(cage.presence.matrix))){ # Happens when no activity in this window (single vector of previous cages).
		cage.presence.matrix = NULL
	}else{
		colnames(cage.presence.matrix) = names(window.data$data)
	}

	subjects = names(window.data$data)

	results = lapply(subjects, function(subject){
		start.time = Sys.time()
		# Activity patterns.
		## Number of cages moved in the minimal interval.
		a = fastmatch::fmatch(window.data$data[[subject]]$Cage, rownames(window.data$layout$shortest.paths))
		b = fastmatch::fmatch(c(NA, window.data$data[[subject]][-nrow(window.data$data[[subject]]), "Cage"]), colnames(window.data$layout$shortest.paths))
		raw = window.data$layout$shortest.paths[matrix.unmap(a, b, dim(window.data$layout$shortest.paths))] # If no shortest path found, the animal must be stuck in a tunnel. i.e. not moving.
		path.length = sum(raw, na.rm = T) # Not scaled

		individual = double()
		ethogram.data = list()
		hourly.starts = seq(time.bounds[1], time.bounds[2], by = 3600)
		hourly.intervals = cbind(hourly.starts, c(hourly.starts[-1], time.bounds[2]))
		if(diff(hourly.intervals[nrow(hourly.intervals), ]) == 0) hourly.intervals = hourly.intervals[-nrow(hourly.intervals), ]
		social.interaction.time = NULL
		distance.from.each = NULL
		cage.probability = setNames(rep(NA, length(colnames(window.data$layout$shortest.paths))), colnames(window.data$layout$shortest.paths))
		follow.events = NULL

		if(nrow(window.data$data[[subject]]) < 3){ # If the animal did not move at all all night, consider it to be missing (first timestamp is previous cage, need at least two other timestamps to calculate an interval).
			individual = get_na_defaults()
			#
			ethogram.data = apply(hourly.intervals, 1, function(hourly.interval){
				return(list(ethogram.activity = NA, ethogram.sociality = NA, ethogram.exploration = NA))
			})
			#
			follow.events = setNames(lapply(subjects, function(id) data.frame(Timestamp = numeric(),  TransitionFrom = character(),  TransitionTo = character()) ), subjects)
		}else{
			## Length of time in each cage.
			time.in.cage = diff(window.data$data[[subject]]$Timestamp[-1]) # Only include intervals completely within the window.
			max.time.in.cage = max(time.in.cage, na.rm = TRUE)
			mean.time.in.cage = mean(time.in.cage, na.rm = TRUE)
			min.time.in.cage = min(time.in.cage, na.rm = TRUE)
			sd.time.in.cage = stats::sd(time.in.cage, na.rm = TRUE)
			sorted.time.in.cage = sort(time.in.cage)
			upper.time.in.cage = unname(sorted.time.in.cage)[ceiling(length(sorted.time.in.cage) * .95)]
			median.time.in.cage = stats::median(time.in.cage, na.rm = TRUE)
			lower.time.in.cage = unname(sorted.time.in.cage)[floor(length(sorted.time.in.cage) * .05)]

			## Class activity levels.
			active = time.in.cage < 10
			inactive = time.in.cage >= 10
			high.activity = time.in.cage < 1
			sustained.activity = time.in.cage >= 1 & time.in.cage < 10

			## Total time spent in each activity class.
			time.high.activity = sum(time.in.cage[high.activity])
			time.sustained.activity = sum(time.in.cage[sustained.activity])
			time.active = sum(time.in.cage[active])
			time.inactive = sum(time.in.cage[inactive])

			# How long was the longest high activity burst (consecutive periods of high activity)?
			activity.burst = time.in.cage < 2
			if(any(activity.burst)){
				burst.cumsum = cumsum(!activity.burst)
				length.activity.bursts = table(burst.cumsum) # These are 1 longer than the real burst as cumsum increments on any change...
				length.activity.bursts = length.activity.bursts[length.activity.bursts > 2] # ...so just take any sequences 2 or longer.
				if(length(length.activity.bursts) > 0){
					length.activity.bursts = sort(length.activity.bursts, decreasing = T)
					activity.burst.intervals = sapply(as.numeric(names(length.activity.bursts)), function(i) time.in.cage[which(burst.cumsum == i)][-1] )
					total.activity.burst.time = sum(unlist(activity.burst.intervals))
					max.activity.burst.length = max(length.activity.bursts)
					max.activity.burst.time = sum(activity.burst.intervals[[1]])
				}else{
					total.activity.burst.time = 0
					max.activity.burst.length = 0
					max.activity.burst.time = 0
				}
			}else{
				total.activity.burst.time = 0
				max.activity.burst.length = 0
				max.activity.burst.time = 0
			}

			# Exploration
			## How much time in each cage?
			cage.time = by(time.in.cage, window.data$data[[subject]]$Cage[-c(1:2)], sum)[colnames(window.data$layout$shortest.paths)]# Only include intervals completely within the window.
			cage.time[is.na(cage.time)] = 0 # For reporting.
			cage.probability = stats::setNames(cage.time / sum(cage.time, na.rm = T), colnames(window.data$layout$shortest.paths))

			## Cage use.
			max.cage.use = max(cage.probability, na.rm = T)
			mean.cage.use = mean(cage.probability, na.rm = T)
			min.cage.use = min(cage.probability, na.rm = T)
			sd.cage.use = sd(cage.probability, na.rm = T)
			sorted.cage.probability = sort(cage.probability)
			upper.cage.use =  unname(sorted.cage.probability)[ceiling(length(sorted.cage.probability) * .95)]
			median.cage.use = stats::median(cage.probability, na.rm = TRUE)
			lower.cage.use = unname(sorted.cage.probability)[floor(length(sorted.cage.probability) * .05)]

			## Cage time entropy
			cage.time.entropy = -sum(cage.probability * log(cage.probability), na.rm = T) / log(length(cage.probability))

			## Adjusted cage probability based on cage betweenness.
			res = stats::resid(stats::lm(cage.probability ~ window.data$layout$centrality))
			res = res - min(res, na.rm = T) + min(cage.probability, na.rm = T) # Lowest assumed to be unaffected by topology.
			adj.cage.probability = res / sum(res, na.rm = T) # Scaled to sum to 1.

			## Adjusted cage use.
			max.adj.cage.use = max(adj.cage.probability, na.rm = T)
			mean.adj.cage.use = mean(adj.cage.probability, na.rm = T)
			min.adj.cage.use = min(adj.cage.probability, na.rm = T)
			sd.adj.cage.use = sd(adj.cage.probability, na.rm = T)
			sorted.adj.cage.probability = sort(adj.cage.probability)
			upper.adj.cage.use =  unname(sorted.adj.cage.probability)[ceiling(length(sorted.adj.cage.probability) * .95)]
			median.adj.cage.use = stats::median(adj.cage.probability, na.rm = TRUE)
			lower.adj.cage.use = unname(sorted.adj.cage.probability)[floor(length(sorted.adj.cage.probability) * .05)]

			## Adjusted cage time entropy
			adjusted.cage.time.entropy = -sum(adj.cage.probability * log(adj.cage.probability), na.rm = T) / log(length(adj.cage.probability))

			## 'Location' entropy
			## Which cages were used?
			cage.transitions = as.numeric(table(window.data$data[[subject]]$Cage)[colnames(window.data$layout$shortest.paths)])
			cage.transition.probability = stats::setNames(cage.transitions / sum(cage.transitions, na.rm = T), colnames(window.data$layout$shortest.paths))
			cage.transition.probability[is.na(cage.transition.probability)] = 0

			## Cage transitions.
			max.cage.transitions = max(cage.transition.probability, na.rm = T)
			mean.cage.transitions = mean(cage.transition.probability, na.rm = T)
			min.cage.transitions = min(cage.transition.probability, na.rm = T)
			sd.cage.transitions = sd(cage.transition.probability, na.rm = T)
			sorted.cage.transition.probability = sort(cage.transition.probability)
			upper.cage.transitions =  unname(sorted.cage.transition.probability)[ceiling(length(sorted.cage.transition.probability) * .95)]
			median.cage.transitions = stats::median(cage.transition.probability, na.rm = TRUE)
			lower.cage.transitions = unname(sorted.cage.transition.probability)[floor(length(sorted.cage.transition.probability) * .05)]

			## Cage location entropy
			cage.location.entropy = -sum(cage.transition.probability * log(cage.transition.probability), na.rm = T) / log(length(cage.transition.probability))

			## Adjusted cage probability based on cage betweenness.
			res = stats::resid(stats::lm(cage.transition.probability ~ window.data$layout$centrality))
			res = res - min(res, na.rm = T) + min(cage.transition.probability, na.rm = T) # Lowest assumed to be unaffected by topology.
			adj.cage.transition.probability = res / sum(res, na.rm = T) # Scaled to sum to 1.

			## Adjusted cage transitions.
			max.adj.cage.transitions = max(adj.cage.transition.probability, na.rm = T)
			mean.adj.cage.transitions = mean(adj.cage.transition.probability, na.rm = T)
			min.adj.cage.transitions = min(adj.cage.transition.probability, na.rm = T)
			sd.adj.cage.transitions = sd(adj.cage.transition.probability, na.rm = T)
			sorted.adj.cage.transition.probability = sort(adj.cage.transition.probability)
			upper.adj.cage.transitions =  unname(sorted.adj.cage.transition.probability)[ceiling(length(sorted.adj.cage.transition.probability) * .95)]
			median.adj.cage.transitions = stats::median(adj.cage.transition.probability, na.rm = TRUE)
			lower.adj.cage.transitions = unname(sorted.adj.cage.transition.probability)[floor(length(sorted.adj.cage.transition.probability) * .05)]

			## Adjusted location entropy
			adjusted.cage.location.entropy = -sum(adj.cage.transition.probability * log(adj.cage.transition.probability), na.rm = T) / log(length(adj.cage.transition.probability))

			# Revisiting.
			## How long does it take until the same cage is visited again (on the same night)?
			revisit.time = do.call("c", lapply(colnames(window.data$layout$shortest.paths), function(cage){
				intervals = window.data$data[[subject]]$Timestamp[-1][which(window.data$data[[subject]]$Cage == cage)]
				if(length(intervals) > 0){
					return(diff(intervals) - 1)
				}else{
					return(NA)
				}
			}))
			if(length(stats::na.omit(revisit.time)) == 0) revisit.time = rep(diff(time.bounds), 2)
			if(length(stats::na.omit(revisit.time)) == 0) revisit.time = rep(0, 2) # If no such intervals found.
			max.revisit.time = max(revisit.time, na.rm = TRUE)
			mean.revisit.time = mean(revisit.time, na.rm = TRUE)
			min.revisit.time = min(revisit.time, na.rm = TRUE)
			sd.revisit.time = stats::sd(revisit.time, na.rm = TRUE)
			sorted.revisit.time = sort(revisit.time)
			upper.revisit.time = unname(sorted.revisit.time)[ceiling(length(sorted.revisit.time) * .95)]
			median.revisit.time = median(revisit.time, na.rm = TRUE)
			lower.revisit.time = unname(sorted.revisit.time)[floor(length(sorted.revisit.time) * .05)]

			## How many cages are visited before returning to the cage?
			revisit.length = do.call("c", lapply(colnames(window.data$layout$shortest.paths), function(cage){
				intervals = which(window.data$data[[subject]]$Cage == cage)
				if(length(intervals) > 0){
					return(diff(intervals) - 1)
				}else{
					return(NA)
				}
			}))
			if(length(stats::na.omit(revisit.length)) == 0) revisit.length = rep(diff(time.bounds), 2)
			if(length(stats::na.omit(revisit.length)) == 0) revisit.length = rep(0, 2) # If no such intervals found.
			max.revisit.length = max(revisit.length, na.rm = TRUE)
			mean.revisit.length = mean(revisit.length, na.rm = TRUE)
			min.revisit.length = min(revisit.length, na.rm = TRUE)
			sd.revisit.length = stats::sd(revisit.length, na.rm = TRUE)
			sorted.revisit.length = sort(revisit.length)
			upper.revisit.length = unname(sorted.revisit.length)[ceiling(length(sorted.revisit.length) * .95)]
			median.revisit.length = median(revisit.length, na.rm = TRUE)
			lower.revisit.length = unname(sorted.revisit.length)[floor(length(sorted.revisit.length) * .05)]

			## Time of night least active.
			peak.inactive.timepoint = 0
			peak.active.timepoint = 0
			number.activity.blocks = 0
			if(nrow(window.data$data[[subject]]) > 0){
				activity.density = stats::density(window.data$data[[subject]]$Timestamp[-1], bw = "SJ", adjust = 2, kernel = "cosine", cut = 0)
				within.interval = which(activity.density$x > time.bounds[1] & activity.density$x < time.bounds[2])
				activity.density$x = activity.density$x[within.interval] - time.bounds[1]
				activity.density$y = activity.density$y[within.interval]
				peak.inactive.timepoint = activity.density$x[order(activity.density$y)[1]] / (time.bounds[2] - time.bounds[1])
				peak.active.timepoint = activity.density$x[order(activity.density$y, decreasing = T)[1]] / (time.bounds[2] - time.bounds[1])

				## How many blocks of activity are there?
				activity.blocks = activity.density$y > mean(activity.density$y)
				activity.block.transitions = which(activity.blocks[-length(activity.blocks)] != activity.blocks[-1])
				if(activity.blocks[1]) activity.block.transitions = c(1, activity.block.transitions) # Add a block transition if already high at start.
				if(activity.blocks[length(activity.blocks)]) activity.block.transitions = c(activity.block.transitions, length(activity.blocks)) # Add a block transition if already high at end.
				number.activity.blocks = ceiling(length(activity.block.transitions) / 2)
			}

			# Sociality
			## Cage sharing
			other.subjects = setdiff(subjects, subject)
			cage.sharing.matrix = cage.presence.matrix[, other.subjects] == cage.presence.matrix[, subject]
			cage.sharing = Rfast::rowsums(cage.sharing.matrix, na.rm = T)

			max.cage.sharing = max(cage.sharing, na.rm = T)
			mean.cage.sharing = mean(cage.sharing, na.rm = T)
			min.cage.sharing = min(cage.sharing, na.rm = T)
			sd.cage.sharing = sd(cage.sharing, na.rm = T)
			sorted.cage.sharing = sort(cage.sharing)
			upper.cage.sharing = unname(sorted.cage.sharing)[ceiling(length(sorted.cage.sharing) * .95)]
			median.cage.sharing = stats::median(sorted.cage.sharing, na.rm = T)
			lower.cage.sharing = unname(sorted.cage.sharing)[floor(length(sorted.cage.sharing) * .05)]

			## Time alone.
			alone.intervals = c(cage.presence.matrix.timestamp[(cage.sharing == 0)][-1], NA) - cage.presence.matrix.timestamp[(cage.sharing == 0)]
			alone.intervals = alone.intervals[-c(-1, -length(alone.intervals))]
			time.alone = sum(alone.intervals, na.rm = T)

			## Social interaction time
			# NOTE This calculates the total _time_ spent with each other animal.
			social.interaction.time = sapply(other.subjects, function(other){
				shared.intervals = (c(cage.presence.matrix.timestamp[-1], time.bounds[2]) - cage.presence.matrix.timestamp)[cage.sharing.matrix[, other]]
				sum(shared.intervals, na.rm = T)
			}) / diff(time.bounds)
			max.social.interaction.time = max(social.interaction.time, na.rm = T)
			mean.social.interaction.time = mean(social.interaction.time, na.rm = T)
			min.social.interaction.time = min(social.interaction.time, na.rm = T)
			sd.social.interaction.time = sd(social.interaction.time, na.rm = T)
			sorted.social.interaction.time = sort(social.interaction.time)
			upper.social.interaction.time = unname(sorted.social.interaction.time)[ceiling(length(sorted.social.interaction.time) * .95)]
			median.social.interaction.time = stats::median(social.interaction.time, na.rm = T)
			lower.social.interaction.time = unname(sorted.social.interaction.time)[floor(length(sorted.social.interaction.time) * .05)]

			## Social exposure
			### The fraction of animals the subject came in contact with at any point within the window.
			present = apply(cage.presence.matrix[, other.subjects], 2, function(col) sum(!is.na(col)) ) > 0
			social.exposure = sum(Rfast::colsums(cage.sharing.matrix, na.rm = T) > 0) / sum(present)

			## Distance from all others.
			dcpi = apply(cage.presence.matrix[, subjects], 2, fastmatch::fmatch, rownames(window.data$layout$shortest.paths))
			ix = dcpi[, subject]
			distance.matrix = apply(dcpi[, other.subjects], 2, function(jx){
				window.data$layout$shortest.paths[matrix.unmap(ix, jx, dim(window.data$layout$shortest.paths))]
			})
			intervals = diff(c(cage.presence.matrix.timestamp, time.bounds[2]))
			distance.from.all = Rfast::rowsums(distance.matrix, na.rm = T) / Rfast::rowsums(!is.na(distance.matrix), na.rm = T)
			if(all(is.na(distance.from.all))) distance.from.all = max(window.data$layout$shortest.paths) + 1
			max.distance.from.all = max(distance.from.all, na.rm = T)
			mean.distance.from.all = mean(distance.from.all, na.rm = T)
			min.distance.from.all = min(distance.from.all, na.rm = T)
			sd.distance.from.all = sd(distance.from.all, na.rm = T)
			sorted.distances = sort(distance.from.all)
			upper.distance.from.all = unname(sorted.distances)[ceiling(length(sorted.distances) * .95)]
			median.distance.from.all = stats::median(distance.from.all, na.rm = T)
			lower.distance.from.all = unname(sorted.distances)[floor(length(sorted.distances) * .05)]

			distance.from.each = Rfast::colsums(distance.matrix, na.rm = T) / Rfast::colsums(!is.na(distance.matrix), na.rm = T)
			weighted.distance.from.each = Rfast::colsums(distance.matrix * intervals, na.rm = T) / diff(time.bounds)
			if(all(is.na(distance.from.each))) distance.from.each = max(window.data$layout$shortest.paths) + 1
			max.distance.from.each = max(distance.from.each, na.rm = T)
			#mean.distance.from.each = mean(distance.from.each, na.rm = T) # Same as mean.distance.from.all
			min.distance.from.each = min(distance.from.each, na.rm = T)
			sd.distance.from.each = sd(distance.from.each, na.rm = T)
			sorted.distances = sort(distance.from.each)
			upper.distance.from.each = unname(sorted.distances)[ceiling(length(sorted.distances) * .95)]
			median.distance.from.each = stats::median(distance.from.each, na.rm = T)
			lower.distance.from.each = unname(sorted.distances)[floor(length(sorted.distances) * .05)]

			## Social gradient
			### Was the cage change to a more- or less-populated cage?
			true.transition = which(cage.presence.matrix[, subject][-nrow(cage.presence.matrix)] != cage.presence.matrix[, subject][-1]) + 1
			next.transition = true.transition[-1]
			true.transition = true.transition[-length(true.transition)]
			previous.cage = cage.presence.matrix[true.transition - 1, subject]
			this.cage = cage.presence.matrix[true.transition, subject]
			cage.occupancy.counts = data.frame(
				# _before_ is the occupancy of the cage the subject is leaving at the point it leaves.
				before = Rfast::rowsums(cage.presence.matrix[true.transition, other.subjects, drop = F] == previous.cage, na.rm = T),
				# _this_ is the occupancy of the cage the subject is entering at the point it enters
				this = Rfast::rowsums(cage.presence.matrix[true.transition, other.subjects, drop = F] == this.cage, na.rm = T),
				# _after_ is occupancy of the current cage at the point the subject leaves.
				after = Rfast::rowsums(cage.presence.matrix[next.transition, other.subjects, drop = F] == this.cage, na.rm = T)
			)

			cage.occupancy.change = cage.occupancy.counts$this - cage.occupancy.counts$before
			if(length(cage.occupancy.change) == 0) cage.occupancy.change = c(0, 0) # If there are no transitions
			max.sharing.change = max(cage.occupancy.change, na.rm = T)
			mean.sharing.change = mean(cage.occupancy.change, na.rm = T)
			min.sharing.change = min(cage.occupancy.change, na.rm = T)
			sd.sharing.change = sd(cage.occupancy.change, na.rm = T)
			sorted.cage.occupancy.change = sort(cage.occupancy.change)
			upper.sharing.change = unname(sorted.cage.occupancy.change)[ceiling(length(sorted.cage.occupancy.change) * .95)]
			median.sharing.change = stats::median(cage.occupancy.change, na.rm = T)
			lower.sharing.change = unname(sorted.cage.occupancy.change)[floor(length(sorted.cage.occupancy.change) * .05)]

			## Influence.
			## How many animals were in the cage when the subject leaves, vs. the number that were in the cage when it entered?
			cage.occupancy.influence = cage.occupancy.counts$after - cage.occupancy.counts$this
			if(length(cage.occupancy.influence) == 0) cage.occupancy.influence = c(0, 0) # If there are no transitions
			max.influence = max(cage.occupancy.influence, na.rm = T)
			mean.influence = mean(cage.occupancy.influence, na.rm = T)
			min.influence = min(cage.occupancy.influence, na.rm = T)
			sd.influence = sd(cage.occupancy.influence, na.rm = T)
			sorted.cage.occupancy.influence = sort(cage.occupancy.influence)
			upper.influence = unname(sorted.cage.occupancy.influence)[ceiling(length(sorted.cage.occupancy.influence) * .95)]
			median.influence = stats::median(cage.occupancy.influence, na.rm = T)
			lower.influence = unname(sorted.cage.occupancy.influence)[floor(length(sorted.cage.occupancy.influence) * .05)]

			## Following.
			## How often was this animal in close pursuit of each other animal?
			## Every cage entry (in the same direction) up to 1 s after another animal is recorded. Returned values are sums.
			# Timestamps converted to relative millisecond integers for IRanges.
			icole = data.frame(Timestamp = (window.data$data[[subject]]$Timestamp[-1] - time.bounds[1]) * 1000, From = window.data$data[[subject]]$Cage[-nrow(window.data$data[[subject]])], To = window.data$data[[subject]]$Cage[-1])
			# Following interval starts 1000 ms before cage transition.
			icoler = IRanges::IRanges(start = icole$Timestamp - 1000, end = icole$Timestamp - 1)
			follow.events = lapply(other.subjects, function(other){
				jcole = data.frame(Timestamp = (window.data$data[[other]]$Timestamp[-1] - time.bounds[1]) * 1000, From = window.data$data[[other]]$Cage[-nrow(window.data$data[[other]])], To = window.data$data[[other]]$Cage[-1])
				jcoler = IRanges::IRanges(start = jcole$Timestamp, end = jcole$Timestamp)
				overlaps = IRanges::findOverlaps(icoler, jcoler)
				follow = Rfast::rowsums(icole[IRanges::from(overlaps), c("From", "To")] == jcole[IRanges::to(overlaps), c("From", "To")]) == 2
				follow.result = icole[IRanges::from(overlaps)[follow], ]
				follow.result$Timestamp = follow.result$Timestamp / 1000 + time.bounds[1]
				return(follow.result)
			})
			names(follow.events) = other.subjects
			follow.events[[subject]] = data.frame(Timestamp = numeric(),  Transition = character())
			follow.events = follow.events[subjects]

			# Ethogram data.
			# This is prepared hourly.
			ethogram.data = apply(hourly.intervals, 1, function(hourly.interval){
				hcol = window.data$data[[subject]][which(window.data$data[[subject]]$Timestamp[-1] >= hourly.interval[1] & window.data$data[[subject]]$Timestamp[-1] < hourly.interval[2]) + 1, ]

				# Activity is the number of cage transitions per hour.
				ethogram.activity = nrow(hcol)

				# Exploration is the cage time entropy calculated hourly.
				hourly.time.in.cage = diff(hcol$Timestamp) # Only include intervals completely within the window.
				hourly.cage.time = by(hourly.time.in.cage, hcol$Cage[-1], sum)[colnames(window.data$layout$shortest.paths)]
				hourly.cage.probability = stats::setNames(hourly.cage.time / sum(hourly.cage.time, na.rm = T), colnames(window.data$layout$shortest.paths))
				ethogram.exploration = -sum(hourly.cage.probability * log(hourly.cage.probability), na.rm = T) / log(length(hourly.cage.probability))

				# Sociality is the time-weighted number of other animals in the same cage during the hour
				hourly.cage.presence.indices = which(cage.presence.matrix.timestamp >= hourly.interval[1] & cage.presence.matrix.timestamp < hourly.interval[2])
				hourly.cage.presence.timestamp = cage.presence.matrix.timestamp[hourly.cage.presence.indices]
				hourly.cage.sharing.matrix = cage.presence.matrix[hourly.cage.presence.indices, other.subjects, drop = FALSE] == cage.presence.matrix[hourly.cage.presence.indices, subject]
				hourly.cage.sharing = Rfast::rowsums(hourly.cage.sharing.matrix, na.rm = T)
				weighted.hourly.cage.sharing = hourly.cage.sharing[-length(hourly.cage.sharing)] * (diff(hourly.cage.presence.timestamp) / diff(suppressWarnings(range(hourly.cage.presence.timestamp))))
				ethogram.sociality = sum(weighted.hourly.cage.sharing, na.rm = T) # Weighted mean number of cagemates.

				return(list(ethogram.activity = ethogram.activity, ethogram.exploration = ethogram.exploration, ethogram.sociality = ethogram.sociality))
			})
			names(ethogram.data) = hourly.intervals[, 1]

			# Collation
			# Cast each metric to double (some are natively integers which yields a matrix of lists after rbinding).
			individual = c(
				path.length = c(as.double(path.length), NA)[1],
				max.time.in.cage = c(as.double(max.time.in.cage), NA)[1],
				mean.time.in.cage = c(as.double(mean.time.in.cage), NA)[1],
				min.time.in.cage = c(as.double(min.time.in.cage), NA)[1],
				sd.time.in.cage = c(as.double(sd.time.in.cage), NA)[1],
				upper.time.in.cage = c(as.double(upper.time.in.cage), NA)[1],
				median.time.in.cage = c(as.double(median.time.in.cage), NA)[1],
				lower.time.in.cage = c(as.double(lower.time.in.cage), min.time.in.cage)[1],
				time.high.activity = c(as.double(time.high.activity), NA)[1],
				time.sustained.activity = c(as.double(time.sustained.activity), NA)[1],
				time.active = c(as.double(time.active), NA)[1],
				time.inactive = c(as.double(time.inactive), NA)[1],
				total.activity.burst.time = c(as.double(total.activity.burst.time), NA)[1],
				max.activity.burst.length = c(as.double(max.activity.burst.length), NA)[1],
				max.activity.burst.time = c(as.double(max.activity.burst.time), NA)[1],
				max.cage.use = c(as.double(max.cage.use), NA)[1],
				mean.cage.use = c(as.double(mean.cage.use), NA)[1],
				min.cage.use = c(as.double(min.cage.use), NA)[1],
				sd.cage.use = c(as.double(sd.cage.use), NA)[1],
				upper.cage.use = c(as.double(upper.cage.use), NA)[1],
				median.cage.use = c(as.double(median.cage.use), NA)[1],
				lower.cage.use = c(as.double(lower.cage.use), min.cage.use)[1],
				cage.time.entropy = c(as.double(cage.time.entropy), NA)[1],
				max.adj.cage.use = c(as.double(max.adj.cage.use), NA)[1],
				mean.adj.cage.use = c(as.double(mean.adj.cage.use), NA)[1],
				min.adj.cage.use = c(as.double(min.adj.cage.use), NA)[1],
				sd.adj.cage.use = c(as.double(sd.adj.cage.use), NA)[1],
				upper.adj.cage.use = c(as.double(upper.adj.cage.use), NA)[1],
				median.adj.cage.use = c(as.double(median.adj.cage.use), NA)[1],
				lower.adj.cage.use = c(as.double(lower.adj.cage.use), min.adj.cage.use)[1],
				adjusted.cage.time.entropy = c(as.double(adjusted.cage.time.entropy), NA)[1],
				max.cage.transitions = c(as.double(max.cage.transitions), NA)[1],
				mean.cage.transitions = c(as.double(mean.cage.transitions), NA)[1],
				min.cage.transitions = c(as.double(min.cage.transitions), NA)[1],
				sd.cage.transitions = c(as.double(sd.cage.transitions), NA)[1],
				upper.cage.transitions = c(as.double(upper.cage.transitions), NA)[1],
				median.cage.transitions = c(as.double(median.cage.transitions), NA)[1],
				lower.cage.transitions = c(as.double(lower.cage.transitions), min.cage.transitions)[1],
				cage.location.entropy = c(as.double(cage.location.entropy), NA)[1],
				max.adj.cage.transitions = c(as.double(max.adj.cage.transitions), NA)[1],
				mean.adj.cage.transitions = c(as.double(mean.adj.cage.transitions), NA)[1],
				min.adj.cage.transitions = c(as.double(min.adj.cage.transitions), NA)[1],
				sd.adj.cage.transitions = c(as.double(sd.adj.cage.transitions), NA)[1],
				upper.adj.cage.transitions = c(as.double(upper.adj.cage.transitions), NA)[1],
				median.adj.cage.transitions = c(as.double(median.adj.cage.transitions), NA)[1],
				lower.adj.cage.transitions = c(as.double(lower.adj.cage.transitions), min.adj.cage.transitions)[1],
				adjusted.cage.location.entropy = c(as.double(adjusted.cage.location.entropy), NA)[1],
				max.revisit.time = c(as.double(max.revisit.time), NA)[1],
				mean.revisit.time = c(as.double(mean.revisit.time), NA)[1],
				min.revisit.time = c(as.double(min.revisit.time), NA)[1],
				sd.revisit.time = c(as.double(sd.revisit.time), NA)[1],
				upper.revisit.time = c(as.double(upper.revisit.time), NA)[1],
				median.revisit.time = c(as.double(median.revisit.time), NA)[1],
				lower.revisit.time = c(as.double(lower.revisit.time), min.revisit.time)[1],
				max.revisit.length = c(as.double(max.revisit.length), NA)[1],
				mean.revisit.length = c(as.double(mean.revisit.length), NA)[1],
				min.revisit.length = c(as.double(min.revisit.length), NA)[1],
				sd.revisit.length = c(as.double(sd.revisit.length), NA)[1],
				upper.revisit.length = c(as.double(upper.revisit.length), NA)[1],
				median.revisit.length = c(as.double(median.revisit.length), NA)[1],
				lower.revisit.length = c(as.double(lower.revisit.length), min.revisit.length)[1],
				peak.inactive.timepoint = c(as.double(peak.inactive.timepoint), NA)[1],
				peak.active.timepoint = c(as.double(peak.active.timepoint), NA)[1],
				number.activity.blocks = c(as.double(number.activity.blocks), NA)[1],
				max.cage.sharing = c(as.double(max.cage.sharing), NA)[1],
				mean.cage.sharing = c(as.double(mean.cage.sharing), NA)[1],
				upper.cage.sharing = c(as.double(upper.cage.sharing), NA)[1],
				median.cage.sharing = c(as.double(median.cage.sharing), NA)[1],
				lower.cage.sharing = c(as.double(lower.cage.sharing), 0)[1],
				time.alone = c(as.double(time.alone), NA)[1],
				max.social.interaction.time = c(as.double(max.social.interaction.time), NA)[1],
				mean.social.interaction.time = c(as.double(mean.social.interaction.time), NA)[1],
				min.social.interaction.time = c(as.double(min.social.interaction.time), NA)[1],
				sd.social.interaction.time = c(as.double(sd.social.interaction.time), NA)[1],
				upper.social.interaction.time = c(as.double(upper.social.interaction.time), NA)[1],
				median.social.interaction.time = c(as.double(median.social.interaction.time), NA)[1],
				lower.social.interaction.time = c(as.double(lower.social.interaction.time), min.social.interaction.time)[1],
				social.exposure = c(as.double(social.exposure), NA)[1],
				max.distance.from.all = c(as.double(max.distance.from.all), NA)[1],
				mean.distance.from.all = c(as.double(mean.distance.from.all), NA)[1],
				min.distance.from.all = c(as.double(min.distance.from.all), NA)[1],
				sd.distance.from.all = c(as.double(sd.distance.from.all), NA)[1],
				upper.distance.from.all = c(as.double(upper.distance.from.all), NA)[1],
				median.distance.from.all = c(as.double(median.distance.from.all), NA)[1],
				lower.distance.from.all = c(as.double(lower.distance.from.all), min.distance.from.all)[1],
				max.distance.from.each = c(as.double(max.distance.from.each), NA)[1],
				#mean.distance.from.each = c(as.double(mean.distance.from.each), NA)[1],
				min.distance.from.each = c(as.double(min.distance.from.each), NA)[1],
				sd.distance.from.each = c(as.double(sd.distance.from.each), NA)[1],
				upper.distance.from.each = c(as.double(upper.distance.from.each), NA)[1],
				median.distance.from.each = c(as.double(median.distance.from.each), NA)[1],
				lower.distance.from.each = c(as.double(lower.distance.from.each), min.distance.from.each)[1],
				max.sharing.change = c(as.double(max.sharing.change), NA)[1],
				mean.sharing.change = c(as.double(mean.sharing.change), NA)[1],
				min.sharing.change = c(as.double(min.sharing.change), NA)[1],
				sd.sharing.change = c(as.double(sd.sharing.change), NA)[1],
				upper.sharing.change = c(as.double(upper.sharing.change), NA)[1],
				median.sharing.change = c(as.double(median.sharing.change), NA)[1],
				lower.sharing.change = c(as.double(lower.sharing.change), min.sharing.change)[1],
				max.influence = c(as.double(max.influence), NA)[1],
				mean.influence = c(as.double(mean.influence), NA)[1],
				min.influence = c(as.double(min.influence), NA)[1],
				sd.influence = c(as.double(sd.influence), NA)[1],
				upper.influence = c(as.double(upper.influence), NA)[1],
				median.influence = c(as.double(median.influence), NA)[1],
				lower.influence = c(as.double(lower.influence), min.influence)[1],
				number.follow.events = c(as.double(sum(sapply(follow.events, nrow))), NA)[1],
				sd.follow.events = c(as.double(sd(sapply(follow.events, nrow))), NA)[1],
				mean.follow.wins = NA, # Placeholder: value calculated below.
				sd.follow.wins = NA # Placeholder: value calculated below.
			)
		}

		ethogram = data.frame(
			activity = as.double(sapply(ethogram.data, "[[", "ethogram.activity")),
			exploration = as.double(sapply(ethogram.data, "[[", "ethogram.exploration")),
			sociality = as.double(sapply(ethogram.data, "[[", "ethogram.sociality"))
		)
		rownames(ethogram) = hourly.intervals[, 1]

		clustering = list(
			interaction.time = setNames(rep(NA, length(subjects)), subjects),
			social.distance = setNames(rep(NA, length(subjects)), subjects),
			following = setNames(rep(NA, length(subjects)), subjects)
		)
		if(nrow(window.data$data[[subject]]) >= 3){ # If the animal did not move at all all night, consider it to be missing (first timestamp is previous cage, need at least two other timestamps to calculate an interval).
			clustering$interaction.time[subjects] = c(setNames(1, subject), social.interaction.time)[subjects]
			clustering$social.distance[subjects] = c(setNames(0, subject), setNames(weighted.distance.from.each, other.subjects))[subjects]
			clustering$following[subjects] = c(setNames(0, subject), sapply(follow.events, nrow))[subjects]
		}

		end.time = Sys.time()
		dev = list(
			start.time = start.time,
			end.time = end.time,
			elapsed.time = as.numeric(end.time) - as.numeric(start.time)
		)

		subject.results = list(window = window.data$window.definition["id"], subject = subject, individual = individual, cage.probability = cage.probability, ethogram = ethogram, clustering = clustering, follow.events = follow.events, dev = dev)
		if(log) file.create(paste0("colonytrack.progress/", window.data$window.definition["id"], "_", subject))

		return(subject.results)
	})
	names(results) = subjects


	# How often does each animal win in a follow encounter?
	## This needs the full subject matrix, so is calculated here.
	follow.matrix = sapply(results, function(result) result$clustering$following )
	win.matrix = follow.matrix / (follow.matrix + t(follow.matrix))
	diag(win.matrix) = NA
	follow.means = colMeans(win.matrix, na.rm = T)
	follow.means[is.nan(follow.means)] = NA # Turn NaNs also into NA.
	follow.sds = sqrt(colVars(win.matrix, na.rm = T))
	follow.sds[is.nan(follow.sds)] = NA # Turn NaNs also into NA.
	for(subject in subjects){
		results[[subject]]$individual["mean.follow.wins"] = follow.means[subject]
		results[[subject]]$individual["follow.sds"] = follow.sds[subject]
	}

	rm(window.data)
	gc()

	return(results)

}
