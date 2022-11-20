#' Preparation of tracking data.
#'
#' Reads raw antenna contact data and metadata files.
#'
#' This function requires raw CSV data from the Colony Cage as well as a
#' subject/animal description file, a cage layout description file and a log of
#' events. An optional metadata file allows finer description of the cages.
#'
#' The processed data are padded to start at "lights on" of the first day to
#' "lights off" of the last day (as specified in the 'Events' file).
#' Nevertheless, the first and last days (and any interruptions in recording)
#' will result in missing data that may need to be handled during analysis. The
#' actogram (produced by \code{\link{plot}} of the \code{colonytrack_data}
#' object) can be used to identify such missing blocks.
#'
#' @param dataFiles A vector of filenames. Each file is a CSV dump of antenna
#'   contact data. These files should cover a contiguous period encompassing the
#'   whole experiment. Files can be given as full pathnames so that the often
#'   large data volumes can be stored outside the analysis directory (for
#'   example on an external fileserver). Any file format readable by
#'   \code{\link[utils]{read.table}} can be used.
#' @param subjectFile A tab-delimited file describing the subjects/animals. This
#'   information is required to link RFID tags to variables/factors such as
#'   experimental group, age, weight etc.
#' @param networkFile A tab-delimited file describing the layout of the cage
#'   system. This is currently in the the form of an unweighted undirected
#'   network.
#' @param eventsFile A tab-delimited file describing events. This must minimally
#'   include details of the light cycle. See Details for more information.
#' @param cageQualityFile An optional tab-delimited file describing the content
#'   of the cages (food, bedding, toys etc.). See Details for more information.
#'
#' @return A \code{colonytrack_data} object.
#'
#'
#' @importFrom stats setNames na.omit
#' @importFrom utils read.delim read.table
#' @importFrom tools file_ext
#' @importFrom pbapply pboptions pblapply
#' @importFrom parallel makeForkCluster parLapply stopCluster
#' @importFrom igraph graph_from_edgelist shortest.paths betweenness
#' @importFrom fastmatch fmatch
#' @importFrom Rfast sort_cor_vectors
#'
#' @export
read_data = function(dataFiles, subjectFile, networkFile, eventsFile, cageQualityFile = NULL){
	## This network file contains cage-to-cage links
	network = utils::read.delim(networkFile, sep = "\t")
	network = network[network$SourceType != "None" & network$TargetType != "None", ]
	cages = union(network$Source, network$Target)
	readers = unique(network$Link)

	transitions = do.call("rbind", lapply(cages, function(cage){
		is.source = network$Source %in% cage
		is.target = network$Target %in% cage
		cage.rfids = network$Link[is.source | is.target]
		cage.transitions = expand.grid(cage.rfids, cage.rfids, stringsAsFactors = FALSE)
		nonself.cage.transitions = cage.transitions[cage.transitions$Var1 != cage.transitions$Var2, , drop = F]
		cage.type = c(network$SourceType[is.source], network$TargetType[is.target])[1]
		if(nrow(nonself.cage.transitions) > 0){
			if(cage.type == "Tunnel"){
				return(do.call("rbind", lapply(seq_along(nonself.cage.transitions$Var1), function(a){
					link = network[network$Link == nonself.cage.transitions[a, "Var1"], ]
					if(link$Source == cage){
						cbind(nonself.cage.transitions[a, , drop = F], cage = link$Target)
					}else{
						cbind(nonself.cage.transitions[a, , drop = F], cage = link$Source)
					}
				})))
			}else{
				return(cbind(nonself.cage.transitions, cage))
			}
		}else if(nrow(cage.transitions) > 0){
			link = network[network$Link == cage.transitions$Var1, , drop = F]
			if(!(link$SourceType == "Tunnel" | link$TargetType == "Tunnel")){
				warning(paste0("The cage '", cage, "' is a cul-de-sac."))
				return(NULL)
			}
		}else{
			warning(paste0("The cage '", cage, "' is not connected to any other cages and is being ignored. Please check your network description file."))
			return(NULL)
		}
	}))
	colnames(transitions) = c("Reader1", "Reader2", "Cage")
	rownames(transitions) = paste(transitions$Reader1, transitions$Reader2, sep = ".")

	subjects = utils::read.delim(subjectFile, sep = "\t")
	subjects$Tag = toupper(subjects$Tag) # Make all upper case to ensure matches.
	if(any(subjects$Tag == "")) warn("Some subjects do not have tags assigned. These have been skipped, so will be absent in the resulting data object.")
	subjects = subjects[subjects$Tag != "", , drop = FALSE]
	if(is.null(subjects$SubjectID)) subjects$SubjectID = subjects$AnimalID
	if(is.null(subjects$SubjectID)) subjects$SubjectID = subjects$IDs # Compatibilty with idLabelR.
	if(is.null(subjects$SubjectID)) subjects$SubjectID = subjects$ID
	if(is.null(subjects$SubjectID)) stop("There must be a column 'SubjectID' in the subjectFile.")
	subjects$SubjectID = as.character(subjects$SubjectID)

	# Cage distances
	edgelist = network[, c("Source", "Target")]
	graph = igraph::graph_from_edgelist(as.matrix(edgelist))
	shortest.paths = igraph::shortest.paths(graph)

	# Read events data
	events = NULL
	events = utils::read.delim(eventsFile)
	if(nrow(events[events$Event == "LightsOn", , drop = FALSE]) == 0) stop("The 'events' file must contain an entry for 'LightsOn'.")
	# Convert to UTC.
	events$Start = as.POSIXct(as.numeric(strptime(events$Start, format = "%Y-%m-%d %H:%M:%S", tz = rev(strsplit(events$Start, " ")[[1]])[1])), origin = "1970-01-01 00:00:00", tz = "UTC")
	events$End = as.POSIXct(as.numeric(strptime(events$End, format = "%Y-%m-%d %H:%M:%S", tz = rev(strsplit(events$End, " ")[[1]])[1])), origin = "1970-01-01 00:00:00", tz = "UTC")
	events$Value = as.POSIXct(as.numeric(strptime(events$Value, format = "%H:%M:%S", tz = rev(strsplit(events$Value, " ")[[1]])[1])), origin = "1970-01-01 00:00:00", tz = "UTC")

	# Read cage 'quality' data
	cage.quality = NULL
	if(!is.null(cageQualityFile)){
		cage.quality = utils::read.delim(cageQualityFile)
		# Convert to UTC.
		cage.quality$Start = as.POSIXct(as.numeric(strptime(cage.quality$Start, format = "%Y-%m-%d %H:%M:%S", tz = rev(strsplit(cage.quality$Start, " ")[[1]])[1])), origin = "1970-01-01 00:00:00", tz = "UTC")
		cage.quality$End = as.POSIXct(as.numeric(strptime(cage.quality$End, format = "%Y-%m-%d %H:%M:%S", tz = rev(strsplit(cage.quality$End, " ")[[1]])[1])), origin = "1970-01-01 00:00:00", tz = "UTC")
	}

	# Quality control info.
	qc = list()

	# Read raw tracking data
	msg("Reading tracking data")
	# Run a preliminary check first to make sure all files are found and of the right sort.
	files.exist = sapply(dataFiles, function(file) file.exists(file) )
	files.areCSV = sapply(dataFiles, function(file) tolower(tools::file_ext(file)) == "csv" )
	if(!all(files.exist & files.areCSV)){
		if(!all(files.exist)) message(paste0("The file(s) '",  paste(basename(dataFiles)[which(!files.exist)], collapse = "', '"), "' were not found."))
		if(!all(files.areCSV)) message(paste0("The file(s) '",  paste(basename(dataFiles)[which(!files.areCSV)], collapse = "', '"), "' is not in CSV format."))
		stop("The input data files are not all valid. Please check the list of file paths.")
	}
	files.areUTF16 = sapply(dataFiles, function(file){
		length(scan(file, what = character(), fileEncoding = "UTF-16LE", nlines = 2, quiet = TRUE)) == 2
	})
	if(!all(files.areUTF16)){
		stop("The input data files are not all correctly encoded. The raw CSV files must be in UTF-16LE.")
	}

	# Set up cluster
	cluster = parallel::makePSOCKcluster(min(length(dataFiles), parallel::detectCores() - 1)) # One thread per file.
	raw.data = do.call("rbind", parallel::parLapply(cluster, dataFiles, function(file){
		raw.data = utils::read.delim(file, sep=";", header = TRUE, comment.char = "#", fileEncoding = "UTF-16LE")
		names(raw.data) = c("Timestamp", "SubjectID", "X1", "ReaderID", "Duration", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8", "Special", "X.10", "X.11")
		raw.data = raw.data[, c("Timestamp", "SubjectID", "ReaderID", "Duration")]
		raw.data$Timestamp = as.numeric(sub(",", ".", raw.data$Timestamp)) # In case of European comma decimals.
		raw.data$SubjectID = toupper(raw.data$SubjectID) # Ensure upper case for ID matching.
		return(raw.data)
	}))
	parallel::stopCluster(cluster)

	# Resolve multiple tags (collapse to first in list)
	msg("Resolving subject ids")
	clean.tags = stats::setNames(subjects$Tag, subjects$Tag)
	for(tag in subjects$Tag){
		tags = trimws(strsplit(tag, ",")[[1]])
		clean.tags[tag] = tags[1]
		if(length(tags) > 1){ # Retagging has occurred
			raw.data$SubjectID[raw.data$SubjectID %in% tags] = tags[1] # Replace all with first tag
		}
	}
	names(clean.tags) = subjects$SubjectID

	# Clean up tags not assigned to a subject.
	clean = raw.data$SubjectID %in% clean.tags
	qc$dirty.data.count = sum(!clean) # Should only be control entries.
	qc$dirty.data = raw.data[!clean, ] # Should only be control entries.
	raw.data = raw.data[clean, ]

	# Make sure all raw.data are in chronological order
	# Convert timestamp (in fractions of days from SQL origin) to seconds (using POSIX origin)
	raw.data$Timestamp = as.numeric(as.POSIXct(raw.data$Timestamp * 86400, origin="1899-12-30 00:00:00", tz="UTC"))
	msg("Checking timestamp chronology")
	raw.data = raw.data[Rfast::sort_cor_vectors(seq_along(raw.data$Timestamp), raw.data$Timestamp, stable = T), ]
	# Clean up raw data and add proper IDs and accurate timestamp.
	raw.data = data.frame(Timestamp = raw.data$Timestamp, TagID = raw.data$SubjectID, SubjectID = subjects$SubjectID[fastmatch::fmatch(raw.data$SubjectID, subjects$Tag)], ReaderID = raw.data$ReaderID, Duration = raw.data$Duration)

	# All data are converted to and processed in UTC
	# Every day starts at lights on for that day and ends at end of the following lights off period.
	# Where the recording has been clipped (first and last tracking days), these should be padded to prevent downstream issues.
	start.day = as.Date(as.POSIXct(raw.data$Timestamp[1], origin = "1970-01-01 00:00:00", tz = "UTC"))
	end.day = as.Date(as.POSIXct(raw.data$Timestamp[length(raw.data$Timestamp)], origin = "1970-01-01 00:00:00", tz = "UTC"))
	light.cycle.blocks = events[events$Event == "LightsOn", , drop = FALSE]
	# In case of conflicts, the last entry wins.
	start.light.cycle.block = rev(which(sapply(seq_along(rownames(light.cycle.blocks)), function(i){
		as.Date(light.cycle.blocks$Start[i]) <= start.day	&	as.Date(light.cycle.blocks$End[i]) >= start.day
	})))[1]
	start.day.begin = as.integer(as.POSIXct(paste0(start.day, " ", format(light.cycle.blocks[start.light.cycle.block, "Value"], format = "%H:%M:%S"), ":00:00"), tz = "UTC"))
	if(start.day.begin > raw.data$Timestamp[1]){ # First data was before 'lights on' on calculated first day.
		start.day = start.day - 1
		start.day.begin = as.integer(as.POSIXct(paste0(start.day, " ", format(light.cycle.blocks[start.light.cycle.block, "Value"], format = "%H:%M:%S"), ":00:00"), tz = "UTC"))
	}
	end.light.cycle.block = rev(which(sapply(seq_along(rownames(light.cycle.blocks)), function(i){
		as.Date(light.cycle.blocks$Start[i]) <= end.day	&	as.Date(light.cycle.blocks$End[i]) >= end.day
	})))[1]
	end.night.end = as.integer(as.POSIXct(paste0(end.day + 1, " ", format(light.cycle.blocks[end.light.cycle.block, "Value"], format = "%H:%M:%S"), ":00:00"), tz = "UTC"))
	if(end.night.end < raw.data$Timestamp[length(raw.data$Timestamp)]){ # Last data was after 'lights on' on calculated last day.
		end.day = end.day + 1
		end.night.end = as.integer(as.POSIXct(paste0(end.day + 1, " ", format(light.cycle.blocks[end.light.cycle.block, "Value"], format = "%H:%M:%S"), ":00:00"), tz = "UTC"))
	}
	# Check for timestamps that are not covered by light cycle information. This is a fatal error if it occurs.
	time.gaps = rep(TRUE, nrow(raw.data))
	for(i in seq_along(rownames(light.cycle.blocks))){
		this.start = as.numeric(light.cycle.blocks$Start[i])
		this.end = as.numeric(light.cycle.blocks$End[i])
		this.block = which(raw.data$Timestamp >= this.start & raw.data$Timestamp <= this.end)
		time.gaps[this.block] = FALSE
	}
	if(any(time.gaps)){
		bounds = c(1, which(time.gaps[-1] != time.gaps[-length(time.gaps)]), length(time.gaps))
		bounds[is.na(bounds)] = FALSE
		gap.starts = bounds[which(!time.gaps[bounds] & time.gaps[bounds + 1])]
		gap.ends = bounds[which(time.gaps[bounds] & !time.gaps[bounds + 1])]
		gap.starts = as.POSIXct(raw.data$Timestamp[gap.starts], origin = "1970-01-01 00:00:00", tz = "UTC")
		gap.ends = as.POSIXct(raw.data$Timestamp[gap.ends], origin = "1970-01-01 00:00:00", tz = "UTC")

		stop(paste0("There are measurements that fall outside the defined light cycles. Please check the 'events' file for the intervals: ", paste(paste0(gap.starts, " UTC - ", gap.ends, " UTC"), collapse = "; ")))
	}

	# Get the start and finish timestamps for each day/night.
	dates = seq(as.Date(start.day), as.Date(end.day), by="days")
	daystart = as.numeric(as.POSIXct(paste0(dates, " ", format(light.cycle.blocks[start.light.cycle.block, "Value"], format = "%H:%M:%S"), ":00:00"), tz = "UTC"))
	days = data.frame(
		id = as.character(dates),
		start = daystart,
		end = daystart + (12 * 3600) # Fixed day length of 12 h for now.
	)
	rownames(days) = as.character(dates)
	class(days) = c("data.frame", "colonytrack_windows")
	nights = data.frame(
		id = as.character(dates),
		start = days$end,
		end = daystart + (24 * 3600) # Fixed night length of 12 h for now.
	)
	rownames(nights) = as.character(dates)
	class(nights) = c("data.frame", "colonytrack_windows")

	# For each subject, get cage presence and add timestamp info.
	msg("Calculating trajectories")
	qc$trajectory = list()
	trajectory.list = lapply(subjects$SubjectID, function(subject){
		subject.data = raw.data[which(raw.data$TagID == clean.tags[subject]), ]
		time = NULL
		trajectory = data.frame(Timestamp = NA, Cage = NA)
		qc$trajectory[[subject]] <<- list()
		if(nrow(subject.data) != 0){ # ID exists
			# Remove duplicate antenna contacts (keeping the first one). [Animal 'stays in the cage' until it can be unequivocally placed in a new one].
			subject.data$Transition = subject.data$ReaderID != c("x", subject.data$ReaderID[-nrow(subject.data)])
			qc$trajectory[[subject]]$duplicate.read.count = sum(!subject.data$Transition)
			qc$trajectory[[subject]]$duplicate.reads = subject.data[!subject.data$Transition, ]
			subject.data = subject.data[subject.data$Transition, ]
			# Calculate trajectory.
			if(sum(subject.data$Transition) > 1){ # Check again after de-duplication of antenna contacts.
				start.indices = 1:(length(subject.data$Timestamp) - 1)
				end.indices = 2:length(subject.data$Timestamp)
				ReaderPairId = paste(subject.data$ReaderID[start.indices], subject.data$ReaderID[end.indices], sep = ".")
				Cage = transitions[ReaderPairId, "Cage"]
				trajectory = data.frame(Timestamp = subject.data$Timestamp[start.indices], Cage = Cage, ReaderPairId = ReaderPairId)
				is.trajectory = !is.na(trajectory$Cage)
				#
				qc$trajectory[[subject]]$non.trajectory.read.count <<- sum(!is.trajectory)
				qc$trajectory[[subject]]$non.trajectory.reads <<- trajectory[!is.trajectory, ]
				#
				trajectory = trajectory[which(!is.na(trajectory$Cage)), ]
				trajectory = trajectory[which(trajectory$Cage != c("", trajectory$Cage[-length(trajectory$Cage)])), ]
				if(nrow(trajectory) > 1){ # Check once more after removal of non-cage transitions.
					# Convert timestamps to date/time
					datetime = as.POSIXct((trajectory$Timestamp), origin = "1970-01-01 00:00:00", tz = "UTC")
					time = data.frame(
						Timestamp = as.numeric(datetime),
						DateTime = datetime,
						Day = as.character(format(datetime, format="%F")),
						Hour = as.numeric(format(datetime, format="%H")),
						ZTDay = rep(NA, length(datetime)),
						ZT = rep(NA, length(datetime)),
						Nighttime = rep(NA, length(datetime)),
						Cage = rep(NA, length(datetime)),
						stringsAsFactors = FALSE
					)
					light.cycle.blocks = events[events$Event == "LightsOn", , drop = FALSE] # Each block processed separately then merged
					for(i in seq_along(rownames(light.cycle.blocks))){
						this.block = which(datetime >= light.cycle.blocks[i, "Start"] & datetime <= light.cycle.blocks[i, "End"])
						lights.on = unlist(strsplit(format(light.cycle.blocks[i, "Value"], format = "%Y-%m-%d %H:%M:%S"), " "))
						ZT0 = strptime(paste0(time$Day[this.block], " ", lights.on[2]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
						previous.days = as.character(format(as.POSIXct(time$Day[this.block]) - (3600 * 24), format="%F"))
						ZT0.previous = strptime(paste0(previous.days, " ", lights.on[2]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
						time$ZTDay[this.block] = format(time$Day[this.block], format="%F")
						time$ZT[this.block] = (as.numeric(time$DateTime[this.block]) - as.numeric(ZT0)) / 3600
						is.previous = time$ZT[this.block] < 0
						time$ZTDay[this.block][is.previous] = format(previous.days[is.previous], format="%F")
						time$ZT[this.block][is.previous] = (as.numeric(time$DateTime[this.block][is.previous]) - as.numeric(ZT0.previous[is.previous])) / 3600
					}
					time$Nighttime = time$ZT >= 12 & time$ZT < 24
					# Add on cage trajectory.
					time$Cage = trajectory$Cage
					# Remove any submillisecond events (take last cage of any duplicate timestamps).
					submillisecond = rev(duplicated(rev(time$Timestamp)))
					qc$trajectory[[subject]]$submillisecond.count <<- sum(submillisecond)
					time = time[!submillisecond, ]
				}
			}
		}
		if(is.null(time)){
			time = data.frame(
				Timestamp = NA,
				DateTime = NA,
				Day = NA,
				Hour = NA,
				ZTDay = NA,
				ZT = NA,
				Nighttime = NA,
				Cage = NA,
				stringsAsFactors = F)
		}
		return(time)
	})
	names(trajectory.list) = subjects$SubjectID

	result = list(
		info = list(
			timestamp.range = c(raw.data$Timestamp[1], raw.data$Timestamp[length(raw.data$Timestamp)]),
			padded.timestamp.range = c(start.day.begin, end.night.end),
			days = days,
			nights = nights,
			subjects = subjects$SubjectID,
			subject.info = subjects,
			processed = Sys.time(),
			version = paste("ColonyTrack", packageVersion("ColonyTrack"))
		),
		qc = qc,
		data = trajectory.list,
		raw.data = raw.data,
		layout = list(
			cages = cages,
			cage.quality = cage.quality,
			readers = readers,
			transitions = transitions,
			network = network,
			graph = graph,
			shortest.paths = shortest.paths,
			centrality = igraph::betweenness(graph)
		)
	)
	class(result) = "colonytrack_data"

	return(result)
}


