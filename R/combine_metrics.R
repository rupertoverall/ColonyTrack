#' Combine metrics objects.
#'
#' Joins metrics results into one contiguous metrics object.
#'
#' This function takes processed results from several
#' \code{\link{calculate_metrics}} runs and merges them into one
#' \code{colonytrack_metrics} object. This allows metrics to be calculated in
#' batches to limit memory usage.
#'
#' @param metrics.list A list containing any number of
#'   \code{colonytrack_metrics} objects in any order. These will be merged into
#'   one combined \code{colonytrack_metrics} object sorted by date. This
#'   parameter may also be a character list of filenames pointing to saved RData
#'   archives containing the \code{colonytrack_metrics} objects.
#'
#' @return A \code{colonytrack_metrics} object.
#'
#' @seealso \code{\link{calculate_metrics}}.
#'
#' @export
combine_metrics = function(metrics.list){
	metrics.list = as.list(metrics.list)
	if(all(sapply(metrics.list, class) == "character")){
		# Load the metrics RData files.
		for(i in 1:length(metrics.list)){
			metrics.list[[i]] = eval(parse(text = load(metrics.list[[i]])))
		}
	}
	if(!all(sapply(metrics.list, class) == "colonytrack_metrics")){
		stop("One or more of the supplied objects/files is not a 'colonytrack_metrics' object.")
	}

	metrics = list()

	metrics$info = list(windows = list(), subjects = NULL, feature.names = NULL, var.names = NULL, cage.layout = NULL, processed = NULL, version = NULL)
	windows = do.call("rbind", lapply(lapply(metrics.list, "[[", "info"), "[[", "windows"))
	ordered = order(windows$start) # Reorder by starting timestamp.
	metrics$info$windows = windows[ordered ,]
	processed.dates = lapply(lapply(metrics.list, "[[", "info"), "[[", "processed")
	metrics$info$processed = processed.dates[[order(unlist(processed.dates), decreasing = T)[1]]]

	if(!all(sapply(lapply(lapply(metrics.list, "[[", "info"), "[[", "subjects"), identical, metrics.list[[1]]$info$subjects))) warning("Not all objects contain the same subjects. Were these all from the same experiment? \nOnly the intersection of subjects will be saved.")
	metrics$info$subjects =  Reduce(intersect, lapply(lapply(metrics.list, "[[", "info"), "[[", "subjects"))
	if(!all(sapply(lapply(lapply(metrics.list, "[[", "info"), "[[", "feature.names"), identical, metrics.list[[1]]$info$feature.names))) warning("Not all objects contain the same metrics. Were these created with the same version of ColonyTrack? \nOnly the intersection of individual metrics names will be saved.")
	metrics$info$feature.names = Reduce(intersect, lapply(lapply(metrics.list, "[[", "info"), "[[", "feature.names"))
	metrics$info$var.names = Reduce(intersect, lapply(lapply(metrics.list, "[[", "info"), "[[", "var.names"))
	metrics$info$cage.layout = lapply(lapply(metrics.list, "[[", "info"), "[[", "cage.layout")[ordered]
	versions = sapply(lapply(metrics.list, "[[", "info"), "[[", "version")
	metrics$info$version = unique(versions)
	if(!all(sapply(versions, identical, metrics$info$version))) warning("Not all objects were created with the same version of ColonyTrack. \nThis may cause problems with the resulting merged metrics object.")

	# The following will be trimmed to the intersections of subjects and variable names (also ensuring a consistent ordering).
	metrics$features = lapply(do.call("c", lapply(metrics.list, "[[", "features"))[ordered],  function(m) m[metrics$info$subjects, metrics$info$feature.names] )
	metrics$individual = lapply(do.call("c", lapply(metrics.list, "[[", "individual"))[ordered],  function(m) m[metrics$info$subjects, metrics$info$var.names] )
	metrics$cage.use = lapply(do.call("c", lapply(metrics.list, "[[", "cage.use"))[ordered],  function(m) m[metrics$info$subjects, ] )
	metrics$ethogram = lapply(do.call("c", lapply(metrics.list, "[[", "ethogram"))[ordered],  function(l){
		l$activity = l$activity[metrics$info$subjects, ]
		l$sociality = l$sociality[metrics$info$subjects, ]
		l$exploration = l$exploration[metrics$info$subjects, ]
		return(l)
	})

	metrics$clustering = list()
	metrics$clustering$interaction.time = lapply(do.call("c", lapply(metrics.list, function(m){ m$clustering$interaction.time }))[ordered], function(m) m[metrics$info$subjects, metrics$info$subjects] )
	metrics$clustering$cage.share = lapply(do.call("c", lapply(metrics.list, function(m){ m$clustering$cage.share }))[ordered], function(m) m[metrics$info$subjects, metrics$info$subjects] )
	metrics$clustering$social.distance = lapply(do.call("c", lapply(metrics.list, function(m){ m$clustering$social.distance }))[ordered], function(m) m[metrics$info$subjects, metrics$info$subjects] )
	metrics$clustering$following = lapply(do.call("c", lapply(metrics.list, function(m){ m$clustering$following }))[ordered], function(m) m[metrics$info$subjects, metrics$info$subjects] )

	metrics$dominance = lapply(do.call("c", lapply(metrics.list, "[[", "dominance"))[ordered], function(v) v[metrics$info$subjects] )
	metrics$follow.events = lapply(do.call("c", lapply(metrics.list, "[[", "follow.events"))[ordered], function(l){
		lapply(l[metrics$info$subjects], function(ll) ll[metrics$info$subjects] )
	})

	class(metrics) = "colonytrack_metrics"

	return(metrics)
}
