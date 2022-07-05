#'Retrieve one or more metrics.
#'
#'Retrieves data for all individual metrics for a single subject.
#'
#'@param subject The name of the subject for which the data is to be retrieved.
#'@param metrics A \code{colonytrack_metrics} object, as returned by
#'  \code{\link{calculate_metrics}}.
#'@param days Specify the days for which metrics should be retrieved. This
#'  may be in any of the standard formats (names, indices, or a logical vector).
#'  Default is to include all days present in the metrics object.
#'
#'@return Returns a matrix with days/nights as rows and metrics as columns.
#'
#'@export
get_metrics = function(subject, metrics, days = "all"){
	if(days[1] == "all"){
		days = names(metrics$individual)
	}else{
		if(is.numeric(days) | is.logical(days)) days = names(metrics$individual)[days]
		if(!all(days %in% names(metrics$individual))) stop("The 'days' parameter must only specify days that are covered in the data file.")
	}

	data.matrix = do.call("rbind", lapply(metrics$individual[days], function(m) as.numeric(m[subject, ]) ))
	colnames(data.matrix) = metrics$info$var.names

	return(data.matrix)
}


