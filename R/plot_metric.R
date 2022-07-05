#'Plot one or more metrics.
#'
#'Plots a line graph of the individual metrics over time.
#'
#'The data used in plotting (the plotted values for each subject and the
#'colours) are returned invisibly to enable further plot customisation.
#'
#'@param metric The name of the metric that is to be plotted.
#'@param metrics A \code{colonytrack_metrics} object, as returned by
#'  \code{\link{calculate_metrics}}.
#'@param days Specify the days to be included is the metrics calculation. This
#'  may be in any of the standard formats (names, indices, or a logical vector).
#'  Default is to include all days present in the input data file.
#'@param subjects Specify the subjects to be included is the metrics
#'  calculation. This may be in any of the standard formats (names, indices, or
#'  a logical vector). Default is to include all subjects present in the input
#'  data file. If a vector of subjects is supplied, the resulting plot will
#'  respect the ordering of this vector.
#'@param scheme A code specifying the colour scheme to be used. See Details for
#'  information on the available schemes. Default is 'rainbow'.
#'@param col A named vector of colours. This vector must contain the SubjectIDs
#'  as names. Any subjects not present in this vector will be drawn in black. If
#'  not \code{NULL}, then this parameter overrides \code{scheme}. Default is
#'  \code{NULL}.
#'@param file An optional filename for PDF output. If supplied, the plot will be
#'  written to a PDF document with this name. The file is saved relative to the
#'  working directory. The default is to plot to the active graphics device.
#'@param cex.axis The scaling factor for the axis labels (see
#'  \code{\link{par}}). Default (0.75) can be overridden for different sized
#'  plot devices.
#'@param las The label aspect (see \code{\link{par}}. Can be overridden based on
#'  the user's preferences.
#'@param mar The margins surrounding the plot (see \code{\link{par}}). Can be
#'  overridden for different length ID labels or to match different sized plot
#'  devices.
#'@param plot If \code{FALSE}, then the plot is not drawn and only the plotting
#'  data are returned.
#'
#'@importFrom graphics par lines points
#'@importFrom grDevices col2rgb colorRampPalette rgb
#'@importFrom stats setNames
#'
#'@export
plot_metric = function(metric, metrics, days = "all", subjects = "all", scheme = "rainbow", col = NULL, file = NULL, cex.axis = 1, las = 2, mar = c(8, 8, 4, 0), plot = TRUE){
	if(!metric %in% metrics$info$var.names){
		stop(paste0("'", metric, "' is not a valid metric. See 'metrics$info$var.names' for a full list of available metrics."))
	}
	if(days[1] == "all"){
		days = names(metrics$individual)
	}else{
		if(is.numeric(days) | is.logical(days)) days = names(metrics$individual)[days]
		if(!all(days %in% names(metrics$individual))) stop("The 'days' parameter must only specify days that are covered in the data file.")
	}
	if(subjects[1] == "all"){
		subjects = metrics$info$subjects
	}else{
		if(is.numeric(subjects) | is.logical(subjects)) subjects = metrics$info$subjects[subjects]
		if(!all(subjects %in% metrics$info$subjects)) stop("The 'subjects' parameter must only specify subjects that are present in the data file.")
	}

	plot.matrix = do.call("cbind", lapply(metrics$individual[days], function(m) m[subjects, metric] ))
	colnames(plot.matrix) = days
	rownames(plot.matrix) = subjects

	colours = stats::setNames(colour.schemes$distinct(nrow(plot.matrix), scheme), rownames(plot.matrix))
	if(!is.null(col) & is.null(names(col))) warning("The parameter 'col' was specified, but without names. Using the colour scheme instead.")
	if(!is.null(names(col))){
		colours = stats::setNames(rep("#000000", nrow(plot.matrix)), rownames(plot.matrix))
		colours[names(col)] = col
	}

	.parprevious = graphics::par(mar = mar)
	on.exit(par(.parprevious))
	matrix.max = max(plot.matrix, na.rm = T)
	matrix.min = min(plot.matrix, na.rm = T)
	if(plot){
		plot(NA, xlim = c(1, ncol(plot.matrix)), ylim = c(ifelse(matrix.min < 0, matrix.min - matrix.min * .1, 0), matrix.max + matrix.max * .1), type = "n", las = las, xaxt = "n", bty = "n", xlab = "", ylab = tosentence(metric, "\\."))
		for(i in 1:nrow(plot.matrix)){
			if(length(days) == 1){
				points(1:ncol(plot.matrix), plot.matrix[i, ], col = colours[i])
			}else{
				lines(1:ncol(plot.matrix), plot.matrix[i, ], col = colours[i])
			}
		}
		axis(1, at = 1:ncol(plot.matrix), labels = colnames(plot.matrix), cex.axis = cex.axis, las = las)
		title(main = tosentence(metric, "\\."))
	}

	invisible(list(matrix = plot.matrix, colours = colours))
}


