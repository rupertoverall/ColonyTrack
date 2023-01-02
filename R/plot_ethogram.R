#'Plot an ethogram.
#'
#'Plots a heatmap representation of the high-level ethogram metrics.
#'
#'This function uses the three high-level metrics 'activity', 'sociality' and
#''exploration' and produces a heatmap representation for all subjects over the
#'given time period.
#'
#'Available colour schemes are 'rgb' ('activity' in the red channel, 'sociality'
#'in the green channel and 'exploration' in the blue channel). Scheme variants
#'beginning with 'd' use discrete colouring such that the most prevalent
#'activity in any time block is taken as the colour (the default is to mix the
#'components). The scheme family 'ymc' uses yellow for 'activity', magenta for
#''sociality' and cyan for 'exploration'. The discrete and scaled versions are
#'also available for this scheme. The (default) scheme family 'yrb' uses yellow
#'for 'activity', red for 'sociality' and blue for 'exploration' in a
#'subtractive colour model. The activity and sociality scores are scaled and
#'thresholded to fall within the range 0-1. The scheme 'basic' uses a discrete
#'colour mapping based on a subtractive colouring so that yellow = 'activity',
#'red = 'sociality', blue = "exploration", orange = 'activity' and 'sociality',
#'green = 'activity' and 'exploration', purple = 'sociality' and 'exploration',
#'brown indicates the presence of all three features and white the absence of
#'all features.
#'
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
#'@param plot A character vector describing which behavioural categories
#'  ('activity', 'sociality' and 'exploration') should be plotted. Abbreviations
#'  may also be used. The default, 'all', plots all three categories mixed using
#'  a colour model determined by the parameter 'scheme'. Note that if 'all' is
#'  present in any of the elements of this argument, then it will override any
#'  other elements. If \code{plot} is \code{FALSE}, then all plotting is
#'  suppressed and only the ethogram colours and components are returned (see
#'  below).
#'@param scheme A code specifying the colour scheme to be used. See Details for
#'  information on the available schemes. Default is 'yrb'.
#'@param file An optional filename for PDF output. If supplied, the plot will be
#'  written to a PDF document with this name. The file is saved relative to the
#'  working directory. The default is to plot to the active graphics device.
#'@param width The width of the PDF (only if \code{file} is specified) in mm.
#'@param height The height of the PDF (only if \code{file} is specified) in mm.
#'@param cex.axis The scaling factor for the axis labels (see
#'  \code{\link{par}}). Default (0.75) can be overridden for different sized
#'  plot devices.
#'@param las The label aspect (see \code{\link{par}}. Can be overridden based on
#'  the user's preferences.
#'@param mar The margins surrounding the plot (see \code{\link{par}}). Can be
#'  overridden for different length ID labels or to match different sized plot
#'  devices.
#'
#'@return Invisibly returns a list with the components 'ethogram' (a matrix of
#'  the colours of each cell), and 'activity', 'sociality' and 'exploration'
#'  (matrices with the values for each component respectively).
#'
#'@importFrom graphics par segments
#'@importFrom grDevices col2rgb colorRampPalette rgb rgb2hsv
#'
#'@export
plot_ethogram = function(metrics, days = "all", subjects = "all", plot = "all", scheme = "yrb", file = NULL, width = NULL, height = NULL, cex.axis = 0.75, las = 2, mar = c(5, 8, 0, 0)){
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

	activity = do.call("cbind", lapply(metrics$ethogram[days], "[[", "activity"))[subjects, ]
	exploration = do.call("cbind", lapply(metrics$ethogram[days], "[[", "exploration"))[subjects, ]
	sociality = do.call("cbind", lapply(metrics$ethogram[days], "[[", "sociality"))[subjects, ]

	if(!"all" %in% plot){
		if(grepl("^a$|^ac", plot)){
			activity = activity * NA
		}
		if(grepl("^e$|^et", plot)){
			exploration = exploration * NA
		}
		if(grepl("^s$|^so", plot)){
			sociality = sociality * NA
		}
	}

	ethogram = NULL
	if(scheme == "rgb"){
		activity = activity + min(activity, na.rm = T)
		activity = activity / max(activity, na.rm = T)
		sociality = sociality + min(sociality, na.rm = T)
		sociality = sociality / max(sociality, na.rm = T)
		exploration = exploration + min(exploration, na.rm = T)
		exploration = exploration / max(exploration, na.rm = T)
		r = activity
		g = sociality
		b = exploration
		r[is.na(r)] = 0
		g[is.na(g)] = 0
		b[is.na(b)] = 0
		ethogram = matrix(rgb(1 - r, 1 - g, 1 - b), nrow = nrow(activity))
	}else if(scheme == "ymc"){
		activity = activity + min(activity, na.rm = T)
		activity = activity / max(activity, na.rm = T)
		sociality = sociality + min(sociality, na.rm = T)
		sociality = sociality / max(sociality, na.rm = T)
		exploration = exploration + min(exploration, na.rm = T)
		exploration = exploration / max(exploration, na.rm = T)
		r = (activity + sociality) / 2
		g = (sociality + exploration) / 2
		b = (exploration + activity) / 2
		r[is.na(r)] = 0
		g[is.na(g)] = 0
		b[is.na(b)] = 0
		ethogram = matrix(rgb(1 - r, 1 - g, 1 - b), nrow = nrow(activity))
	}else if(scheme == "yrb"){
		activity = activity / 600 # Sustained contact every 10 s is considered high activity.
		activity[activity > 1] = 1 # Threshold.
		sociality = sociality / 5 # 5 animals per cage is considered high social density.
		sociality[sociality > 1] = 1 # Threshold.
		activity.colours = grDevices::colorRampPalette(c("white", "#F9D719"))(256)
		activity.mat = grDevices::col2rgb(activity.colours[floor(activity * 255) + 1])
		sociality.colours = grDevices::colorRampPalette(c("white", "#F92C19"))(256)
		sociality.mat = grDevices::col2rgb(sociality.colours[floor(sociality * 255) + 1])
		exploration.colours = grDevices::colorRampPalette(c("white", "#1989F9"))(256)
		exploration.mat = grDevices::col2rgb(exploration.colours[floor(exploration * 255) + 1])
		misch = ((activity.mat / 255) + (sociality.mat / 255) + (exploration.mat / 255)) / 3
		hsv = rgb2hsv(misch, maxColorValue = 1)
		hsv["s", ] = (hsv["s", ] - hsv["v", ])
		hsv["s", ] = hsv["s", ] - min(hsv["s", ])
		hsv["s", ] = (hsv["s", ] / max(hsv["s", ]))
		misch = hsv2rgb(hsv, maxColorValue = 1)
		ethogram = matrix(rgb(misch[1, ], misch[2, ], misch[3, ]), nrow = nrow(activity))
	}else{ #  scheme == "basic"
		activity = activity >= mean(activity, na.rm = T)
		sociality = sociality >= mean(sociality, na.rm = T)
		exploration = exploration >= mean(exploration, na.rm = T)
		colours = rep(NA, length(activity))
		colours[activity & !sociality & !exploration] = "#FFD900" # Yellow.
		colours[!activity & sociality & !exploration] = "#EC1F00" # Red.
		colours[!activity & !sociality & exploration] = "#0251E3" # Blue.
		colours[activity & sociality & !exploration] = "#FE8B05" # Orange.
		colours[activity & !sociality & exploration] = "#6CA90F" # Green.
		colours[!activity & sociality & exploration] = "#C208FD" # Purple.
		colours[activity & sociality & exploration] = "#702E00" # Brown.
		colours[!activity & !sociality & !exploration] = "#FFFFFF"
		colour.mat = grDevices::col2rgb(colours) / 255
		ethogram = matrix(rgb(colour.mat[1, ], colour.mat[2, ], colour.mat[3, ]), nrow = nrow(activity))
	}

	rownames(ethogram) = subjects

	# This enables flexible-width windows to be plotted. (TODO but is not guaranteed bug-free in edge cases yet)
	hours.per.day = sapply(metrics$ethogram[days], function(e) ncol(e$activity) )

	if(is.null(width)) width = (200 + ncol(ethogram) / 4)
	if(is.null(height)) height = (50 + nrow(ethogram) * 2)
	if(!is.null(file)) pdf(file = file, width = width / 25.4, height = height / 25.4)
	if(plot != FALSE){
			.parprevious = graphics::par(mar = mar)
		on.exit(par(.parprevious))

		plot(NA, xlim = c(1, ncol(ethogram) + 1 ), ylim = c(1, nrow(ethogram) + 1), type = "n", las = 2, bty = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xpd = F)
		for(n in 1:nrow(ethogram)){
			# Plot from top to bottom - the reverse of the default behaviour.
			rect(1:ncol(ethogram), nrow(ethogram) - n + 0.5, 1:ncol(ethogram) + 1, nrow(ethogram) - n + 1.5, col = ethogram[n, ], border = NA)
		}
		axis(2, at = (nrow(ethogram):1), labels = gsub("-", "\uad", rownames(ethogram)), cex.axis = cex.axis, las = las, line = 0)
		axis(1, at = (seq_along(days) - 1) * hours.per.day + 0.5, labels = days, cex.axis = cex.axis, las = las, lwd = 0, line = -1)
		if(!is.null(file)) dev.off()
	}
	invisible(list(ethogram = ethogram, activity = activity, sociality = sociality, exploration = exploration))
}


