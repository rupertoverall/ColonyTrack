# Helper functions
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics axis image title par rect lines abline
#' @importFrom crayon green blue
#' @importFrom stats setNames
#' @importFrom grDevices dev.off pdf
#'
matrix.map = function(m, dim.m){
	n = as.vector(m)
  col = ceiling(n / dim.m[1])
  row = n - ((col - 1) * dim.m[1])
  outofbounds = col < 1 | col > dim.m[2] | row < 1 | row > dim.m[1]
  col[outofbounds] = NA
  row[outofbounds] = NA
	colnames.m = rep(colnames(m), each = dim.m[1])
	rownames.m = rep(rownames(m), dim.m[2])
  return(list(row = row, col = col, rownames = rownames.m, colnames = colnames.m))
}

matrix.unmap = function(row, col, dim.m){
  outofbounds = col < 1 | col > dim.m[2] | row < 1 | row > dim.m[1]
  n = ((col - 1) *  dim.m[1]) + row
  n[outofbounds] = NA
  return(n)
}

project.matrix = function(from, to, pad = NA){
	# Two named matrices.
	# The first will have rows and columns added to pad it out to the dimensions of the second.
	# Any rows/columns in the first, but not in the second, will be dropped.
	common.rows = rownames(to)[rownames(to) %in% rownames(from)]
	common.cols = colnames(to)[colnames(to) %in% colnames(from)]
	# Construct matching index matrices
	from.index.matrix = matrix(1:length(as.numeric(from)), ncol = ncol(from), nrow = nrow(from))
	dimnames(from.index.matrix) = dimnames(from)
	to.index.matrix = matrix(1:length(as.numeric(to)), ncol = ncol(to), nrow = nrow(to))
	dimnames(to.index.matrix) = dimnames(to)
	#
	from.indices = as.numeric(from.index.matrix[common.rows, common.cols])
	to.indices = as.numeric(to.index.matrix[common.rows, common.cols])
	to[to.indices] = from[from.indices]
	return(to)
}

subset.even = function(v, n) v[round(seq(1, length(v), (length(v)-1)/(n-1)))]

split.vector = function(v, n){
  ncols = floor(length(v)/n)
  result = split(v[1:(n * ncols)], rep(1:ncols, each=n))
  if((length(v) / n) > ncols) result = c(result, list(v[(n * ncols + 1):length(v)])) # Only if v/n is not a whole number
  names(result) = 1:length(result)
  return(result)
}

colVars = function(x, na.rm=F) {
	# Vectorised version of variance filter
	colSums(t(t(x) - colMeans(x, na.rm=na.rm))^2, na.rm=na.rm) / (nrow(x) - 1)
}

rowVars = function(x, na.rm=F) {
	# Vectorised version of variance filter
	rowSums((x - rowMeans(x, na.rm=na.rm))^2, na.rm=na.rm) / (ncol(x) - 1)
}

colour.schemes = list(
	"warmred.2"=colorRampPalette(c("#FEF8E0", "#d40000"))(256),
	"blue.2"=colorRampPalette(c("#FEFEFC", "#1040C0"))(256),
	"greenred.3"=colorRampPalette(c("#339900", "yellow", "#d40000"))(256),
	"greenwhitered.3"=colorRampPalette(c("#339900", "white", "#d40000"))(256),
	"bluewhitered.3"=colorRampPalette(c("#2956a5", "white", "#e04006"))(256),
	"bluetored.dark.3"=colorRampPalette(c("#003ea3", "white", "#bc3301"))(256),
	"bluewhiteorange.3"=colorRampPalette(c("#2956a5", "#f7f7f7", "#e27324"))(256),
	"brewer.3"=colorRampPalette(c("#998ec3", "#f7f7f7", "#f1a340"))(256),
	"rainbow"=colorRampPalette(c("#D40000", "#FEFA00", "#339900", "#1040C0"))(256),
	"distinct" = function(n, scheme = "default"){
		distinct.colours = NULL
		if(scheme == "default"){
			distinct.colours = c("#4444FF","#FF4444","#44FF44","#DDDDDD","#CCCCFF","#FFCCCC","#FFCCFF","#88FFFF","#FFFF88","#FF88FF","#2222FF","#FF2222","#22FF22","#AAAAFF","#FFAAAA","#AAFFAA","#6666FF","#FF6666","#66FF66","#EEEEFF","#FFEEEE","#EEFFEE")[1:n]
			return(rep(distinct.colours, length.out = n))
		}else if(scheme == "auto"){
			max.n = 12
			i = do.call("c", lapply((2:max.n)^2, function(x){ c(matrix(subset.even(1:(x-1), (x/2)), nrow=2, byrow = T)) / x * sum((1:max.n)^2) }))
			distinct.colours = colorRampPalette(c("#FF2000", "#2000FF", "#20FF00", "#FF2000"))(sum((1:max.n)^2))[order(i)]
			return(rep(distinct.colours, length.out = n))
		}else if(scheme == "pretty"){
			max.n = 8
			i = do.call("c", lapply((2:max.n)^2, function(x){ c(matrix(subset.even(1:(x-1), (x/2)), nrow=2, byrow = T)) / x * sum((1:max.n)^2) }))
			distinct.colours = c( "FF4000", "#CC0C00", "#CC1800", "#CC2500", "#CC3100", "#CC3D00", "#CC4900", "#CC5600", "#CC6200", "#CC6E00", "#CC7A00", "#CC8700", "#CC9300", "#CC9F00", "#CCAB00", "#CCB800", "#CCC400", "#C8CC00", "#BCCC00", "#AFCC00", "#A3CC00", "#97CC00", "#8BCC00", "#7ECC00", "#72CC00", "#66CC00", "#5ACC00", "#4ECC00", "#41CC00", "#35CC00", "#29CC00", "#1DCC00", "#10CC00", "#04CC00", "#00CC08", "#00CC14", "#00CC21", "#00CC2D", "#00CC39", "#00CC45", "#00CC52", "#00CC5E", "#00CC6A", "#00CC76", "#00CC83", "#00CC8F", "#00CC9B", "#00CCA7", "#00CCB4", "#00CCC0", "#00CCCC", "#00C0CC", "#00B4CC", "#00A7CC", "#009BCC", "#008FCC", "#0083CC", "#0076CC", "#006ACC", "#005ECC", "#0052CC", "#0045CC", "#0039CC", "#002DCC", "#0021CC", "#0014CC", "#0008CC", "#0400CC", "#1000CC", "#1D00CC", "#2900CC", "#3500CC", "#4100CC", "#4E00CC", "#5A00CC", "#6600CC", "#7200CC", "#7E00CC", "#8B00CC", "#9700CC", "#A300CC", "#AF00CC", "#BC00CC", "#C800CC", "#CC00C4", "#CC00B8", "#CC00AB", "#CC009F", "#CC0093", "#CC0087", "#CC007A", "#CC006E", "#CC0062", "#CC0056", "#CC0049", "#CC003D", "#CC0031", "#CC0025", "#CC0018", "#CC000C", "#CC0000")[order(i)]
			return(rep(distinct.colours, length.out = n))
		}else if(scheme == "brewer"){
			distinct.colours = c( "#a6cee3", "#ff7f00", "#b2df8a", "#6a3d9a", "#ffff99", "#1f78b4", "#fb9a99", "#b15928", "#33a02c", "#e31a1c", "#cab2d6", "#fdbf6f")
			return(rep(distinct.colours, length.out = n))
		}else if(scheme == "annette"){
			distinct.colours = c( "#4675A2", "#A49967", "#EA8004", "#745F8D", "#4F8E00", "#939393", "#CCCC33", "#931100", "#66A0DC", "#1E1D1D", "#FFFFFF", "#5C5637", "#FF9200", "#005492", "#A64A44")
			return(rep(distinct.colours, length.out = n))
		}else if(scheme == "reduced"){
			distinct.colours = c( "#4675A2", "#A49967", "#EA8004", "#745F8D", "#4F8E00", "#939393", "#CCCC33", "#931100", "#66A0DC", "#5C5637", "#FF9200", "#A64A44")
			return(rep(distinct.colours, length.out = n))
		}else if(scheme == "rainbow"){
			distinct.colours = c(na.omit(subset.even(colour.schemes$rainbow, n)), 1)[1:n]
		}else{

		}
		return(distinct.colours)
	}
)

col.alpha = function(col, alpha){grDevices::rgb(t(grDevices::col2rgb(col)), alpha = round(alpha * 255), maxColorValue = 255)}

merge.colours = function(colours){
	merged = rowMeans(cbind(col2rgb(colours)))
	return(rgb(merged["red"], merged["green"], merged["blue"], maxColorValue = 255))
}

hsv2rgb = function(h, s = NULL, v = NULL, maxColorValue = 255){
	hsv = if(is.null(s) && is.null(v)) as.matrix(h) else rbind(h, s, v)
	if(!is.numeric(hsv)) stop("hsv matrix must be numeric")
	d = dim(hsv)
	if(d[1L] != 3L) stop("hsv matrix must have 3 rows")
	n = d[2L]
	if(n == 0L) return(cbind(c(h = 1, s = 1, v = 1))[, 0L])
	hsv = hsv / maxColorValue
	if(any(0 > hsv) || any(hsv > 1)) stop("hsv values must be in [0, maxColorValue]")

	C = hsv["v", ] * hsv["s", ]
	X = C * (1 - abs((hsv["h", ] * 6) %% 2 - 1))
	m = hsv["v", ] - C
	rgb = rbind(r = rep(NA, ncol(hsv)), g = rep(NA, ncol(hsv)), b = rep(NA, ncol(hsv)))
	i = hsv["h", ] >= 0 & hsv["h", ] < 1/6
	rgb["r", i] = C[i]
	rgb["g", i] = X[i]
	rgb["b", i] = 0
	i = hsv["h", ] >= 1/6 & hsv["h", ] < 2/6
	rgb["r", i] = X[i]
	rgb["g", i] = C[i]
	rgb["b", i] = 0
	i = (hsv["h", ] >= 2/6 & hsv["h", ] < 3/6)
	rgb["r", i] = 0
	rgb["g", i] = C[i]
	rgb["b", i] = X[i]
	i = hsv["h", ] >= 3/6 & hsv["h", ] < 4/6
	rgb["r", i] = 0
	rgb["g", i] = X[i]
	rgb["b", i] = C[i]
	i = hsv["h", ] >= 4/6 & hsv["h", ] < 5/6
	rgb["r", i] = X[i]
	rgb["g", i] = 0
	rgb["b", i] = C[i]
	i = hsv["h", ] >= 5/6 & hsv["h", ] <= 1
	rgb["r", i] = C[i]
	rgb["g", i] = 0
	rgb["b", i] = X[i]

	rgb["r", ] = (rgb["r", ] + m) * maxColorValue
	rgb["g", ] = (rgb["g", ] + m) * maxColorValue
	rgb["b", ] = (rgb["b", ] + m) * maxColorValue
	return(rgb)
}

colourise = function(v, scheme){
	v = v - min(v)
	v = v / max(v)
	v = round(v * 255)
	colour.schemes$"warmred.2"[v]
}

plot_matrix = function(mat, col = grDevices::colorRampPalette(c("white", "black"))(256), xlab = names(dimnames(mat))[2], ylab = names(dimnames(mat))[1], las = 2, tick = FALSE, ...){
	graphics::image(0:ncol(mat), 0:nrow(mat), t(mat[nrow(mat):0, , drop = FALSE]), col = col, axes = F, xlab = "", ylab = "", ...)
	graphics::axis(2, at = (1:nrow(mat)) - .5, labels = rev(dimnames(mat)[[1]]), las = las, tick = tick)
	graphics::axis(1, at = (1:ncol(mat)) - .5, labels = dimnames(mat)[[2]], las = las, tick = tick)
	graphics::title(xlab = xlab, ylab = ylab)
}

tosentence = function(s, split=NA) {
	s = tolower(s)
	s = as.character(sapply(s, function(ss){
		ss = unlist(strsplit(ss, split))
		if(!is.na(split)) ss = paste(ss, collapse=" ")
		substring(ss, 1, 1) = toupper(substring(ss, 1, 1))
		return(ss)
	}))
	return(s)
}

circle = function(x, y, radius, res=100){
	shape = cbind(x = (cos((0:res) * 2 * pi / res) * radius) + x, y = (sin((0:res) * 2 * pi / res) * radius) + y)
	shape[nrow(shape), ] = shape[1, ] # Ensure the polygon is closed
	return(shape)
}

plot_logo = function(style = "square"){
	.parprevious = graphics::par(mar = c(0, 0, 0, 0))
	on.exit(par(.parprevious))
	plot(0, xlim = c(1, 3.5), ylim = c(1, 3.5), type = "n", bty = "n", axes = F, ylab = "", xlab = "", asp = 1)
	rect(2.4, 2.4, 3.5, 3.5, col = col.alpha("lightblue", 1), border = NA)
	rect(1, 2.4, 2.1, 3.5, col = col.alpha("lightblue", .8), border = NA)
	rect(1, 1, 2.1, 2.1, col = col.alpha("lightblue", .6), border = NA)
	rect(2.4, 1, 3.5, 2.1, col = col.alpha("lightblue", .4), border = NA)
	lwd = min(par()$pin) * 3
	if(style == "circle"){
		circ = circle(2.25, 2.25, .9)
		circ = circ[circ[, "x"] < 2.95, ]
		lines(circ, lwd = lwd)
		segments(circ[1, "x"], circ[1, "y"], 2.71, 2.82, lwd = lwd)
		segments(circ[1, "x"], circ[1, "y"], 2.9, 3.09, lwd = lwd)
	}else if(style == "square"){
		segments(3, 1.5, 1.5, 1.5, lwd = lwd)
		segments(1.5, 1.5, 1.5, 3, lwd = lwd)
		segments(1.5, 3, 3, 3, lwd = lwd)
		segments(3, 3, 2.8, 2.8, lwd = lwd)
		segments(3, 3, 2.8, 3.2, lwd = lwd)
	}else{
		stop(paste0("The logo style '", style, "' is not supported."))
	}
}

msg = function(text){cat("  ", crayon::green(text), crayon::green("..."), ("\n"), sep = "")}
progress = function(text){cat("    ", crayon::blue(text), "\n", sep = "")}
warn = function(text){cat("  ", crayon::red(text), "\n", sep = "")}
dot = function(){cat(crayon::blue("."), sep = "")}
reddot = function(){cat(crayon::red("."), sep = "")}

print_data = function(data){
	cat(paste0("  A 'colonytrack_data' object containing data for ", length(data$data), " subjects over ", nrow(data$info$days), " days."), "\n\n", sep = "")
}

print_metrics = function(metrics){
	nwindows = nrow(metrics$info$windows)
	windows.text = ifelse(nwindows == 1, paste0(nwindows, " day."), paste0(nwindows, " days."))
	cat(paste0("  A 'colonytrack_metrics' object containing metrics data for ", length(metrics$info$subjects), " subjects over ", windows.text), "\n\n", sep = "")
	for(subject in rownames(metrics$individual[[1]])){
		cat("    ", subject, ": ", metrics$info$windows$id[1], " ... ", metrics$info$windows$id[nrow(metrics$info$windows)], "\n", sep = "")
	}
}

plot_data = function(data, days = "all", subjects = "all", file = NULL, cex.axis = 0.75, las = 2){
	if(!is.null(file)) pdf(file = file, width = 10, height = (4 + length(names(data$data)) / 10))
	.parprevious = graphics::par(mar = c(6, 8, 0, 0))
	on.exit(par(.parprevious))

	if(days[1] == "all"){
		days = rownames(data$info$days)
	}else{
		if(is.numeric(days) | is.logical(days)) days = rownames(data$info$days)[days]
		if(!all(days %in% rownames(data$info$days))) stop("The 'days' parameter must only specify days that are covered in the data file.")
	}
	if(subjects[1] == "all"){
		subjects = data$info$subjects
	}else{
		if(is.numeric(subjects) | is.logical(subjects)) subjects = data$info$subjects[subjects]
		if(!all(subjects %in% data$info$subjects)) stop("The 'subjects' parameter must only specify subjects that are present in the data file.")
	}

	subject.colours = rep(col.alpha(c("#0066AE"), 0.2), length.out = length(subjects))

	for(day in days){
		start = data$info$days[day, "start"]
		end = data$info$nights[day, "end"]
		width = (end - start) * 0.001
		subject.present = setNames(rep(TRUE, length(subjects)), subjects)

		plot(NA, xlim = c(start, end), ylim = c(1, length(subjects) + 1), type = "n", las = 2, bty = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
		rect(data$info$nights[day, "start"], -100, data$info$nights[day, "end"], 100, col = "#FBF6E9", border = NA)
		axis(1, at = seq(start, by = 3600, length.out = 25), labels = 0:24, cex.axis = .65)
		for(subject in subjects){
			n = which(subjects == subject) # Plot from top to bottom - the reverse of the default behaviour.
			trajectory = data$data[[subject]]$Timestamp[which(data$data[[subject]]$Timestamp >= start & data$data[[subject]]$Timestamp < end)]
			if(length(trajectory) > 0){
				rect(trajectory, n, trajectory + width, n + 1, col = subject.colours[n], border = NA)
			}else{
				subject.present[subject] = FALSE
			}
		}
		axis(2, at = (1:length(subjects))[subject.present] + 0.5, labels = gsub("-", "\uad", rev(subjects[subject.present])), cex.axis = cex.axis, las = las)
		axis(2, at = (1:length(subjects))[!subject.present] + 0.5, labels = gsub("-", "\uad", subjects[!subject.present]), cex.axis = cex.axis, las = las, lwd = 0, col.axis = "red", lwd.ticks = 1)
		title(xlab = day, line = 3)
	}
	if(!is.null(file)) dev.off()
}

