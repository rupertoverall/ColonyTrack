#' Analysis of Multi-Subject Tracking Data from Interconnected Cage Networks.
#'
#' This package prepares raw timestamped Radio Frequency ID data into a standardised storage format, infers the position of each subject at each time point and extracts behavioural metrics.
#'
#' @section Functions provided:
#'
#'   \code{\link{read_data}}
#'
#'   \code{\link{calculate_metrics}}
#'
#'   \code{\link{combine_metrics}}
#'
#'   \code{\link{get_metrics}}
#'
#'   \code{\link{plot_metric}}
#'
#'   \code{\link{plot_ethogram}}
#'
#' @docType package
#' @name ColonyTrack
NULL

.onLoad <- function(...) {
	registerS3method("print", "colonytrack_data", print_data)
	registerS3method("plot", "colonytrack_data", plot_data)
	registerS3method("print", "colonytrack_metrics", print_metrics)
	registerS3method("plot", "colonytrack_metrics", plot_metric)
}
