#' Parse Apple Health Export
#'
#' Read the exported XML data from Apple Health
#'
#' @param path Path to Apple Health directory
#' @param verbose Print status messages
#'
#' @return A data frame
#' @export
ihealth_read <- function(path, verbose = TRUE) {

  xml_file <- file.path(path, "export.xml")

  if (!file.exists(xml_file)) {
    stop("Can not find file")
  }

  verbose_message("Starting to read XML file...", verbose)

  xml_data <- xml2::read_xml(xml_file)

  verbose_message("Finished reading XML file", verbose)

  verbose_message("Converting XML data to a data frame...", verbose)

  records <- xml2::xml_find_all(xml_data, "//Record") %>%
    purrr::map(xml2::xml_attrs) %>%
    purrr::map_df(as.list)

  verbose_message("Updating column types...", verbose)

  records$type <- gsub(
    'HKQuantityTypeIdentifier|HKCategoryTypeIdentifier',
    "",
    records$type
  )

  names(records) <- camel_to_snake(names(records))

  datetime_cols <- c("creation_date", "start_date", "end_date")

  for (col in datetime_cols) {
    records[[col]] <- as.POSIXct(records[[col]], format = "%Y-%m-%d %H:%M:%S %z")
  }

  # Drop device info for now - large amount of data, not many uses
  records$device <- NULL

  return(records)

}

is.Date <- function(x) inherits(x, 'Date')

fetch_metric <- function(data, metric, start, end) {

  data <- data[data$type == metric, ]

  if (!is.na(start)) {

    if (!is.Date(start)) {
      stop("start must be a date or NA")
    }

    data <- data[as.Date(data$start_date) >= start, ]
  }

  if (!is.na(end)) {
    if (!is.Date(end)) {
      stop("end must be a date or NA")
    }

    data <- data[as.Date(data$start_date) <= end, ]
  }

  data
}

#' Calculate distance traveled
#'
#' Get distance traveled by foot and bike (walking, running, cycling). Currently,
#' only records recorded in miles are accepted.
#'
#' @param data Output from ihealth_read
#' @param start First date to include.  If NA, the oldest records are used.
#' @param end Last date to include. If NA, the most recent records are used.
#' @param method Approach to summarize distance by foot.  If `distance`, then
#'     miles are used.  If `steps`, then the number of steps is used to estimate distance.
#'     If `max`, then the max of `distance` and `steps` is used.
#' @param steps_per_mile The conversion rate from steps to miles
#'
#' @return A data frame
#' @export
ihealth_distance <- function(data, start = NA, end = NA, method = c("max", "distance", "steps"), steps_per_mile = 2000) {

  method <- match.arg(method)

  walking_running_distance <- fetch_metric(data, "DistanceWalkingRunning", start, end)
  biking_distance <- fetch_metric(data, "DistanceCycling", start, end)
  steps <- fetch_metric(data, "StepCount", start, end)

  unique_wr_metrics <- unique(walking_running_distance$unit)
  unique_biking_metrics <- unique(biking_distance$unit)
  unique_steps_metrics <- unique(steps$unit)

  if (!identical(unique_wr_metrics, "mi")) {
    stop("This package currently only supports miles for walking/running distance")
  }

  distance_wr <- sum(as.numeric(walking_running_distance$value))

  if (!identical(unique_steps_metrics, "count")) {
    stop("This package currently only supports counts for steps")
  }

  if (nrow(biking_distance) > 0) {
    if (!identical(unique_biking_metrics, "mi")) {
      stop("This package currently only supports miles for biking distance")
    }
    distance_bike <- sum(as.numeric(biking_distance$value))
  } else {
    distance_bike <- 0
  }

  distance_steps <- sum(as.numeric(steps$value)) / 2000

  if (method == "max") {
    distance_foot = max(distance_steps, distance_wr)
  } else if (method == "distance") {
    distance_foot = distance_wr
  } else if (method == "steps") {
    distance_foot = distance_steps
  }

  message(paste0("Distance by foot: ", round(distance_foot, 2)))
  message(paste0("Distance by bike: ", round(distance_bike, 2)))
  message(paste0("Distance Total: ", round(distance_bike + distance_foot, 2)))

  list(
    distance_steps = distance_steps,
    distance_foot = distance_wr,
    distance_bike = distance_bike,
    distance_steps = distance_steps
  )


}

camel_to_snake <- function(input_string) {
  snake_string <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", input_string)
  tolower(snake_string)
}


verbose_message <- function(msg, verbose) {
  if (verbose) {
    message(msg)
  }
}



