#' Segmentation of the cycling route according to activity
#'
#' This function determines the total time, moving time and resting time on a cycling route described with GPS data.
#' Additionally, the function calculates the time spent ascending, descending and on the flat.
#' @keywords cycling, GPS data analysis, moving time
#' @param gps the GPS data of the cycling route (longitude, latitude, time, elevation, ...)
#' @return A list containing:
#'  \itemize{
#'    \item moving_ind indexes of GPS data points with velocity greater than 1 m/s,
#'    \item resting_ind indexes of GPS data points with velocity below 1 m/s.
#'    \item time_div data frame
#'      \itemize{
#'        \item total_time total duration of the cycling route,
#'        \item moving_time duration of the cyclist traveling faster than 1 m/s,
#'        \item t_mov_rel moving time relative to total time,
#'        \item resting_time duration of the cyclist traveling slower than 1 m/s,
#'        \item t_rest_rel resting time relative to total time,
#'        \item time_asc total time spent ascending,
#'        \item t_asc_rel time spent ascending relative to total time,
#'        \item time_desc total time spent descending,
#'        \item t_desc_rel time spent descending relative to total time,
#'        \item time_flat total time spent on flat,
#'        \item t_flat_rel time spent on flat relative to total time,
#'        \item time_NA total time with undefined (NA) grade because of smoothing,
#'        \item t_NA_rel time with undefined (NA) grade because of smoothing relative to total time.
#'      }
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gps.Rda", package="cycleR"))
#' segments <- segment_time(gps)


segment_time <- function(gps) {

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")

  if(sum(names(gps) == "delta_time") == 0) {
    gps$delta_time <- c(NA, as.numeric(diff(gps$time)))
  }

  if(sum(names(gps) == "d") == 0) {
    gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)], lon1 = gps$lon[1:(length(gps$lon)-1)],
                                           lat2 = gps$lat[2:length(gps$lat)], lon2 = gps$lon[2:length(gps$lon)]))
  }

  if(sum(names(gps) == "speed") == 0) {
    gps$speed <- c(NA, analyzeGPS::speedGPS(gps$time, gps$d[-1]))
  }

  if(sum(names(gps) == "grade_smooth") == 0) {
    # Smooth out the elevation and hill grade vectors usng rolling mean value
    # Length of the smoothing window is determined so that it's approximately 1 minute long
    win_smooth <- round(60 / as.numeric(difftime(tail(gps$time, 1), gps$time[1], units = "secs") / length(gps$time)), digits = -1)
    ele <- zoo::rollmean(gps$ele, win_smooth)
    d_smooth <- zoo::rollapply(gps$d, win_smooth, mean)
    grade <- analyzeGPS::gradeGPS(ele, d_smooth)

    # Insert the smoothened vectors into the main data frame
    gps$ele_smooth <- NA
    gps$ele_smooth[1:length(ele)] <- ele
    gps$d_smooth <- NA
    gps$d_smooth[1:length(d_smooth)] <- d_smooth
    gps$grade_smooth <- NA
    gps$grade_smooth[2:(length(grade) + 1)] <- grade
  }

  # Determine the total time of the cycling route
  total_time = sum(gps$delta_time, na.rm = TRUE)

  resting_ind <- which(gps$speed < 1)
  resting_time <- sum(gps$delta_time[resting_ind], na.rm = TRUE)
  t_rest_rel <- resting_time / total_time

  moving_ind <- which(gps$speed >= 1)
  moving_time <- sum(gps$delta_time[-resting_ind], na.rm = TRUE)
  t_mov_rel <- moving_time / total_time

  # Segment only the moving part of the route
  gps <- gps[-resting_ind,]

  # Determine the total time spent ascending
  time_asc <- sum(gps$delta_time[which(gps$grade_smooth > 0)], na.rm = TRUE)
  t_asc_rel <- time_asc / total_time

  # Determine the total time spent descending
  time_desc <- sum(gps$delta_time[which(gps$grade_smooth < 0)], na.rm = TRUE)
  t_desc_rel <- time_desc / total_time

  # Determine the total time spent on flat
  time_flat <- sum(gps$delta_time[which(gps$grade_smooth == 0)], na.rm = TRUE)
  t_flat_rel <- time_flat / total_time

  # Determine the time with undefined (NA) grade because of smoothing
  time_NA <- sum(gps$delta_time[which(is.na(gps$grade_smooth))], na.rm = TRUE)
  t_NA_rel <- time_NA / total_time

  time_div <- data.frame(total_time = total_time, moving_time = moving_time,
                         t_mov_rel = t_mov_rel, resting_time = resting_time,
                         t_rest_rel = t_rest_rel, time_asc = time_asc,
                         t_asc_rel = t_asc_rel, time_desc = time_desc,
                         t_desc_rel = t_desc_rel, time_flat = time_flat,
                         t_flat_rel = t_flat_rel, time_NA = time_NA,
                         t_NA_rel = t_NA_rel)

  return(list(time_div = time_div, resting_ind = resting_ind, moving_ind = moving_ind))

}
