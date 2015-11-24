#' Segmentation of the running route according to activity
#'
#' This function determines the total time, moving time and resting time on a running route described with GPS data.
#' Additionally, the function calculates the time spent ascending, descending and on the flat.
#' @keywords running, GPS data analysis, moving time
#' @param gps The GPS data of the running route (longitude, latitude, time, elevation, ...). The longitude and latitude variables must be named "lon" and "lat", respectfully.
#' @param smoothing Indicator of whether the results should be calculated on smoothed data or not. It is set to FALSE by default.
#' @return A list containing:
#'  \itemize{
#'    \item moving_ind indexes of GPS data points with velocity greater than 1 m/s,
#'    \item resting_ind indexes of GPS data points with velocity below 1 m/s.
#'    \item time_div data frame
#'      \itemize{
#'        \item active_time total duration of the running route,
#'        \item moving_time duration of the runner traveling faster than 1 m/s (3.6 km/h),
#'        \item t_mov_rel moving time relative to total time,
#'        \item resting_time duration of the runner traveling slower than 1 m/s (3.6 km/h),
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
#'    \item pace data frame
#'      \itemize{
#'        \item avg_pace average pace for the entire activity,
#'        \item avg_moving_pace average pace for the moving time,
#'        \item best_pace fastest achieved pace,
#'        \item worst_pace slowest achieved pace,
#'      }
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gpsHR.Rda", package="runneR"))
#' segments <- analyze_run(gpsHR)


analyze_run <- function(gps, smoothing = FALSE) {

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")

  # Length of the smoothing window is determined so that it's approximately 1 minute long
  win_smooth <- round(60 / as.numeric(difftime(tail(gps$time, 1), gps$time[1], units = "secs") / length(gps$time)), digits = -1)

  gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)], lon1 = gps$lon[1:(length(gps$lon)-1)],
                                         lat2 = gps$lat[2:length(gps$lat)], lon2 = gps$lon[2:length(gps$lon)]))
  gps$delta_time <- c(NA, as.numeric(difftime(gps$time[-1], gps$time[1:(length(gps$time) - 1)], units = "secs")))
  samp_avg <- round(mean(gps$delta_time, na.rm = T), digits = 0)

  if(smoothing) {
    # Smooth out the distances using rolling mean value
    gps$d_smooth <- NA
    gps$d_smooth[1:(length(gps$d) - win_smooth + 1)] <- zoo::rollapply(gps$d, win_smooth, mean)
    gps$speed_smooth <- gps$d_smooth / samp_avg

    if(sum(names(gps) == "ele") > 0) {
      # Smooth out the elevation and hill grade vectors using rolling mean value
      gps$ele_smooth <- rep(NA, length(gps$ele))
      gps$ele_smooth[1:(length(gps$ele) - win_smooth + 1)] <- zoo::rollapply(gps$ele, win_smooth, mean)
      gps$grade_smooth <- c(NA, analyzeGPS::gradeGPS(gps$ele_smooth, gps$d_smooth))

      gps$delta_ele <- c(NA, gps$ele_smooth[2:length(gps$ele_smooth)] - gps$ele_smooth[1:(length(gps$ele_smooth)-1)])
      total_vam <- sum(gps$delta_ele[which(gps$grade_smooth > 0)], na.rm = TRUE)
    } else {
      message("The elevation data is not provided. Calculating VAM and segmenting the route to ascending, descending and flat is not possible.")
      total_vam <- NA
    }
  } else {
    gps$d_smooth <- gps$d
    gps$speed_smooth <- c(NA, analyzeGPS::speedGPS(gps$time, gps$d[-1]))
    if(sum(names(gps) == "ele") > 0) {
      gps$ele_smooth <- gps$ele
      gps$grade_smooth <- c(NA, analyzeGPS::gradeGPS(gps$ele_smooth, gps$d_smooth))
      gps$delta_ele <- c(NA, gps$ele_smooth[2:length(gps$ele_smooth)] - gps$ele_smooth[1:(length(gps$ele_smooth)-1)])
      total_vam <- sum(gps$delta_ele[which(gps$grade_smooth > 0)], na.rm = TRUE)
    } else {
      message("The elevation data is not provided. Calculating VAM and segmenting the route to ascending, descending and flat is not possible.")
      total_vam <- NA
    }
  }

  # Determine the total time of the cycling route
  active_time = sum(gps$delta_time, na.rm = TRUE)

  resting_ind <- which(gps$speed_smooth < 1)
  resting_time <- sum(gps$delta_time[resting_ind], na.rm = TRUE)
  t_rest_rel <- resting_time / active_time

  moving_ind <- which(gps$speed_smooth >= 1)
  moving_time <- sum(gps$delta_time[-resting_ind], na.rm = TRUE)
  t_mov_rel <- moving_time / active_time

  # Segment only the moving part of the route
  if(sum(names(gps) == "grade_smooth") > 0) {
    gps2 <- gps[-resting_ind,]

    # Determine the total time spent ascending
    time_asc <- sum(gps2$delta_time[which(gps2$grade_smooth > 0)], na.rm = TRUE)
    t_asc_rel <- time_asc / moving_time

    # Determine the total time spent descending
    time_desc <- sum(gps2$delta_time[which(gps2$grade_smooth < 0)], na.rm = TRUE)
    t_desc_rel <- time_desc / moving_time

    # Determine the total time spent on flat
    time_flat <- sum(gps2$delta_time[which(gps2$grade_smooth == 0)], na.rm = TRUE)
    t_flat_rel <- time_flat / moving_time

    # Determine the time with undefined (NA) grade because of smoothing
    time_NA <- sum(gps2$delta_time[which(is.na(gps2$grade_smooth))], na.rm = TRUE)
    t_NA_rel <- time_NA / moving_time
  } else {
    gps2 <- gps[-resting_ind,]

    time_asc <- NA
    t_asc_rel <- NA

    time_desc <- NA
    t_desc_rel <- NA

    time_flat <- NA
    t_flat_rel <- NA

    # Determine the time with undefined (NA) grade because of smoothing
    time_NA <- sum(gps2$delta_time[which(is.na(gps2$speed_smooth))], na.rm = TRUE)
    t_NA_rel <- time_NA / moving_time
  }

  # Calculate pace from the speed data min/km. Speed data is in m/s,
  # therefore we need to use the factor 100/6 and inverse the speed values.
  gps$pace <- (100/6) / gps$speed_smooth
  avg_pace <- 100/6/(sum(gps$d_smooth, na.rm = TRUE) /
                       sum(gps$delta_time[!is.na(gps$d_smooth)], na.rm = TRUE))
  avg_mov_pace <- 100/6/(sum(gps2$d_smooth, na.rm = TRUE) /
                       sum(gps2$delta_time[!is.na(gps2$d_smooth)], na.rm = TRUE))
  best_pace <- min(gps$pace, na.rm = TRUE)
  worst_pace <- max(gps$pace, na.rm = TRUE)
  pace <- data.frame(avg_pace = avg_pace, avg_mov_pace = avg_mov_pace,
                     best_pace = best_pace, worst_pace = worst_pace)

  time_div <- data.frame(active_time = active_time, moving_time = moving_time,
                         t_mov_rel = t_mov_rel, resting_time = resting_time,
                         t_rest_rel = t_rest_rel, time_asc = time_asc,
                         t_asc_rel = t_asc_rel, time_desc = time_desc,
                         t_desc_rel = t_desc_rel, time_flat = time_flat,
                         t_flat_rel = t_flat_rel, time_NA = time_NA,
                         t_NA_rel = t_NA_rel)

  return(list(time_div = time_div, resting_ind = resting_ind,
              moving_ind = moving_ind, total_vam = total_vam,
              pace = pace, gps = gps))

}
