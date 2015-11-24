#' Estimate calorie burn based on running speed
#'
#' This function estimates the amount of calories that you burn while running activity based on the MET (Metabolic Equivalent) data for physical activities from "The Compendium of Physical Activities Tracking Guide" by B. E. Ainsworth.
#' All input parameters are mandatory.
#' @keywords running, GPS data analysis, calorie burn
#' @param gender String, either "male" or "female"
#' @param age Age of the person in years
#' @param m Weight in kg
#' @param h Height in cm
#' @param gps the GPS data of the running route (longitude, latitude, time, elevation, ...). The longitude and latitude variables must be named "lon" and "lat", respectfully.
#' @return list containing
#'  \itemize{
#'    \item netCB Estimated net calories burned during the run,
#'    \item gps the input data frame with additional columns of net calorie burn values (netCB), distances (d) and time differences (delta_time).
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gpsHR.Rda", package="runneR"))
#' activity_cal_burn <- cb_activity(gender = "male", age = 41, m = 80, h = 180, gpsHR)

cb_activity <- function(gender, age, m, h, gps) {

  # Basal metabolic rate over 24 hours
  if(gender == "male") {
    BMR <- 13.75 * m + 5 * h - 6.76 * age + 66.47
  } else {
    BMR <- 9.56 * m + 1.85 * h - 4.68 * age + 655.1
  }

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")
  gps$delta_time <- c(NA, as.numeric(difftime(gps$time[-1], gps$time[1:(length(gps$time) - 1)], units = "secs")))
  duration <- sum(gps$delta_time, na.rm = T) / 3600
  gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)],
                                         lon1 = gps$lon[1:(length(gps$lon)-1)],
                                         lat2 = gps$lat[2:length(gps$lat)],
                                         lon2 = gps$lon[2:length(gps$lon)]))

  # average pace in min/km
  avg_pace <- 100/6/(sum(gps$d, na.rm = TRUE) / (duration *3600))

  # MET (Metabolic Equivalent) data for physical activities
  if(avg_pace > 9) {
    MET <- 6
  }
  if(avg_pace > 7.5 & avg_pace <= 9.0) {
    MET <- 8.3
  }
  if(avg_pace > 7.0 & avg_pace <= 7.5) {
    MET <- 9
  }
  if(avg_pace > 6.2 & avg_pace <= 7.0) {
    MET <- 9.8
  }
  if(avg_pace > 5.6 & avg_pace <= 6.2) {
    MET <- 10.5
  }
  if(avg_pace > 5.3 & avg_pace <= 5.6) {
    MET <- 11
  }
  if(avg_pace > 4.7 & avg_pace <= 5.3) {
    MET <- 11.8
  }
  if(avg_pace > 4.3 & avg_pace <= 4.7) {
    MET <- 12.3
  }
  if(avg_pace > 4.0 & avg_pace <= 4.3) {
    MET <- 12.8
  }
  if(avg_pace > 3.7 & avg_pace <= 4.0) {
    MET <- 14.5
  }
  if(avg_pace > 3.4 & avg_pace <= 3.7) {
    MET <- 16
  }
  if(avg_pace > 3.1 & avg_pace <= 3.4) {
    MET <- 19
  }
  if(avg_pace > 2.8 & avg_pace <= 3.1) {
    MET <- 19.8
  }
  if(avg_pace <= 2.8) {
    MET <- 23
  }

  grossCB <- BMR * MET * duration / 24

  RMRCB <- ((1.1 * BMR) / 24) * duration

  netCB <- grossCB - RMRCB

  # vectors
  gps$netCB <- NA
  for(i in 1:dim(gps)[1]) {
    # average pace in min/km
    pace <- 100/6/(gps$d[i] / gps$delta_time[i])

    # MET (Metabolic Equivalent) data for physical activities
    if(is.na(pace)){
      gps$netCB[i] <- NA
      next
    }
    if(pace > 9) {
      MET <- 6
    }
    if(pace > 7.5 & pace <= 9.0) {
      MET <- 8.3
    }
    if(pace > 7.0 & pace <= 7.5) {
      MET <- 9
    }
    if(pace > 6.2 & pace <= 7.0) {
      MET <- 9.8
    }
    if(pace > 5.6 & pace <= 6.2) {
      MET <- 10.5
    }
    if(pace > 5.3 & pace <= 5.6) {
      MET <- 11
    }
    if(pace > 4.7 & pace <= 5.3) {
      MET <- 11.8
    }
    if(pace > 4.3 & pace <= 4.7) {
      MET <- 12.3
    }
    if(pace > 4.0 & pace <= 4.3) {
      MET <- 12.8
    }
    if(pace > 3.7 & pace <= 4.0) {
      MET <- 14.5
    }
    if(pace > 3.4 & pace <= 3.7) {
      MET <- 16
    }
    if(pace > 3.1 & pace <= 3.4) {
      MET <- 19
    }
    if(pace > 2.8 & pace <= 3.1) {
      MET <- 19.8
    }
    if(pace <= 2.8) {
      MET <- 23
    }

    grossCB_vec <- BMR * MET * (gps$delta_time[i] / 3600) / 24

    RMRCB_vec <- ((1.1 * BMR) / 24) * (gps$delta_time[i] / 3600)

    gps$netCB[i] <- grossCB_vec - RMRCB_vec
  }


  return(list(netCB = netCB, gps = gps))

}
