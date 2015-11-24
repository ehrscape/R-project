#' Estimate calorie burn while running any given distance
#'
#' This running calorie burn calculator estimates the calories that you burn while running any given distance.
#' The calculator takes into consideration the grade of the running surface that you are on (i.e. the incline or decline),
#' whether you are running on a treadmill or not, and your fitness level.
#' @keywords running, GPS data analysis, calorie burn
#' @param age Age of the person in years
#' @param m Weight in kg
#' @param h Height in cm
#' @param RHR Resting heart rate in beats/min
#' @param treadmill Indicator of whether the running was performed on a treadmill or not (TRUE or FALSE)
#' @param gps the GPS data of the running route containing time, longitude, latitude and elevation. The longitude, latitude and elevation variables must be named "lon", "lat" and "ele", respectfully.
#' @return list containing
#'  \itemize{
#'    \item netCB Estimated net calories burned during the run,
#'    \item gps the input data frame with additional columns of net calorie burn values (netCB), distances (d) and time differences (delta_time).
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gpsHR.Rda", package="runneR"))
#' running_cal_burn <- cb_running(age = 41, m = 80, h = 180, RHR = 60, treadmill = FALSE, gpsHR)

cb_running <- function(age, m, h, RHR, treadmill = FALSE, gps) {
  BMI <- m / ((h / 100)^2)
  MHR <- 208 - (0.685 * age)
  VO2max <- 15.3 * MHR / RHR

  if(treadmill) {
    TF <- 0
  } else {
    TF <- 0.84
  }

  if(VO2max >= 56) {
    CFF <- 1
  }
  if(VO2max >= 54 & VO2max < 56) {
    CFF <- 1.01
  }
  if(VO2max >= 52 & VO2max < 54) {
    CFF <- 1.02
  }
  if(VO2max >= 50 & VO2max < 52) {
    CFF <- 1.03
  }
  if(VO2max >= 48 & VO2max < 50) {
    CFF <- 1.04
  }
  if(VO2max >= 46 & VO2max < 48) {
    CFF <- 1.05
  }
  if(VO2max >= 44 & VO2max < 46) {
    CFF <- 1.06
  }
  if(VO2max < 44) {
    CFF <- 1.07
  }

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")
  gps$delta_time <- c(NA, as.numeric(difftime(gps$time[-1], gps$time[1:(length(gps$time) - 1)], units = "secs")))
  duration <- sum(gps$delta_time, na.rm = T) / 3600
  gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)],
                                         lon1 = gps$lon[1:(length(gps$lon)-1)],
                                         lat2 = gps$lat[2:length(gps$lat)],
                                         lon2 = gps$lon[2:length(gps$lon)]))
  distance <- sum(gps$d, na.rm = T) / 1000

  gps$grade <- c(NA, analyzeGPS::gradeGPS(gps$ele, gps$d))
  avg_grade <- mean(gps$grade, na.rm = TRUE) * 100

  if(avg_grade > 10) {
    netCB <- (((0.07 * avg_grade) + 0.75) * m + TF) * distance * CFF
  }
  if(avg_grade > 0 & avg_grade <= 10) {
    netCB <- (((0.05 * avg_grade) + 0.95) * m + TF) * distance * CFF
  }
  if(avg_grade > -10 & avg_grade <= 0) {
    netCB <- (((0.04 * avg_grade) + 0.95) * m + TF) * distance * CFF
  }
  if(avg_grade > -15 & avg_grade <= -10) {
    netCB <- (((-0.02 * avg_grade) + 0.35) * m + TF) * distance * CFF
  }
  if(avg_grade <= -15) {
    netCB <- (((-0.01 * avg_grade) + 0.5) * m + TF) * distance * CFF
  }

  # vectors
  gps$netCB <- NA
  for(i in 1:dim(gps)[1]) {
    if(is.na(gps$grade[i])){
      gps$netCB[i] <- NA
      next
    }
    if((100 * gps$grade[i]) > 10) {
      gps$netCB[i] <- (((0.07 * gps$grade[i]) + 0.75) * m + TF) * (gps$d[i] / 1000) * CFF
    }
    if((100 * gps$grade[i]) > 0 & (100 * gps$grade[i]) <= 10) {
      gps$netCB[i] <- (((0.05 * gps$grade[i]) + 0.95) * m + TF) * (gps$d[i] / 1000) * CFF
    }
    if((100 * gps$grade[i]) > -10 & (100 * gps$grade[i]) <= 0) {
      gps$netCB[i] <- (((0.04 * gps$grade[i]) + 0.95) * m + TF) * (gps$d[i] / 1000) * CFF
    }
    if((100 * gps$grade[i]) > -15 & (100 * gps$grade[i]) <= -10) {
      gps$netCB[i] <- (((-0.02 * gps$grade[i]) + 0.35) * m + TF) * (gps$d[i] / 1000) * CFF
    }
    if((100 * gps$grade[i]) <= -15) {
      gps$netCB[i] <- (((-0.01 * gps$grade[i]) + 0.5) * m + TF) * gps$d[i] * CFF
    }
  }

  return(list(netCB = netCB, gps = gps))

}
