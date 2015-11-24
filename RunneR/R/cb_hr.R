#' Estimate calorie burn based on heart rate
#'
#' This heart rate based calorie burn calculator provides an estimate of the
#' rate at which you are burning calories during aerobic (i.e. cardiorespiratory)
#' exercise, based on your average heart rate while performing the exercise.
#' All input parameters are mandatory.
#' @keywords running, GPS data analysis, calorie burn
#' @param gender String, either "male" or "female"
#' @param age Age of the person in years
#' @param m Weight in kg
#' @param h Height in cm
#' @param RHR Resting heart rate
#' @param HR heart rate time series data for the run
#' @param gps the GPS data of the running route (longitude, latitude, time, elevation, ...). The longitude and latitude variables must be named "lon" and "lat", respectfully.
#' @return list containing
#'  \itemize{
#'    \item netCB Estimated net calories burned during the run,
#'    \item gps the input data frame with additional columns of net calorie burn values (netCB), distances (d) and time differences (delta_time).
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gpsHR.Rda", package="runneR"))
#' hr_cal_burn <- cb_hr(gender = "male", age = 41, m = 80, h = 180, RHR = 60, gpsHR$hr, gpsHR)

cb_hr <- function(gender, age, m, h, RHR, HR, gps) {

  MHR <- 208 - (0.685 * age)
  avg_hr <- mean(HR, na.rm = TRUE)
  VO2max <- 15.3 * MHR / RHR

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")
  gps$delta_time <- c(NA, as.numeric(difftime(gps$time[-1], gps$time[1:(length(gps$time) - 1)], units = "secs")))
  duration <- sum(gps$delta_time, na.rm = T) / 3600

  if(gender == "male") {
    BMR <- 13.75 * m + 5 * h - 6.76 * age + 66.47
    grossCB <- ((-95.7735 + (0.634 * avg_hr) + (0.404 * VO2max) + (0.394 * m) + (0.271 * age))/4.184) * 60 * duration
    RMRCB <- ((1.1 * BMR) / 24) * duration
    netCB <- grossCB - RMRCB
  } else {
    BMR <- 9.56 * m + 1.85 * h - 4.68 * age + 655.1
    grossCB <- ((-59.3954 + (0.45 * avg_hr) + (0.38 * VO2max) + (0.103 * m) + (0.274 * age))/4.184) * 60 * duration
    RMRCB <- ((1.1 * BMR) / 24) * duration
    netCB <- grossCB - RMRCB
  }

  # vectors
  gps$netCB <- NA

  if(gender == "male") {
    BMR <- 13.75 * m + 5 * h - 6.76 * age + 66.47
    for(i in 1:dim(gps)[1]) {
      if(is.na(HR[i])){
        gps$netCB[i] <- NA
        next
      }
      grossCB <- ((-95.7735 + (0.634 * HR[i]) + (0.404 * VO2max) + (0.394 * m) + (0.271 * age))/4.184) * 60 * (gps$delta_time[i] / 3600)
      RMRCB <- ((1.1 * BMR) / 24) * (gps$delta_time[i] / 3600)
      gps$netCB[i] <- grossCB - RMRCB
    }
  } else {
    BMR <- 9.56 * m + 1.85 * h - 4.68 * age + 655.1
    for(i in 1:dim(gps)[1]) {
      if(is.na(HR[i])){
        gps$netCB[i] <- NA
        next
      }
      grossCB <- ((-59.3954 + (0.45 * HR[i]) + (0.38 * VO2max) + (0.103 * m) + (0.274 * age))/4.184) * 60 * (gps$delta_time[i] / 3600)
      RMRCB <- ((1.1 * BMR) / 24) * (gps$delta_time[i] / 3600)
      gps$netCB <- grossCB - RMRCB
    }
  }

  return(list(netCB = netCB, gps = gps))

}
