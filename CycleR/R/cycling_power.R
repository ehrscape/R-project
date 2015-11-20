#' Assessment of the total power produced by a cyclist on a bike ride given the GPS data and additional physical parameters
#' The power required from a person to move on a bike between measured points of a GPS route is calculated
#'
#' This function calculates the total power required from a person to move on a bike between measured points of a GPS route, based on the formula \eqn{P = Proll + Pdrag + Pclimb + Pacc}, where \eqn{Proll} is the power required to overcome the rolling resistance, \eqn{Pdrag} is the power required to overcome the wind resistance, \eqn{Pclimb} is the power required to overcome the pull of gravity and \eqn{Pacc} is the power required to accelerate from one speed to another.
#' @keywords cycling power
#' @param gps the GPS data of the cycling route (longitude, latitude, time, elevation, ...),
#' @param m the combined mass of the rider and the bicycle (in kilograms),
#' @param Crr the rolling resistance coefficient defined based on the type of bike (road, mtb, cross) used,
#' @param Cd the drag coefficient (its value depends on the bike, equipment and rider position; typical values can be found in: Wilson, David Gordon, and Jim Papadopoulos. Bicycling science, p. 188. Mit Press, 2004.),
#' @param ro air density (in kg/m^3). At 25 degrees Celsius and 300 m elevation air density is approximately 1.13 kg/m^3,
#' @param A the surface area of the rider facing the wind (typical values can be found in: Wilson, David Gordon, and Jim Papadopoulos. Bicycling science, p. 188. Mit Press, 2004.),
#' @param windspeed average wind speed in m/s. Headwind is given as a negative number and tail wind is given as a positive number. This argument has value of 0 if there is no information about wind speed (default).
#' @return A data frame with variables:
#'  \itemize{
#'    \item Proll the power required to overcome the rolling resistance,
#'    \item Pdrag the power required to overcome the wind resistance,
#'    \item Pclimb the power required to overcome the gravity pull,
#'    \item Pacc the power required to accelerate from one speed to another.
#'    \item Ptotal total cycling power for the given gps route.
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gps.Rda", package="cycleR"))
#' Pcycling <- cycling_power(gps, Crr = 0.004, Cd = 0.9, ro = 1.13, A = 0.5, windspeed = 0)


cycling_power <- function(gps, m = 0, Crr = 0, Cd = 0, ro = 0, A = 0, windspeed = 0) {

  g <- 9.81

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")

  # Length of the smoothing window is determined so that it's approximately 1 minute long
  win_smooth <- round(60 / as.numeric(difftime(tail(gps$time, 1), gps$time[1], units = "secs") / length(gps$time)), digits = -1)

  # Smooth out the distances using rolling mean value
  d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)], lon1 = gps$lon[1:(length(gps$lon)-1)],
                                            lat2 = gps$lat[2:length(gps$lat)], lon2 = gps$lon[2:length(gps$lon)]))

  d_smooth <- rep(NA, length(d))
  d_smooth[1:(length(d_smooth) - win_smooth + 1)] <- zoo::rollapply(d, win_smooth, mean)

  delta_time <- c(NA, as.numeric(difftime(gps$time[-1], gps$time[1:(length(gps$time) - 1)], units = "secs")))
  samp_avg <- round(mean(delta_time, na.rm = T), digits = 0)

  speed_smooth <- d_smooth / samp_avg

  acc_smooth <- c(NA, analyzeGPS::accGPS(gps$time, speed_smooth))

  ele_smooth <- rep(NA, length(gps$ele))
  ele_smooth[1:(length(gps$ele) - win_smooth + 1)] <- zoo::rollmean(gps$ele, win_smooth)
  grade_smooth <- c(NA, analyzeGPS::gradeGPS(ele_smooth, d_smooth))


  if((Crr != 0) & (m != 0)){
    Proll <- Crr * m * g * cos(atan(grade_smooth)) * speed_smooth
  } else {
    Proll <- rep(NA, length(speed_smooth))
  }

  if((Cd != 0) & (ro != 0) & (A != 0)){
    Pdrag <- 0.5 * ro * (speed_smooth - windspeed)^3 * Cd * A
  } else {
    Pdrag <- rep(NA, length(speed_smooth))
  }

  if(m != 0){
    Pclimb <- m * g * sin(atan(grade_smooth)) * speed_smooth
  } else {
    Pclimb <- rep(NA, length(speed_smooth))
  }

  if(m != 0){
    Pacc <- m * acc_smooth * speed_smooth
  } else {
    Pacc <- rep(NA, length(speed_smooth))
  }

  Pcycle <- data.frame(Proll = Proll, Pdrag = Pdrag, Pclimb = Pclimb, Pacc = Pacc)

  Pcycle$Ptotal <- rowSums(Pcycle, na.rm = T)

  return(Pcycle)

}
