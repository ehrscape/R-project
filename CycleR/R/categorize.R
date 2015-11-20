#' Allocation and categorization of climbs on the route
#'
#' This function determines the starting and ending point, length, average grade and category of climbs on a cycling route described with GPS data.
#' The categorization data is added to the gps data, while the details of the climbs are returned in a separate data frame.
#' Also added is the number of total vertical ascent meters (VAM) of the route.
#' @keywords cycling power, GPS data analysis, climb categorization
#' @param gps the gps data of the cycling route (longitude, latitude, time, elevation, ...).
#' @return A list containing:
#'   \itemize{
#'    \item gps the input data frame complemented with the smoothed elevation and hill grade and categorization of climbs (additional columns \code{ele_smooth}, \code{grade_smooth}, \code{category}),
#'    \item climbs data frame with details about all climbs with columns:
#'      \itemize{
#'        \item ind_start location of climb start (index of the input date frame),
#'        \item ind_end location of climb end (index of the input date frame),
#'        \item len length of the climb in meters,
#'        \item avg_grade average grade of the climb between the start and end points,
#'        \item vam vertical ascent in meters of each climb,
#'        \item category climb category (factorized),
#'      }
#'    \item total_vam total vertical ascent meters (VAM),
#'  }
#' @export
#' @examples
#' load(system.file("extdata", "gps.Rda", package="cycleR"))
#' hill_cat <- categorize(gps)


categorize <- function(gps) {

  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")
  # Calculate distances between GPS data points if it doesn't exist
  if(sum(names(gps) == "d") == 0){
    gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)], lon1 = gps$lon[1:(length(gps$lon)-1)],
                                           lat2 = gps$lat[2:length(gps$lat)], lon2 = gps$lon[2:length(gps$lon)]))
  }

  # Smooth out the elevation and hill grade vectors using rolling mean value
  # Length of the smoothing window is determined so that it's approximately 1 minute long
  win_smooth <- round(60 / as.numeric(difftime(tail(gps$time, 1), gps$time[1], units = "secs") / length(gps$time)), digits = -1)
  d_smooth <- zoo::rollapply(gps$d, win_smooth, mean)
  ele <- zoo::rollmean(gps$ele, win_smooth)
  grade <- analyzeGPS::gradeGPS(ele, d_smooth)

  # Insert the smoothened vectors into the main data frame
  gps$d_smooth <- NA
  gps$d_smooth[1:length(d_smooth)] <- d_smooth
  gps$ele_smooth <- NA
  gps$ele_smooth[1:length(ele)] <- ele
  gps$grade_smooth <- NA
  gps$grade_smooth[2:(length(grade) + 1)] <- grade

  if(sum(names(gps) == "delta_ele") == 0) {
    gps$delta_ele <- c(NA, gps$ele_smooth[2:length(gps$ele_smooth)] - gps$ele_smooth[1:(length(gps$ele_smooth)-1)])
  }

  if(sum(names(gps) == "delta_time") == 0) {
    gps$delta_time <- c(NA, as.numeric(diff(gps$time)))
  }

  # Search for runs of grade values greater than 2 %
  runs <- rle(gps$grade_smooth >= 0.02)
  positives <- which(runs$values)

  # Write hill data (index of start and end, length and average grade) into a special data frame
  hill <- data.frame(ind_start = rep(0,length(positives)), ind_end = rep(0,length(positives)), len = rep(0,length(positives)), avg_grade = rep(0,length(positives)), vam = rep(0,length(positives)))

  for(i in 1:length(positives)) {
    hill$ind_start[i] <- sum(runs$lengths[1:(positives[i]-1)]) + 1
    hill$ind_end[i] <- sum(runs$lengths[1:positives[i]])
    ind <- seq(hill$ind_start[i],hill$ind_end[i])
    hill$len[i] <- sum(gps$d[ind])
    hill$avg_grade[i] <- mean(gps$grade_smooth[ind])
    hill$vam[i] <- gps$ele_smooth[hill$ind_end[i]] - gps$ele_smooth[hill$ind_start[i]]
  }

  # Aggregate all slopes that are close together and interrupted by short (less than win_smooth) reductions of grade (below 2 %)
  del_lines <- NA

  groups <- which(diff(hill$ind_start) > win_smooth)
  for(i in 1:length(groups)) {
    if(i == 1) {
      hill$ind_end[i] <- hill$ind_end[groups[i]]
      hill$len[i] <- sum(gps$d[hill$ind_start[i]: hill$ind_end[i]])
      hill$avg_grade[i] <- mean(gps$grade_smooth[hill$ind_start[i]: hill$ind_end[i]])
      hill$vam[i] <- gps$ele_smooth[hill$ind_end[i]] - gps$ele_smooth[hill$ind_start[i]]

      del_lines <- c(del_lines, (i + 1):groups[i])
    } else {
      hill$ind_end[groups[i - 1] + 1] <- hill$ind_end[groups[i]]
      hill$len[groups[i - 1] + 1] <- sum(gps$d[hill$ind_start[groups[i - 1] + 1]: hill$ind_end[groups[i - 1] + 1]])
      hill$avg_grade[groups[i - 1] + 1] <- mean(gps$grade_smooth[hill$ind_start[groups[i - 1] + 1]: hill$ind_end[groups[i - 1] + 1]])
      hill$vam[groups[i - 1] + 1] <- gps$ele_smooth[hill$ind_end[groups[i - 1] + 1]] - gps$ele_smooth[hill$ind_start[groups[i - 1] + 1]]

      if((groups[i - 1] + 1) != groups[i]) {
        del_lines <- c(del_lines, (groups[i - 1] + 1 + 1):groups[i])
      }

    }
    if(i == length(groups)){
      hill$ind_end[groups[i] + 1] <- tail(hill$ind_end, 1)
      hill$len[groups[i] + 1] <- sum(gps$d[hill$ind_start[groups[i] + 1]: hill$ind_end[groups[i] + 1]])
      hill$avg_grade[groups[i] + 1] <- mean(gps$grade_smooth[hill$ind_start[groups[i] + 1]: hill$ind_end[groups[i] + 1]])
      hill$vam[groups[i] + 1] <- gps$ele_smooth[hill$ind_end[groups[i] + 1]] - gps$ele_smooth[hill$ind_start[groups[i] + 1]]

      if((groups[i] + 1) != dim(hill)[1]) {
        del_lines <- c(del_lines, (groups[i] + 1 + 1):dim(hill)[1])
      }

    }

  }

  del_lines <- del_lines[-1]
  hill <- hill[-del_lines,]

  # Aggregate all slopes that are close together (difference between the start of the current and end of the previous slope is less than half of win_smooth)
  i <- 2
  while(i <= dim(hill)[1]) {
    if((hill$ind_start[i] - hill$ind_end[i - 1]) < (win_smooth / 2)) {
      hill$ind_end[i - 1] <- hill$ind_end[i]
      hill$len[i - 1] <- sum(gps$d[hill$ind_start[i - 1]: hill$ind_end[i - 1]])
      hill$avg_grade[i - 1] <- mean(gps$grade_smooth[hill$ind_start[i - 1]: hill$ind_end[i - 1]])
      hill$vam[i - 1] <- gps$ele_smooth[hill$ind_end[i - 1]] - gps$ele_smooth[hill$ind_start[i - 1]]
      hill <- hill[-i,]
    } else {
      i <- i + 1
    }
  }

  # Aggregate all slopes that are interrupted by relatively short downslopes
  groups2 <- NA
  for(i in 1:(dim(hill)[1] - 1)){
    if((sum(gps$d[hill$ind_end[i]: hill$ind_start[i + 1]])) < sum(hill$len[i], hill$len[i + 1])) {
      groups2 <- c(groups2, i)
    }
  }
  groups2 <- groups2[-1]
  gr_difference <- which(diff(groups2) > 1)
  del_lines <- NA

  for(i in 1:length(gr_difference)) {
    if(i == 1){
      hill$ind_end[groups2[i]] <- hill$ind_end[groups2[gr_difference[i]]]
      hill$len[groups2[i]] <- sum(gps$d[hill$ind_start[groups2[i]]: hill$ind_end[groups2[i]]])
      hill$avg_grade[groups2[i]] <- mean(gps$grade_smooth[hill$ind_start[groups2[i]]: hill$ind_end[groups2[i]]])
      hill$vam[groups2[i]] <- gps$ele_smooth[hill$ind_end[groups2[i]]] - gps$ele_smooth[hill$ind_start[groups2[i]]]
      del_lines <- c(del_lines, (groups2[i]+1):groups2[gr_difference[i]])
    } else {
      hill$ind_end[groups2[gr_difference[i - 1] + 1]] <- hill$ind_end[groups2[gr_difference[i]]]
      hill$len[groups2[gr_difference[i - 1] + 1]] <- sum(gps$d[hill$ind_start[groups2[gr_difference[i - 1] + 1]]:
                                                                 hill$ind_end[groups2[gr_difference[i - 1] + 1]]])
      hill$avg_grade[groups2[gr_difference[i - 1] + 1]] <- mean(gps$grade_smooth[hill$ind_start[groups2[gr_difference[i - 1] + 1]]:
                                                                               hill$ind_end[groups2[gr_difference[i - 1] + 1]]])
      hill$vam[groups2[gr_difference[i - 1] + 1]] <- gps$ele_smooth[hill$ind_end[groups2[gr_difference[i - 1] + 1]]] -
        gps$ele_smooth[hill$ind_start[groups2[gr_difference[i - 1] + 1]]]

      if((groups2[gr_difference[i - 1] + 1]) != (groups2[gr_difference[i]])) {
        del_lines <- c(del_lines, (groups2[gr_difference[i - 1] + 1] + 1):groups2[gr_difference[i]])
      } else {
        del_lines <- del_lines
      }
    }
    if(i == length(gr_difference)) {
      hill$ind_end[groups2[gr_difference[i] + 1]] <- hill$ind_end[groups2[length(groups2)]]
      hill$len[groups2[gr_difference[i] + 1]] <- sum(gps$d[hill$ind_start[groups2[gr_difference[i] + 1]]:
                                                             hill$ind_end[groups2[gr_difference[i] + 1]]])
      hill$avg_grade[groups2[gr_difference[i] + 1]] <- mean(gps$grade_smooth[hill$ind_start[groups2[gr_difference[i] + 1]]:
                                                                           hill$ind_end[groups2[gr_difference[i] + 1]]])
      hill$vam[groups2[gr_difference[i] + 1]] <- gps$ele_smooth[hill$ind_end[groups2[gr_difference[i] + 1]]] -
        gps$ele_smooth[hill$ind_start[groups2[gr_difference[i] + 1]]]

      if((groups2[gr_difference[i] + 1]) != (groups2[length(groups2)])) {
        del_lines <- c(del_lines, (groups2[gr_difference[i] + 1] + 1):(groups2[length(groups2)]))
      } else {
        del_lines <- del_lines
      }
    }
  }

  del_lines <- del_lines[-1]
  hill <- hill[-del_lines,]

  # Calculate slope category
  hill$category <- hill$len * 100 * hill$avg_grade
  hill$category <- findInterval(hill$category, c(8000, 16000, 32000, 64000, 80000))
  hill$category <- factor(hill$category, levels = c(0, 1, 2, 3, 4, 5), labels = c("no_category", "4", "3", "2", "1", "HC"))

  # Insert hill data into the main gps data frame
  gps$category <- 0
  for(i in 1:dim(hill)[1]) {
    if(hill$category[i] != "no_category") {
      gps$category[hill$ind_start[i]:hill$ind_end[i]] <- as.character(hill$category[i])
    }
  }
  gps$category <- factor(gps$category, levels = c("0", "1", "2", "3", "4", "HC"), labels = c("no_category", "1", "2", "3", "4", "HC"))

  # Determine the total vertical acsent meters (VAM) of the cycling route
  total_vam <- sum(gps$delta_ele[which(gps$grade_smooth > 0)], na.rm = TRUE)

  return(list(gps = gps, climbs = hill, total_vam = total_vam))

}
