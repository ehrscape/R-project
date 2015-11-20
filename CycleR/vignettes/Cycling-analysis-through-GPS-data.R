## ---- eval=FALSE---------------------------------------------------------
#  time_segmentation <- segment_time(gps, resting_ind)

## ------------------------------------------------------------------------
load(system.file("extdata", "gps.Rda", package="cycleR"))
str(gps, strict.width = "wrap")

## ------------------------------------------------------------------------
gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")

## ------------------------------------------------------------------------
if(sum(names(gps) == "delta_time") == 0) {
    gps$delta_time <- c(NA, as.numeric(diff(gps$time)))
  }

## ---- tidy=TRUE----------------------------------------------------------
if(sum(names(gps) == "d") == 0) {
    gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)], lon1 = gps$lon[1:(length(gps$lon)-1)],
                                           lat2 = gps$lat[2:length(gps$lat)], lon2 = gps$lon[2:length(gps$lon)]))
  }

## ---- tidy=TRUE----------------------------------------------------------
if(sum(names(gps) == "speed") == 0) {
    gps$speed <- c(NA, analyzeGPS::speedGPS(gps$time, gps$d[-1]))
  }

## ---- tidy=TRUE----------------------------------------------------------
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

## ------------------------------------------------------------------------
str(gps, strict.width = "wrap")

## ---- eval=-11-----------------------------------------------------------
total_time = sum(gps$delta_time, na.rm = TRUE)

resting_ind <- which(gps$speed < 1)
resting_time <- sum(gps$delta_time[resting_ind], na.rm = TRUE)
t_rest_rel <- resting_time / total_time

moving_ind <- which(gps$speed >= 1)
moving_time <- sum(gps$delta_time[-resting_ind], na.rm = TRUE)
t_mov_rel <- moving_time / total_time

## ------------------------------------------------------------------------
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

## ---- eval=-7------------------------------------------------------------
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
time_div
return(list(time_div = time_div, resting_ind = resting_ind, moving_ind = moving_ind))

## ---- eval=FALSE---------------------------------------------------------
#  climb_cat <- categorize(gps)

## ---- eval=FALSE, tidy=TRUE----------------------------------------------
#  gps$time <- as.POSIXct(gps$time, format = "%Y-%m-%dT%H:%M:%OSZ")
#  
#  # Calculate distances between GPS data points if it doesn't exist
#  if(sum(names(gps) == "d") == 0){
#    gps$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gps$lat[1:(length(gps$lat)-1)], lon1 = gps$lon[1:(length(gps$lon)-1)],
#                                           lat2 = gps$lat[2:length(gps$lat)], lon2 = gps$lon[2:length(gps$lon)]))
#  }
#  
#  # Smooth out the elevation and hill grade vectors using rolling mean value
#  # Length of the smoothing window is determined so that it's approximately 1 minute long
#  win_smooth <- round(60 / as.numeric(difftime(tail(gps$time, 1), gps$time[1], units = "secs") /
#                                        length(gps$time)), digits = -1)
#  d_smooth <- zoo::rollapply(gps$d, win_smooth, mean)
#  ele <- zoo::rollmean(gps$ele, win_smooth)
#  grade <- analyzeGPS::gradeGPS(ele, d_smooth)
#  
#  # Insert the smoothened vectors into the main data frame
#  gps$d_smooth <- NA
#  gps$d_smooth[1:length(d_smooth)] <- d_smooth
#  gps$ele_smooth <- NA
#  gps$ele_smooth[1:length(ele)] <- ele
#  gps$grade_smooth <- NA
#  gps$grade_smooth[2:(length(grade) + 1)] <- grade
#  
#  if(sum(names(gps) == "delta_time") == 0) {
#    gps$delta_time <- c(NA, as.numeric(diff(gps$time)))
#  }

## ---- tidy=TRUE----------------------------------------------------------
if(sum(names(gps) == "delta_ele") == 0) {
  gps$delta_ele <- c(NA, gps$ele_smooth[2:length(gps$ele_smooth)] - gps$ele_smooth[1:(length(gps$ele_smooth)-1)])
}

## ------------------------------------------------------------------------
runs <- rle(gps$grade_smooth >= 0.02)
str(runs)
positives <- which(runs$values)
str(positives)

## ---- tidy=TRUE, fig.width=7, fig.height=5, fig.align='center'-----------
hill <- data.frame(ind_start = rep(0,length(positives)), ind_end = rep(0,length(positives)), len = rep(0,length(positives)), avg_grade = rep(0,length(positives)), vam = rep(0,length(positives)))

for(i in 1:length(positives)) {
  hill$ind_start[i] <- sum(runs$lengths[1:(positives[i]-1)]) + 1
  hill$ind_end[i] <- sum(runs$lengths[1:positives[i]])
  ind <- seq(hill$ind_start[i],hill$ind_end[i])
  hill$len[i] <- sum(gps$d[ind])
  hill$avg_grade[i] <- mean(gps$grade_smooth[ind])
  hill$vam[i] <- gps$ele_smooth[hill$ind_end[i]] - gps$ele_smooth[hill$ind_start[i]]
}

str(hill)

plot(ele, type="l")
for(i in 1:dim(hill)[1]) {
  lines(x = seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1), y = ele[seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1)], col = "red")
}


## ---- tidy=TRUE, fig.width=7, fig.height=5, fig.align='center'-----------
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
del_lines
hill <- hill[-del_lines,]

str(hill)

plot(ele, type="l")
for(i in 1:dim(hill)[1]) {
  lines(x = seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1), y = ele[seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1)], col = "red")
}

## ---- fig.width=7, fig.height=5, fig.align='center'----------------------
dim_start <- dim(hill)[1]
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

deleted_lines <- dim_start - dim(hill)[1]
deleted_lines
str(hill)

plot(ele, type="l")
for(i in 1:dim(hill)[1]) {
  lines(x = seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1), y = ele[seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1)], col = "red")
}

## ---- fig.width=7, fig.height=5, fig.align='center'----------------------
groups2 <- NA
for(i in 1:(dim(hill)[1] - 1)){
  if((sum(gps$d[hill$ind_end[i]: hill$ind_start[i + 1]])) < sum(hill$len[i], hill$len[i + 1])) {
    groups2 <- c(groups2, i)
  }
}
groups2 <- groups2[-1]
groups2
gr_difference <- which(diff(groups2) > 1)
gr_difference
del_lines <- NA

for(i in 1:length(gr_difference)) {
  if(i == 1){
    hill$ind_end[groups2[i]] <- hill$ind_end[groups2[gr_difference[i]]]
    hill$len[groups2[i]] <- sum(gps$d[hill$ind_start[groups2[i]]:
                                        hill$ind_end[groups2[i]]])
    hill$avg_grade[groups2[i]] <- mean(gps$grade_smooth[hill$ind_start[groups2[i]]:
                                                          hill$ind_end[groups2[i]]])
    hill$vam[groups2[i]] <- gps$ele_smooth[hill$ind_end[groups2[i]]] - 
      gps$ele_smooth[hill$ind_start[groups2[i]]]
    del_lines <- c(del_lines, (groups2[i]+1):groups2[gr_difference[i]])
  } else {
    hill$ind_end[groups2[gr_difference[i - 1] + 1]] <- 
      hill$ind_end[groups2[gr_difference[i]]]
    hill$len[groups2[gr_difference[i - 1] + 1]] <- 
      sum(gps$d[hill$ind_start[groups2[gr_difference[i - 1] + 1]]:
                  hill$ind_end[groups2[gr_difference[i - 1] + 1]]])
    hill$avg_grade[groups2[gr_difference[i - 1] + 1]] <- 
      mean(gps$grade_smooth[hill$ind_start[groups2[gr_difference[i - 1] + 1]]:
                              hill$ind_end[groups2[gr_difference[i - 1] + 1]]])
    hill$vam[groups2[gr_difference[i - 1] + 1]] <- 
      gps$ele_smooth[hill$ind_end[groups2[gr_difference[i - 1] + 1]]] -
      gps$ele_smooth[hill$ind_start[groups2[gr_difference[i - 1] + 1]]]
    
    if((groups2[gr_difference[i - 1] + 1]) != (groups2[gr_difference[i]])) {
      del_lines <- c(del_lines, (groups2[gr_difference[i - 1] + 1] + 1):
                       groups2[gr_difference[i]])
    } else {
      del_lines <- del_lines
    }
    
  }
  if(i == length(gr_difference)) {
    hill$ind_end[groups2[gr_difference[i] + 1]] <- 
      hill$ind_end[groups2[length(groups2)]]
    hill$len[groups2[gr_difference[i] + 1]] <- 
      sum(gps$d[hill$ind_start[groups2[gr_difference[i] + 1]]:
                  hill$ind_end[groups2[gr_difference[i] + 1]]])
    hill$avg_grade[groups2[gr_difference[i] + 1]] <- 
      mean(gps$grade_smooth[hill$ind_start[groups2[gr_difference[i] + 1]]:
                              hill$ind_end[groups2[gr_difference[i] + 1]]])
    hill$vam[groups2[gr_difference[i] + 1]] <- 
      gps$ele_smooth[hill$ind_end[groups2[gr_difference[i] + 1]]] -
      gps$ele_smooth[hill$ind_start[groups2[gr_difference[i] + 1]]]
    
    if((groups2[gr_difference[i] + 1]) != (groups2[length(groups2)])) {
      del_lines <- c(del_lines, (groups2[gr_difference[i] + 1] + 1):
                       (groups2[length(groups2)]))
    } else {
      del_lines <- del_lines
    }
  }
}

del_lines <- del_lines[-1]
del_lines
hill <- hill[-del_lines,]
str(hill)

plot(ele, type="l")
for(i in 1:dim(hill)[1]) {
  lines(x = seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1), y = ele[seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1)], col = "red")
}

## ------------------------------------------------------------------------
hill$category <- hill$len * 100 * hill$avg_grade
hill$category <- findInterval(hill$category, c(8000, 16000, 32000, 64000, 80000))
hill$category <- factor(hill$category, levels = c(0, 1, 2, 3, 4, 5), 
                        labels = c("no_category", "4", "3", "2", "1", "HC"))

# Insert hill data into the main gps data frame
gps$category <- 0
for(i in 1:dim(hill)[1]) {
  if(hill$category[i] != "no_category") {
    gps$category[hill$ind_start[i]:hill$ind_end[i]] <- as.character(hill$category[i])
  }
}
gps$category <- factor(gps$category, 
                       levels = c("0", "1", "2", "3", "4", "HC"), 
                       labels = c("no_category", "1", "2", "3", "4", "HC"))


## ------------------------------------------------------------------------
total_vam <- sum(gps$delta_ele[which(gps$grade_smooth > 0)], na.rm = TRUE)

str(list(gps = gps, climbs = hill, total_vam = total_vam))

## ---- fig.width=7, fig.height=5, fig.align='center'----------------------
hill[which(hill$category != "no_category"),]

plot(ele, type="l")
for(i in 1:dim(hill)[1]) {
  lines(x = seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1), y = ele[seq(hill$ind_start[i] + 1, hill$ind_end[i] - 1)], col = "red")
}

for(i in 1:dim(hill)[1]) {
  if(hill$category[i] != "no_category") {
    text(x = round(mean(c(hill$ind_start[i], hill$ind_end[i]))),
         y = mean(gps$ele_smooth[c(hill$ind_start[i], hill$ind_end[i])]),
         labels = paste("Category", hill$category[i], "\n climb"))
  }
}

## ---- eval=-1, tidy=TRUE-------------------------------------------------
Pcycling <- cycling_power(gps, m = 90, Crr = 0.004, Cd = 0.9, ro = 1.13, A = 0.5, windspeed = 0)
m <- 90
Crr <- 0.004
Cd <- 0.9 
ro <- 1.13 
A <- 0.5 
windspeed <- 0

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
if((Crr != 0) & (m != 0)){
  Proll <- Crr * m * g * cos(atan(grade_smooth)) * speed_smooth
} else {
  Proll <- rep(NA, length(speed_smooth))
}

## ------------------------------------------------------------------------
if((Cd != 0) & (ro != 0) & (A != 0)){
  Pdrag <- 0.5 * ro * (speed_smooth - windspeed)^3 * Cd * A
} else {
  Pdrag <- rep(NA, length(speed_smooth))
}

## ------------------------------------------------------------------------
if(m != 0){
  Pclimb <- m * g * sin(atan(grade_smooth)) * speed_smooth
} else {
  Pclimb <- rep(NA, length(speed_smooth))
}

## ------------------------------------------------------------------------
if(m != 0){
  Pacc <- m * acc_smooth * speed_smooth
} else {
  Pacc <- rep(NA, length(speed_smooth))
}

## ------------------------------------------------------------------------
Pcycle <- data.frame(Proll = Proll, Pdrag = Pdrag, Pclimb = Pclimb, Pacc = Pacc)

Pcycle$Ptotal <- rowSums(Pcycle, na.rm = T)

str(Pcycle)

## ---- fig.width=7, fig.height=5, fig.align='center'----------------------
plot(gps$time, Pcycle$Ptotal, type = "l")

## ------------------------------------------------------------------------
mean(Pcycle$Ptotal, na.rm = TRUE)

## ---- fig.width=7, fig.height=5, fig.align='center'----------------------
mean(Pcycle$Ptotal / m, na.rm = TRUE)
plot(gps$time, Pcycle$Ptotal / m, type = "l")

