## ---- eval=-1, echo=-c(2,3)----------------------------------------------
run_info <- analyze_run(gps, smoothing = FALSE)
library(ggplot2)
library(RColorBrewer)

## ----echo=-2-------------------------------------------------------------
load(system.file("extdata", "gpsHR.Rda", package="runneR"))
str(gpsHR, strict.width = "wrap")

## ------------------------------------------------------------------------
gpsHR$time <- as.POSIXct(gpsHR$time, format = "%Y-%m-%dT%H:%M:%OSZ")

## ---- tidy=TRUE----------------------------------------------------------
# Length of the smoothing window is determined so that it's approximately 1 minute long
win_smooth <- round(60 / as.numeric(difftime(tail(gpsHR$time, 1), gpsHR$time[1], units = "secs") / length(gpsHR$time)), digits = -1)

gpsHR$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gpsHR$lat[1:(length(gpsHR$lat)-1)], 
                                         lon1 = gpsHR$lon[1:(length(gpsHR$lon)-1)],
                                         lat2 = gpsHR$lat[2:length(gpsHR$lat)], 
                                         lon2 = gpsHR$lon[2:length(gpsHR$lon)]))
gpsHR$delta_time <- c(NA, as.numeric(difftime(gpsHR$time[-1], gpsHR$time[1:(length(gpsHR$time) - 1)], units = "secs")))
samp_avg <- round(mean(gpsHR$delta_time, na.rm = T), digits = 0)

## ---- tidy=TRUE, fig.width=7, fig.height=5, fig.align='center', echo=-12----
gpsHR$d <- gpsHR$d

ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = d)) + 
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Distances [m]")

gpsHR$ele <- gpsHR$ele
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = ele))+
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Elevation [m]")

gpsHR$speed <- c(NA, analyzeGPS::speedGPS(gpsHR$time, gpsHR$d[-1]))
gpsHR$grade <- c(NA, analyzeGPS::gradeGPS(gpsHR$ele, gpsHR$d))
gpsHR$delta_ele <- c(NA, gpsHR$ele[2:length(gpsHR$ele)] - gpsHR$ele[1:(length(gpsHR$ele)-1)])
total_vam <- sum(gpsHR$delta_ele[which(gpsHR$grade > 0)], na.rm = TRUE)
total_vam

## ---- tidy=TRUE, fig.width=7, fig.height=5, fig.align='center', echo=-13----
gpsHR$d_smooth <- NA
gpsHR$d_smooth[1:(length(gpsHR$d) - win_smooth + 1)] <- zoo::rollapply(gpsHR$d, win_smooth, mean)
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = d_smooth)) + 
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Smoothed distances [m]")

gpsHR$ele_smooth <- rep(NA, length(gpsHR$ele))
gpsHR$ele_smooth[1:(length(gpsHR$ele) - win_smooth + 1)] <- zoo::rollapply(gpsHR$ele, win_smooth, mean)
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = ele_smooth))+
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Smoothed elevation [m]")

gpsHR$speed_smooth <- gpsHR$d_smooth / samp_avg 
gpsHR$grade_smooth <- c(NA, analyzeGPS::gradeGPS(gpsHR$ele_smooth, gpsHR$d_smooth))
gpsHR$delta_ele_smooth <- c(NA, gpsHR$ele_smooth[2:length(gpsHR$ele_smooth)] - gpsHR$ele_smooth[1:(length(gpsHR$ele_smooth)-1)])
total_vam_smooth <- sum(gpsHR$delta_ele[which(gpsHR$grade_smooth > 0)], na.rm = TRUE)
total_vam_smooth

## ---- echo=-c(2,6,8,12,14)-----------------------------------------------
active_time = sum(gpsHR$delta_time, na.rm = TRUE)
active_time

resting_ind <- which(gpsHR$speed_smooth < 1)
resting_time <- sum(gpsHR$delta_time[resting_ind], na.rm = TRUE)
resting_time
t_rest_rel <- resting_time / active_time
t_rest_rel

moving_ind <- which(gpsHR$speed_smooth >= 1)
moving_time <- sum(gpsHR$delta_time[-resting_ind], na.rm = TRUE)
moving_time
t_mov_rel <- moving_time / active_time
t_mov_rel

## ------------------------------------------------------------------------
if(sum(names(gpsHR) == "grade_smooth") > 0) {
  gps2 <- gpsHR[-resting_ind,]

  # Determine the total time spent ascending
  time_asc <- sum(gps2$delta_time[which(gps2$grade_smooth > 0)], na.rm = TRUE)
  time_asc
  t_asc_rel <- time_asc / moving_time

  # Determine the total time spent descending
  time_desc <- sum(gps2$delta_time[which(gps2$grade_smooth < 0)], na.rm = TRUE)
  time_desc
  t_desc_rel <- time_desc / moving_time

  # Determine the total time spent on flat
  time_flat <- sum(gps2$delta_time[which(gps2$grade_smooth == 0)], na.rm = TRUE)
  time_flat
  t_flat_rel <- time_flat / moving_time

  # Determine the time with undefined (NA) grade because of smoothing
  time_NA <- sum(gps2$delta_time[which(is.na(gps2$grade_smooth))], na.rm = TRUE)
  time_NA
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

time_div <- data.frame(active_time = active_time, moving_time = moving_time,
                         t_mov_rel = t_mov_rel, resting_time = resting_time,
                         t_rest_rel = t_rest_rel, time_asc = time_asc,
                         t_asc_rel = t_asc_rel, time_desc = time_desc,
                         t_desc_rel = t_desc_rel, time_flat = time_flat,
                         t_flat_rel = t_flat_rel, time_NA = time_NA,
                         t_NA_rel = t_NA_rel)
time_div

## ---- fig.width=7, fig.height=5, fig.align='center'----------------------
gpsHR$pace <- (100/6) / gpsHR$speed_smooth
avg_pace <- 100/6/(sum(gpsHR$d_smooth, na.rm = TRUE) /
                     sum(gpsHR$delta_time[!is.na(gpsHR$d_smooth)], na.rm = TRUE))
avg_mov_pace <- 100/6/(sum(gps2$d_smooth, na.rm = TRUE) /
                     sum(gps2$delta_time[!is.na(gps2$d_smooth)], na.rm = TRUE))
best_pace <- min(gpsHR$pace, na.rm = TRUE)
worst_pace <- max(gpsHR$pace, na.rm = TRUE)
pace <- data.frame(avg_pace = avg_pace, avg_mov_pace = avg_mov_pace,
                   best_pace = best_pace, worst_pace = worst_pace)
pace

ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = pace)) + 
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Pace [min/km]")

## ---- eval=FALSE---------------------------------------------------------
#  return(list(time_div = time_div, resting_ind = resting_ind,
#              moving_ind = moving_ind, total_vam = total_vam,
#              pace = pace, gps = gps))

## ---- tidy=TRUE, fig.width=7, fig.height=5, fig.align='center'-----------
myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 20))
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = ele, color = pace)) + 
  sc +
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Elevation [m]")

## ---- eval=-1------------------------------------------------------------
activity_cal_burn <- cb_activity(gender = "male", age = 41, m = 80, h = 180, gpsHR)

gender <- "male"
age <- 41
m <- 80
h <- 180

## ------------------------------------------------------------------------
if(gender == "male") {
  BMR <- 13.75 * m + 5 * h - 6.76 * age + 66.47
} else {
  BMR <- 9.56 * m + 1.85 * h - 4.68 * age + 655.1
}
BMR

## ---- tidy=TRUE----------------------------------------------------------
gpsHR$time <- as.POSIXct(gpsHR$time, format = "%Y-%m-%dT%H:%M:%OSZ")
gpsHR$delta_time <- c(NA, as.numeric(difftime(gpsHR$time[-1], gpsHR$time[1:(length(gpsHR$time) - 1)], units = "secs")))
duration <- sum(gpsHR$delta_time, na.rm = T) / 3600
gpsHR$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gpsHR$lat[1:(length(gpsHR$lat)-1)],
                                       lon1 = gpsHR$lon[1:(length(gpsHR$lon)-1)],
                                       lat2 = gpsHR$lat[2:length(gpsHR$lat)],
                                       lon2 = gpsHR$lon[2:length(gpsHR$lon)]))

avg_pace <- 100/6/(sum(gpsHR$d, na.rm = TRUE) / (duration *3600))

## ------------------------------------------------------------------------
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

MET

## ------------------------------------------------------------------------
grossCB <- BMR * MET * duration / 24

RMRCB <- ((1.1 * BMR) / 24) * duration

netCB <- grossCB - RMRCB
netCB

## ------------------------------------------------------------------------
gpsHR$netCB <- NA
for(i in 1:dim(gpsHR)[1]) {
  # average pace in min/km
  pace <- 100/6/(gpsHR$d[i] / gpsHR$delta_time[i])

  # MET (Metabolic Equivalent) data for physical activities
  if(is.na(pace)){
    gpsHR$netCB[i] <- NA
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

  grossCB_vec <- BMR * MET * (gpsHR$delta_time[i] / 3600) / 24

  RMRCB_vec <- ((1.1 * BMR) / 24) * (gpsHR$delta_time[i] / 3600)

  gpsHR$netCB[i] <- grossCB_vec - RMRCB_vec
}

## ---- eval=-1, tidy=TRUE, fig.width=7, fig.height=5, fig.align='center'----
return(list(netCB = netCB, gpsHR = gpsHR))
netCB
str(gpsHR)

myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 5))
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = ele, color = netCB)) + 
  sc +
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Elevation [m]")

## ---- eval=-1------------------------------------------------------------
running_cal_burn <- cb_running(age = 41, m = 80, h = 180, RHR = 60, treadmill = FALSE, gpsHR)

age <- 41
m <- 80
h <- 180
RHR <- 60
treadmill <- FALSE

## ---- echo=-c(2,4,6)-----------------------------------------------------
BMI <- m / ((h / 100)^2)
BMI
MHR <- 208 - (0.685 * age)
MHR
VO2max <- 15.3 * MHR / RHR
VO2max

## ------------------------------------------------------------------------
if(treadmill) {
  TF <- 0
} else {
  TF <- 0.84
}

## ------------------------------------------------------------------------
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
CFF

## ---- echo=-c(4,7,11), tidy=TRUE-----------------------------------------
gpsHR$time <- as.POSIXct(gpsHR$time, format = "%Y-%m-%dT%H:%M:%OSZ")
gpsHR$delta_time <- c(NA, as.numeric(difftime(gpsHR$time[-1], gpsHR$time[1:(length(gpsHR$time) - 1)], units = "secs")))
duration <- sum(gpsHR$delta_time, na.rm = T) / 3600
duration
gpsHR$d <- c(NA, analyzeGPS::distanceGPS(lat1 = gpsHR$lat[1:(length(gpsHR$lat)-1)],
                                       lon1 = gpsHR$lon[1:(length(gpsHR$lon)-1)],
                                       lat2 = gpsHR$lat[2:length(gpsHR$lat)],
                                       lon2 = gpsHR$lon[2:length(gpsHR$lon)]))
distance <- sum(gpsHR$d, na.rm = T) / 1000
distance

gpsHR$grade <- c(NA, analyzeGPS::gradeGPS(gpsHR$ele, gpsHR$d))
avg_grade <- mean(gpsHR$grade, na.rm = TRUE) * 100
avg_grade

## ------------------------------------------------------------------------
if(avg_grade > 10) {
  netCB_run <- (((0.07 * avg_grade) + 0.75) * m + TF) * distance * CFF
}
if(avg_grade > 0 & avg_grade <= 10) {
  netCB_run <- (((0.05 * avg_grade) + 0.95) * m + TF) * distance * CFF
}
if(avg_grade > -10 & avg_grade <= 0) {
  netCB_run <- (((0.04 * avg_grade) + 0.95) * m + TF) * distance * CFF
}
if(avg_grade > -15 & avg_grade <= -10) {
  netCB_run <- (((-0.02 * avg_grade) + 0.35) * m + TF) * distance * CFF
}
if(avg_grade <= -15) {
  netCB_run <- (((-0.01 * avg_grade) + 0.5) * m + TF) * distance * CFF
}

netCB_run

## ------------------------------------------------------------------------
gpsHR$netCB_run <- NA
for(i in 1:dim(gpsHR)[1]) {
  if(is.na(gpsHR$grade[i])){
    gpsHR$netCB_run[i] <- NA
    next
  }
  if((100 * gpsHR$grade[i]) > 10) {
    gpsHR$netCB_run[i] <- (((0.07 * gpsHR$grade[i]) + 0.75) * m + TF) * (gpsHR$d[i] / 1000) * CFF
  }
  if((100 * gpsHR$grade[i]) > 0 & (100 * gpsHR$grade[i]) <= 10) {
    gpsHR$netCB_run[i] <- (((0.05 * gpsHR$grade[i]) + 0.95) * m + TF) * (gpsHR$d[i] / 1000) * CFF
  }
  if((100 * gpsHR$grade[i]) > -10 & (100 * gpsHR$grade[i]) <= 0) {
    gpsHR$netCB_run[i] <- (((0.04 * gpsHR$grade[i]) + 0.95) * m + TF) * (gpsHR$d[i] / 1000) * CFF
  }
  if((100 * gpsHR$grade[i]) > -15 & (100 * gpsHR$grade[i]) <= -10) {
    gpsHR$netCB_run[i] <- (((-0.02 * gpsHR$grade[i]) + 0.35) * m + TF) * (gpsHR$d[i] / 1000) * CFF
  }
  if((100 * gpsHR$grade[i]) <= -15) {
    gpsHR$netCB_run[i] <- (((-0.01 * gpsHR$grade[i]) + 0.5) * m + TF) * gpsHR$d[i] * CFF
  }
}

## ---- eval=-1, tidy=TRUE, fig.width=7, fig.height=5, fig.align='center'----
return(list(netCB = netCB_run, gpsHR = gpsHR))
netCB_run
str(gpsHR)

myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 5))
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = ele, color = netCB_run)) + 
  sc +
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Elevation [m]")

## ---- eval=-1------------------------------------------------------------
hr_cal_burn <- cb_hr(gender = "male", age = 41, m = 80, h = 180, RHR = 60, gpsHR$hr, gpsHR)

gender <- "male"
age <- 41
m <- 80
h <- 180
RHR <- 60
HR <- gpsHR$hr

## ---- echo=-c(2,4,6), tidy=TRUE------------------------------------------
MHR <- 208 - (0.685 * age)
avg_hr <- mean(HR, na.rm = TRUE)
VO2max <- 15.3 * MHR / RHR

gpsHR$time <- as.POSIXct(gpsHR$time, format = "%Y-%m-%dT%H:%M:%OSZ")
gpsHR$delta_time <- c(NA, as.numeric(difftime(gpsHR$time[-1], gpsHR$time[1:(length(gpsHR$time) - 1)], units = "secs")))
duration <- sum(gpsHR$delta_time, na.rm = T) / 3600

## ---- tidy=TRUE----------------------------------------------------------
if(gender == "male") {
  BMR <- 13.75 * m + 5 * h - 6.76 * age + 66.47
  grossCB <- ((-95.7735 + (0.634 * avg_hr) + (0.404 * VO2max) + (0.394 * m) + (0.271 * age))/4.184) * 60 * duration
  RMRCB <- ((1.1 * BMR) / 24) * duration
  netCB_hr <- grossCB - RMRCB
} else {
  BMR <- 9.56 * m + 1.85 * h - 4.68 * age + 655.1
  grossCB <- ((-59.3954 + (0.45 * avg_hr) + (0.38 * VO2max) + (0.103 * m) + (0.274 * age))/4.184) * 60 * duration
  RMRCB <- ((1.1 * BMR) / 24) * duration
  netCB_hr <- grossCB - RMRCB
}

netCB_hr

## ---- tidy=TRUE----------------------------------------------------------
gpsHR$netCB_hr <- NA

if(gender == "male") {
  BMR <- 13.75 * m + 5 * h - 6.76 * age + 66.47
  for(i in 1:dim(gpsHR)[1]) {
    if(is.na(HR[i])){
      gpsHR$netCB_hr[i] <- NA
      next
    }
    grossCB <- ((-95.7735 + (0.634 * HR[i]) + (0.404 * VO2max) + (0.394 * m) + (0.271 * age))/4.184) * 60 * (gpsHR$delta_time[i] / 3600)
    RMRCB <- ((1.1 * BMR) / 24) * (gpsHR$delta_time[i] / 3600)
    gpsHR$netCB_hr[i] <- grossCB - RMRCB
  }
} else {
  BMR <- 9.56 * m + 1.85 * h - 4.68 * age + 655.1
  for(i in 1:dim(gpsHR)[1]) {
    if(is.na(HR[i])){
      gpsHR$netCB_hr[i] <- NA
      next
    }
    grossCB <- ((-59.3954 + (0.45 * HR[i]) + (0.38 * VO2max) + (0.103 * m) + (0.274 * age))/4.184) * 60 * (gpsHR$delta_time[i] / 3600)
    RMRCB <- ((1.1 * BMR) / 24) * (gpsHR$delta_time[i] / 3600)
    gpsHR$netCB_hr <- grossCB - RMRCB
  }
}

## ---- eval=-1, tidy=TRUE, fig.width=7, fig.height=5, fig.align='center'----
return(list(netCB = netCB_hr, gpsHR = gpsHR))
netCB_hr
str(gpsHR)

myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 5))
ggplot() + 
  geom_line(data = gpsHR, aes(x = time, y = ele, color = netCB_hr)) + 
  sc +
  xlab("Time (GMT) [HH:MM:SS]") + 
  ylab("Elevation [m]")

