#' Assessment of emotional valence and arousal
#'
#' This function uses the EEG data collected with Muse and assesses emotional valence and arousal based on asymmetry analysis.
#' The EEG data contains EEG signal in microvolts and alpha and beta absolute band powers (in Bels!).
#' Emotional valence is calcluated based on the ratio of alpha power between right and left EEG chanels.
#' Emotional arousal is calculated based on the mean value of beta to alpha ratios of left and right EEG channels.
#' @keywords EEG, alpha, beta, valence, arousal, EEG assymetry
#' @param data Data frame containing ECG, EEG, GPS and heart rate data.
#' @return
#'  The function returns the input data frame complemented with columns:
#'  \itemize{
#'    \item \code{timecest}: the time vector in \code{POSIXct} format (converted from the \code{time} column),
#'    \item \code{fp1_fp2_beta_alpha_avg}: average values of beta-alpha ratios of channels FP1 and FP2,
#'    \item \code{arousal}: factorized arousal classes averaged for channels FP1 and FP2,
#'    \item \code{valence}: factorized valence classes for channels FP1 and FP2,
#'    \item \code{arousalFP1}: factorized arousal classes for channel FP1,
#'    \item \code{arousalFP2}: factorized arousal classes for channel FP2. }
#' @export

emotion_analysis <- function(data) {

  # Change options for this function and reset the change on exit.
  digs <- options(digits = 14, digits.secs = 3)
  on.exit(options(digs))

  # Load acquired data. Values of different EEG channels are in separate columns.
  emotion <- data

  if(emotion$time[1] < 1e12) {
    emotion$timecest <- as.POSIXct(emotion$time, origin = "1970-01-01")
  } else {
    emotion$timecest <- as.POSIXct(emotion$time/1000, origin = "1970-01-01")
  }

  # Create subset of data for calculating arousal.
  emotion$fp1_fp2_beta_alpha_avg <-
    (emotion$fp1_beta_alpha + emotion$fp2_beta_alpha) / 2
  fp1_fp2_beta_alpha <- subset(emotion,
                               emotion$type == "beta",
                               select = c("time","fp1_fp2_beta_alpha_avg"))

  # Use the average of beta/alpha ratio between FP1 and FP2 as arousal indicator.
  arousal <- classInt::classIntervals(fp1_fp2_beta_alpha$fp1_fp2_beta_alpha_avg,
                                      n = 5, style = "equal")
  fp1_fp2_beta_alpha$arousal <- classInt::findCols(arousal)
  fp1_fp2_beta_alpha$arousal <- as.factor(fp1_fp2_beta_alpha$arousal)

  # Insert the arousal vector into the main dataframe (emotion).
  emotion$arousal <- NA
  emotion$arousal[which(emotion$type == "beta")] <- fp1_fp2_beta_alpha$arousal
  emotion$arousal <- as.factor(emotion$arousal)

  # Create subset of data for calculating valence.
  alpha_fp2_fp1 <- subset(emotion, emotion$type == "alpha",
                          select = c("timecest","fp2_fp1"))

  # Use the alpha band ratio between FP1 and FP2 as valence indicator.
  valence <- classInt::classIntervals(alpha_fp2_fp1$fp2_fp1, n = 3, style = "equal")
  alpha_fp2_fp1$valence <- classInt::findCols(valence)
  alpha_fp2_fp1$valence <- as.factor(alpha_fp2_fp1$valence)

  # Insert the valence vector into the main datarame (emotion).
  emotion$valence <- NA
  emotion$valence[which(emotion$type == "alpha")] <- alpha_fp2_fp1$valence
  emotion$valence <- as.factor(emotion$valence)

  # Arousal based on ratios of beta and alpha bands in the same channel
  fp1_beta_alpha <- subset(emotion, emotion$type == "beta",
                           select = c("timecest","fp1_beta_alpha"))

  arousalFP1 <- classInt::classIntervals(fp1_beta_alpha$fp1_beta_alpha,
                                         n = 5, style = "equal")
  fp1_beta_alpha$arousal <- classInt::findCols(arousalFP1)
  fp1_beta_alpha$arousal <- as.factor(fp1_beta_alpha$arousal)

  emotion$arousalFP1 <- NA
  emotion$arousalFP1[which(emotion$type == "beta")] <- fp1_beta_alpha$arousal
  emotion$arousalFP1 <- as.factor(emotion$arousalFP1)


  fp2_beta_alpha <- subset(emotion, emotion$type == "beta",
                           select = c("timecest","fp2_beta_alpha"))

  arousalFP2 <- classInt::classIntervals(fp2_beta_alpha$fp2_beta_alpha,
                                         n = 5, style = "equal")
  fp2_beta_alpha$arousal <- classInt::findCols(arousalFP2)
  fp2_beta_alpha$arousal <- as.factor(fp2_beta_alpha$arousal)

  emotion$arousalFP2 <- NA
  emotion$arousalFP2[which(emotion$type == "beta")] <- fp2_beta_alpha$arousal
  emotion$arousalFP2 <- as.factor(emotion$arousalFP2)

  return(emotion)

}
