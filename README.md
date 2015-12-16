ehrscape.com R packages
========

Collection of R packages to demonstrate the use of [R Project](https://www.r-project.org/) software for basic analysis tasks. 

Installation instructions
--------

The packages are intended to be used as a part of R. Therefore, the basic requirement is to have R installed beforehand. 
R Project is available for download from any of the list of [CRAN mirrors](https://cran.r-project.org/mirrors.html). 
Detailed installation instructions and support is available [here](https://cran.r-project.org/faqs.html). 
For a more user friendly experience with R we recommend the [RStudio](https://www.rstudio.com/products/RStudio/), an open source free IDE for R. 

Once R is up and running (with or without RStudio), we are almost ready to use the packages from this repository. R enables downloading and installing packages directly from GitHub by using the function `install_github` of the `devtools` package in just two lines of code: 

```r
install.packages("devtools")
devtools::install_github("username/packagename")
```

An example of the second line for one of the packages in this repository would be

```r
devtools::install_github("ehrscape/R-project/AnalyzeGPS")
```

Included packages
-------

### EhrscapeR - using EhrScape REST API from R 

The package is designed to collect data from open health data platform [EhrScape](https://www.ehrscape.com/index.html) with [AQL](http://www.openehr.org/releases/QUERY/latest/docs/AQL/AQL.html) queries via [REST](https://en.wikipedia.org/wiki/Representational_state_transfer) web service architecture. Healthcare data is obtained by calling a REST API, and then format the returned result set in R to ready the data for more sophisticated analysis. Saving data records (compositions) to EhrScape is also enabled.  

The package **EhrscapeR** includes functions: 

* `get_query`: for a given AQ query returns the result set in a data frame. 
* `get_query_csv`: for a given AQL query returns the CSV formated result set in a data frame. 
* `post_composition`: stores new data records (compositions) in EhrScape.

For more details on the usage of the package please refer to the included vignette.

### ParseGPX - parsing GPX data to R data frame

The R package **parseGPX** was designed for reading and parsing of GPX files containing GPS data. 
GPS data has become broadly available by integrating low-cost GPS chips into portable consumer devices. 
Consequently, there is an abundance of online and offline tools for GPS data visualization and analysis with R project being in the focus in this example. 
The data itself can be generated in several different file formats, such as txt, csv, xml, kml, gpx. 
Among these the [GPX data format](http://www.topografix.com/gpx.asp) is ment to be the most universal intended for exchanging GPS data between programs, and for sharing GPS data with other users. 
Unlike many other data files, which can only be understood by the programs that created them, GPX files actually contain a description of what's inside them, allowing anyone to create a program that can read the data within. 
Several R packages already exist with functions for reading and parsing of GPX data files, e.g. `plotKML`, `maptools`, `rgdal` with corresponding functions `readGPX`, `readGPS` and `readOGR`.

The presented package **parseGPX** contains the function `parse_gpx` to read, parse and optionally save GPS data.

For more details on the usage of the package please refer to the included vignette.

### AnalyzeGPS - analyze GPS data

The R package **analyzeGPS** offers functions for basic preparation and analysis of the GPS data: 

* `readGPS`: imports the GPS data in csv format into R data frame,
* `distanceGPS`: calculation of distance between two data points or vectors of data points,
* `speedGPS`: calculation of velocity between GPS data points,
* `accGPS`: calculation of acceleration between GPS data points,
* `gradeGPS`: calculation of grade or inclination between GPS data points.

Additionally, an example GPS dataset *myGPSData.csv*, acquired during cycling of a person. It is a data frame with 7771 rows and 5 variables:

* `lon`: longitude data,
* `lat`: latitude data,
* `ele`: elevation data,
* `time`: GPS time stamp - GMT time zone,
* `tz_CEST`: time stamp converted to CEST time zone. 

For more details on the usage of the package please refer to the included vignette.

### ZephyrECG - parse and read Zephyr BH3 ECG data

The R package **zephyrECG** was designed to parse ECG data acquired with the Zephyr BioHarness 3 (BH3) monitor and how to import it into R. 
The package includes functions

* `separate_bh3`:  parses and separates multiple sessions of ECG data recorded with the Zephyr BH3 monitor into separate csv files,
* `read_ecg`: imports the ECG data stored in a csv file to a data frame in R.

For more details on the usage of the package please refer to the included vignette.

### HeartBeat - heart beat detection from single-lead ECG signal

The R package **heartBeat** was designed for heart beat detection from single-lead ECG signals. 
The ECG data is expected to be already loaded into a data frame and ready to use 
(for importing data recorded with Zephyr BioHarness 3 monitor, please see the package **zephyrECG**). 
The package includes functions

* `heart_beat`: detection of heart beats,
* `HRdistribution`: reads the signal and the output of `heart_beat` function and determines instant heart rates, their distribution and a basic histogram,
* `annotateHR`: adds factorized code to ECG data points according to heart rate determined previously with functions `heart_beat` and `HRdistribution`.

For more details on the usage of the package please refer to the included vignette.

### StressHR - heart rate variability analysis for stress assessment 

The R package **stressHR** assesses mental stress based on heart rate. 
Heart beats and heart rate are previously detected from single-lead ECG signal by using the `heartBeat` package.
The package includes functions

* `hrv_analyze`: executes the [heart rate variability (HRV)](https://en.wikipedia.org/wiki/Heart_rate_variability) on heart beat positions written in an ASCII file (`Rsec_data.txt`),
* `merge_hrv`: merges the HRV data with the initial ECG data frame.

For more details on the usage of the package please refer to the included vignette. 

### MuseEEG - parse and read EEG data from InterAxon Muse device 

The R package **museEEG** was designed to parse and read the EEG data collected with the [InterAxon Muse device](http://www.choosemuse.com/). The device stores the acquired data directly in .muse format and the manufacturer offers a tool [MusePlayer](http://developer.choosemuse.com/research-tools/museplayer) that converts the .muse data to .csv format. 
The package is comprised of `read_eeg` function, which reads a file in csv format acquired by MUSE hardware and sorts the EEG data in it into a data frame.

For more details on the usage of the package please refer to the included vignette. 

### EmotionEEG - emotional valence and arousal assessment based on EEG recordings

The R package **emotionEEG** uses the EEG data collected with the [InterAxon Muse device](http://www.choosemuse.com/) and assesses emotional valence and arousal based on asymmetry analysis. 
The EEG data prepared by the `museEEG` package contains EEG signal values in microvolts and alpha and beta absolute band powers and ratios in decibels. 
Emotional valence is calcluated based on the ratio of alpha band power between right and left EEG chanels FP2 and FP1. 
Emotional arousal is calculated based on the mean value of beta to alpha ratios of left and right EEG channels FP1 and FP2. 
The package includes functions

* `read_eeg`: reads raw EEG data in csv format acquired by MUSE hardware and sorts it into a data frame,
* `emotion_analysis`: uses the EEG data collected with Muse and assesses emotional valence and arousal based on asymmetry analysis.

For more details on the usage of the package please refer to the included vignette.

### CycleR - cycling analysis through GPS data 

The R package **cycleR** was designed to calculate advanced cycling parameters based on GPS data of the cycling route, for example power output, climb categorization, route-time segmentation. 
Calculations used in the package are based on [Strava glossary & calculations](https://strava.zendesk.com/forums/20246821-Strava-Glossary-Calculations)
The package [**analyzeGPS**](https://github.com/ehrscape/R-project/tree/master/AnalyzeGPS) is required.
The package is comprised of functions 
* `segment_time`: segments the cycling route according to activity, 
* `categorize`: detects and categorizes the climbs on the route, 
* `cycling_power`: assesses the total power produced by a cyclist on a bike ride given the GPS data and additional physical parameters.

For more details on the usage of the package please refer to the included vignette. 

### RunneR - running analysis with GPS data

The R package **runneR** was designed to calculate advanced running parameters based on GPS data of the running route, for example pace, calories burned, route segmentation, moving time. 
Calculations used in the package are based on [Strava glossary & calculations](https://strava.zendesk.com/forums/20246821-Strava-Glossary-Calculations)
The package [**analyzeGPS**](https://github.com/ehrscape/R-project/tree/master/AnalyzeGPS) is required.
The package is comprised of functions 
* `analyze_run`: determines the total time, moving time, resting time, time spent ascending, descending and on the flat and also pace on a running route described with GPS data, 
* `cb_activity`: estimates the amount of calories that you burn while running activity based on the MET (Metabolic Equivalent) data for physical activities, 
* `cb_running`: estimates the calories that you burn while running any given distance,
* `cb_hr`: estimates the calories that you burn during aerobic (i.e. cardiorespiratory) exercise, based on your average heart rate while performing the exercise.

For more details on the usage of the package please refer to the included vignette. 
