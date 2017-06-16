#### Script: Muka Head EC Data import for website ##########################
# Purpose: To import and analyze data from Muka Head EC station
# 
# TITLE: Air-sea interaction of a semi-enclosed tropical ocean:
# a study on carbon, water, and energy budget
#
# Grant: Research University Individual
# Grant no.:  1001/PTEKIND/811316
#
# Author: Yusri Yusup, PhD
# Date: 2017-06-10
# 
# Note: 
# 1. For data before 2015-12-02 10:30:00, all RN_1_1_1 should be
# divided by 13.6, corrections were made for data after this date.
# 2. Before the infrared sensor was TW_1_1_1, now it is TW_0_0_1
# since analysis 2017-05-21
# 3. Changed from TW_0_0_1 back to TW_1_1_1 since 2017-06-17

#### 1. Preliminaries #########################################
source('/home/eddy_cov/Documents/muka_head/R/tools/tool_convert_magic.R')
source('/home/eddy_cov/Documents/muka_head/R/tools/tool_charactersNumeric.R')
source('/home/eddy_cov/Documents/muka_head/R/tools/trapezium_intg_3.R')
source('/home/eddy_cov/Documents/muka_head/R/tools/trapezium_intg_2.R')

#### 2. Importing and processing the data #####################
# NOTE only works for the server atmosfera.usm.my 
# Import individual processed data files
date_ec <- Sys.Date()
path <- '/home/eddy_cov/Downloads/ec_data/results'
path_ec <- Sys.glob(paste(path,'/eddypro_muka_head01_full_output_',
                          date_ec,'T*.csv', sep =''))
path_biomet <- Sys.glob(paste(path,'/eddypro_muka_head01_biomet_',
                              date_ec,'T*.csv', sep =''))

df_web <- read.csv(path_ec, skip=1)
df_biomet_web <- read.csv(path_biomet)

# Delete unnecessary columns and rows in EC data files
df_web <- df_web[,-1] # Remove 1st column
df_web <- df_web[-1,] # Remove 1st row

# Delete unnecessary columns and rows in Biomet data files
df_biomet_web <- df_biomet_web[-1,] # Remove the 1st row
df_biomet_web <- df_biomet_web[,-c(3)] # Remove the first 3 columns to combine with df_web

# Merge the df_web and df_biomet_web
df_web <- merge(df_web,df_biomet_web,by=c('date','time'),all=TRUE)


# Using convert_magic to convert all columns to 'character' first
df_web <- convert_magic(df_web[,c(seq(1,ncol(df_web)))],c(rep('character',times = ncol(df_web))))

# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 1:length(df_web)){
  df_web[i][df_web[i] == '-9999' | df_web[i] == '-9999.0'] <- NA
}
rm(i)

# Formatting time
time_stamp <- paste(df_web$date,df_web$time)

# Might need to change format of date 1/1/2014 or 2014-1-1
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df_web$time <- time_stamp
df_web <- df_web[,c(-1)]
colnames(df_web)[1] <-'time_stamp'
# Remove 'DOY' (Day Of Year)
df_web <- df_web[,c(-2)]

# Changing all relevant columns to factors
df_web$daytime <- as.factor(df_web$daytime)
df_web$file_records <- as.factor(df_web$file_records)
df_web$used_records <- as.factor(df_web$used_records)
df_web$qc_Tau <- as.factor(df_web$qc_Tau)
df_web$qc_H <- as.factor(df_web$qc_H)
df_web$qc_LE <- as.factor(df_web$qc_LE)
df_web$qc_co2_flux <- as.factor(df_web$qc_co2_flux)
df_web$qc_h2o_flux <- as.factor(df_web$qc_h2o_flux)
df_web$co2_def_timelag <- as.factor(df_web$co2_time_lag)
df_web$h2o_def_timelag <- as.factor(df_web$h2o_def_timelag)
df_web$spikes <- as.factor(df_web$spikes)
df_web$amplitude_resolution <- as.factor(df_web$amplitude_resolution)
df_web$drop_out <- as.factor(df_web$drop_out)
df_web$absolute_limits <- as.factor(df_web$absolute_limits)
df_web$skewness_kurtosis_sf <- as.factor(df_web$skewness_kurtosis_sf)
df_web$skewness_kurtosis_hf <- as.factor(df_web$skewness_kurtosis_hf)
df_web$discontinuities_sf <- as.factor(df_web$discontinuities_sf)
df_web$discontinuities_hf <- as.factor(df_web$discontinuities_hf)
df_web$timelag_sf <- as.factor(df_web$timelag_sf)
df_web$timelag_hf <- as.factor(df_web$timelag_hf)
df_web$attack_angle_hf <- as.factor(df_web$attack_angle_hf)
df_web$non_steady_wind_hf <- as.factor(df_web$non_steady_wind_hf)
df_web$model <- as.factor(df_web$model)

# Change all non-factors (or characters) to numeric)
df_web <- charactersNumeric(df_web)

# Convert TA_1_1_1, TS_1_1_1, and TS_2_1_1 from K to C
df_web$TA_1_1_1 <- df_web$TA_1_1_1 - 273.15
df_web$TS_1_1_1 <- df_web$TS_1_1_1 - 273.15
df_web$TS_2_1_1 <- df_web$TS_2_1_1 - 273.15

# Remove all improbable values of T
df_web$TA_1_1_1[which(df_web$TA_1_1_1 < 0 | df_web$TA_1_1_1 > 100 )] <- NA
df_web$TS_1_1_1[which(df_web$TS_1_1_1 < 0 )] <- NA
df_web$TS_2_1_1[which(df_web$TS_2_1_1 < 0 )] <- NA

# Correct TW_1_1_1 values using calibration equation from Mei Thung's experiment
df_web$TW_1_1_1 <- (df_web$TW_1_1_1 - 14.00) / 0.69

# Change column name of (z-d)/L to Z.L
colnames(df_web)[which(colnames(df_web) == 'X.z.d..L')] <- 'Z.L'

# Delete temporary variables
rm(time_stamp,df_biomet_web)

# Obtain the indices to filter out the data for 180 < wind_dir < 270 
# to exclude data from land.
wind_check <- df_web$wind_dir <= 90 | df_web$wind_dir >= 270 # index to use flux data
wind_check_strict <- df_web$wind_dir <= 90 # stricter wind check 
df_web <- cbind(df_web,wind_check,wind_check_strict)
rm(wind_check)

#### Classifying into number of days ####
start <- as.numeric(df_web$time_stamp[1])
difference <- 86400 # 24 * 60 * 60 seconds

index = 1 # The number the days
day = numeric(length=nrow(df_web)) # Initialize day variable

for (i in 1:nrow(df_web)){
  # A failsafe if time_stamp is NA
  if(is.na(as.numeric(df_web$time_stamp[i]))){
    # Assigned the NA value as the day before
    day[i] <- index
  } else {
    if(as.numeric(df_web$time_stamp[i]) >= start & 
       as.numeric(df_web$time_stamp[i]) < (difference + start)){
      day[i] <- index
    } else {
      day[i] <- index
      index <- index + 1
      start <- difference + start
    }
  }
}

df_web <- cbind(df_web,day)
rm(start,difference,index,day)

#### Filter TS_1_1_1 values ####
# Create temporary TS_1_1_1 value
ts <- df_web$TS_1_1_1
# Standard dev of TS_1_1_1
ts_sd <- numeric(nrow(df_web))
# Mean of TS_1_1_1
ts_mean <- numeric(nrow(df_web))
# Level of standard deviation
level <- 0.5  # Just a very low bandwith filter to ensure that outside water
              # is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df_web)){
  if(df_web$day[i] == j){
    ts_sd[i] <- sd(df_web$TS_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    ts_mean[i] <- mean(df_web$TS_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    temp_sd <- ts_sd[i]
    temp_mean <- ts_mean[i]
    j <- j + 1
  } else {
    ts_sd[i] <- temp_sd
    ts_mean[i] <- temp_mean
  }
}
# Remove all above water temperature readings by X level std. dev.
df_web$TS_1_1_1[which(ts < ts_mean - (level * ts_sd))] <- NA 
rm(i,j,level,temp_mean,temp_sd,ts,ts_mean,ts_sd)

#### Filter TS_2_1_1 values ####
# Create temporary TS_2_1_1 value
ts2 <- df_web$TS_2_1_1
# Standard dev of TS_2_1_1
ts2_sd <- numeric(nrow(df_web))
# Mean of TS_2_1_1
ts2_mean <- numeric(nrow(df_web))
# Level of standard deviation
level <- 0.5  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df_web)){
  if(df_web$day[i] == j){
    ts2_sd[i] <- sd(df_web$TS_2_1_1[which(df_web$day==j)], na.rm = TRUE)
    ts2_mean[i] <- mean(df_web$TS_2_1_1[which(df_web$day==j)], na.rm = TRUE)
    temp_sd2 <- ts2_sd[i]
    temp_mean2 <- ts2_mean[i]
    j <- j + 1
  } else {
    ts2_sd[i] <- temp_sd2
    ts2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df_web$TS_2_1_1[which(ts2 > ts2_mean + (level * ts2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,ts2,ts2_mean,ts2_sd)

#### Filter TW_1_1_1 values ####
# Create temporary TW_1_1_1 value
tw <- df_web$TW_1_1_1
# Standard dev of TW_1_1_1
tw2_sd <- numeric(nrow(df_web))
# Mean of TW_1_1_1
tw2_mean <- numeric(nrow(df_web))
# Level of standard deviation
level <- 20  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df_web)){
  if(df_web$day[i] == j){
    tw2_sd[i] <- sd(df_web$TW_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    tw2_mean[i] <- mean(df_web$TW_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    temp_sd2 <- tw2_sd[i]
    temp_mean2 <- tw2_mean[i]
    j <- j + 1
  } else {
    tw2_sd[i] <- temp_sd2
    tw2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df_web$TW_1_1_1[which(tw > tw2_mean + (level * tw2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,tw,tw2_mean,tw2_sd)

#### Filter RH_1_1_1 ambient RH ####
# Improbable values of RH
df_web$RH_1_1_1[which(df_web$RH_1_1_1 > 100 | df_web$RH_1_1_1 < 50)] <- NA

# Create temporary RH_1_1_1 value
RH <- df_web$RH_1_1_1
# Standard dev of RH_1_1_1
RH2_sd <- numeric(nrow(df_web))
# Mean of RH_1_1_1
RH2_mean <- numeric(nrow(df_web))
# Level of standard deviation
level <- 10  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df_web)){
  if(df_web$day[i] == j){
    RH2_sd[i] <- sd(df_web$RH_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    RH2_mean[i] <- mean(df_web$RH_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    temp_sd2 <- RH2_sd[i]
    temp_mean2 <- RH2_mean[i]
    j <- j + 1
  } else {
    RH2_sd[i] <- temp_sd2
    RH2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df_web$RH_1_1_1[which(RH > RH2_mean + (level * RH2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,RH,RH2_mean,RH2_sd)

#### Filter TA_1_1_1 ambient RH ####

# Create temporary TA_1_1_1 value
TA <- df_web$TA_1_1_1
# Standard dev of TA_1_1_1
TA2_sd <- numeric(nrow(df_web))
# Mean of RH_1_1_1
TA2_mean <- numeric(nrow(df_web))
# Level of standard deviation
level <- 1  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df_web)){
  if(df_web$day[i] == j){
    TA2_sd[i] <- sd(df_web$TA_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    TA2_mean[i] <- mean(df_web$TA_1_1_1[which(df_web$day==j)], na.rm = TRUE)
    temp_sd2 <- TA2_sd[i]
    temp_mean2 <- TA2_mean[i]
    j <- j + 1
  } else {
    TA2_sd[i] <- temp_sd2
    TA2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df_web$TA_1_1_1[which(TA > TA2_mean + (level * TA2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,TA,TA2_mean,TA2_sd)

#### Filter RN_1_1_1 ####
df_web$RN_1_1_1[which(df_web$RN_1_1_1 > 1000)] <- NA

#### Calculate energy storage in water ####
# Only 3 heights including the water surface temperature
# These are estimated heights
# Level 1: water surface = 0.0001 m 
# Level 2: 2 m 
# Level 3: 3 m
heights <- c(1,2) 
# Calculating rho * cp for each level
rho <- 1025 # Density of sea water = 1020 to 1029 kg m-3
c_p <- 3850 # Specific heat capacity of sea water = 3850 J kg-1 C-1
rho_cp <- rho * c_p
rm(rho,c_p)

# Level 2, 2 m
rho_cp_dT2 <- numeric()
for (i in 1:nrow(df_web)){
  rho_cp_dT2[i] <- ((rho_cp*df_web$TS_1_1_1[i]) - 
                      (rho_cp*df_web$TS_1_1_1[i-1]))/(30 * 60)
}

# Level 3, 5 m
rho_cp_dT3 <- numeric()
for (i in 1:nrow(df_web)){
  rho_cp_dT3[i] <- ((rho_cp*df_web$TS_2_1_1[i]) - 
                      (rho_cp*df_web$TS_2_1_1[i-1]))/(30 * 60)
}

# Integrating using the trapezium area rule
H_stor <- numeric()
for (i in 1:nrow(df_web)){
  H_stor[i] <- trapezium_intg_2(heights,rho_cp_dT2[i],rho_cp_dT3[i])
}

# Adding to df_EC
df_web <- cbind(df_web,H_stor)
rm(heights,i,rho_cp,rho_cp_dT2,rho_cp_dT3,H_stor)

#### Filter H_stor values ####

# Create temporary H_stor value
H_stor_filter <- df_web$H_stor
# Standard dev of H_stor
hstor_sd <- numeric(nrow(df_web))
# Mean of H_stor
hstor_mean <- numeric(nrow(df_web))

# Level of standard deviation
level <- 20 # A large bandwidth to ensure most of the data is not removed

## Calculate standard deviation of H_stor
# To count number of days
j <- 1
for (i in 1:nrow(df_web)){
  if(df_web$day[i] == j){
    hstor_sd[i] <- sd(df_web$H_stor[which(df_web$day==j)], na.rm = TRUE)
    hstor_mean[i] <- mean(df_web$H_stor[which(df_web$day==j)], na.rm = TRUE)
    hstor1_sd <- hstor_sd[i]
    hstor1_mean <- hstor_mean[i]
    j <- j + 1
  } else {
    hstor_sd[i] <- hstor1_sd
    hstor_mean[i] <- hstor1_mean
  }
}
# Remove all above water temperature readings by X level std. dev.
H_stor_filter[which(H_stor_filter < hstor_mean - (level * hstor_sd) | 
                      H_stor_filter > hstor_mean + (level * hstor_sd))] <- NA
df_web <- cbind(df_web,H_stor_filter)
rm(i,j,level,hstor_mean,hstor_sd,H_stor_filter,hstor1_mean,hstor1_sd)

