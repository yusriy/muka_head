###############################################################
# Script: Data import
# Purpose: To import data from Muka Head EC station
# 
# TITLE: Air-sea interaction of a semi-enclosed tropical ocean:
# a study on carbon, water, and energy budget
#
# Grant: Research University Individual
# Grant no.:  1001/PTEKIND/811316
#
# Author: Yusri Yusup, PhD
# Date: 2015-11-12
# 
###############################################################

#### 1. Preliminaries #########################################
source('R/tools/convert_magic.R')
source('R/tools/charactersNumeric.R')

#### 2. Importing and processing the data #####################
# Import individual processed data files
df <- read.csv('data/eddypro_20151112_full_output_2015-11-12T200521_exp.csv',
               skip=1)
df_biomet <- read.csv('data/eddypro_20151112_biomet_2015-11-12T200521_exp.csv')

# Delete unnecessary columns and rows in EC data files
df <- df[,-1] # Remove 1st column
df <- df[-1,] # Remove 1st row

# Delete unnecessary columns and rows in Biomet data files
df_biomet <- df_biomet[-1,] # Remove the 1st row
df_biomet <- df_biomet[,-c(1,2,3)] # Remove the first 3 columns to combine with df

# Combine df_biomet with df
df <- cbind(df,df_biomet)

# Using convert_magic to convert all columns to 'character' first
df <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times = ncol(df))))

# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 1:length(df)){
  df[i][df[i] == '-9999' | df[i] == '-9999.0'] <- NA
}
rm(i)

# Formatting time
time_stamp <- paste(df$date,df$time)

# Might need to change format of date 1/1/2014 or 2014-1-1
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df$time <- time_stamp
df <- df[,c(-1)]
colnames(df)[1] <-'time_stamp'
# Remove 'DOY' (Day Of Year)
df <- df[,c(-2)]

# Changing all relevant columns to factors
df$daytime <- as.factor(df$daytime)
df$file_records <- as.factor(df$file_records)
df$used_records <- as.factor(df$used_records)
df$qc_Tau <- as.factor(df$qc_Tau)
df$qc_H <- as.factor(df$qc_H)
df$qc_LE <- as.factor(df$qc_LE)
df$qc_co2_flux <- as.factor(df$qc_co2_flux)
df$qc_h2o_flux <- as.factor(df$qc_h2o_flux)
df$co2_def_timelag <- as.factor(df$co2_time_lag)
df$h2o_def_timelag <- as.factor(df$h2o_def_timelag)
df$spikes <- as.factor(df$spikes)
df$amplitude_resolution <- as.factor(df$amplitude_resolution)
df$drop_out <- as.factor(df$drop_out)
df$absolute_limits <- as.factor(df$absolute_limits)
df$skewness_kurtosis_sf <- as.factor(df$skewness_kurtosis_sf)
df$skewness_kurtosis_hf <- as.factor(df$skewness_kurtosis_hf)
df$discontinuities_sf <- as.factor(df$discontinuities_sf)
df$discontinuities_hf <- as.factor(df$discontinuities_hf)
df$timelag_sf <- as.factor(df$timelag_sf)
df$timelag_hf <- as.factor(df$timelag_hf)
df$attack_angle_hf <- as.factor(df$attack_angle_hf)
df$non_steady_wind_hf <- as.factor(df$non_steady_wind_hf)
df$model <- as.factor(df$model)

# Change all non-factors (or characters) to numeric)
df <- charactersNumeric(df)

# Convert TA_1_1_1, TS_1_1_1, and TS_2_1_1 from K to C
df$TA_1_1_1 <- df$TA_1_1_1 - 273.15
df$TS_1_1_1 <- df$TS_1_1_1 - 273.15
df$TS_2_1_1 <- df$TS_2_1_1 - 273.15

# Change column name of (z-d)/L to Z.L
colnames(df)[which(colnames(df) == 'X.z.d..L')] <- 'Z.L'

# Delete temporary variables
rm(time_stamp,df_biomet)

# Export data
write.table(df,'data/df.csv',sep=',')
