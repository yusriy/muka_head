##############################################################
# TITLE: Coastal eddy covariance and meteorology data analysis
# 
#
#
#
##############################################################

#### 1. Preliminaries ####
require(openair)

#### 2. Analysis and plots ####

# This step is needed if the dataframes are named other than 'df'
# Just need to change the name of df here and the rest of the script
# would be the same.
df <- df

# Wind speed and direction
# Plot wind speed, U [m s-1]
plot(df$time_stamp,df$wind_speed,type='l',xlab='Date',ylab='U',lwd=2)
# Plot a new plot on top of the previous plot
par(new=T)
# Plot wind direction, WD [deg]
plot(df$time_stamp,df$wind_dir,type='l',axes=FALSE,xlab=NA,ylab=NA,
     col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'WD')

# Temperature and relative humidity
# Plot temperature, T [deg C]
plot(df$time_stamp,df$TA_1_1_1,type='l',xlab='Date',ylab='T',lwd=2)
# Plot a new plot on top of the previous plot
par(new=T)
# Plot relative humidity, RH [%]
plot(df$time_stamp,df$RH,type='l',axes=FALSE,xlab=NA,ylab=NA,
     col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'RH')

# Water surface temperature and below water surface temperatures
# Plot water surface temperature, TW [deg C]
plot(df$time_stamp,temp,type='l',xlab='Date',ylab='TS1',lwd=2)

# Plot water surface temperature TS1 [deg C]
lines(df$time_stamp,df$TS_2_1_1,col='red')
