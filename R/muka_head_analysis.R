##############################################################
# TITLE: Coastal eddy covariance and meteorology data analysis
# 
#
#
#
##############################################################

#### 1. Preliminaries ####

library(openair)
library(Hmisc)
library(dplyr)



# Saving old plotting parameters
#old.par <- par()

#### 2. Analysis and plots ####

# This step is needed if the dataframes are named other than 'df'
# Just need to change the name of df here and the rest of the script
# would be the same.
df <- df

#### Wind speed and direction ####
# Plot wind speed, U [m s-1]
plot(df$time_stamp,df$wind_speed,type='l',xlab='Date',ylab='U',lwd=2)
# Plot a new plot on top of the previous plot
par(new=T)
# Plot wind direction, WD [deg]
plot(df$time_stamp,df$wind_dir,type='l',axes=FALSE,xlab=NA,ylab=NA,
     col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'WD')
windRose(df,ws='wind_speed',wd='wind_dir',paddle=FALSE)

#### Temperature and relative humidity ####
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
plot(df$time_stamp,df$TW_1_1_1,type='l',xlab='Date',ylab='TS1',lwd=2)

# Plot water surface temperature TS1 [deg C]
lines(df$time_stamp,df$TS_2_1_1,col='red')

#### Some water temperature and water heat storage ####

plot(df$time_stamp,df$TS_1_1_1,type='l',lwd=2,xlab='Date',ylab='Temperature')
lines(df$time_stamp,df$TS_2_1_1,lwd=2,col='red')
#lines(df$time_stamp,df$TW_1_1_1,lwd=2,xlab = 'Date',ylab='Temperature',type='l')

plot(df$time_stamp,df$H_stor_filter,type='l',xlab='Date',ylab='Energy',lwd=2,
     main='Filtered H storage')
plot(df$time_stamp,df$H_stor,type='l',xlab='Date',ylab='Energy',lwd=2,
     main='Unfiltered H storage')

#### Diurnal plots ####
### Preparation of data ###
# Grouping into hours for all data
# Note: since the update have to create separate groups for mean and sd.
df_grp_mean <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            LE=mean(LE,na.rm=TRUE),
            H=mean(H,na.rm=TRUE),
            WS=mean(wind_speed,na.rm=TRUE),
            WD=mean(wind_dir,na.rm=TRUE),
            zL=mean(Z.L,na.rm=TRUE),
            H_stor=mean(H_stor,na.rm=TRUE),
            H_stor_filter=mean(H_stor_filter,na.rm=TRUE),
            RG=mean(RG_1_1_1,na.rm=TRUE),
            RN=mean(RN_1_1_1,na.rm=TRUE),
            TW1=mean(TS_1_1_1,na.rm=TRUE),
            TW2=mean(TS_2_1_1,na.rm=TRUE),
            TA=mean(TA_1_1_1,na.rm=TRUE),
            RH=mean(RH_1_1_1,na.rm=TRUE))
df_grp_sd <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            LE_sd=sd(LE,na.rm=TRUE),
            H_sd=sd(H,na.rm=TRUE),
            WS_sd=sd(wind_speed,na.rm=TRUE),
            WD_sd=sd(wind_dir,na.rm=TRUE),
            zL_sd=sd(Z.L,na.rm=TRUE),
            H_stor_sd=sd(H_stor,na.rm=TRUE),
            H_stor_filter_sd=sd(H_stor_filter,na.rm=TRUE),
            RG_sd=sd(RG_1_1_1,na.rm=TRUE),
            RN_sd=sd(RN_1_1_1,na.rm=TRUE),
            TW1_sd=sd(TS_1_1_1,na.rm=TRUE),
            TW2_sd=sd(TS_2_1_1,na.rm=TRUE),
            TA_sd=sd(TA_1_1_1,na.rm=TRUE),
            RH_sd=sd(RH_1_1_1,na.rm=TRUE))
# Merge the two dataframe
df_grp <- merge(df_grp_mean,df_grp_sd,by='hour')
rm(df_grp_sd,df_grp_mean)


### Diurnal plots ###
# Diurnal H
plot(df_grp$hour,df_grp$H,lwd=2,type='l')
# Diurnal LE
plot(df_grp$hour,df_grp$LE,lwd=2,type='l')
# Diurnal CO2
plot(df_grp$hour,df_grp$co2_flux,lwd=2,type='l')
# Diurnal atmospheric stability
plot(df_grp$hour,df_grp$zL,lwd=2,type='l')
# Diurnal wind speed
plot(df_grp$hour,df_grp$WS,lwd=2,type='l')
# Diurnal wind direction
plot(df_grp$hour,df_grp$WD,lwd=2,type='l')
# Diurnal heat storage
plot(df_grp$hour,df_grp$H_stor,lwd=2,type='l')
# Diurnal heat storage (filtered)
plot(df_grp$hour,df_grp$H_stor_filter,lwd=2,type='l')
# Diurnal global radiation
plot(df_grp$hour,df_grp$RG,lwd=2,type='l')
# Diurnal global radiation
plot(df_grp$hour,df_grp$RN,lwd=2,type='l')
# Diurnal water temperature (surface) level 1
plot(df_grp$hour,df_grp$TW1,lwd=2,type='l')
# Diurnal water temperature (under) level 2
plot(df_grp$hour,df_grp$TW2,lwd=2,type='l')
# Diurnal air temperature
plot(df_grp$hour,df_grp$TA,lwd=2,type='l')
# Diurnal RH
plot(df_grp$hour,df_grp$RH,lwd=2,type='l')
