#### Indices for usable fluxes ####
# An index for co2 of qc = 1, 2 and wind_check_strict = TRUE and rain_index 
indexCO2 <- (df$qc_co2_flux==1 | df$qc_co2_flux == 0) & 
  df$wind_check_strict & rain_index != TRUE
# An index for LE of qc = 1, 2 and wind_check_strict = TRUE and rain_index 
indexLE <- (df$qc_LE==1 | df$qc_LE == 0) & 
  df$wind_check_strict & rain_index != TRUE
# An index for H of qc = 1, 2 and wind_check_strict = TRUE and rain_index 
indexH <- (df$qc_H==1 | df$qc_H == 0) & 
  df$wind_check_strict & rain_index != TRUE






# median co2 flux during ENSO
median(df$co2_flux[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# median co2 flux after ENSO
median(df$co2_flux[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean LE flux during ENSO
mean(df$LE[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean LE after ENSO
mean(df$LE[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean H flux during ENSO
mean(df$H[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean LE after ENSO
mean(df$H[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean net radiation during ENSO
mean(df$RN_1_1_1[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean co2 flux after ENSO
mean(df$RN_1_1_1[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean global radiation during ENSO
mean(df$RG_1_1_1[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean co2 flux after ENSO
mean(df$RG_1_1_1[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean SST flux during ENSO
mean(df$TW_1_1_1[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean SST flux after ENSO
mean(df$TW_1_1_1[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean atm T flux during ENSO
mean(df$TA_1_1_1[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean SST flux after ENSO
mean(df$TA_1_1_1[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean RH during ENSO
mean(df$RH_1_1_1[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean RH after ENSO
mean(df$RH_1_1_1[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# mean TS_2 during ENSO
mean(df$TS_2_1_1[which(index == TRUE & enso == TRUE)], na.rm = TRUE)
# mean TS_2 after ENSO
mean(df$TS_2_1_1[which(index == TRUE & enso == FALSE)], na.rm = TRUE)

# Some plots
plot(df$time_stamp[which(index == TRUE & enso == TRUE)],
     df$co2_flux[which(index == TRUE & enso == TRUE)], type = 'l',
     ylim = c(-10,10))
plot(df$time_stamp[which(index == TRUE & enso == FALSE)], 
      df$co2_flux[which(index == TRUE & enso == FALSE)], type = 'l',
     col = 'red', ylim = c(-10,10))
boxp1 <- boxplot(df$co2_flux[which(index == TRUE & enso == TRUE)])

