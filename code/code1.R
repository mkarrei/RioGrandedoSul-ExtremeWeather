#####################################################
# Description: 
#   Extreme weather events in Southern Brazil - Rio Grande do Sul State. September and April, May 2024
#   Historical precipitation anomaly using NASA GPM mission (2000 - 2024)
#
# Author: Mauricio Karrei (mauricio.zientar@ufl.edu)
#   Mod. Date 05/16/2024
#####################################################


library(terra)
library(raster)


source('code/functions.R')

# Read available files
files <- list.files('data2')

# extract dates from the files
dates.files <- sub(".*_(\\d{8}).*", "\\1", files)

# Create a dataframe to work on 
files <- data.frame(filename = files,
                    version = sub(".*_(v[0-9][0-9][a-z])_.*", "\\1", files),
                    date = as.Date(dates.files, '%Y%m%d'),
                    year = as.numeric(substr(dates.files, start = 1, stop = 4)),
                    month = as.numeric(substr(dates.files, start = 5, stop = 6)),
                    day = as.numeric(substr(dates.files, start = 7, stop = 8)))


# filter period of interest
files.sep <- subset(files, month == 9 & day <= 10)
stack.sep <- sum.raster.df(files.sep)

# filter period of interest
files.apr <- subset(files, (month == 5 & day <= 10) | (month == 4 & day >= 26))
stack.apr <- sum.raster.df(files.apr)


# Calculate the all years average
average.sep <- mean(stack.sep)
average.apr <- mean(stack.apr)

current.sep <- stack.sep[[nlayers(stack.sep)]]
current.apr <- stack.apr[[nlayers(stack.apr)]]

# Calculate the deviation from average for all years
deviations.sep <- stack.sep - average.sep
deviations.apr <- stack.apr - average.apr

# Deviation of 2024 from historical average
anomaly.sep <- current.sep - average.sep
anomaly.apr <- current.apr - average.apr

#### 
## Export anomaly maps from the lastest events
####
filename <- 'maps/2024_apr_anomaly.png'
export.raster.map(anomaly.apr, title = paste0('2024 - Desvio da Media Historica (2000 - 2024)\n 26 April - 10 Maio'), filename, deviation.colors, deviation.breaks)

filename <- 'maps/2023_sep_anomaly.png'
export.raster.map(anomaly.sep, title = paste0('2023 - Desvio da Media Historica (2000 - 2024)\n1 Setembro - 10 Setembro'), filename, deviation.colors, deviation.breaks)


# Plot deviation maps 
for (i in 1:nlayers(deviations.sep)) {
  
  filename <- paste0('maps/deviation_from_2024/pcpn_sep_dev_', i, '.png')
  export.raster.map(deviations.sep[[i]], paste0('2023 - ', (2000 + (i - 1)) , ' - Anomalia\n1 Setembro - 10 Setembro'), filename, deviation.colors, deviation.breaks)
  
  
  filename <- paste0('maps/deviation_from_2024/pcpn_apr_dev_', i, '.png')
  export.raster.map(deviations.apr[[i]], paste0('2024 - ', (2000 + i) , ' - Anomalia\n26 Abril - 10 Maio'), filename, deviation.colors, deviation.breaks)
}



# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html