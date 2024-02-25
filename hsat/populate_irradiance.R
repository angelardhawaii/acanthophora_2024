# These are time series of the irradiance measurements
# The 3rd column is the value of interest (irradiance)
irradiance <- read.csv("./hsat/data_irradiance/acan_r1_r2.csv")

# Make sure the datread.csv()# Make sure the date is loaded as date
irradiance$posix_date <- as.POSIXct(irradiance$date, format = "%m/%d/%y", tz = "")
irradiance$date_time <- as.POSIXct(paste(irradiance$date, irradiance$time, sep = " "), format = "%m/%d/%y %H:%M:%S")
irradiance$Epar <- as.numeric(irradiance$Epar)
