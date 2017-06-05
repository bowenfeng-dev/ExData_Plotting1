library(readr)

# Load data
powerdata.all <- read_delim("household_power_consumption.txt", delim=";", na=c("?"), col_types = cols(
  Date = col_date(format = "%d/%m/%Y"),
  Time = col_time(),
  Global_active_power = col_double(),
  Global_reactive_power = col_double(),
  Voltage = col_double(),
  Global_intensity = col_double(),
  Sub_metering_1 = col_double(),
  Sub_metering_2 = col_double(),
  Sub_metering_3 = col_double()
))
powerdata.feb <- powerdata.all[powerdata.all$Date == as.Date("2007-02-01") | powerdata.all$Date == as.Date("2007-02-02"),]

# Generate plot
hist(powerdata.feb$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")

# Save to png file
dev.copy(png,"plot1.png",width=480,height=480)
dev.off()
