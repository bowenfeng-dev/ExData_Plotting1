library(readr)
library(dplyr)

combineDateTime <- function(d, t) {
  as.POSIXct(paste(d, t), format="%Y-%m-%d %H:%M:%S")
}

loadAllPowerData <- function() {
  read_delim("household_power_consumption.txt", delim=";", na=c("?"), col_types = cols(
    Date = col_date(format = "%d/%m/%Y"),
    Time = col_character(),
    Global_active_power = col_double(),
    Global_reactive_power = col_double(),
    Voltage = col_double(),
    Global_intensity = col_double(),
    Sub_metering_1 = col_double(),
    Sub_metering_2 = col_double(),
    Sub_metering_3 = col_double()
  ))
}

# Load data
powerdata <- loadAllPowerData() %>%
  filter(Date==as.Date("2007-02-01") | Date == as.Date("2007-02-02")) %>%
  mutate(dt = combineDateTime(Date, Time)) %>%
  arrange(dt)

# Generate plot
with(powerdata, plot(dt, Global_active_power, type="n", ylab="Global Active Power (kilowatts)", xlab=NA))
with(powerdata, lines(dt, Global_active_power))

# Save to png file
dev.copy(png,"plot2.png",width=480,height=480)
dev.off()
