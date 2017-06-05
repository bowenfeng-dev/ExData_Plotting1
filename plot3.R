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
png("plot3.png", width=480, height=480)

sub1 <- powerdata %>% select(dt, sub=Sub_metering_1) %>% mutate(sub_type="sub1")
sub2 <- powerdata %>% select(dt, sub=Sub_metering_2) %>% mutate(sub_type="sub2")
sub3 <- powerdata %>% select(dt, sub=Sub_metering_3) %>% mutate(sub_type="sub3")
subs <- rbind(sub1, sub2, sub3)
with(subs, plot(dt, sub, type="n", ylab="Energy sub metering", xlab=NA))
with(sub1, lines(dt, sub, col="black"))
with(sub2, lines(dt, sub, col="red"))
with(sub3, lines(dt, sub, col="blue"))
legend("topright", pch="_", col=c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()
