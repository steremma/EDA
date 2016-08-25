# This script will construct 4 plots needed to explore the power consumption trends 
# observed during the first 2 days of February 2007.

# Assuming that the input file lies at the project's root 
input <- "power_consumption.txt"

# Read all data, less than 1Gb of memory required
raw <- read.table(input, sep = ";", header = T)

# Convert the date column
raw$Date = as.Date(raw$Date, "%d/%m/%Y")

# Subset the data i am interested in and remove tha old object for space efficiency
data  <- raw[raw$Date == "2007-2-1" | raw$Date == "2007-2-2", ]
remove(raw)

# Assign descriptive and shorter column names. Note to self:
# Sub_metering_1 -> Kitchen
# Sub_metering_2 -> Laundry
# Sub_metering_3 -> Heat
names(data) <- c("Date", "Time", "Watt", "VAR", "V", "I", "Kitchen", "Laundry", "Heat")

# Drop previous row indexing
rownames(data) <- seq(length=nrow(data))

# Transform numeric data from factor to number
cols = c(3, 4, 5, 6, 7, 8, 9);    
data[ , cols] = apply(data[ , cols], 2, function(x) as.numeric(as.character(x)))

# Create the figures directory if it does not already exist
dir.create("figures", showWarnings = False)

# Plot 1 - Histogram of global active power
png(filename = "figures/plot_1.png")
hist(data$Watt, col = "orange", 
     xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.off()

# Create a custom minute-counting sequence to serve as the x axis in my plots
start <- 1
minPerDay <- 24 * 60
stop  <- 2 * minPerDay
time  <- start:stop

# Plot 2 - Plotting the active power consumption over time
timePlot <- function(colName, ylab, xlab) {
  plot(time, data[[colName]], type = "l",
       xaxt = "n",
       ylab = ylab,
       xlab = xlab)
  axis(1, at= c(start, start + minPerDay, stop), labels = c("Thu", "Fri", "Sat"))
}
png(filename = "figures/plot_2.png")
timePlot("Watt", "Global Active Power (kilowatts)", "")
dev.off()

# Plot 3 - Showing power consumption trends in each room over time
subMeteringPlot <- function() {
  plot(time, data$Kitchen, type = "l",
       xaxt = "n",
       ylab = "Energy sub metering",
       xlab = "")
  axis(1, at= c(start, start + minPerDay, stop), labels = c("Thu", "Fri", "Sat"))
  lines(time, data$Laundry, col = "red")
  lines(time, data$Heat, col = "blue")
  legend("topright", c("Sub metering 1", "Sub metering 2", "Sub metering 3"),
         lty=c(1,1),
         col = c("black","red","blue"))
}
png(filename = "figures/plot_3.png", width = 600, height = 600)
subMeteringPlot()
dev.off()

# Plot 4 - Plotting all electric quantities over mine in separate panels
png(filename = "figures/plot_4.png")
par(mfrow = c(2, 2))
timePlot("Watt", "Global Active Power", "")
timePlot("V", "Voltage", "datetime")
subMeteringPlot()
timePlot("VAR", "Global Reactive Power", "datetime")
dev.off()
