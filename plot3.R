#0..Set working directory and load libraries:

library(dplyr)    #to have access to select(), rename(), arrange(), summarize() and group_by() functions
library(tidyr)    #to have access to gather(), separate(), bind_rows and bind_cols() functions
library(lubridate)  #to play with dates and times

#1..Load the data:

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
folder <- paste0(getwd(), "/", "Dataset.zip")
download.file(url, folder)
#Get an idea of what contains the given unzipped folder by displaying a table of its content:
dataset_folder <- unzip(folder, list = TRUE)
#View(dataset_folder)
#Extract content (by default in the working directory) of the Dataset.zip folder previously created:
unzip(folder)
#The resulting file is "household_power_consumption.txt".

#2..Prepare the data for analysis:

#Put the data into a variable called "dataset":
dataset <- tbl_df(read.csv("household_power_consumption.txt", 
                           header = T, 
                           sep = ';',
                           na.strings = "?", 
                           nrows = 2075259, 
                           check.names = F, 
                           stringsAsFactors = F, 
                           comment.char = "", 
                           quote = '\"'))
#View(dataset)

#Change Date and Time variables into associated formats:
dataset_2 <- mutate(dataset, Date = as.Date(Date, format = "%d/%m/%Y"))
dataset_2 <- mutate(dataset_2, Time = strftime(strptime(Time, format = "%H:%M:%S"), format = "%H:%M:%S"))
dataset_2 <- mutate(dataset_2, Global_active_power = as.numeric(as.character(Global_active_power)))
dataset_2 <- mutate(dataset_2, Global_reactive_power = as.numeric(as.character(Global_reactive_power)))
#View(dataset_2)

#Keep only the two dates we are interested in:
dataset_3 <- filter(dataset_2, year(Date) == 2007 & month(Date) == 2 & (day(Date) == 1 | day(Date) == 2))
#View(dataset_3)

#Create a new date&time together variable:
datetime <- paste(dataset_3$Date, dataset_3$Time)
dataset_4 <- mutate(dataset_3, Datetime = as.POSIXct(datetime))
#View(dataset_4)

#3..Draw the 3rd plot:

with(dataset_4, 
     {plot(as.numeric(Sub_metering_1) ~ Datetime, 
           type = "l",
           xlab = "",
           ylab = "Global Active Power (kilowatts)")
       lines(as.numeric(Sub_metering_2) ~ Datetime, col = 'Red')
       lines(as.numeric(Sub_metering_3) ~ Datetime, col = 'Blue')}
)

legend("topright", 
       col = c("black", "red", "blue"), 
       lty = 1, 
       lwd = 2, 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
)

dev.copy(png,'plot3.png')
dev.off()
