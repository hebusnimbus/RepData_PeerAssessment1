# ===============================================================================
# CONFIGURATION
# ===============================================================================


# source("E:/Education/Coursera - Data Science 05 - Reproducible Research/04. Peer assessments/01. Assessment 1/RepData_PeerAssessment1/work/analysis.R")


# ----------  Source utils script  ----------

script.dir <- dirname(sys.frame(1)$ofile)

source(file.path(script.dir, 'utils.R'))


# ----------  Libraries  ----------

install_package("plyr");    library(plyr)
install_package("dplyr");   library(dplyr)
install_package("knitr");   library(knitr)
install_package("lattice"); library(lattice)


# ----------  Constants & variables  ----------

data.url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'

dir.data <- create_dir(file.path(script.dir, 'data'))

path.zip  <- download_file(data.url, dir.data)
file.data <- 'activity.csv'
path.data <- file.path(dir.data, file.data)


# ===============================================================================
# MAIN
# ===============================================================================

# ----------  Extract data  ----------

unzip(
    zipfile = path.zip,
    files   = c(file.data),
    exdir   = dir.data
)


# ----------  Load data  ----------

data.raw <- read.csv(path.data, header=TRUE);
summary(data.raw);

#to remove each 'NA' from a vector:
#vx = vx[!is.na(a)]
#
#to remove each 'NA' from a vector and replace it w/ a '0':
#ifelse(is.na(vx), 0, vx)
#
#to remove entire each row that contains 'NA' from a data frame:
#dfx = dfx[complete.cases(dfx),]


# ----------  Calculate & plot totals per day  ----------

data <- ddply(data.raw, .(date), summarize, steps=sum(steps))
#data <- aggregate(steps ~ date, data=data.raw, sum)
#data <- aggregate(steps ~ date, data=data.raw, mean)

hist(
    x      = data$steps,
    breaks = 10,
    col    = "lightgreen",
    main   = "Total number of steps taken each day",
    xlab   = "Number of steps"
)


# ----------  Calculate & plot totals per day (density)  ----------

if ( FALSE ) {
    # Replace NA values with 0
    data[is.na(data)] <- 0

    hist(
        x      = data$steps,
        breaks = 20,
        freq = FALSE,
        col    = "lightgreen",
        main   = "Total number of steps taken each day",
        xlab   = "Number of steps"
    )

    # Add normal distribution curve
    # http://www.r-bloggers.com/basics-of-histograms/
    curve(
        dnorm(x, mean=mean(data$steps), sd=sd(data$steps)),
        add = TRUE,
        col = "darkblue",
        lwd = 2
    )
}


# ----------  Calculate & plot average daily activity pattern  ----------

data <- ddply(data.raw, .(interval), summarize, steps=mean(steps, na.rm=TRUE))
#data <- aggregate(steps ~ interval, data=data.raw, mean)

plot(
    x    = data$interval,
    y    = data$steps,
    type = 'l',
    main = 'Average daily activity pattern',
    xlab = 'Interval',
    ylab = 'Average number of steps'
)

num_na     <- sum(is.na(data.raw$steps))
percent_na <- num_na / dim(data.raw)[1] * 100


# ----------  Filling missing values  ----------

data.clean      <- data.raw
data.clean$mean <- ave(data.clean$steps, data.clean$interval, FUN=function(x) mean(x, na.rm=T))

data.clean[is.na(data.clean$steps),]$steps <- data.clean[is.na(data.clean$steps),]$mean

data.clean$mean <- NULL

#data.raw[ , !names(data.raw) %in% c("mean")]


# ----------  New histogram and mean/median  ----------

data <- ddply(data.clean, .(date), summarize, steps=sum(steps))

hist(
    x      = data$steps,
    breaks = 10,
    col    = "lightgreen",
    main   = "Total number of steps taken each day",
    xlab   = "Number of steps"
)


# ----------  Weekday vs weekend  ----------

data.clean$dayOfWeek <- weekdays(as.Date(data.clean$date))
data.clean$typeOfDay <- "weekday"
data.clean[weekdays(as.Date(data.clean$date))=="Saturday",]$typeOfDay <- "weekend"
data.clean[weekdays(as.Date(data.clean$date))=="Sunday",]$typeOfDay   <- "weekend"

#data.clean$average <- ave(data.clean$steps, data.clean$interval, data.clean$typeOfDay, FUN=mean)
#
#xyplot(
#    average ~ interval | typeOfDay,
#    data   = data.clean,
#    layout = c(1, 2),
#    type = 'l'
#)

data <- ddply(data.clean, .(interval, typeOfDay), summarize, average=mean(steps))

xyplot(
    average ~ interval | typeOfDay,
    data   = data,
    layout = c(1, 2),
    type   = 'l',
    main   = 'Average daily activity pattern',
    xlab   = 'Interval',
    ylab   = 'Average number of steps'
)

#> unique(data.raw[weekdays(as.Date(data.raw$date))=="Saturday",]$date)
#[1] 2012-10-06 2012-10-13 2012-10-20 2012-10-27 2012-11-03 2012-11-10 2012-11-17
#[8] 2012-11-24
#61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 ... 2012-11-30
#> unique(data.raw[weekdays(as.Date(data.raw$date))=="Sunday",]$date)
#[1] 2012-10-07 2012-10-14 2012-10-21 2012-10-28 2012-11-04 2012-11-11 2012-11-18
#[8] 2012-11-25



# ----------  Generate documentation  ----------

setwd("E:/Education/Coursera - Data Science 05 - Reproducible Research/04. Peer assessments/01. Assessment 1/RepData_PeerAssessment1/")
knit2html('PA1_template.Rmd')
#browseURL('PA1_template.html')


# ===============================================================================
