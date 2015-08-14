library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(lubridate)
library(data.table)
library(xts)
library(shinydashboard)
library(scales)
library(devtools)
library(dygraphs)
# library(XLConnect)
library(readxl)

### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

# install missing packages.
list.of.packages <- c("shiny","ggplot2","reshape2","plyr","lubridate","data.table","dygraphs","xts","devtools","shinydashboard","scales",'dygraphs','readxl')
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

#global functions
alt.diff <- function (x, n = 1, na.pad = TRUE) {
  NAs <- NULL
  if (na.pad) {NAs <- rep(NA, n)}
  diffs <- c(NAs, diff(x, n))
}

round.minutes <- function(x, noOfMinutes=5){
	tz <- tz(x[1])
	class <- class(x[1])
	structure((noOfMinutes*60) * (as.numeric(x + (noOfMinutes*60*0.5)) %/% (noOfMinutes*60)), class=class,tz=tz)
}

read.sum <- function(file, fname,tzone="GMT"){
	fileCheck <- file.info(file)$size>0
	if(fileCheck){
	sums <- fread(file)
	names(sums)[1:3]  <- c('datetime','temp','serial')
	sums[,datetime:=ymd_hms(datetime, tz="Africa/Accra")]
 
	}else{warning(paste("File", file, "does not contain valid iButton data", sep=" "))}
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#create the data

files <- list.files("~/Dropbox/Ghana_adoption_data_SHARED/serverTest/archive", full.names=T, recursive=T)
files <- grep('attributes', files, value=T, invert=T)
all <- lapply(files, fread)
all <- do.call(rbind, all)
all[,device_id:=substring(serial, nchar(serial)-7, nchar(serial))]

#data prep
log.sheet <- read_excel('~/Dropbox/Ghana_adoption_data_SHARED/Protocols/Data_sheet/version_0.xlsx')[1:79,]
log.sheet <- as.data.table(log.sheet)
setnames(log.sheet, '1', 'device_id')
log.sheet <- log.sheet[,c('device_id','Maternal ID','Stove location','BUFFER OPTIONS','LOCATION OPTION'), with=F]
setnames(log.sheet, c('device_id','mid','stove_loc','buffer','loc_option'))
log.sheet

all <- merge(all, log.sheet, by='device_id', all.x=T)
all[,datetime:=ymd_hms(datetime)]

all[,stove_loc:=gsub(" ","", stove_loc)]

setkey(all)
all <- unique(all)