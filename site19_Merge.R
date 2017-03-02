# Combine all Temp. observations of Main Lake - Site 19
# Smeltzer et al. 2012 database includes data not included in the sonde and Chem
# the LTM databases. Add these together to make on master table.
#
# Also add on the 2016 data from Luke and Pete
#
# Eric Leibensperger - Jan. 11, 2017
# eleib003@plattsburgh.edu
####################################
library(chron) # for month.day.year
dir <- '/Users/eleib003/Google\ Drive/Lake.Champlain/'

# LTM Sonde database
LTM.file <- paste(dir,'19_temp.table',sep='')
LTM <- read.table(LTM.file,sep='\t')
colnames(LTM)<-c('ID','Site','Date','Year','Team','Depth','Variable','Temp')
LTM$Date <- as.Date(LTM$Date,format="%m/%d/%y")
LTM <- subset(LTM,!duplicated(subset(LTM,select = c(Date,Depth))))

# LTM Chem database
Chem.File <- paste(dir,'19_temp_chem.table',sep='')
Chem <- read.table(Chem.File,sep=',',header=TRUE)
Chem$VisitDate <- as.Date(Chem$VisitDate,format="%m/%d/%y")
# Subset to extract just temperature
Chem <- Chem[Chem$Test == 'Temperature',]

# 2016 Data from Pete
Hyd.File <- paste(dir,'2016\ Hydrolab\ VT\ noUnits.csv',sep='')
Hyd <- read.table(Hyd.File,sep=',',header=TRUE)
Hyd <- Hyd[2:dim(Hyd)[1],] # Remove the unit line
Hyd <- Hyd[Hyd$Station==19,] # Pull out only Station 19
Hyd$Date <- as.Date(Hyd$Date,format="%m/%d/%y")

# 2016 NY Data
NY.File <- paste(dir,'NY2016LTMProfiles.csv',sep='')
NY <- read.table(NY.File,sep=',',header=TRUE)
NY$DATE<-as.Date(NY$DATE,format="%m/%d/%y")
NY <- NY[NY$Station==19,]

# Remove Chem data if sonde data also collected on that day
uniqueDates<-unique(LTM$Date)
for (d in 1:length(uniqueDates)){
  Chem<-Chem[Chem$VisitDate != uniqueDates[d],]
}
uniqueDates<-unique(NY$DATE)
for (d in 1:length(uniqueDates)){
  Hyd<-Hyd[Hyd$Date != d,]
}

# Merge records together:
# Date, Depth, Temperature
allData <- data.frame(Date=c(LTM$Date,Chem$VisitDate,Hyd$Date,NY$DATE),
                      Month=c(month.day.year(LTM$Date)$month,month.day.year(Chem$VisitDate)$month,month.day.year(Hyd$Date)$month,month.day.year(NY$DATE)$month),
                      Year=c(month.day.year(LTM$Date)$year,month.day.year(Chem$VisitDate)$year,month.day.year(Hyd$Date)$year,month.day.year(NY$DATE)$year),
                      Depth=c(LTM$Depth,Chem$Depth,Hyd$Dep100,NY$Depth),
                      Temp=c(LTM$Temp,Chem$Result,Hyd$Temp,NY$TEMP))

# Sort by date
allDataSorted <- allData[order(allData$Date),]

# Records are duplicated in some of the databases, so remove them
allDataSortedDupRemoved <- allDataSorted[!duplicated(allDataSorted),]

# Write out new table file
file.out<-paste(dir,'19_temp_table_all_92to16.csv',sep='')
write.table(allDataSortedDupRemoved,file=file.out,row.names=FALSE)
