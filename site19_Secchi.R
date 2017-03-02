# Is there a trend in Secchi depth?
# Eric Leibensperger (eleib003@plattsburgh.edu)
# Original date unknown, branded on March 2, 2017

rm(list=ls())
library(lubridate)
library(mblm)

do_Graph=T

dir <- '/Users/eleib003/Google\ Drive/Lake.Champlain/'
file<-paste(dir,'19_Secchi_92to15.csv',sep='')
data<-read.table(file=file,header=T,sep=',')

data$VisitDate<-as.Date(data$VisitDate,format='%m/%d/%y')

data<-data.frame(data,Year=year(data$VisitDate),Month=month(data$VisitDate))

# Find the minimum, maximum, and number of years in database
minYr<- min(data$Year)
maxYr<- max(data$Year)
nYr  <- maxYr - minYr + 1
yrs<-minYr:maxYr # Create vector of years

# Create array to hold all of the data
ts<-array(NA,dim=c(13,nYr))

# Loop through years extracting and averaging data of this year, month, and depth
for (y in minYr:maxYr){
  yy<-y-minYr+1 # index to store data
  # Loop through the months
  for (m in 1:12){
    ts[m,yy]<-mean(data$Result[data$Year==y & data$Month==m])
  }
  # Create summer mean, only if all three months have valid entries
  summer<-ts[5:8,yy]
  if(length(summer[!is.na(summer)]) != 4) {} else {ts[13,yy]<-mean(summer)}
}

# Calculate simple linear regression and Theil-Sen trends
# Store in a data frame
trends <- data.frame(lm.slope=rep(NA,13),lm.int=rep(NA,13),lm.pvalue=rep(NA,13),
                     ts.slope=rep(NA,13),ts.int=rep(NA,13),ts.pvalue=rep(NA,13),
                     nYr=rep(NA,13))
row.names(trends)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','JJA')
# Loop through each month, calculating a trend for each
for (m in 1:13){
  # Find where the data is good
  okTs<-!is.na(ts[m,])&yrs>=1999
  
  nTs <- length(ts[m,okTs]) # how many good data points?
  trends$nYr[m] <- nTs # Store in the dataframe for reference
  
  # If we have at least three points, calculate a trend
  if(nTs > 3){
    # extract the relevant data for the trend estimation
    subTs<-ts[m,okTs]
    subYr<-yrs[okTs]
    # Use lm() for a simple linear regression; store the summary statistics
    output<-lm(subTs~subYr); sum.output<-summary(output)
    # Archive interesting data (slope, intercept, slope p-value)
    trends$lm.slope[m]<-output$coefficients[2]
    trends$lm.int[m]<-output$coefficients[1]
    trends$lm.pvalue[m]<-coef(sum.output)[2,4]
    
    # Use mblm() to calculate the Theil-Sen estimator; store the summary statistics
    output<-mblm(subTs~subYr); sum.output<-summary(output)
    # Archive interesting data (slope, intercept, slope p-value)
    trends$ts.slope[m]<-output$coefficients[2]
    trends$ts.int[m]<-output$coefficients[1]
    trends$ts.pvalue[m]<-coef(sum.output)[2,4]
  }
}

# Write out table of values
write.table(trends,file=paste(dir,'site19_Trends_Secchi.table',sep=''),sep='\t')

# Make a graph
if(do_Graph){
  m<-6
  df<-data.frame(yrs=yrs,summer=ts[13,],aug=ts[m,],
                 modAugT=yrs*trends$lm.slope[m]+trends$lm.int[m],
                 modSumT=yrs*trends$lm.slope[13]+trends$lm.int[13],
                 modTSAugT=yrs*trends$ts.slope[m]+trends$ts.int[m],
                 modTSSumT=yrs*trends$ts.slope[13]+trends$ts.int[13])
  
  
  p1<- ggplot(data=df)+
    geom_line(aes(x=yrs,y=summer),color='firebrick1')+geom_line(aes(x=yrs,y=modSumT),color='red')+geom_line(aes(x=yrs,y=modTSSumT),color='red',linetype='dashed')+
    geom_line(aes(x=yrs,y=aug),color='green')  + geom_line(aes(x=yrs,y=modAugT),color='cyan')+geom_line(aes(x=yrs,y=modTSAugT),color='cyan',linetype='dashed')+
    xlab('Year')+ylab('Secchi Depth (m)')+ggtitle('L. Champlain Long-term Secchi Trends')+
    scale_x_continuous(breaks=c(1995,2000,2005,2010,2015))+
    scale_y_continuous(limits=c(0,10),breaks=scales::pretty_breaks(n=9))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.25),
          panel.grid.minor = element_blank(), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=18),plot.title=element_text(size=24),
          axis.title=element_text(size=18,face="bold"))   
  p1
}
