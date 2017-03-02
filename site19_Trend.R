# Calculate long-term trend at Site 19
# Eric Leibensperger
# eleib003@plattsburgh.edu
# original date unknown; branded as March 2, 2017
###########################
library(mblm)
do_Graph=T

#
# Read the merged T data file
dir <- '/Users/eleib003/Google\ Drive/Lake.Champlain/'
file<-paste(dir,'19_temp_table_all_92to16.csv',sep='')
data<-read.table(file=file,header=T)

# Find the minimum, maximum, and number of years in database
minYr<- min(data$Year)
maxYr<- max(data$Year)
nYr  <- maxYr - minYr + 1
yrs<-minYr:maxYr # Create vector of years

# Depths of interest:
Ds<-c(1,2,5,10,15,20,25,30,40,50,60,70,80,90)
nDs <- length(Ds) # number of depths to analyze
# Create array to hold all of the data
ts<-array(NA,dim=c(nDs,13,nYr))
nnTs<-array(NA,dim=c(nDs,12,nYr))
doyTs<-array(NA,dim=c(nDs,12,nYr))
ms.SLR<-array(NA,dim=c(nDs,13))
ms.SLR.range<-array(NA,dim=c(nDs,13,2))
ms.TS<-ms.SLR
for (dd in 1:length(Ds)){
#d<-1 # m
d<-Ds[dd] # do this depth
#d<-10
# Loop through years extracting and averaging data of this year, month, and depth
for (y in minYr:maxYr){
  yy<-y-minYr+1 # index to store data
  # Loop through the months
  for (m in 1:12){
    sub<-data$Temp[data$Year==y & data$Month == m & data$Depth == d]
    nnTs[dd,m,yy]<-length(sub)
    if(length(sub) >= 1) {doyTs[dd,m,yy]<-mean(as.numeric(strftime(data$Date[data$Year==y & data$Month==m & data$Depth==d],'%j')))}
    ts[dd,m,yy]<-mean(data$Temp[data$Year==y & data$Month==m & data$Depth==d])
  }
  # Create summer mean, only if all three months have valid entries
  summer<-ts[dd,6:8,yy]
  if(length(summer[!is.na(summer)]) != 3) {} else {ts[dd,13,yy]<-mean(summer)}
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
  okTs<-!is.na(ts[dd,m,])
  nTs <- length(ts[dd,m,okTs]) # how many good data points?
  trends$nYr[m] <- nTs # Store in the dataframe for reference
  
  # If we have at least three points, calculate a trend
  if(nTs > 3){
    # extract the relevant data for the trend estimation
    subTs<-ts[dd,m,okTs]
    subYr<-yrs[okTs]
    # Use lm() for a simple linear regression; store the summary statistics
    output<-lm(subTs~subYr); sum.output<-summary(output)
    # Archive interesting data (slope, intercept, slope p-value)
    trends$lm.slope[m]<-output$coefficients[2]
    ms.SLR[dd,m]<-output$coefficients[2]
    ms.SLR.range[dd,m,]<-confint(output)[2,]
    trends$lm.int[m]<-output$coefficients[1]
    trends$lm.pvalue[m]<-coef(sum.output)[2,4]
    
    # Use mblm() to calculate the Theil-Sen estimator; store the summary statistics
    output<-mblm(subTs~subYr); sum.output<-summary(output)
    # Archive interesting data (slope, intercept, slope p-value)
    trends$ts.slope[m]<-output$coefficients[2]
    ms.TS[dd,m]<-output$coefficients[2]
    trends$ts.int[m]<-output$coefficients[1]
    trends$ts.pvalue[m]<-coef(sum.output)[2,4]
  }
}

# Write out table of values
write.table(trends,file=paste(dir,'site19_Trends_',as.character(d),'m.table',sep=''))

# Make a graph
if(do_Graph){
m<-5
df<-data.frame(yrs=yrs,summer=ts[dd,13,],aug=ts[dd,m,],
               modAugT=yrs*trends$lm.slope[m]+trends$lm.int[m],
               modSumT=yrs*trends$lm.slope[13]+trends$lm.int[13],
               modTSAugT=yrs*trends$ts.slope[m]+trends$ts.int[m],
               modTSSumT=yrs*trends$ts.slope[13]+trends$ts.int[13])


p1<- ggplot(data=df)+
  geom_line(aes(x=yrs,y=summer),color='firebrick1')+geom_line(aes(x=yrs,y=modSumT),color='red')+geom_line(aes(x=yrs,y=modTSSumT),color='red',linetype='dashed')+
  geom_line(aes(x=yrs,y=aug),color='green')  + geom_line(aes(x=yrs,y=modAugT),color='cyan')+geom_line(aes(x=yrs,y=modTSAugT),color='cyan',linetype='dashed')+
  xlab('Year')+ylab('Temperature (C)')+ggtitle('L. Champlain Long-term Temp. Trends')+
  scale_x_continuous(breaks=1991:2017,
                     labels=c(rep("",4),1995,rep("",4),2000,rep("",4),2005,rep("",4),2010,rep("",4),2015,"",""))+
  #scale_y_continuous(limits=c(16,24),breaks=scales::pretty_breaks(n=9))+
  theme(panel.grid.major = element_line(colour = "grey",size=0.25),
        panel.grid.minor = element_blank(), panel.border=element_rect(color="black",fill=NA),
        panel.background = element_rect(fill = "white"),
        axis.text=element_text(size=18),plot.title=element_text(size=24),
        axis.title=element_text(size=18,face="bold"))   
p1
}

#### WRITE OUT DATA
doIt<-F
if(doIt){
  YEAR<-1992:2016
  all<-data.frame(YEAR=YEAR)
  all<-data.frame(all,JAN=ts[1,],FEB=ts[2,],MAR=ts[3,],APR=ts[4,],MAY=ts[5,],JUN=ts[6,],
                  JUL=ts[7,],AUG=ts[8,],SEP=ts[9,],OCT=ts[10,],NOV=ts[11,],DEC=ts[12,],JJA=ts[13,])
  
  write.table(all,file=paste(dir,'site19_data_',as.character(d),'m.table',sep=''),sep=',',row.names=F)
}
}

doIt<-F
m<-5
if(doIt){
  m<-8
  plot(ms.SLR[,m],Ds,ylim=c(100,0),xlim=c(-.15,0.15),col='red',typ='b')
  lines(c(0,0),c(-200,200))

  lines(ms.SLR.range[,m,1],Ds,lty=2,col='red')
  lines(ms.SLR.range[,m,2],Ds,lty=2,col='red')
  
}
