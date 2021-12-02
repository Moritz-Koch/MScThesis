### time series analysis
# mk
###### wd
if("tsoutliers" %in% rownames(installed.packages()) == FALSE) {install.packages("tsoutliers")}
if("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}

print("missing packages installed ...")

library("ggplot2")
library("rstudioapi")                                 # Load rstudioapi package
library("tsoutliers")  
library("lubridate")

print("packages loaded...")  

#?ts

wd <- setwd(dirname(getActiveDocumentContext()$path))           #set working directory to source file location
print("set working directory to:")
#working directory
print(wd)

#####
print("files in wd:")

fileList <- list.files(path=wd, pattern=".csv", full.names=FALSE) # creating list of all .csv files in folder

print(fileList[9])

testDf<- as.data.frame(read.csv(fileList[9]))                     # writing out a single file as dataframe

names(testDf)[names(testDf) == 'X'] <- 'Date'                     # replace "X" in first column header to "Date"

# vector of timesteps extracted
dates <- as.vector(testDf[,1])

#empty vector that is beeing filled in the loop below
proper.dates <- c()

i=1
for (i in 1:length(dates))
  {
  proper.dates[i] <- paste(dates[i], "01", sep="_")
  i<-i+1
}

# replace as "proper" formated dates
Date <- as.Date(proper.dates, format="%Y_%m_%d")

class(Date)

testDf.sub <- subset(testDf, select=-Date) # drop date in case you need it 

testDf.sub
testDf.dateC <- cbind(Date, testDf.sub)

row.names(testDf) <- Date # rename Column Names with list of dates

class(testDf.dateC$Date)

#?tsoutliers

data(testDf)

g <- ts(testDf.dateC[,55], start = c(2014,10), end = c(2020,11), frequency = 12)
f <- ts(testDf.dateC[,3], frequency = 12)
plot(g)
plot(f)

vec <- data.frame(newcol = c(t(testDf.sub)), stringsAsFactors=FALSE)
vec <- as.numeric(vec)

ID <- as.numeric(seq(1, 9990, 1)) 

vecID<- cbind (ID, vec)
vecID.df <- as.data.frame(vecID)

plot(vecID, type="l")

p <- ggplot(data = vecID.df, aes(x = ID, y = newcol))+
      geom_line(color = "#00AFBB", size = 0.5) 
p 

p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)

vec.ts <- ts(vec)

plot(vec.ts)
#?tso

install.packages(c("ggfortify", "changepoint","strucchange", "ggpmisc"))
#vec.tso <- tso(vec.ts, type="TC")

library(ggfortify)
library(magrittr)               
library(changepoint)
library(ggpmisc)


autoplot(vec.ts)

vec.ts.noNa <- na.omit(vec)

vec.ts.noNa <- ts(vec.ts.noNa)
autoplot(vec.ts.noNa)

#### subset vec.ts.noNa to the timeframe of the surge 

vec.ts.noNa.window <- window(vec.ts.noNa, start=5000, end=10000)
plot(vec.ts.noNa.window)


vec.ts.noNa %>% changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot()


#vec.ts.noNa.window.outlier <- tsoutliers::tso(vec.ts.noNa.window,types = c("TC"),maxit.iloop=5) 

#plot(tso(vec.ts.noNa, discard.method = "bottom-up"))

######################################## testing tso and changepoint detection
set.seed(1000000)
sampleVec <- c(sample(1:1000000, 1000, replace=FALSE))
sampleVec.sort <- sort(sampleVec)

### building a random timeseries
normal <- c(sample(500:2000, 100, replace=FALSE))
surge1 <- c(sample(4000:10000, 20, replace=FALSE))
surge2 <- c(sample(3000:6000,20, replace=FALSE))
surge3 <- c(sample(2000:4000,20, replace=FALSE))
quiet <- c(sample(100:200, 20, replace=FALSE))
quiet_2 <- c(sample(200:500, 20, replace=FALSE))


cycle <- c(normal, normal, surge3, normal, normal, surge3, surge2, surge)

cycle_2 <- c(normal, normal, surge3, surge2, surge, normal)

cycle_3 <- c(surge, surge, normal, normal, normal)

cycle <- as.numeric(cycle) %>%
  ts() %>%
    autoplot()

cycle.n <- as.numeric(cycle) 
cycle.ts <- ts(cycle.n)

cycle_2.n <- as.numeric(cycle_2)
cycle_2.ts <- ts(cycle_2.n)

cycle_2.ts %>% changepoint::cpt.meanvar() %>%
  autoplot()

cycle_3.n <- as.numeric(cycle_3)
cycle_3.ts <- ts(cycle_3.n)

#cycle_3.n
cycle.outlier <- tsoutliers::tso(cycle.ts,types = c("AO","TC", "LS","IO"),maxit.iloop=10, discard.method = c("en-masse", "bottom-up")) 

cycle.outlier_2 <- tsoutliers::tso(cycle_2.ts,types = c("AO","TC", "LS", "IO"),maxit.iloop=10, discard.method = c("en-masse", "bottom-up"))

cycle.outlier_3 <- tsoutliers::tso(cycle_3.ts,types = c("AO","TC", "LS", "IO"), maxit.iloop=10, discard.method = c("en-masse", "bottom-up"))

### HIER GEHTS WEITER 26.07.2021
locate.outliers()


plot.tsoutliers(cycle.outlier)

plot.tsoutliers(cycle.outlier_2)

plot.tsoutliers(cycle.outlier_3)

cycle.outlier$cval
cycle.outlier_2$cval
cycle.outlier_3$cval

cycle.outlier$outliers

drop <- c(cycle.outlier$time)

cycle.outlier.drop <- subset(cycle.outlier, select = -drop)

#cycle.outlier$outliers




#### here i just explain why i have selected a tc filter with a certain value. 
#  if in TC the delta equals to 0 it collapses into an additive outlier 
#  if in TC the delta equals to 1 it basically becomes a level shift 
#  see below:

tc <- rep(0, 50)
tc[20] <- 1
tc1 <- filter(tc, filter = 0, method = "recursive")
tc2 <- filter(tc, filter = 0.3, method = "recursive")
tc3 <- filter(tc, filter = 0.7, method = "recursive")
tc4 <- filter(tc, filter = 1, method = "recursive")
par(mfrow = c(2,2))
plot(tc1, main = "TC delta = 0")
plot(tc2, main = "TC delta = 0.3")
plot(tc3, main = "TC delta = 0.7")
plot(tc4, main = "TC delta = 1", type = "s")


#The temporary change, TC, is a general type of outlier. The equation given in the documentation of the package and that you wrote is the equation that describes the dynamics of this type of outlier. You can generate it by means of the function filter as shown below. It is illuminating to display it for several values of delta. For ??=0 the TC collapses in an additive outlier; on the other extreme, ??=1, the TC is like a level shift.

######################################################## this is for increase or chaos tests
sampleVec.ts <- ts(sampleVec)
sampleVec.sort.ts <- ts(sampleVec.sort)

autoplot(sampleVec.ts)
autoplot(sampleVec.sort.ts)

sampleVec.ts %>% changepoint::cpt.meanvar() %>%
  autoplot()

sampleVec.sort.ts %>% changepoint::cpt.meanvar() %>%
  autoplot()

plot(tso(sampleVec.ts, discard.methid= "bottom-up"))
plot(tso(sampleVec.sort.ts, discard.method = "bottom-up"))  

###################################### works with 500 variables 
