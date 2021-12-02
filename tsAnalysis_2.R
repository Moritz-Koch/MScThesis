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
library("dplyr")

print("packages loaded...")  

wd <- setwd(dirname(getActiveDocumentContext()$path))           #set working directory to source file location
print("set working directory to:")
#working directory
print(wd)

###############################################################################################################
print("files in wd:")

fileList <- list.files(path=wd, pattern=".csv", full.names=FALSE) # creating list of all .csv files in folder


print(fileList[7])

testDf<- as.data.frame(read.csv(fileList[7]))                     # writing out a single file as dataframe

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


# to do:

################## Na interpolation ###################
library(zoo)
# use test.df.sub here it has no date column
# Construct linear model based on non-NA pairs
# i am using na.approx now which does linear interpolation 
# the first loop interpolates only variables in the first row
# the 2nd loop interpolates all variables in a column 

ncol(testDf.sub)

View(testDf.sub[1,])



if (is.na(testDf.sub[1,1]) == TRUE) {
    testDf.sub[1,1] <- testDf.sub[1,2]
  }


i=1

while (i <= ncol(testDf.sub)) {
  
  if(i == 1){
    
    if (is.na(testDf.sub[1,1]) == TRUE) {
      testDf.sub[1,1] <- testDf.sub[1,2]
    }
    
    testDf.sub[1,] <- na.approx(testDf.sub[1,])
    
  } else {
    testDf.sub[,i] <- na.approx(testDf.sub[,i])
  }
  i <- i+1 
}

################### BASE R INTERPOLATION - REMOVES ALL NA'S ######################
# Linear interpolation function handling ties,
# returns interpolated vector the same length 
# a the input vector: -> vector
l_interp_vec <- function(na_vec){
  approx(x = na_vec, method = "linear", ties = "constant", n = length(na_vec))$y
}


vec.sub <- data.frame(newcol = c(t(testDf.sub)))
vec.sub.interp <- l_interp_vec(vec.sub)

# Applied to a dataframe, replacing NA values
# in each of the numeric vectors,
# with interpolated values.
# input is dataframe: -> dataframe()
interped_df_2 <- function(df){
  data.frame(lapply(df, function(x) {
    if (is.numeric(x)) {
      # Store a scalar of min row where x isn't NA: -> min_non_na
      min_non_na <- min(which(!(is.na(x))))
      # Store a scalar of max row where x isn't NA: -> max_non_na
      max_non_na <- max(which(!(is.na(x))))
      # Store scalar of the number of rows needed to impute prior
      # to first NA value: -> ru_lower
      ru_lower <- ifelse(min_non_na > 1, min_non_na - 1, min_non_na)
      # Store scalar of the number of rows needed to impute after
      # the last non-NA value: -> ru_upper
      ru_upper <- ifelse(max_non_na == length(x),
                         length(x) - 1,
                         (length(x) - (max_non_na + 1)))
      
      # Store a vector of the ramp to function: -> l_ramp_up:
      ramp_up <-
        as.numeric(cumsum(rep(x[min_non_na] / (min_non_na), ru_lower)))
      
      # Apply the interpolation function on vector "x": -> y
      y <-
        as.numeric(l_interp_vec(as.numeric(x[min_non_na:max_non_na])))
      
      # Create a vector that combines the ramp_up vector
      # and y if the first NA is at row 1: -> z
      if (length(ramp_up) > 1 & max_non_na != length(x)) {
        # Create a vector interpolations if there are
        # multiple NA values after the last value: -> lower_l_int
        lower_l_int <-
          as.numeric(cumsum(rep(mean(diff(
            c(ramp_up, y)
          )),
          ru_upper + 1)) +
            as.numeric(x[max_non_na]))
        
        # Store the linear interpolations in  a vector: -> z
        z <- as.numeric(c(ramp_up, y, lower_l_int))
        
      } else if (length(ramp_up) > 1 & max_non_na == length(x)) {
        # Store the linear interpolations in  a vector: -> z
        z <- as.numeric(c(ramp_up, y))
        
      } else if (min_non_na == 1 & max_non_na != length(x)) {
        # Create a vector interpolations if there are
        # multiple NA values after the last value: -> lower_l_int
        lower_l_int <-
          as.numeric(cumsum(rep(mean(diff(
            c(ramp_up, y)
          )),
          ru_upper + 1)) +
            as.numeric(x[max_non_na]))
        
        # Store the linear interpolations in  a vector: -> z
        z <- as.numeric(c(y, lower_l_int))
        
      } else{
        # Store the linear interpolations in  a vector: -> z
        z <- as.numeric(y)
        
      }
      
      # Interpolate between points in x, return new x:
      return(as.numeric(ifelse(is.na(x), z, x)))
      
    } else{
      x
      
    }
    
  }))}

#df_list_interped_extrapped <- lapply(df_list, interped_df #i dont need this now 

# Subset interped df to only contain
# the time values in C, store a data frame: -> int_df_subset




############### end %-thresh NA removal ###############




g <- ts(testDf.dateC[,55], start = c(2014,10), end = c(2020,11), frequency = 12)
f <- ts(testDf.dateC[,3], frequency = 12)
plot(g)
plot(f)

vec <- data.frame(newcol = c(t(testDf.sub)), stringsAsFactors=FALSE)
vec <- as.numeric(vec)

ID <- as.numeric(seq(1, 9842, 1))

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


vec.ts <- ts(vec, frequency = 133)

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

vec.ts.noNa.window <- window(vec.ts.noNa, start=0, end=1000)
plot(vec.ts.noNa.window)


vec.ts.noNa %>% changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot()


vec.ts.noNa.tso <- tsoutliers::tso(vec.ts.noNa)
plot(vec.ts.noNa.tso)

vec.ts.noNa.tso <- tsoutliers::tso(vec.ts.noNa ,maxit.iloop=10, maxit.oloop = 5)




RGI60.14.07176.tso <- vec.ts.noNa.tso



plot(tso(vec.ts.noNa, discard.method = "bottom-up"))

######################################## testing tso and changepoint detection
set.seed(10000000)
sampleVec <- c(sample(1:1000000, 1000, replace=FALSE))
sampleVec.sort <- sort(sampleVec)

### building a random timeseries
normal <- c(sample(500:2000, 200, replace=FALSE))
surge <- c(sample(4000:10000, 20, replace=FALSE))
quiet <- c(sample(10:100, 20, replace=FALSE))
quiet_2 <- c(sample(200:400, 40, replace=FALSE))


cycle <- c(normal, surge/2, surge, quiet, normal)

cycle_2 <- c(normal, surge/2, surge, normal)

cycle_3 <- c(normal,normal, normal, normal, surge )

cycle %>%  
  as.numeric() %>%
  ts() %>%
  autoplot()

cycle_2 %>%  
  as.numeric() %>%
  ts() %>%
  autoplot()

cycle_3 %>%  
  as.numeric() %>%
  ts() %>%
  autoplot()



cycle2.ts %>% changepoint::cpt.meanvar() %>%
  autoplot()

# TC = temporary change
# LS = level shift
# AO = additive outlier
# IO = innovative outliers
# SLS = seasonal level shifts

cycle.outlier <- tsoutliers::tso(cycle.ts,types = c("LS","TC","AO"),maxit.iloop=10)

plot(cycle.outlier)

cycle2.outlier <- tsoutliers::tso(cycle2.ts,types = c("LS","TC","AO"),maxit.iloop=10)

plot(cycle2.outlier)

discard.outliers(cycle2.outlier.removed ,cycle2.outlier, method = "en-masse")

plot(tso(cycle.ts, types = c("TC")))



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

######################################################## this is for increase or chaos tests ################################
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

################################## works with 500 variables ##################################################################

