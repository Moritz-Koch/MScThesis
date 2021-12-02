### time series analysis 
# mk 

###### load packages
if("tsoutliers" %in% rownames(installed.packages()) == FALSE) {install.packages("tsoutliers")} else {print("packages loaded...")}

library("tsoutliers")



print("all necessary packages for ts analysis loaded... ")
###### working directory
print("set working directory to:")

# wd<-setwd("/media/vetinari/data2/Moritz/retreat/MonatsMosaike/")
wd <- setwd("C:/Users/morit/OneDrive/Desktop/Master_Arbeit/RStudio_stuff")
print("curent working directory:")
print(wd)

##### listing all result files saved as csv to work with 
print("files in wd:")
lf <- list.files(path=wd, pattern=".csv", full.names=FALSE)

#####


##### load files
for (file in lf){
  cl <- read.csv(file=lf)                     # read csv from list
  cl.df   <- as.data.frame(cl)                # convert list to dataframe
  cl.df.short <- subset( cl.df, select = -X ) # remove the first column of the dataframe (named "X")
  
  }

cl.ts <- ts(cl.df.short, frequency=1)
data.ts.outliers <- tso(cl.ts)
plot(cl.ts)


