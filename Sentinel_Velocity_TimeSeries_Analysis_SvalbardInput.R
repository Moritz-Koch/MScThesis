# timeseries analysis of sentinel 1 A/B centerline velocity data

# Author: Moritz Koch
# Wrote under v R 4.1

# REQUIRED:

# Rtools needs to be installed in .../Users/Documents/R/win-library/4.1
# It is required to install devtools and R4.0 or newer
#
# change libPaths if default is wrong / run R as admin and then run the following 3 lines:
# myPaths <- .libPaths()   # get the paths
# myPaths <- c(myPaths[2], myPaths[1])  # switch them
# .libPaths(myPaths)  # reassign them


#install.packages("rlang")
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("broom")
#install.packages("anomalize")
#install.packages("rstudioapi")
#install.packages("devtools")
#install.packages("zoo")
#install.packages("tsoutliers")
#install.packages("forecast")
#install.packages("performance", dependencies = TRUE) #careful this is big and needs to be compiled!
#install.packages("ggrepel")
#install.packages("cluster") # optional for numerical cluster analysis // not needed now
#install.packages("klaR") # optional for modal cluster analysis // not needed now
#install.packages("runner")

library("rlang")
library("broom")
library("tidyverse")
library("anomalize")
library("ggplot2")
library("rstudioapi")
library("devtools")
library("zoo")
library("tsoutliers")
library("forecast")
library("performance")
library("ggrepel")
#library("cluster")
#library("klaR")
library("runner")


############## Section 1 // Data Import #######################################################################
# Safe this file in the folder where the data is beeing stored
print("missing packages installed ...")
print("packages loaded...")  

wd <- setwd(dirname(getActiveDocumentContext()$path))           #set working directory to source file location
wd <- setwd(dirname(getActiveDocumentContext()$path))           #sometimes you need to exec. this line twice

setwd(wd)
# svalbard


#create dir for anomaly result .csv files
outputFolder <- "AnomalyMatrix"
dir.create(file.path(wd, outputFolder), showWarnings = FALSE)
outputPath <- paste(wd,"AnomalyMatrix", sep = "/" )

print("files in wd:")
fileList <- list.files(path=wd, pattern=".csv", full.names=FALSE) # creating list of all .csv files in folder
fileList # this should print a .csv file list

### This is for testing the script on a single file // will be automated later (mk)
j = 1

for (k in 1:length(fileList)){
  
  print(fileList[j])
  
  testDf<- as.data.frame(read.csv(fileList[j]))
  
  # writing out a single file as dataframe
  
  names(testDf)[names(testDf) == 'X'] <- 'Date'           # replace "X" in first column header to "Date"
  
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
  row.names(testDf) <- Date                     # rename Column Names with list of dates
  testDf.sub <- subset(testDf, select=-Date)    # drop date in case you need it
  
  testDf.dateC <- cbind(Date, testDf.sub)
  
  ################## Section (2) Linear Interpolation of NA Values in dataframe ###########################
  
  testDf.approx <- na.approx(testDf.sub, rule = 2) ## Linear Interpolating all NA values
  testDf.approx.bycolumn <- t(na.approx(t(testDf.approx))) ## t-transform and extrapolate
  #print(Date)
  
  xdc <- as.data.frame(testDf.approx.bycolumn)
  
  testDF.approx.date <- cbind(Date, xdc)
  
  
  ##### just reformatting header here
  colnames(testDF.approx.date) <- colnames(testDf.dateC)
  
  header <- unlist(colnames(testDF.approx.date))
  
  header <- str_replace(header, "X", "")
  
  colnames(testDF.approx.date) <- header
  
  ################## Section (3) Time-Series Analysis #####################################################
  
  # The assumption is that the periodicity of a year is the most significant
  # therefore the number of observations (columns) * 12 equals one year
  # if the dataframe is beeing converted to a timeseries
  
  period <- ncol(testDf.approx)
  
  frequency <- period * 12
  
  ## creating a vector ouzt of the data frame (without dates)
  vec <- data.frame(newcol = c(t(xdc)))
  
  # number of observations
  obs <- nrow(vec)
  
  #cbinding ID's for plotting i guess
  ID <- as.numeric(seq(1, obs, 1))
  vecID<- cbind (ID, vec)
  vecID.df <- as.data.frame(vecID)
  
  #line plot
  plot(vecID, type="l")
  
  
  ######## ggplot stuff
  #p <- ggplot(data = vecID.df, aes(x = ID, y = newcol))+
  #  geom_line(color = "#00AFBB", size = 0.5)
  #p
  # this plots a loess function in the plot
  # p + stat_smooth(
  #  color = "#FC4E07", fill = "#FC4E07",
  #  method = "loess"
  #)
  
  
  
  # i need to continue here tomorrow 03/08/2021  # oke #05/08/2021
  # so this would be a detrendet ts analysis
  # THIS DOES ONLY WORK IF NO NA VALUE IS PRESENT IN THE DATASET
  # SOME AREAS MIGHT HAVE TOO MANY NA VALUES TO INTER/EXTRAPOLATE ALL VALUES
  
  vec.ts <- ts(vec$newcol, frequency = frequency)
  
  vec.stl <- stl(vec.ts, s.window = frequency)
  
  
  plotHeader <- str_remove(fileList[j], pattern=".csv")
  
  fileT <- paste(plotHeader, "png", sep = ".")
  fileP <- paste(outputPath, fileT, sep = "/")
  
  png(file = fileP, width=870, height=397)
  
  plot(stl(vec.ts, s.window=frequency, l.window=period), main = plotHeader)
  
  
  
  # https://otexts.com/fpp2/seasonal-strength.html
  # here ^ an article about trend change analysis?
  # i dont know if this works
  
  
  ########### tidy column approach
  # use the testDf.approx.date
  
  # https://towardsdatascience.com/tidy-anomaly-detection-using-r-82a0c776d523
  # R anomalize sources: https://rdrr.io/cran/anomalize/man/anomalize.html
  # as tibble obj
  
  
  testDF.approx.date.tibble <- as_tibble(testDF.approx.date)
  
  #head(testDF.approx.date.tibble)
  
  #testDF.approx.date.tibble %>%
  #  time_decompose("4.94" , method = "stl", frequency = 12 , trend = "auto") %>%
  #  anomalize(remainder, method = "gesd", alpha = 0.1, max_anoms = 0.2) %>%
  #  plot_anomaly_decomposition()
  
  
  #testDF.approx.date.tibble %>%
  #  time_decompose(V15, method = "stl", frequency = 12, trend = "auto") %>%
  #  anomalize(remainder, method ="gesd", alpha = 0.1) %>%
  #  time_recompose() %>%
  #  plot_anomalies(time_recomposed = TRUE, ncol = 5, alpha_dots = 0.5)
  
  # extract actual data points that are outliers
  
  #testDF.approx.date.tibble %>%
  #  time_decompose(V20) %>%
  #  anomalize(remainder) %>%
  #  time_recompose() %>%
  #  filter(anomaly == 'Yes')
  
  
  
  # model evaluation
  #?check_model
  #model_lm <- lm(observed ~ season + remainder, data=recom)
  #check_model(model_lm)
  
###### Section (4) building anomaly cluster matrix######################################################################
  #       1)  Iterating over every observation point along the glacier
  #       2)  Write anomaly matrix
  #       3)  create threshold for certain anomaly clusters in x/y directions
  #       4)  next  
  
  
  # first we build a matrix based on the input
  # if we loop we do this only on the first one
  # We use 2 here since it indexes the header // the column of the dataframe
  
  
  # building empty matrix
  inputCol = ncol(xdc)
  inputRow = nrow(xdc)
  s <- matrix(data = NA, nrow = inputRow, ncol = inputCol)
  AnomalyMatrix <- as.data.frame(s, row.names = Date)
  NaMatH <- header[-1]
  colnames(AnomalyMatrix) <- NaMatH
  rownames(AnomalyMatrix) <- Date
  
  
  # loop over data // headerloop indexes the column
  i = 1
  headerloop = 2
  
  for (i in 1:ncol(xdc)) {
    
    # decompose the dataset
    decomp <- time_decompose(testDF.approx.date.tibble, header[headerloop], method = "stl", frequency = 12 , trend = "auto")
    #anomaly detection
    anom <- anomalize(decomp, remainder, method = "gesd", alpha = 0.01, max_anoms = 0.4)
    #recomposition of the dataset
    recom <- time_recompose(anom)
    
    AnomalyMatrix[,i]<- recom$anomaly  
    
    headerloop <- headerloop + 1
    
  }
  
  filename <- paste("Anomaly", fileList[j], sep ="_")
  write.csv(AnomalyMatrix, file.path(outputPath, filename))
  
  j = j+1
  dev.off()
}


################################################################################################################################
###### Section (5) - Row wise cluster Analysis #################################################################################
################################################################################################################################

outputFolder_2 <- "RunningWindow"

dir.create(file.path(wd, outputFolder_2), showWarnings = FALSE)

outputPath_2 <- paste(wd,"RunningWindow", sep = "/" )

AnomalyFiles <- list.files(path=outputPath, pattern=".csv", full.names=FALSE)

l <- 1

for (r in 1:length(AnomalyFiles)){
  
  setwd(outputPath)
  file <- list.files(path=outputPath, pattern=".csv", full.names=FALSE)
  
  Anom <- as.data.frame(read.csv(file[l]))
  
  # annoying but necessary - copying the first row (dates) as rownames
  Date <- Anom[,1]
  row.names(Anom) <- Date
  Anom <- Anom[-1]
  
  
  ## exchanging 'Yes' and 'No' with 1/0 for moving window approach / count sums
  anomNumeric <- as.data.frame(Anom, stringsAsFactors = FALSE)
  anomNumeric <- anomNumeric %>% mutate_if(is.character, str_replace_all, pattern = 'Yes', replacement = '1')
  anomNumeric <- anomNumeric %>% mutate_if(is.character, str_replace_all, pattern = 'No', replacement = '0')
  
  anomNumeric <- as.data.frame(anomNumeric)
  
  ## moving window approach
  
  # So this loop checks if the glacier is smaller than 1.5km.
  # if TRUE then the average of the whole glacier is calculated
  if (ncol(anomNumeric)<10) {
    
    sum_runwin<-matrix(NA, nrow(anomNumeric), ncol(anomNumeric))
    rownames(sum_runwin) <- rownames(anomNumeric)
    
    
    # running window
    for (u in 1:nrow(anomNumeric))  {
      
      for (i in 1:(ncol(anomNumeric))) {
        
        row_con<-as.vector(t(as.numeric(anomNumeric[u,])))
        
        o <- ncol(anomNumeric)
        
        sum_con<-sum(row_con[i:o])
        
        sum_runwin[u,i]<-sum_con
        
      }
    }
    
    # any glacier with a length above 1.5 km is handled here
    
  } else {    
    
    
    
    sum_runwin<-matrix(NA, nrow(anomNumeric), ncol(anomNumeric)-9)
    rownames(sum_runwin) <- rownames(anomNumeric)
    
    
    # running window
    for (u in 1:nrow(anomNumeric))  {
      
      for (i in 1:(ncol(anomNumeric)-9)) {
        
        row_con<-as.vector(t(as.numeric(anomNumeric[u,])))
        o <- i+9
        sum_con<-sum(row_con[i:o])
        sum_runwin[u,i]<-sum_con
      }
      
    }
  }
  
  filename2 <- paste("RW", AnomalyFiles[r], sep ="_")
  
  write.csv(sum_runwin, file.path(outputPath_2, filename2))
  
  l <- l+1
}

######### Last part. Script that looks into every row and checks if the sum is greater than whatever
# files are located in "outputPath_2
setwd(outputPath_2)

## looks into files that will be analysed and substrings for the header file
RW_Files <- list.files(outputPath_2, pattern = ".csv")
RW_Matrix_header <- str_replace(RW_Files, "-", "_")
RW_Matrix_header <- str_remove(RW_Matrix_header, pattern=".csv")
RW_Matrix_header <- str_remove(RW_Matrix_header, pattern= "RW_Anomaly_")
ncol.length <- length(RW_Files)
RWF <- as.data.frame(read.csv(RW_Files[1]))


# how many months in input? 
months <- nrow(RWF)

# building Matrix for results
results.df <- as.data.frame(matrix(NA, months, ncol.length))
rownames(RWF) <- RWF[,1]
rownames(results.df) <- rownames(RWF)
colnames(results.df) <- RW_Matrix_header

for (i in 1:length(RW_Files)){
  
  # reading in files for analyis 
  container <- as.data.frame(read.csv(RW_Files[i]))
  
    if (ncol(container)<=2) next # if there is only one column (with data) skip to next the loop cant handle single column files
  
  container <- container[,-1]
  
  for (j in 1:nrow(container)){
    
    if (max(container[j,]) >= 7){
      
      results.df[j,i] <- 1          # 
    }
  }
write.csv(results.df, file = "AnomalyMatrix_rw7.csv")             # writing out results as .csv file in the outputfolder
}


# results where? #########
###### This is for later
# https://www.youtube.com/watch?v=6FvStEghDdg
# ggplot tables
# tidy style 