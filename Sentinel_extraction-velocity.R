# script data extraction
########################################################################################
print("starting script...")

# load libraries
library(raster)


#########################################################################
# read data
#########################################################################
print("read data...")

# load example list of rasters
files<-as.vector(unlist((read.table("analysis_list.txt"))))
# get projection of rasters
r_CRS<-proj4string(raster(files[1]))
# filenames and dates
if (grepl("Mosaics", files[1], fixed = TRUE)) {
  # get dates of rasters 
  dates<-unlist(strsplit(files,split="/"))
  filenames<-dates[seq(7,length(dates),7)]
  dates<-filenames
  i<-1
  for (filename in filenames) {
    nl<-nchar(filename)
    date<-substr(filename, nl-7, nl-4)
    if (grepl("_", date, fixed = TRUE)) date<-substr(filename, nl-10, nl-4)
    dates[[i]]<-date
    i<-i+1
  }
} else {
  # get dates of rasters
  dates<-unlist(strsplit(files,split="/"))
  filenames<-dates[seq(10,length(dates),10)]
  dates<-unlist(strsplit(dates[seq(8,length(dates),10)], split="_"))
  dates<-dates[seq(4,length(dates),4)]
}

# load polyline
transect<-shapefile("polyline.shp")
# get projection of polyline
t_CRS<-proj4string(transect)
# reproject polyline to raster crs
t_coords<-transect@lines[[1]]@Lines[[1]]@coords
t_coords_utm<- data.frame(X = c(t_coords[,1]), Y = c(t_coords[,2]))
coordinates(t_coords_utm)<-c("X", "Y")
proj4string(t_coords_utm)<-t_CRS
t_coords_utm<-as.data.frame(spTransform (t_coords_utm, CRS(r_CRS)))
t<-SpatialLines(list(Lines(list(Line(t_coords_utm)),1)))


##########################################################################
# extract data
##########################################################################
print("extract data...")

#loop over raster files
i<-1
while (i <= length(files)){
  print(i)
  #read raster
  r<-raster(files[i])
  # extract values
  velocities<-unlist(extract(r, t))
  if (is.null(velocities)) {
    velocities<-c(NaN)
  }
  # append extracted values to matrix
  if (i==1){
    vel_m<-matrix(velocities, nrow=1, ncol=length(velocities), byrow=T)
  }else{
    vel_m<-rbind(vel_m,velocities)
  }

  i<-i+1
}

# add filename as row names
rownames(vel_m)<-dates

####################################################################################
# calculate distance along profile
####################################################################################
print("calculate distance along profile...")

#get distance of measuring points along transect
coords<-matrix(unlist(t_coords_utm), ncol=2, byrow=F)   #transform to matrix
mdist<-spDists(coords, coords, longlat=F)         #cal. distance matrix of coordinates
dist<-0
for (k in 1:(nrow(coords)-1)){                    #cal. distance along shapefile line
  j<-k+1
  dist<-mdist[j,k]+dist
}
#calculate mean distance between measurements
ddist=dist/ncol(vel_m)
#vector with equidistant length along the profile pixels (in km)
dist_c<-round(c(seq(from=ddist,by=ddist, length.out=ncol(vel_m)))/1000,digits=3)

#add distance as column names and convert to data frame
colnames(vel_m)<-dist_c
vel_m<-as.data.frame(vel_m)

########################################################################################
# export data as csv
########################################################################################
print("export as csv...")

write.csv(x=vel_m, "analysis_results.csv")

####################################################################################
# simple plot of data (points seem to be better than lines if there are outliers)  #
####################################################################################
print("plot data...")

#get maximum velocity
max_vel<-round(max(vel_m[1:nrow(vel_m),], na.rm=T), digits=0)+1

#get maximum distance
max_dist<-max(round(dist_c, digits=0))+1

additional_dist<-max_dist/10

#open plotting device
png("analysis_plot.png", width=1500, height=1000, pointsize=18)

  #get colors
  cl<-rainbow(nrow(vel_m))

  #make empty plot
  plot(x=0,y=0,xlim=c(0,max_dist+additional_dist), ylim=c(0,max_vel), type="n", xlab="Distance [km]", ylab="Velocity [m/d]")

  #add legend
  legend (x=max_dist, y=max_vel,legend=dates, col=cl, pch=16, ncol=1)

  #draw points
  i<-1
  while(i<=ncol(vel_m)-1){
    points(x=dist_c, y=vel_m[i,], col=cl[i], pch=16)
    i<-i+1
  }

#close plotting device
dev.off()
