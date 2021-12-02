
# workspace
setwd("/media/vetinari/data2/Moritz")

### package section
# add required packages 

require(raster)
require(rgeos)
require(sp)
require(rgdal)


#### section 01 
# read in data 
# shape files / centerline features

shapefiles <- list.files(path="Shapefiles/", pattern='*.shp')
centerlines <- shapefiles[1]
glacieroutlines <- shapefiles[2]

# velocity fields



#### section 02
# cut raster data
# add bins to centerline
# measurement?

#### section 03 
# build velocity baselines
#
#