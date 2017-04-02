library(rgdal)
library(maptools)
library(spdep)
library(dplyr)
library(tmap)
library(GISTools)
library(REAT)
library(spatialEco)


# read subzones shapefile
Singapore <- readOGR(dsn = "/Users/shuying/Desktop/Moran1/shapefile", layer = "MP14_SUBZONE_NO_SEA_PL") # read shapefile

# read private housing energy consumption data
phec <- read.csv ("/Users/shuying/Desktop/Moran1/attributes/private_housing.csv") # read csv
coordinates(phec) <- ~X+Y

# convert files to same CRS
proj4string(Singapore) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")

proj4string(phec) <- CRS("+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs")

# use spatiaEco to map houses to subzones
phec_in_subzone <- point.in.poly(phec, Singapore)
phec_in_subzone_df <- as.data.frame(phec_in_subzone)

# remove NA values from table
phec_in_subzone_df[is.na(phec_in_subzone_df)] <- 0

# set column from factor to num type
phec_in_subzone_df[, 2] <- as.numeric(as.character(phec_in_subzone_df[, 2] ))

# aggregate energy consumption for Jan by subzones
aggregate_ec_by_subzone <- aggregate(phec_in_subzone_df$Jan, by=list(SUBZONE_N=phec_in_subzone_df$SUBZONE_N), FUN=sum)


# convert subzone shapefile to data frame
Singapore_df <- as.data.frame(Singapore)

# merge aggregate of energy consumption for Jan to subzone
merged <- merge(aggregate_ec_by_subzone, Singapore, by.x="SUBZONE_N", by.y="SUBZONE_N")

# get energy consumption in Jan per km square of subzone
merged <- transform(merged, avg = x / (SHAPE_Area / 1000000))


##################################################################################################################



Singapore@data <- left_join(Singapore@data,phec) # relational join btw two tables

wm_q <- poly2nb(Singapore, queen=TRUE) # create contiguity based neighbours (queen)
summary(wm_q)

plot(Singapore, border='lightgrey')
plot(wm_q, coordinates(Singapore), pch = 19, cex = 0.6, add = TRUE, col= "red") # plot neighbours based on queen

coords <- coordinates(Singapore)


# row-standardised weights matrix
# increase the influence of links from observations with few neighbours
rswm_q <- nb2listw(wm_q, zero.policy = TRUE)

moran.test(Hunan$GDPPC2012, listw=rswm_q, zero.policy = TRUE, na.action=na.omit)

set.seed(1234)
bperm= moran.mc(Hunan$GDPPC2012, listw=rswm_q, nsim=999, zero.policy = TRUE, na.action=na.omit)



MS <- moran.plot(Hunan$GDPPC2012, rswm_q, zero.policy = TRUE, spChk=FALSE, labels=as.character(Hunan$SUBZONE_C), xlab="GDPPC(RMB)", ylab="Spatially Lag GDPPC (RMB)")