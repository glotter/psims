#18763109 find what that point corresponds to on the map



library(raster)
library(sp)
library(rgdal)
library( ncdf4)
library( ncdf)

setwd("C:/Work/Climate stuff")

#########################################################################################
#read in some of the original data to get the shape of nz
prec<- raster("Original_data/AR4_climate_change_ASCII_grids/pr_2040a1b_apr_cccma_cgcm3.txt") #this is the grid we have

#but need to take this grid and adjust just a bit so that the numbers line up. ie need to shift it by 0.025
##prec<-shift(prec, x=0.025, y=-0.025)
ext<-c(166.45, 178.5,-47.25, -34.35)
prec<-crop(prec,ext)


#convert to matrix so can see where the numbers are
shp<-as.matrix(prec)

#now delete rows and colums that are extra


#grid ids where pixel values are the grid id in world
################################################
world <- raster()
res( world) <- 3/60
world[] <- 1:ncell( world)

#need to crop to nz extent
#here the grid is once more off a little bit
#also 0.05 grid
#but 166.425,-34.425 is the middle of a point so that pixel covers
# 166.4,166.45,-34.45, -34.4
#so make extent for this 166.45, 178.5,-47.25, -34.35
ext<-c(166.45, 178.5,-47.25, -34.35)
nzres<-crop(world,ext) 

nzresm<-as.matrix(nzres)
#each pixel value is the grid id
#######################################

a<-1
b<-1



while (b<242)
{a<-1
while (a< 259)
{
if(nzresm[a,b]==18035876)
{ pointa<-a
  pointb<-b
  }

a<-a+1
}
b<-b+1}

prec[pointa,pointb]<-100000
plot(prec)


#########################################################

#select new point and find it as gridid
ext<-c(173.7, 173.8,-35.28, -35.2)
prec1<-crop(prec,ext)
nzres1<-crop(nzres,ext)
as.matrix(nzres1)