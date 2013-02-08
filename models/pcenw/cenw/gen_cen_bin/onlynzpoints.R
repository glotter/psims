## try to find point in the square that are only nz ie not ocean points so there are no errors in the long run.



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

nzres<-as.matrix(nzres)
#each pixel value is the grid id
#######################################



#now want to take the grid and zeor out all the points in the ocean so we are left with only gridids in the points with land
a<-1
b<-1
new<-nzres

while (b<242){
  a<-1

while (a< 259)
{
  if(is.na(shp[a,b]))
  {new[a,b]<-0
   }
  a<-a+1
}
b<-b+1}
#############

col<- matrix(new, 1) #switch to single row
col<-col[,which(!apply(col,2,FUN = function(x){all(x == 0)}))] #remove all 0
col<-sort(col) #sort the column

#now just need to rewrite it so it is ####/########
dir<-c(1792:1976) #directories
a<-1
gridlist<-1

while (a< 13902)
{
  b<-col[a]
jjj<-pmatch(dir,b) #match the dir to the grid
  
  file_num<-NaN
  kkk<-0 ##looping index
  

  while (kkk<187)
  {
    if (is.na(file_num) ) {
      kkk<-kkk+1
      file_num<-jjj[kkk]
      
    }else
      if(file_num==1)
      {
        gl<- paste(dir[kkk],"/",b, sep="")
       gridlist[a]<-gl 
       
{break}
}
  }
  a<-a+1
}

write.csv(gridlist,file="C:/Work/CenW/CenW_SVN/gen_cen_bin/gridlist",quote=FALSE, row.names=F, col.names=NA)