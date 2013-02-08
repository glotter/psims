library( ncdf)
library( zoo)
library( raster)
library(rgdal)



setwd("/gpfs/pads/projects/emc-gct/data/")

#read in site info file for CenW
site<-read.csv(file='/gpfs/pads/projects/emc-gct/data/raw/CenW_site_info/Site info.csv', sep=',',header=TRUE)

#and orig site file
st<-read.csv(file='/gpfs/pads/projects/emc-gct/data/raw/CenW_site_info/CenWst.csv', header=F, sep=",", stringsAsFactors=F)


#there are 7 layers of soil
l<-0
l[1]<-5
l[2]<-10
l[3]<-10
l[4]<-25
l[5]<-50
l[6]<-50
l[7]<-50

tot<-200


#create nz grid with grid ids
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
#each pixel value is the grid id
##


#write each row as individ file in gridid folder as name
st1<-st
i<-1
while (i<11455) 
  {
  
a=site[i,c("Latitude")] #lat
b=site[i,c("Longitude")] #lon
c<- a+0.05
d<-b+0.05

newwhc<- site[i,c("WHC")]
pc_whc<-(newwhc-tot)/tot #percentage change in WHC
pc_whc<-1+pc_whc

lay<-l*pc_whc  #change l to 

#set st back to original data
st<-st1

#replace WHC data in st file with new data
st[54,1]<-lay[1]
st[63,1]<-lay[2]
st[72,1]<-lay[3]
st[81,1]<-lay[4]
st[90,1]<-lay[5]
st[99,1]<-lay[6]
st[108,1]<-lay[7]


#also need to replace file soil froaction
soil<- site[i,c("Fine.soil")] ######i
soil<-soil/100
st[9,1]<-soil


dig<-st[,1]
dig<-as.numeric(dig)
dig<-format(dig,digits=11)

st[,1]<-dig
#change some of the data into text format so that can be written with same format
#as original file

st[1,1]<-"4.0"
st[2,1]<-"          7"
st[3,1]<-"         19"
st[4,1]<-"        356"
st[51,1]<-"FALSE Give a"

#figure out how to replace set lines in code with new line of data

#extrat corresponding grid id
ext<-c(b, d, a, c)
gridid<-crop(nzres,ext)
gridid<-getValues(gridid)
grid_one<- gridid[1]


  dir<-c(1791:1976)  
  jjj<-pmatch(dir,grid_one) #match the dir to the grid
  
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
        root<-getwd()
        ggg<-paste(root,"/tree/",dir[kkk], sep="")
        if (!file.exists(ggg))
        {dir.create(paste(root,"/tree/",dir[kkk], sep=""))}
        ggg<-paste(root,"/tree/",dir[kkk],"/",grid_one, sep="")
        if(!file.exists(ggg))
        {dir.create(paste(root,"/tree/",dir[kkk],"/",grid_one, sep=""))} 
        ###write file here
        ggg<-paste(root,"/tree/",dir[kkk],"/",grid_one,"/CenW.ST!", sep="")
        write.table(st, file=ggg, sep=" ", quote=F, row.names=F, col.names=F)
        kkk<-190
      }

       
    }  
i<-i+1
}

