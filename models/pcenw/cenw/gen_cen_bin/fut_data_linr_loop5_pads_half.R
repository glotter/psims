####### uses niwa climate data
#takes the rainfall change projections for whole of nz 
# does linear regression between 1990, 2040 and 2090 


##note the projection =%change of original rainfall 
#so abs valuee of rainfall= (orig*change/100)+orig
############

library(raster)
library(sp)
library(rgdal)
library( ncdf4)
library( ncdf)

setwd("//gpfs/pads/projects/emc-gct/data/")
root<-getwd()
#########################################################################################
#read in orig ppt data and reorganise to raster
prec<-list.files(path= "climatology",pattern="^prec", full.names=TRUE)
prec<-lapply(prec, raster )
prec<-stack(prec)


#read in climate change grid data and stack
#list all files in single model for 2040
hadcm3_40<- list.files(path= "climchange",pattern="^pr_2040*.*a1b.*hadcm3", full.names=TRUE)
hadcm3_40.r <- lapply(hadcm3_40, raster )
hadcm3_40.s<- stack(hadcm3_40.r)

#list all files in single model for 2090
hadcm3_90<- list.files(path= "climchange",pattern="^pr_2090*.*a1b.*hadcm3", full.names=TRUE)
hadcm3_90.r <- lapply(hadcm3_90, raster )
hadcm3_90.s<- stack(hadcm3_90.r)

########line only needed for prec
hadcm3_40.s<- (hadcm3_40.s*prec)/100
ppt_tot_40<- hadcm3_40.s+prec ### total ppt in the month

hadcm3_90.s<- (hadcm3_90.s*prec)/100
ppt_tot_90<- hadcm3_90.s+prec ### total ppt in the month
####################

#####read in the temp data
#read in tmax and tmin
tmax<-list.files(path= "climatology",pattern="^tmax", full.names=TRUE)
tmax<-lapply(tmax, raster )
tmax<-stack(tmax)

tmin<-list.files(path= "climatology",pattern="^tmin", full.names=TRUE)
tmin<-lapply(tmin, raster )
tmin<-stack(tmin)

#read in climate change grid data and stack
#list all files in single model for 2040 and 2090
hadcm3_40<- list.files(path= "climchange",pattern="^tas_2040.*a1b.*hadcm3", full.names=TRUE)
hadcm3_40.r <- lapply(hadcm3_40, raster )
hadcm3_40.s<- stack(hadcm3_40.r)

hadcm3_90<- list.files(path= "climchange",pattern="^tas_2090.*a1b.*hadcm3", full.names=TRUE)
hadcm3_90.r <- lapply(hadcm3_90, raster )
hadcm3_90.s<- stack(hadcm3_90.r)

#add the av to the change in temp in order to make it abs temps
tmin_40<-tmin+hadcm3_40.s
tmax_40<-tmax+hadcm3_40.s

tmin_90<-tmin+hadcm3_90.s
tmax_90<-tmax+hadcm3_90.s
#############################################################################################


###################################################################
#make all the loop variables constant
months<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
mnth_order<-c(5,4,8,1,9,7,6,2,12,11,10,3)

###############################################################################3
################ corresponding to leap years and non leap years for later
#need to do this for historic data from 1972 then 51 years 1990-2090 and then also out to  2100 for predction purposes

yrs<-c(1:129) #non leap years
leap<-0
leap[1:129]<-0
i<-1
a<-129

while(a>0)
{
yrs[a]<-0
a<-a-4
i<-i+1}


a<-1
i<-1
while(a<=129)
{
  leap[a]<-a #leap years
  a<-a+4
  i<-i+1
}


###loop to determine length of residual data and to keep it consistent
#do this for the first loop
# one sequence with days in 5-8 years from year 1-36
#Tmax_res_i<-14610 ###dummy
#newtmax<-0
#p<-0
#q<-0
#yr_5to8=c(1826,2191,2556,2922)
#i=1
#long<-0#43465#, 40177
#while(long<=43465)
#{
#g=sample(yr_5to8,1,replace=T)
#h=sample(1:31,1, replace=T)
#q[i]<-h
#h=h*365    ##* number of days
#newtmax=Tmax_res_i[c((g+1):(g+h))]
#newrain=rain_res_i[c((a+1):(a+b))]
#long=length(Tmax_res_i) ##find length of the tmax_res_i to add the next chunk on
#Tmax_res_i[(long+1):(long+h)]=newtmax
#rain_res_i[(long+1):(long+b)]=newrain
#long=length(Tmax_res_i)
#p[i]<-g
#i=i+1}



##now p and q are used to keep all the mixing data consistent!!!
#write datat to files to load later if needed
#write(p,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/p")
#write(q,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/q")
p<-c(2191, 2922 ,2556 ,2922, 2556,2191)
q<- c(27, 29, 14, 24, 19,20)


###################################
numdays<-c(31,28,31,30,31,30,31,31,30,31,30,31) #num of days in non leap year
numstartnl<-c(1,32,60,91,121,152,182,213,244,274,305,335) #day that the month starts on in non leap year
numstart<-c(1,32,61,92,122,153,183,214,245,275,306,336) #day that the month starts on in  leap year

#######################################################################################

#######################################################################################3
#get climate variables (future) in right order in terms of months
#loop over months in order to get new var with climvar_mnth 
#add stacks to list for each motnh

ppt_mnth<-list()   #ppt [[1]]=jan, [[2]]=feb.. etc
tmax_mnth<-list()
tmin_mnth<-list()
a<-1
for(i in mnth_order)
{
  
  ppt_had_1990<-raster(prec,mnth_order[a])
  ppt_had_2040<-raster(ppt_tot_40,mnth_order[a])
  ppt_had_2090<-raster(ppt_tot_90,mnth_order[a])
  ppt_prj<-stack(ppt_had_1990,ppt_had_2040,ppt_had_2090)
  
  ppt_mnth[[a]]<-ppt_prj ## makes a list with 1=jan etc
  ppt_mnth[[a]]<-ppt_mnth[[a]]/numdays[a]
  
  tmax_had_1990<-raster(tmax,mnth_order[a])
  tmax_had_2040<-raster(tmax_40,mnth_order[a])
  tmax_had_2090<-raster(tmax_90,mnth_order[a])
  tmax_prj<-stack(tmax_had_1990,tmax_had_2040,tmax_had_2090)
  
  tmax_mnth[[a]]<-tmax_prj ## makes a list with 1=jan etc
  
  tmin_had_1990<-raster(tmin,mnth_order[a])
  tmin_had_2040<-raster(tmin_40,mnth_order[a])
  tmin_had_2090<-raster(tmin_90,mnth_order[a])
  tmin_prj<-stack(tmin_had_1990,tmin_had_2040,tmin_had_2090)
  
  tmin_mnth[[a]]<-tmin_prj ## makes a list with 1=jan etc
  
  ###assign(mnth[[a]],ppt_prj)
  a<-a+1
}
##################################################################################


#grid ids where pixel values are the grid id in world
#this bit is incomplete at the moment
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
#######################################

#need to get value from each pixel
#######################################nzres1= getValues (nzres)
####################################



###########################################################################
#the mighty loop that does everything

#need to extract single timeseries from this stak
#need to loop over 0.05 degree grid
#want to extract a single row of data for all the map
#know historic grid is bigger than prediction grid
#leave out first 2 rows and columns of historic grid
#so want 166.474,178.476 and -47.276,-34.424 which will encompass whole grid

##make a loop to extract data each row and put in list. 
#loop over month for each point then loop over dif points. 




a<-177.924#168.724
c<-177.9#168.7
while (a<177.93){   ##outer loop (1) over long

b<-0
d<-0
j<-1 # each long row
l<-2 # latitude
domlong<-a+0.001

  #read in the historic data files which we will have to extract before hand with lat_long or gridids in the name
  # prob just long in names is enough because will be looping over lat
  #dom<-nc_open(paste("//media/sf_Examples/split/domain_",domlong,".nc", sep=""), write=FALSE, readunlim=TRUE, verbose=TRUE) #### change this i should be a or c
  dom<-nc_open(paste(root,"/vcsn-nc/lines/domain_",domlong,".nc", sep=""), write=FALSE, readunlim=TRUE, verbose=TRUE)
  Tmin<-ncvar_get(dom, varid="Tmin",   verbose=TRUE, signedbyte=TRUE)
  Tmax<-ncvar_get(dom, varid="Tmax",   verbose=TRUE, signedbyte=TRUE)
  Rain<-ncvar_get(dom, varid="Rain",   verbose=TRUE, signedbyte=TRUE)
  Rad<-ncvar_get(dom, varid="Rad",   verbose=TRUE, signedbyte=TRUE)




while (l<259) ##loop 2 over lat
{
i<-1 #month
ppt_mnth_long<-vector("list", 258) ##longitude is the dif matricies
tmax_mnth_long<-vector("list", 258) ##longitude is the dif matricies
tmin_mnth_long<-vector("list", 258) ##longitude is the dif matricies
nzres_long<-vector("list",258)
day<-c(1,18263,36526) #old jan1 in 1990, 2040 and 2090
day1<-day+6575 #jan 1 in 1990 2040 and 2090 because added on some years of hist data
ppt_hist_yr<-0 # predicted interpolated data combined with months
tmax_hist_yr<-0
tmin_hist_yr<-0


while (i<13){ ### loop 3 over month

  b<-a+0.05
  d<-c+0.05
  e<-c(a, b,-47.276, -34.424)
  ext<-c(c,d,-47.25, -34.35) ##extent for nzres off by a bit so needs to be different
  
  ppt_mnth1=crop(ppt_mnth[[i]],e)
  ppt_mnth1= getValues (ppt_mnth1)
  
  ppt_mnth_long[[j]] <- matrix(data = ppt_mnth1,
                         nrow = 258,
                         ncol =3,
                         byrow = FALSE,
                         dimnames = NULL)
  
  
  tmin_mnth1=crop(tmin_mnth[[i]],e)
  tmin_mnth1= getValues (tmin_mnth1)
  
  tmin_mnth_long[[j]] <- matrix(data = tmin_mnth1,
                               nrow = 258,
                               ncol =3,
                               byrow = FALSE,
                               dimnames = NULL)
  
  
  
  tmax_mnth1=crop(tmax_mnth[[i]],e)
  tmax_mnth1= getValues (tmax_mnth1)
  
  tmax_mnth_long[[j]] <- matrix(data = tmax_mnth1,
                               nrow = 258,
                               ncol =3,
                               byrow = FALSE,
                               dimnames = NULL)

#l<-226##################### delete 
  #extract lat data
    ppt_pt<-lapply(ppt_mnth_long,'[',l,) # extracts lat and puts in a list
    ppt_pt_num<-0
    ppt_pt_num<-sapply(ppt_pt[[1]],'[') 

    tmax_pt<-lapply(tmax_mnth_long,'[',l,) # extracts lat and puts in a list
    tmax_pt_num<-0
    tmax_pt_num<-sapply(tmax_pt[[1]],'[')
  
    tmin_pt<-lapply(tmin_mnth_long,'[',l,) # extracts lat and puts in a list
    tmin_pt_num<-0
    tmin_pt_num<-sapply(tmin_pt[[1]],'[')

    #line up all the dates ie 1/1, 1/2 etc
  if(i>1){  
  day<-day+numdays[i]#first of month in 1990, 2040 and 2090
  }

 
  hist_days<-data.frame(day=c(1:47118)) #number of days 1972- 1990-2040
   if (!is.na(ppt_pt_num) || !is.na(tmax_pt_num) || !is.na(tmin_pt_num) )
   {
   #predict data
    ppt_pred_hist<-predict(lm(ppt_pt_num~day), hist_days)  #when values are NAN then produces error.
    tmin_pred_hist<-predict(lm(tmax_pt_num~day), hist_days)
    tmax_pred_hist<-predict(lm(tmin_pt_num~day), hist_days)
  
 #takes the ppt that has been spit out and puts all the data in the appropriate place 
  #all jan data goes in jan spot acc to year ie fills out month for 111 years(1990-2100)
########################################################################
  #####loop over 1990-2040 of jans then febs etc to put all data in a time series
  #first for non leap year data

      
  m<-numdays[i] #number of days in a month
  first<- numstart[i]#firstday of the month of year 
  last<- first+m-1#last day of month of year day of year

   
  yr<-1
  o<-1 # index for leap years

    while (yr<=129) ##129 years historic and till 2100 years of data
  { if (yr==yrs[o])
    {
    ppt_hist_yr[first:last]<-ppt_pred_hist[first:last]
    tmax_hist_yr[first:last]<-tmax_pred_hist[first:last]
    tmin_hist_yr[first:last]<-tmin_pred_hist[first:last]
    first<-first+365
    last<-last+365
    }
  if (yr==leap[o])
  {if(yr>1) #only do if yr is bigger than one because first year is a leap year
    {
    first<-first+1
    last<-last+1
    }
   if(i==2) #if feb and leap
   {last<-last+1}
   ppt_hist_yr[first:last]<-ppt_pred_hist[first:last]
   tmax_hist_yr[first:last]<-tmax_pred_hist[first:last]
   tmin_hist_yr[first:last]<-tmin_pred_hist[first:last]
    first<-first+365
    last<-last+365
    }
  yr<-yr+1
    o<-o+1}
############################################################################# 
} # close loop for if not NA for the point
  
  
#  assign(mnth_long[[i]],jan_ppt1)
  i<-i+1
} #end i loop over months
#now have data for a point which is total till 2100 of extrapolated data



#############################use historic daily data to do extrapolation etc
##calc residual

ppt_resid<-Rain[l,]-ppt_hist_yr[1:14610] ###l is corresponding to lat
tmax_resid<-Tmax[l,]-tmax_hist_yr[1:14610]
tmin_resid<-Tmin[l,]-tmin_hist_yr[1:14610]
rad_resid<-Rad[l,] #radiation is different. want to copy the data over the time

o<-1  ##new dummy looping variable
longer<-0 #length of variable
while(longer<47118)
{
g<-p[o]
h<-q[o]
h=h*365    ##* number of days

ppt_new<-ppt_resid[c((g+1):(g+h))]
longer<-length(ppt_resid) ##find length of the tmax_res_i to add the next chunk on
ppt_resid[(longer+1):(longer+h)]<-ppt_new

tmax_new<-tmax_resid[c((g+1):(g+h))]
tmax_resid[(longer+1):(longer+h)]<-tmax_new

tmin_new<-tmin_resid[c((g+1):(g+h))]
tmin_resid[(longer+1):(longer+h)]<-tmin_new

rad_new<-rad_resid[c((g+1):(g+h))]
rad_resid[(longer+1):(longer+h)]<-rad_new

longer<-length(ppt_resid)
o=o+1
}


##make app the residuals same legnth as predictinons with 0 for extras


##make app the residuals same legnth as predictinons with 0 for extras
length(ppt_hist_yr)<-length(ppt_resid)
#ppt_resid[is.na(ppt_resid)]<-0

length(tmin_hist_yr)<-length(tmin_resid)
#tmin_resid[is.na(tmin_resid)]<-0

length(tmax_hist_yr)<-length(tmax_resid)
#tmax_resid[is.na(tmax_resid)]<-0

length(rad_resid)<- length(tmax_resid)
#rad_resid[is.na(rad_resid)]<-0


#add the residuals onto the extrapolated data
ppt_ex<-ppt_hist_yr+ppt_resid
ppt_ex[1:14610]<-Rain[l,]

### if ppt is -ve then make it 0 because no neg rain
ppt_ex[ppt_ex<0]<-0


tmax_ex<-tmax_hist_yr+tmax_resid
tmax_ex[1:14610]<-Tmax[l,]

tmin_ex<-tmin_hist_yr+tmin_resid
tmin_ex[1:14610]<-Tmin[l,]


#ppt_ex[1:14610]<-Rain[21,]
#tmax_ex[1:14610]<-Tmax[21,]
#tmin_ex[1:14610]<-Tmin[21,]

rad_ex<-rad_resid[1:47118]
tmin_ex<-tmin_ex[1:47118]
tmax_ex<-tmax_ex[1:47118]
ppt_ex<-ppt_ex[1:47118]




#####get grid id which needs to be the file name

gridid<-crop(nzres,ext)
gridid<-getValues(gridid)
grid_one<-gridid[l]

#############################
#write data to file
#always in order of Tmax, tmin, radn and rain from jan1 1972- some date passed 2100
qqq<-data.frame(tmax_ex, tmin_ex, rad_ex, ppt_ex)
#qqq<-matrix(qqq)
qqq<-format(round(qqq,2), nsmall=2)


###loop for writing file to directory
#grid_one<-18013331
dir<-c(1791:1976)

jjj<-pmatch(dir,grid_one) #match the dir to the grid

file_num<-NaN
kkk<-0 ##looping index


while(kkk<187)#187)
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
    dir.create(paste(root,"/tree/",dir[kkk],"/",grid_one, sep="")) 
    ###write file here
    ggg<-paste(root,"/tree/",dir[kkk],"/",grid_one,"/CenW.CL!", sep="")
  write.table(qqq, paste(file=ggg),quote=FALSE, row.names=F, col.names=F, eol="\r\n")
if (file.exists(paste(root,"/tree/",dir[kkk],"/",grid_one, sep="")))
{break
}


    }
}
write(l,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/l")


  l<-l+1
} #end l loop over lats
write(c,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/c")
write(a,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/a")
write(e,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/e")
write(ext,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/ext")
write(domlong,file="/gpfs/pads/projects/emc-gct/users/kanikaj/split/domlong")
  c<-c+0.05
  j<-j+1
  a<-a+0.05
} # end a loop over longs

###############################################################################################






