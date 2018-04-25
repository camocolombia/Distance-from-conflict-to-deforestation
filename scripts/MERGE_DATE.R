require(lubridate)

terra<-read.csv("X:/DIST_RASTER/TERRA-i/Terra_Day.csv",header=T,sep="|")
terra$DATE<-as.Date(terra$DATE,"%m/%d/%Y")



def_dir<-"X:/DIST_RASTER/TERRA-i/COORDS/RASTER_TO_POINTS/NA_FUN"
def_dir_files<-list.files(def_dir,".csv$",F)
out_dir<-"X:/DIST_RASTER/TERRA-i/COORDS/RASTER_TO_POINTS/VALUES"



lapply()
def<-read.csv(paste0(def_dir,"/",def_dir_files[[i]]),header=T);gc()
colnames(def)<-c("LON","LAT","VALUE")

def<-merge(x=def,y=terra,by.x = "VALUE",by.y ="TERRA")
def<-def[,c("LON","LAT","DATE")]
write.csv(def,paste0(out_dir,"/",def_dir_files[[i]]),quote=F,row.names=F,na="")







