require(lubridate);require(sp);require(geosphere);library(rgeos);require(raster);require(dismo);require(ggplot2)



GAD<-shapefile("X:/DIST_RASTER/REGION_UCLA/region.shp")

GAD<-subset(GAD, REGION %in% c("Middle Africa","Northern Africa","Southern Africa","Western Africa","Eastern Africa"))

OUT_DIR<-"X:/DIST_RASTER/TERRA-i/GRAPHICS"









########################

conflict<-read.csv("X:/DIST_RASTER/TERRA-i/CONFLICT/ged171.csv",header=T)

conflict$date_start<-as.Date(conflict$date_start,"%Y-%m-%d")
conflict$date_end<-as.Date(conflict$date_end,"%Y-%m-%d")
conflict<-conflict[conflict$date_start>date("2002-12-31"),]
conflict<-conflict[,c("id","longitude", "latitude","date_start","date_end")]



# coordinates(conflict)<-~longitude+latitude
# proj4string(conflict) = "+proj=longlat +datum=WGS84"

#conflict_dum<-conflict

i=1
input_dir<-"X:/DIST_RASTER/TERRA-i/COORDS/RASTER_TO_POINTS/VALUES"
input_dir_files<-list.files(input_dir,".csv$",F)

def<-read.csv(paste0(input_dir,"/",input_dir_files[[i]]),header=T);gc()
def$id<-1:nrow(def)
colnames(def)<-c("LON","LAT","VALUE","id")
def$VALUE<-as.Date(def$VALUE,"%Y-%m-%d")
def<-def[,c("id","LON","LAT","VALUE")]
# coordinates(def)<-~LON+LAT
# proj4string(def) = "+proj=longlat +datum=WGS84"


#def_dum<-def


# dummyset1= def
# dummyset2= conflict
# 
# 
coordinates(def) = c('LON', 'LAT')
coordinates(conflict) = c('longitude', 'latitude')
proj4string(def) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(conflict) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# 
# set1sp = def
# set2sp = conflict
# set1 = dummyset1
# set2 = dummyset2
# 
# 
# 

ovr <- over(conflict, GAD)
cntr <- ovr$REGION
i <- which(!is.na(cntr))
conflict<-conflict[i,]

conflict$dist<-NA
conflict$def_id<-NA

for(i in 1:length(conflict$id)){
  cat(round(i/length(conflict$id)*100,2)," %","\n")
  conflict$dist[[i]]<-min(distm(conflict[i,],def)/1000,na.rm=T)
  conflict$def_id[[i]]<-which.min(distm(conflict[i,],def)/1000)
  
};gc()

###################################################################
###################################################################
###################################################################
###################################################################

conflict2<-conflict
###################################################################
###################################################################
###################################################################
###################################################################

conflict2<-merge(conflict,def,by.x = "def_id",by.y ="id" )
conflict2$dif_date<-NA
for(i in 1:length(conflict2$id)){
  cat(round(i/length(conflict2$id)*100,2)," %","\n")
  
  x<-difftime(conflict2$VALUE[[i]], conflict2$date_end[[i]])
if(as.numeric(x)<0){
  conflict2$dif_date[[i]]<-NA
  
}else{
  conflict2$dif_date[[i]]<-x
  }
}

#########################
conflict2<-conflict2[!is.na(conflict2$dif_date),]

ovr_region<-over(conflict2,GAD)

conflict2$REGION<-NA
conflict2$REGION<-ovr_region$REGION

x=as.numeric(conflict2$dist)
y=as.numeric(conflict2$dif_date)
Region<-conflict2$REGION
DF <- data.frame(x, y,Region)

####################

m2 = lm(y ~ x, data=DF)
summary(m2)$r.squared 


####################
find_hull <- function(df) df[chull(df$x, df$y), ]
library(plyr)
hulls <- ddply(DF, "Region", find_hull)
               
##############
   
  
  

p<-ggplot(data=DF,aes(x=x ,y=y,colour=Region))+
  xlim(c(0,100))+
  ylim(c(0,365))+
    geom_polygon(data=hulls,aes(fill=Region), alpha = 0.3,)+ 
  geom_point() +
  stat_smooth(method = "loess",colour="black") +
  geom_smooth(method=lm,colour="black",linetype = "dashed")+
  xlab("Distance from conflict to deforestation event (Km)") +
  ylab("Days from conflict until deforestation detection") + 
  annotate("text", x=600, y=900, label = "R^2 ==  0.020", parse=T, size = 18) +
  
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

 p 

ggsave(paste0(OUT_DIR,"/","Africa_POS",".pdf"),p,dpi=600,width =28,height=25,units = "cm",scale=1.2)


boxplot(y~Region)
boxplot(x~Region)

hist(x)
hist(y)


library(scales);library(date)
library(ggplot2)
hist1<-ggplot(conflict2@data,aes(x=date_end, group=REGION,colour=REGION,fill=REGION))+
  geom_freqpoly(binwidth=100, alpha=1) +
  #stat_bin(aes(fill=REGION), binwidth=1, alpha=0.5,
           #position="identity") 



 #theme_bw()+
  xlab("conflict date")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b %y"))+
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#hist1


ggsave(paste0(OUT_DIR,"/","Africa_POS_HIST_CONF",".pdf"),hist1,dpi=600,width =90,height=25,units = "cm",scale=1.2)
####
hist11<-ggplot(conflict2@data,aes(x=VALUE, group=REGION,colour=REGION,fill=REGION))+
  geom_freqpoly(binwidth=100, alpha=1) +
  #stat_bin(aes(fill=REGION), binwidth=1, alpha=0.5,
  #position="identity") 
  
  
  
  #theme_bw()+
  xlab("Deforestation detection date")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b %y"))+
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#hist1


ggsave(paste0(OUT_DIR,"/","Africa_POS_HIST_DETECTION",".pdf"),hist11,dpi=600,width =90,height=25,units = "cm",scale=1.2)






###################################################################
###################################################################
###################################################################
###################################################################

conflict3<-conflict
###################################################################
###################################################################
###################################################################
###################################################################

conflict3<-merge(conflict3,def,by.x = "def_id",by.y ="id" )
conflict3$dif_date<-NA
for(i in 1:length(conflict3$id)){
  cat(round(i/length(conflict3$id)*100,2)," %","\n")
  

#   if(as.numeric(x)<0){
#     conflict2$dif_date[[i]]<-NA
#     
#   }else{
conflict3$dif_date[[i]]<-difftime(conflict3$VALUE[[i]], conflict3$date_end[[i]])
#  }
}

#########################
conflict3<-conflict3[!is.na(conflict3$dif_date),]

ovr_region<-over(conflict3,GAD)

conflict3$REGION<-NA
conflict3$REGION<-ovr_region$REGION

x=as.numeric(conflict3$dist)
y=as.numeric(conflict3$dif_date)
Region<-conflict3$REGION
DF <- data.frame(x, y,Region)

####################

m2 = lm(y ~ x, data=DF)
summary(m2)$r.squared 


####################
find_hull <- function(df) df[chull(df$x, df$y), ]
library(plyr)
hulls <- ddply(DF, "Region", find_hull)

##############




p2<-ggplot(data=DF,aes(x=x ,y=y,colour=Region))+
  geom_polygon(data=hulls,aes(fill=Region), alpha = 0.3,)+ 
  geom_point() +
  stat_smooth(method = "loess",colour="black") +
  geom_smooth(method=lm,colour="black",linetype = "dashed")+
  xlab("Distance from conflict to deforestation event (Km)") +
  ylab("Days from conflict until deforestation detection") + 
  annotate("text", x=600, y=900, label = "R^2 ==  0.00003", parse=T, size = 18) +
  
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#p2 

ggsave(paste0(OUT_DIR,"/","Africa_ALL",".pdf"),p2,dpi=600,width =28,height=25,units = "cm",scale=1.2)


# 
# 
# boxplot(y~Region)
# boxplot(x~Region)
# 
# 
# hist(x)
# hist(y)



library(scales);library(date)
library(ggplot2)
hist1<-ggplot(conflict3@data,aes(x=date_end, group=REGION,colour=REGION,fill=REGION))+
  geom_freqpoly(binwidth=100, alpha=1) +
  #stat_bin(aes(fill=REGION), binwidth=1, alpha=0.5,
  #position="identity") 
  
  
  
  #theme_bw()+
  xlab("conflict date")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b %y"))+
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#hist1


ggsave(paste0(OUT_DIR,"/","Africa_ALL_HIST_CONF",".pdf"),hist1,dpi=600,width =300,height=25,units = "cm",scale=1.2,limitsize = FALSE)
####
hist11<-ggplot(conflict3@data,aes(x=VALUE, group=REGION,colour=REGION,fill=REGION))+
  geom_freqpoly(binwidth=100, alpha=1) +
  #stat_bin(aes(fill=REGION), binwidth=1, alpha=0.5,
  #position="identity") 
  
  
  
  #theme_bw()+
  xlab("Deforestation detection date")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b %y"))+
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#hist1


ggsave(paste0(OUT_DIR,"/","Africa_ALL_HIST_DETECTION",".pdf"),hist11,dpi=600,width =90,height=25,units = "cm",scale=1.2)


###################################################################
###################################################################
###################################################################
###################################################################

conflict4<-conflict
###################################################################
###################################################################
###################################################################
###################################################################

conflict4<-merge(conflict4,def,by.x = "def_id",by.y ="id" )
conflict4$dif_date<-NA
for(i in 1:length(conflict4$id)){
  cat(round(i/length(conflict4$id)*100,2)," %","\n")
  
  x<-difftime(conflict4$VALUE[[i]], conflict4$date_end[[i]])
  
    if(as.numeric(x)>=0){
      conflict4$dif_date[[i]]<-NA
      
    }else{
      conflict4$dif_date[[i]]<-difftime(conflict4$VALUE[[i]], conflict4$date_end[[i]])
   }
}

#########################
conflict4<-conflict4[!is.na(conflict4$dif_date),]

ovr_region<-over(conflict4,GAD)

conflict4$REGION<-NA
conflict4$REGION<-ovr_region$REGION

x=as.numeric(conflict4$dist)
y=as.numeric(conflict4$dif_date)
Region<-conflict4$REGION
DF <- data.frame(x, y,Region)

####################

m2 = lm(y ~ x, data=DF)
summary(m2)$r.squared 


####################
find_hull <- function(df) df[chull(df$x, df$y), ]
library(plyr)
hulls <- ddply(DF, "Region", find_hull)

##############




p2<-ggplot(data=DF,aes(x=x ,y=y,colour=Region))+
  geom_polygon(data=hulls,aes(fill=Region), alpha = 0.3,)+ 
  geom_point() +
  stat_smooth(method = "loess",colour="black") +
  geom_smooth(method=lm,colour="black",linetype = "dashed")+
  xlab("Distance from conflict to deforestation event (Km)") +
  ylab("Days from conflict until deforestation detection") + 
  annotate("text", x=600, y=900, label = "R^2 ==  0.0002", parse=T, size = 18) +
  
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

p2 

ggsave(paste0(OUT_DIR,"/","Africa_NEG",".pdf"),p2,dpi=600,width =28,height=25,units = "cm",scale=1.2)




# boxplot(y~Region)
# boxplot(x~Region)
# 
# 
# hist(x)
# hist(y)



library(scales);library(date)
library(ggplot2)
hist1<-ggplot(conflict4@data,aes(x=date_end, group=REGION,colour=REGION,fill=REGION))+
  geom_freqpoly(binwidth=100, alpha=1) +
  #stat_bin(aes(fill=REGION), binwidth=1, alpha=0.5,
  #position="identity") 
  
  
  
  #theme_bw()+
  xlab("conflict date")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b %y"))+
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#hist1


ggsave(paste0(OUT_DIR,"/","Africa_NEG_HIST_CONF",".pdf"),hist1,dpi=600,width =300,height=25,units = "cm",scale=1.2,limitsize = FALSE)
####
hist11<-ggplot(conflict4@data,aes(x=VALUE, group=REGION,colour=REGION,fill=REGION))+
  geom_freqpoly(binwidth=100, alpha=1) +
  #stat_bin(aes(fill=REGION), binwidth=1, alpha=0.5,
  #position="identity") 
  
  
  
  #theme_bw()+
  xlab("Deforestation detection date")+
  ylab("Number of cases")+
  scale_x_date(breaks=date_breaks("2 month"), labels=date_format("%b %y"))+
  theme(plot.title = element_text(size=38),
        
        panel.background = element_rect(fill = "gray95"),
        text=element_text(size=38),
        axis.text.x  = element_text(size=30,colour="black"),
        axis.text.y  = element_text(size=30,colour="black"))

#hist1


ggsave(paste0(OUT_DIR,"/","Africa_NEG_HIST_DETECTION",".pdf"),hist11,dpi=600,width =90,height=25,units = "cm",scale=1.2)










    ???      +#,xlim=c(-2,5),ylim=c(-2,8))+
#p<-p+geom_abline(intercept =l$coefficients[1],slope=l$coefficients[2],colour="red")+
 # geom_hline(yintercept=0,0,linetype="dotted")+
 # geom_vline(xintercept =0, 0,linetype="dotted")+
  #
#   stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
#   stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
#   stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
#   stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), aes(colour = 'Exponential'), se = FALSE, start = list(a=1,b=1))+

  
 # ggtitle(paste0("Moran`s I: "," ",round(as.numeric(l$coefficients[2]),3)))+
  


# for (i in 1:length(set1$id)){
#   
#  cat(round(i/length(set1$id)*100,2)," %","\n")
#  
#   #Store the projected data in a dummy variable sub
#   sub <- set2sp
#   #for (j in 4:8){
#     #if (j == 4){
#   j=5
#       set1[i,j] <- apply(gDistance(set2sp['id'], set1sp['id'][i,], byid=TRUE), 1, which.min)
#       #Remove the index of the closest point from sub.
#       sub <- sub[which(sub$id != set1[i,j]), ]
#     #}
#    # else {
#       #Note that sub is now being checked instead of set2sp. This is because sub has had the index of the closest point removed.
#       #set1[i,j] <- apply(gDistance(sub['id'], set1sp['id'][i,], byid=TRUE), 1, which.min)
#      # sub <- sub[which(sub$id != set1[i,j]), ]
#    # }
#       
#   #}
# }

