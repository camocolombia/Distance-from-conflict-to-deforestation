require(lubridate);require(sp);require(geosphere);library(rgeos);require(raster);require(dismo);require(ggplot2);require(sf);require(countrycode)
library(spdep)

library(stringr)
library(spdep)
library(rgdal)
library(magrittr)
library(ggplot2)
library(sf)


shp_path<-"X:/DIST_RASTER/GAUL_2014"
layer_name<-"G2014_2013_1.shp"

STATE3<-shapefile(paste0(shp_path,"/",layer_name))

countrycode_data<-read.csv("X:/DIST_RASTER/GAUL_2014/GAUL.csv",header = T)
countrycode_data<-countrycode_data[!is.na(countrycode_data$GAUL),]

con_table<-countrycode::countrycode_data
con_table<-con_table[which(!is.na(con_table$iso3c)),]



STATE3$ISO3<-NA
STATE3$REGION<-NA

for(i in 1:nrow(countrycode_data)){
  STATE3$ISO3[which(as.numeric(STATE3$ADM0_CODE)==as.numeric(countrycode_data$GAUL[[i]]))]<-as.character(countrycode_data$ISO3[[i]])
};rm(i)

for(i in 1:nrow(con_table)){
  STATE3$REGION[which(as.character(STATE3$ISO3)==as.character(con_table$iso3c[[i]]))]<-as.character(con_table$region[[i]])
};rm(i)



conflict<-read.csv("X:/DIST_RASTER/TERRA-i/CONFLICT/ged171.csv",header=T)

conflict$date_start<-as.Date(conflict$date_start,"%Y-%m-%d")
conflict$date_end<-as.Date(conflict$date_end,"%Y-%m-%d")
conflict<-conflict[conflict$date_start>date("2002-12-31"),]
conflict<-conflict[,c("id","longitude", "latitude","date_start","date_end")]


coordinates(conflict) = c('longitude', 'latitude')
proj4string(conflict) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
####
ovr <- over(conflict, STATE3)
conflict@data<-cbind(conflict@data,ovr$ADM1_CODE,ovr$ADM0_CODE,ovr$ISO3,ovr$REGION)
colnames(conflict@data)<-c("id","date_start","date_end","ADM1_CODE","ADM0_CODE","ISO3","REGION")
cntr <- ovr$ISO3
i <- which(!is.na(cntr))
conflict<-conflict[i,]

rm(ovr,cntr,i);gc()

###########

i=1
input_dir<-"X:/DIST_RASTER/TERRA-i/COORDS/RASTER_TO_POINTS/VALUES"
input_dir_files<-list.files(input_dir,".csv$",F)

def<-read.csv(paste0(input_dir,"/",input_dir_files[[i]]),header=T);gc()
def$id<-1:nrow(def)
colnames(def)<-c("LON","LAT","VALUE","id")
def$VALUE<-as.Date(def$VALUE,"%Y-%m-%d")
def<-def[,c("id","LON","LAT","VALUE")]

coordinates(def) = c('LON', 'LAT')
proj4string(def) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

####


ovr <- over(def, STATE3)
def@data<-cbind(def@data,ovr$ADM1_CODE,ovr$ADM0_CODE,ovr$ISO3,ovr$REGION)
colnames(def@data)<-c("id","VALUE","ADM1_CODE","ADM0_CODE","ISO3","REGION")
cntr <- ovr$ISO3
i <- which(!is.na(cntr))
def<-def[i,]

rm(ovr,cntr,i);gc()

#rm(STATE3,con_table,countrycode_data);gc()

year<-c(2004,2005,2006,2007)
#regions<-unique(con_table$region)
regions<-c("Middle Africa","Northern Africa","Southern Africa","Western Africa","Eastern Africa")


#for(region in regions){


region<-regions[[1]]
countries<-con_table$iso3c[which(con_table$region==region)]


conf_2<-subset(conflict, REGION %in% region)
conf_2$YEAR<-NA;conf_2$YEAR<-lubridate::year(conf_2$date_start)

def_2<-subset(def, REGION %in% region)
def_2$YEAR<-NA;def_2$YEAR<-lubridate::year(def_2$VALUE)

conf_2<-subset(conf_2, YEAR %in% year)
def_2<-subset(def_2, YEAR %in% year)


countries_shps<-lapply(1:length(countries),function(j){

  country<-countries[[j]]
  #country<-countries[[9]]
  cat("                                                                 ","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("Obtaining data for: ",as.character(country),"\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("                                                                 ","\n")
  
  
  

conf_3<-subset(conf_2, ISO3 %in% country)
def_3<-subset(def_2, ISO3 %in% country)



adm1s<-unique(STATE3$ADM1_CODE[which(STATE3$ISO3==country)])

data_adm1<-lapply(1:length(adm1s),function(adm1){

adm1_c<-adm1s[[adm1]]
#adm1_c<-adm1s[[18]]

cat("#############################################################","\n")
cat("#############################################################","\n")
cat("#############################################################","\n")
cat(adm1," of ",length(adm1s)," | ",(adm1/length(adm1s)*100)," %","\n")
cat("#############################################################","\n")
cat("#############################################################","\n")

cat(as.character(adm1_c),"\n")
conf_4<-subset(conf_3, ADM1_CODE %in% adm1_c)#adm1s[[15]])
def_4<-subset(def_3, ADM1_CODE %in% adm1_c)#adm1s[[15]])

cat(country," | ",adm1_c," | ",length(conf_4)," | ",length(def_4),"\n")

if(length(conf_4)>0 & length(def_4)>0){
 
conf_4$dist<-NA
conf_4$def_id<-NA
# 
def_4$dist<-NA
def_4$conf_id<-NA

cat("                                                                 ","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("calculating minimum distance from deforestation to conflict event","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("                                                                 ","\n")


for(i in 1:length(def_4)){
 # cat(round(i/length(def_4$id)*100,4)," %"," | ",i,"\n")
  def_4$dist[[i]]<-min(distm(def_4[i,],conf_4)/1000,na.rm=T)
  #def_4$conf_id[[i]]<-which.min(distm(def_4[i,],conf_4)/1000)
  def_4$conf_id[[i]]<-conf_4$id[which.min(distm(def_4[i,],conf_4)/1000)]
};rm(i)

cat("                                                                 ","\n")
cat("Minimum distance DONE!","\n")
cat("                                                                 ","\n")



def_4a<-merge(def_4@data,conf_4@data,by.x = "conf_id",by.y ="id")
def_4$date_start<-def_4a$date_start
def_4$date_end<-def_4a$date_end


def_4$dif_date<-NA

cat("                                                                 ","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("calculating difference date from deforestation to conflict event","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("                                                                 ","\n")

for(i in 1:length(def_4$id)){
  #cat(round(i/length(def_4$id)*100,2)," %","\n")
  
  x<-difftime(def_4$VALUE[[i]], def_4$date_end[[i]])
  #if(as.numeric(x)<0){
    #def_4$dif_date[[i]]<-NA
    
 # }else{
    def_4$dif_date[[i]]<-as.numeric(x)
  #}
};rm(i)


cat("                                                                 ","\n")
cat("time difference  DONE!","\n")
cat("                                                                 ","\n")


cat("                                                                 ","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("saving data for:", country," | ",as.character(adm1_c),"\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("                                                                 ","\n")

if(length(def_4$dist)>=5 & length(def_4$dif_date)>=5){
tot_cor<-as.numeric(cor.test(x=def_4$dist,y=def_4$dif_date,method = "pearson",alternative = 'two.sided')[[4]])
tot_p<-as.numeric(cor.test(x=def_4$dist,y=def_4$dif_date,method = "pearson",alternative = 'two.sided')[[3]])
}else{
  tot_cor<-NA
  tot_p<-NA
}

if(length(def_4$dist[which(def_4$dif_date>-0.1)])>=5 & length(def_4$dif_date[which(def_4$dif_date>-0.1)])>=5){
  
  pos_cor<-as.numeric(cor.test(x=def_4$dist[which(def_4$dif_date>-0.1)],y=def_4$dif_date[which(def_4$dif_date>-0.1)],method = "pearson",alternative = 'two.sided')[[4]])
  pos_p<-as.numeric(cor.test(x=def_4$dist[which(def_4$dif_date>-0.1)],y=def_4$dif_date[which(def_4$dif_date>-0.1)],method = "pearson",alternative = 'two.sided')[[3]])
}else{
  pos_cor<-NA
  pos_p<-NA
}

if(length(def_4$dist[which(def_4$dif_date<0)])>=5 & length(def_4$dif_date[which(def_4$dif_date<01)])>=5){
  
  neg_cor<-as.numeric(cor.test(x=def_4$dist[which(def_4$dif_date<0)],y=def_4$dif_date[which(def_4$dif_date<0)],method = "pearson",alternative = 'two.sided')[[4]])
  neg_p<-as.numeric(cor.test(x=def_4$dist[which(def_4$dif_date<0)],y=def_4$dif_date[which(def_4$dif_date<0)],method = "pearson",alternative = 'two.sided')[[3]])
}else{
  neg_cor<-NA
  neg_p<-NA
}







sum_table<-as.data.frame(cbind(
as.numeric(adm1_c),
country,
as.numeric(nrow(def_4)),  
as.numeric(nrow(conf_4)),
as.numeric(length(def_4$dif_date[which(def_4$dif_date>-0.1)])),  
as.numeric(length(def_4$dif_date[which(def_4$dif_date<0)])),
as.numeric(length(def_4$dist[which(def_4$dif_date>-0.1)])),  
as.numeric(length(def_4$dist[which(def_4$dif_date<0)])),

as.numeric(year),

as.numeric(median(def_4$dif_date,na.rm=T)),
as.numeric(median(def_4$dist,na.rm=T)),
as.numeric(median(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(median(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T)),
as.numeric(median(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(median(def_4$dist[which(def_4$dif_date<0)],na.rm=T)),

as.numeric(mean(def_4$dif_date,na.rm=T)),
as.numeric(mean(def_4$dist,na.rm=T)),
as.numeric(mean(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(mean(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T)),
as.numeric(mean(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(mean(def_4$dist[which(def_4$dif_date<0)],na.rm=T)),
                                                                                                                                 
as.numeric(sd(def_4$dif_date,na.rm=T)),
as.numeric(sd(def_4$dist,na.rm=T)),
as.numeric(sd(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(sd(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T)),
as.numeric(sd(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(sd(def_4$dist[which(def_4$dif_date<0)],na.rm=T)),

as.numeric(cv(def_4$dif_date,na.rm=T)),
as.numeric(cv(def_4$dist,na.rm=T)),
as.numeric(cv(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(cv(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T)),
as.numeric(cv(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T)),
as.numeric(cv(def_4$dist[which(def_4$dif_date<0)],na.rm=T)),

as.numeric(tot_cor),
as.numeric(tot_p),
as.numeric(pos_cor),
as.numeric(pos_p),
as.numeric(neg_cor),
as.numeric(neg_p)
  )
)

colnames(sum_table)<-c(
  "ADM1_CODE",
  "ISO3",
  "DEF_TOTAL",
  "CONF_TOTAL",
  "DEF_TIME_POS",
  "DEF_TIME_NEG",
  "DIST_POS",
  "DIST_NEG",
  "YEAR",
  
  "MEDIAN_TIME_TOTAL",
  "MEDIAN_DIST_TOTAL",
  "MEDIAN_TIME_POS",
  "MEDIAN_TIME_NEG",
  "MEDIAN_DIST_POS",
  "MEDIAN_DIST_NEG",

  "MEAN_TIME_TOTAL",
  "MEAN_DIST_TOTAL",
  "MEAN_TIME_POS",
  "MEAN_TIME_NEG",
  "MEAN_DIST_POS",
  "MEAN_DIST_NEG",
  
  "SD_TIME_TOTAL",
  "SD_DIST_TOTAL",
  "SD_TIME_POS",
  "SD_TIME_NEG",
  "SD_DIST_POS",
  "SD_DIST_NEG", 
  
  "CV_TIME_TOTAL",
  "CV_DIST_TOTAL",
  "CV_TIME_POS",
  "CV_TIME_NEG",
  "CV_DIST_POS",
  "CV_DIST_NEG",  
  
  "COR_TOTAL",
  "PVAL_TOTAL",
  "COR_POS",
  "PVAL_POS",
  "COR_NEG"  ,
  "PVAL_NEG"
  )
return(sum_table)
}else{
  
  cat("                                                                 ","\n")
  cat("SKIP","\n")
  cat("                                                                 ","\n")
  
  cat("                                                                 ","\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("saving data for:", as.character(adm1_c),"\n")
  cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
  cat("                                                                 ","\n")
  
  sum_table<-as.data.frame(cbind(
    as.numeric(adm1_c),
    country,
    as.numeric(nrow(def_4)),  
    as.numeric(nrow(conf_4)),
    NA,# length(def_4$dif_date[which(def_4$dif_date>-0.1)]),  
    NA,# length(def_4$dif_date[which(def_4$dif_date<0)]),
    NA,# length(def_4$dist[which(def_4$dif_date>-0.1)]),  
    NA,# length(def_4$dist[which(def_4$dif_date<0)]),
    # 
    NA,# as.numeric(year),
    # 
    NA,# median(def_4$dif_date,na.rm=T),
    NA,# median(def_4$dist,na.rm=T),
    NA,# median(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# median(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T),
    NA,# median(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# median(def_4$dist[which(def_4$dif_date<0)],na.rm=T),
    # 
    NA,# mean(def_4$dif_date,na.rm=T),
    NA,# mean(def_4$dist,na.rm=T),
    NA,# mean(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# mean(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T),
    NA,# mean(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# mean(def_4$dist[which(def_4$dif_date<0)],na.rm=T),
    # 
    NA,# sd(def_4$dif_date,na.rm=T),
    NA,# sd(def_4$dist,na.rm=T),
    NA,# sd(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# sd(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T),
    NA, # sd(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# sd(def_4$dist[which(def_4$dif_date<0)],na.rm=T),
    # 
    NA,# cv(def_4$dif_date,na.rm=T),
    NA,# cv(def_4$dist,na.rm=T),
    NA,# cv(def_4$dif_date[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# cv(def_4$dif_date[which(def_4$dif_date<0)],na.rm=T),
    NA,# cv(def_4$dist[which(def_4$dif_date>-0.1)],na.rm=T),
    NA,# cv(def_4$dist[which(def_4$dif_date<0)],na.rm=T),
    # 
    NA,# as.numeric(cor.test(x=def_4$dist,y=def_4$dif_date,method = "pearson",alternative = 'two.sided')[[4]]),
    NA,# as.numeric(cor.test(x=def_4$dist,y=def_4$dif_date,method = "pearson",alternative = 'two.sided')[[3]]),
    NA,# as.numeric(cor.test(x=def_4$dist[which(def_4$dif_date>-0.1)],y=def_4$dif_date[which(def_4$dif_date>-0.1)],method = "pearson",alternative = 'two.sided')[[4]]),
    NA,# as.numeric(cor.test(x=def_4$dist[which(def_4$dif_date<0)],y=def_4$dif_date[which(def_4$dif_date<0)],method = "pearson",alternative = 'two.sided')[[3]])
    NA,
    NA
    
  )
  )
  
  colnames(sum_table)<-c(
    "ADM1_CODE",
    "ISO3",
    "DEF_TOTAL",
    "CONF_TOTAL",
    "DEF_TIME_POS",
    "DEF_TIME_NEG",
    "DIST_POS",
    "DIST_NEG",
    "YEAR",
    
    "MEDIAN_TIME_TOTAL",
    "MEDIAN_DIST_TOTAL",
    "MEDIAN_TIME_POS",
    "MEDIAN_TIME_NEG",
    "MEDIAN_DIST_POS",
    "MEDIAN_DIST_NEG",
    
    "MEAN_TIME_TOTAL",
    "MEAN_DIST_TOTAL",
    "MEAN_TIME_POS",
    "MEAN_TIME_NEG",
    "MEAN_DIST_POS",
    "MEAN_DIST_NEG",
    
    "SD_TIME_TOTAL",
    "SD_DIST_TOTAL",
    "SD_TIME_POS",
    "SD_TIME_NEG",
    "SD_DIST_POS",
    "SD_DIST_NEG", 
    
    "CV_TIME_TOTAL",
    "CV_DIST_TOTAL",
    "CV_TIME_POS",
    "CV_TIME_NEG",
    "CV_DIST_POS",
    "CV_DIST_NEG",  
    
    "COR_TOTAL",
    "PVAL_TOTAL",
    "COR_POS",
    "PVAL_POS",
    "COR_NEG"  ,
    "PVAL_NEG"
    
  )
  return(sum_table)  
}
cat("                                                             ","\n")
cat("#############################################################","\n")
cat("#############################################################","\n")
cat("                                                             ","\n")
cat("         DONE!                                               ","\n")
cat("                                                             ","\n")
cat("#############################################################","\n")
cat("#############################################################","\n")
cat("                                                             ","\n")
})

cat("                                                             ","\n")
cat("#############################################################","\n")
cat("                                                             ","\n")
cat("         CALLING SUMMARY TABLE FOR: ",as.character(country),"\n")
cat("                                                             ","\n")
cat("#############################################################","\n")
cat("                                                             ","\n")

data_adm1s<-do.call(rbind,data_adm1)


data_adm1s[,1]<-as.numeric(as.character(data_adm1s[,1]))

for(i in 3:ncol(data_adm1s)){

  data_adm1s[,i]<-as.numeric(as.character(data_adm1s[,i]))
};rm(i)

#STATE4<-countries_shps[[6]]
cat("                                                             ","\n")
cat("         CALLING SHAPEFILE  FOR: ",as.character(country),"\n")
cat("                                                             ","\n")

data_adm1s2<-data_adm1s[!duplicated(data_adm1s$ADM1_CODE),]
STATE4<-subset(STATE3, ADM1_CODE  %in% adm1s)
STATE4<-merge(STATE4,data_adm1s2,by="ADM1_CODE")
# 
cat("                                                             ","\n")
cat("         LISA APPROACH RUNNING   FOR: ",as.character(country),"\n")
cat("                                                             ","\n")
if(length(STATE4)>2){
nb <- poly2nb(STATE4,queen=TRUE)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0


# Calculating the index and its simulated distribution
# for global and local values

cat("         USING CONFLICT AND DEFORESTATION COUNTS FOR: ",as.character(country),"\n")
cat("                                                             ","\n")

x<-STATE4$CONF_TOTAL#as.numeric(as.character(STATE4$CONF_TOTAL))
y<-STATE4$DEF_TOTAL#as.numeric(as.character(STATE4$DEF_TOTAL))
m <- moran_I(x, y, W=W)


# Global Moral
global_moran <- m[[1]][1]
STATE4$global_moran<-rep(global_moran,nrow(STATE4))


#> 0.2218409

# Local values
m_i <- m[[2]] 
STATE4$m_i<-m_i
# local simulations
local_sims <- simula_moran(x, y, W)$local_sims


# global pseudo p-value  
# get all simulated global moran
global_sims <- simula_moran(x, y, W)$global_sims

# Proportion of simulated global values taht are higher (in absolute terms) than the actual index 
moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)
STATE4$moran_pvalue<-rep(moran_pvalue,nrow(STATE4))

#> 0

# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)



if(sum(colSums(is.na(local_sims)))==ncol(local_sims)*nrow(local_sims)){
  


# Identifying the LISA clusters



STATE4$global_moran<-rep(NA,nrow(STATE4))
STATE4$moran_pvalue<-rep(NA,nrow(STATE4))
STATE4$xp<-rep(NA,nrow(STATE4))
STATE4$yp<-rep(NA,nrow(STATE4))
STATE4$yp_lag<-rep(NA,nrow(STATE4))
STATE4$m_i<-rep(NA,nrow(STATE4))
STATE4$sig<-rep(NA,nrow(STATE4))
STATE4$patterns_lab<-rep(NA,nrow(STATE4))


}else{
  
  
  intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
  sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )
  
  xp <- scale(x)[,1]
  yp <- scale(y)[,1]

STATE4$xp<-xp
STATE4$yp<-yp
STATE4$yp_lag<-as.numeric(W%*%yp)
STATE4$sig <- sig
patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[STATE4$sig==0] <- "Not significant"

STATE4$patterns_lab<- as.character(factor(STATE4$sig, levels=c("High.High", "High.Low", "Low.High", "Low.Low", "Not significant"),
                          labels=c("High income - High access gain", "High income - Low access gain", "Low income - High access gain","Low income - Low access gain", "Not significant")))


    }
}else{
  
  STATE4$global_moran<-rep(NA,nrow(STATE4))
  STATE4$moran_pvalue<-rep(NA,nrow(STATE4))
  STATE4$xp<-rep(NA,nrow(STATE4))
  STATE4$yp<-rep(NA,nrow(STATE4))
  STATE4$yp_lag<-rep(NA,nrow(STATE4))
  STATE4$m_i<-rep(NA,nrow(STATE4))
  STATE4$sig<-rep(NA,nrow(STATE4))
  STATE4$patterns_lab<-rep(NA,nrow(STATE4))
  
}
return(STATE4)

cat("                                                                 ","\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("shapefile done! for: ",as.character(country),"\n")
cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@","\n")
cat("                                                                 ","\n")

})


countries_shps<-do.call(rbind,countries_shps)

setwd("X:/DIST_RASTER/TERRA-i/SHPS")
outdir<-"X:/DIST_RASTER/TERRA-i/SHPS"
writeOGR(obj=countries_shps, dsn="TEST", layer="narea", driver="ESRI Shapefile") # this is in geographical projection

plot(countries_shps$DEF_TOTAL,countries_shps$CONF_TOTAL)
#}

def_p<-ggplot(countries_shps@data, aes(x =ADM0_NAME, y = DEF_TOTAL)) +
  geom_boxplot(aes(fill=ADM0_NAME),outlier.size=NA,position=position_dodge(width=6))+
  stat_boxplot(geom ='errorbar') +
  # ggtitle("Cacao") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point")+
  
  #scale_fill_manual(labels = c("1","2","3","4"),values = c("#267a09","#ff5500","#98e600","#e69800"))+
  #coord_cartesian(ylim=c(lower.limit, upper.limit))+
  #coord_cartesian(ylim=c(y_min,y_max))+
  #scale_shape_discrete(name="",label=c("Very High","High","Low","Very low"))+
  xlab("Countries")+
  ylab("Deforestation (Counts)")+
  #theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=42),axis.text.x  = element_text(size=42,colour="black"),axis.text.y  = element_text(size=42,colour="black"),legend.position="none")+ 
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=60),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=60,colour="black"),
        axis.title=element_text(size=60,face="bold"),
        axis.text.y  = element_text(size=60,colour="black"),
        legend.position="none") 


ggsave(paste0(outdir,"/",sub(" ","_",region),"_DEF","_",Sys.Date(),".pdf"),def_p,units="in",width=90,height=23,scale=1,dpi=600,limitsize = F)

conf_p<-ggplot(countries_shps@data, aes(x =ADM0_NAME, y = CONF_TOTAL)) +
  geom_boxplot(aes(fill=ADM0_NAME),outlier.size=NA,position=position_dodge(width=6))+
  stat_boxplot(geom ='errorbar') +
  # ggtitle("Cacao") + 
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point")+
  
  #scale_fill_manual(labels = c("1","2","3","4"),values = c("#267a09","#ff5500","#98e600","#e69800"))+
  #coord_cartesian(ylim=c(lower.limit, upper.limit))+
  #coord_cartesian(ylim=c(y_min,y_max))+
  #scale_shape_discrete(name="",label=c("Very High","High","Low","Very low"))+
  xlab("Countries")+
  ylab("Conflicts (Counts)")+
  #theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=42),axis.text.x  = element_text(size=42,colour="black"),axis.text.y  = element_text(size=42,colour="black"),legend.position="none")+ 
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=60),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=60,colour="black"),
        axis.title=element_text(size=60,face="bold"),
        axis.text.y  = element_text(size=60,colour="black"),
        legend.position="none") 


ggsave(paste0(outdir,"/",sub(" ","_",region),"_CONF","_",Sys.Date(),".pdf"),conf_p,units="in",width=90,height=23,scale=1,dpi=600,limitsize = F)



for(i in 16:52){
  p_test<-ggplot(countries_shps@data, aes(x =ADM0_NAME, y = countries_shps@data[,i])) +
    geom_boxplot(aes(fill=ADM0_NAME),outlier.size=NA,position=position_dodge(width=6))+
    stat_boxplot(geom ='errorbar') +
    # ggtitle("Cacao") + 
    stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
    stat_summary(fun.y=mean, geom="point")+
    
    #scale_fill_manual(labels = c("1","2","3","4"),values = c("#267a09","#ff5500","#98e600","#e69800"))+
    #coord_cartesian(ylim=c(lower.limit, upper.limit))+
    #coord_cartesian(ylim=c(y_min,y_max))+
    #scale_shape_discrete(name="",label=c("Very High","High","Low","Very low"))+
    xlab("Countries")+
    ylab(colnames(countries_shps@data)[i])+
    #theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=42),axis.text.x  = element_text(size=42,colour="black"),axis.text.y  = element_text(size=42,colour="black"),legend.position="none")+ 
    theme(panel.background = element_rect(fill = "gray90"),
          text=element_text(size=60),
          #axis.text.x  =element_blank(),
          axis.text.x  = element_text(size=60,colour="black"),
          axis.title=element_text(size=60,face="bold"),
          axis.text.y  = element_text(size=60,colour="black"),
          legend.position="none") 
  
  
  ggsave(paste0(outdir,"/",sub(" ","_",region),"_",colnames(countries_shps@data)[i],"_",Sys.Date(),".pdf"),p_test,units="in",width=90,height=23,scale=1,dpi=600,limitsize = F)
 ########################################  ######################################## 
  ########################################   ######################################## 
  ########################################   ######################################## 
  
};rm(i)

