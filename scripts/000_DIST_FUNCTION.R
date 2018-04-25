

DIST_FUNCTION<-function(lon1,lat1,lon2,lat2,threshold,matrix){

  ##############################################
  # CALL LIBRARIES
  ############################################## 
  require(stringdist);require(psych);library(geosphere)
  
  ##############################################    
  #CONVERTING TO NUMERIC
  ############################################## 
  lon1<-as.numeric(as.character(lon1));  lat1<-as.numeric(as.character(lat1));
  lon2<-as.numeric(as.character(lon2));  lat1<-as.numeric(as.character(lat2)); 
  
  ##############################################    
  #CHOOSING AN OUTPUT
  ##############################################   

  if(matrix==TRUE |matrix==T){
  ##############################################
  #CREATING THE FINAL MATRIX
  ############################################## 
   S<-as.data.frame(matrix(ncol = 2,nrow = 1))
  colnames(S)<-c("DIST","THRESHOLD")
  ##############################################
  #CALCULATING THE MATRIX
  ############################################## 
  DISM<-distm(c(lon1,lat1),c(lon2,lat2),distHaversine)/1000
  ##############################################   
  S[1,1]<-DISM
  
  ############################################## 
  #THRESHOLD#
  ############################################## 
  if(is.na(lon1)|is.na(lat1)|is.na(lon2)|is.na(lat2)){
    S[1,2]<-NA
    }else if(DISM>threshold){
      S[1,2]<-0
    }else if(DISM<=threshold){
      S[1,2]<-1
    }  
   
  }else if(matrix==FALSE | matrix==F) {
    
    DISM<-distm(c(lon1,lat1),c(lon2,lat2),distHaversine)/1000
    ##############################################   
    S<-DISM
    
    ############################################## 
    #THRESHOLD#
    ############################################## 
    if(is.na(lon1)|is.na(lat1)|is.na(lon2)|is.na(lat2)){
      S<-NA
    }else if(DISM>threshold){
      S<-0
    }else if(DISM<=threshold){
      S<-1
    }  
  
   }
  return(S)
  } 

#    
# lon1<-RAW$LON[[1]]
# lat1<-RAW$LAT[[1]]  
# 
# lon2<-RDGI$LON[[1]]
# lat2<-RDGI$LAT[[1]]  
# 
# threshold<-10 #Km
#   
#      
#     
#   
# x<-DIST_FUNCTION(lon1,lat1,lon2,lat2,threshold)
#   

