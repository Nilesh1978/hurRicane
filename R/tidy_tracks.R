#' Creates long from wide keeping variables designates; adds inner and outer
#' radius for each given wind speed based on the wind radii of adjacent wind speeds
#'
#'
#'
#'
tidy_tracks<-function(ext_tracks,makeDate=TRUE,
                      keepcols=c("storm_name","date","longitude","latitude")){

  #Join the keepcols with the names we are creating to return proper data
  keepcols<-c(keepcols,c("quadrant","wind_speed","wind_radius"))
  #Step 1: Put data from wide to long gathering radius columns into a single column
  gather_cols<-names(ext_tracks)[dplyr::contains(match="radius_",vars=names(ext_tracks))]
  
  tidy_tracks<-tidyr::gather_(ext_tracks,key_col="speed_quadrant",
                              value_col="wind_radius",gather_cols=gather_cols,
                              na.rm=FALSE,factor_key=FALSE)
  
  tidy_tracks<-tidyr::separate_(tidy_tracks,col="speed_quadrant",
                                into=c("drop","wind_speed","quadrant"),
                                sep="_",remove=TRUE,convert=TRUE)
  #drops the unusued name column
  tidy_tracks$drop<-NULL
  
  
  #create a single time field for ease of equivalence checking
  if (makeDate){tidy_tracks$date<-lubridate::ymd_h(paste(tidy_tracks$year,
                                               tidy_tracks$month,
                                               tidy_tracks$day,
                                               tidy_tracks$hour,sep=""))
  }
  
  #Paring down data to specified data.
  tidy_tracks<-dplyr::select_(tidy_tracks,.dots=setNames(keepcols,keepcols))
  
  return(tidy_tracks)
}
