#'
#'
#'
#'
#'
tidy_tracks<-function(ext_tracks,makeDate=TRUE){

  #Step 1: Put data from wide to long gathering radius columns into a single column
  gather_cols<-names(ext_tracks)[dplyr::contains(match="radius_",vars=names(ext_tracks))]
  
  tidy_tracks<-tidyr::gather_(ext_tracks,key_col="speed_quadrant",
                              value_col="wind_outer_rad",gather_cols=gather_cols,
                              na.rm=FALSE,factor_key=FALSE)
  
  tidy_tracks<-tidyr::separate_(tidy_tracks,col="speed_quadrant",
                                into=c("drop","wind_speed","quadrant"),
                                sep="_",remove=TRUE,convert=TRUE)
  #drops the unusued name column
  tidy_tracks$drop<-NULL
  
  #create a single time field for ease of equivalence checking
  if (makeDate){tidy_tracks$datehour<-lubridate::ymd_h(paste(tidy_tracks$year,
                                               tidy_tracks$month,
                                               tidy_tracks$day,
                                               tidy_tracks$hour,sep=""))
  }
  
  #Step 2: Join data to itself so that we have the nearest and furthest distance for the 
  #the given windspeed.This routine depends on each storm having identical windspeed
  #measurements (currently c(34, 50, 64))
  
  #set a column for the the directly smaller radius (directly faster wind).
  #directly smaller for smallest radius (fastest wind) will be set to 0 at end.
  speeds<-c(unique(tidy_tracks$wind_speed),max(tidy_tracks$wind_speed))
  tidy_tracks$wind_speed2<-speeds[match(tidy_tracks$wind_speed,speeds)+1]
  
  #Create duplicate to self-join (for future look into data.table as has self join)
  #.dots=setNames used to rename wind_speed to wind_speed2 so that wind_long_rad
  #of faster speeds (shorter radii) will get paired into a wind_short_rad field
  tidy_tracks2<- dplyr::select_(tidy_tracks,~storm_id,~datehour,~quadrant,
                                .dots=setNames(list(~wind_speed,~wind_outer_rad),
                                               c("wind_speed2","wind_inner_rad")))
  
  #Join to self, set fastest wind inner radii = 0, 
  #remove wind_speed2, and replace na radius with 0
  tidy_tracks3<-dplyr::left_join(tidy_tracks,tidy_tracks2)
  tidy_tracks3$wind_inner_rad[tidy_tracks3$wind_speed==tidy_tracks3$wind_speed2]<-0
  tidy_tracks3$wind_speed2<-NULL
  tidy_tracks3$wind_inner_rad[is.na(tidy_tracks3$wind_inner_rad)]<-0
  
  #Added a class to the end because it won't control any methods, but will be used
  #to check class exists to prevent calls to stat_hurricane for unclean/non-hurricane
  # data
  attr(tidy_tracks3,"class")<-c(attr(tidy_tracks3,"class"),"tidy_hur_df")
  
  return(tidy_tracks3)
}
