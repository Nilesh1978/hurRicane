#' Creates "tidy" data (long format) for hurricane data.
#' 
#' This function is used to "tidy" the dataset read in by the 
#' \code{\link{read_ext_tracks}} function. Although the assignment this function
#' is for requested to restate the data such that each quadrant had its own column
#' I decided this is just an annoying intermediate step when the ultimate goal is
#' to print the data which would require me to re-tidy the data.
#' 
#' @param ext_tracks This is data as read in via the \code{\link{read_ext_tracks}}
#' function.
#' 
#' @param makeDate This sets whether or not we wish to have the date columns
#' combined into a single date column. 
#' 
#' @param keepcols This is a character vector of the columns from the original data
#' that are desired to be reatined. The defaults represent the minimum items
#' to satifsfy the assignment.
#' 
#' @return A tidy data.frame, tibble per \code{dplyr} package
#' 
#' @importFrom dplyr contains
#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr separate
#' @importFrom lubridate ymd_h
#' @importFrom stats setNames
#' 
#' @export
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
  tidy_tracks<-dplyr::select_(tidy_tracks,.dots=stats::setNames(keepcols,keepcols))
  
  return(tidy_tracks)
}
