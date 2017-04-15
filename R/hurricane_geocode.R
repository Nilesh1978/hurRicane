#' Convert a storm center and wind radius into a set of arc points
#'
#' This function's primary purpose is to transform hurricane data
#' (longitude/latitude of eye and distance of wind speeds) into a set of points
#' outlining an arc for each provided wind speed while preserving the 
#' actual longitude and latitude of where the winds radius would reach.
#' The bulk of the work is done by functions from the \code{geosphere} package 
#' to code the distance to proper spacial coordinates. While generating points
#' with this function may be interesting, its true purpose is to support data
#' transforms necessary for \code{geom_hurricane}.
#' 
#' @param storm_data This input is a transformed set of data from the Colorado
#' State compilation of NHC data. The data must have been run through the 
#' \code{\link{tidy_tracks}} function. Alternatively, a different function can
#' be used so long as the input data contains all needed components.
#'
#' @param x This should be the column name of the longitudinal coordinate of the
#' eye of the hurricanes.
#' 
#' @param y This should be the column name of the latitudinal coordinate of the
#' eye of the hurricanes.
#' 
#' @param r This is the name of the column that holds the farthest distance for
#' which the wind speed was recorded. 
#' 
#' @param quadrant This is the name of the column containing the names of the 
#' spatial quadrants on a map. Currently only ne, se, sw, and nw are acceptable
#' entries. 
#' 
#' @param wind_speed This is the name of the column containing the speeds of wind
#' for which measuments exist. There is an important implicit assumption that
#' faster winds will ALWAYS have a smaller \code{r} value than slower winds for 
#' a given \code{quadrant}. This fact is used to create arc point parings.
#'
#' @param arcRes This specifies the number of angular degrees (as in degrees of
#' a circle not of a longitude nor latitude coord) between each point. Lower
#' numbers produce more points, and smoother curves when put on a map.
#' 
#' 
#' @return A data.frame, tibble per \code{dplyr} package.
#'
#' @importFrom geosphere destPoint
#' @importFrom dplyr filter_
#' @importFrom dplyr select_
#' @importFrom dplyr arrange_
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_
#' @export
#'
hurricane_geocode<-function(storm_data,x="longitude",y="latitude",r="wind_radius",
                                  quadrant="quadrant",wind_speed="wind_speed",
                                  arcRes=1){

  #Renaming arguments to make referencing easier.
  storm_data<-dplyr::rename_(storm_data,.dots=stats::setNames(list(x,y,r,quadrant,wind_speed),
                                           c("x","y","r","quadrant","wind_speed")))
  
  # Step 1: Merge data needed for geodesic inputs with each record
  arc_data<-dplyr::data_frame(quadrant=c("ne","se","sw","nw"),
                              start_angle=c(0,90,180,270),
                              end_angle=c(90,180,270,360))
  merged_data<-merge(storm_data,arc_data,by="quadrant")
  
  
  # Step 2: create points for polygon for each speed and the speed above it.
  # This step has two assumptions: 1)faster speed means smaller radius
  #                                2)levels of "wind_speed" are sorted numerically
  # Radius is expected in nautical miles and is converted
  # to meters by multiplying by 1852. Radius (r) will be reported back in nm

  speeds<-unique(merged_data$wind_speed)
  for (j in 1:length(speeds)){
    
    arc_data<-dplyr::filter_(merged_data,
                             ~wind_speed %in% c(speeds[j],speeds[j+1]))
    
  
    for (i in 1:nrow(arc_data)){
      
      if(arc_data$wind_speed[i]==speeds[j]){arc_data$radtype[i]<-"outer"}
      else arc_data$radtype[i]<-"inner"
      
      temp_p<-c(arc_data$x[i],arc_data$y[i])
      temp_d<-arc_data$r[i]*1852
      
    
      #set up sequence of angles; c() and call to unique()
      #put the "end_angle" in sequence and remove if
      #duplicate.
      temp_s<-unique(c(seq(arc_data$start_angle[i],
                         arc_data$end_angle[i],
                         by=arcRes),arc_data$end_angle[i]))
    
      #Finally geocode arcs for plotting
      points<-data.frame(geosphere::destPoint(p=temp_p,
                                 d=temp_d,
                                 b=temp_s))
      points$angle<-temp_s

      points[,names(arc_data)]<-arc_data[i,]
      points$wind_speed<-speeds[j]
      if (j==1&&i==1) out<-points
      else out<-dplyr::bind_rows(out,points)
  }#end for loop
} #End outer for loop 
  #Sort for plotting and remove collection of points at 0 (radius==zero)

  out$quadrant<-factor(out$quadrant,levels=c("ne","se","sw","nw"))
  out<-dplyr::arrange_(out,~wind_speed,~radtype,~quadrant,~angle)
  out<-dplyr::filter_(out,~r>0)
  
  #Rename the lon/lat from destPoint() to the input variable name usually 
  #   longitude, latitude. Rename storm center coordinates to distinguish.
  out<-dplyr::rename_(out,.dots=stats::setNames(list(~x,~y),c("eye_lon","eye_lat")))
  out<-dplyr::rename_(out,.dots=stats::setNames(list(~lon,~lat),c(x,y)))
  #clear out extra variables
  out$start_angle<-NULL
  out$end_angle<-NULL

  
  return(out)
}#end compute_group