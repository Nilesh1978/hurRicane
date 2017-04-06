#
#
trans_geodesic<-function(data,x="longitude",y="latitude",r_in="wind_inner_rad",
                       r_out="wind_outer_rad",quadrant="quadrant",
                       wind_speed="wind_speed",arcRes=1){
  
  data<-dplyr::select_(data,.dots=setNames(list(x,y,r_in,r_out,quadrant,wind_speed),
                                           c("x","y","r_in","r_out","quadrant","wind_speed")))
  
  # Step 1: Merge geodesic inputs with each record
  arc_data<-dplyr::data_frame(quadrant=c("ne","se","sw","nw"),
                              start_angle=c(0,90,180,270),
                              end_angle=c(90,180,270,360))
  arc_data<-merge(data,arc_data,by="quadrant")
  
  # Step 2: create points for polygon for two arc
  # Radius is expected in nautical miles and is converted
  # to meters by multiplying by 1852. Generalize?
  for (i in 1:nrow(arc_data)){
    temp_p<-c(arc_data$x[i],arc_data$y[i])
    temp_d1<-arc_data$r_in[i]*1852
    temp_d2<-arc_data$r_out[i]*1852
    
    #set up sequence of angles; c() and call to unique()
    #put the "end_angle" in sequence and remove if
    #duplicate.
    temp_s1<-unique(c(seq(arc_data$start_angle[i],
                         arc_data$end_angle[i],
                         by=arcRes),arc_data$end_angle[i]))
    
    #This was necessary to plot the polygon in the right order;
    temp_s2<-temp_s1
    
    points1<-data.frame(geosphere::geodesic(p=temp_p,
                                 d=temp_d1,
                                 azi=temp_s1))
    points1$angle<-temp_s1
    points1$radtype<-"inner"
    points1$rad<-temp_d1
    points1<-points1[order(points1$angle),]
    
    points2<-data.frame(geosphere::geodesic(p=temp_p,
                                 d=temp_d2,
                                 azi=temp_s2))
    points2$angle<-temp_s2
    points2$radtype<-"outer"
    points2$rad<-temp_d2
    points2<-points2[order(points1$angle,decreasing = TRUE),]
    
    points<-dplyr::bind_rows(points1,points2)
    points[,names(arc_data)]<-arc_data[i,]
    if (i==1) out<-points
    if(i>1) out<-dplyr::bind_rows(out,points)
  }#end for loop
  out$r_in<-NULL
  out$r_out<-NULL
  out$start_angle<-NULL
  out$end_angle<-NULL
  out$azimuth<-NULL
  out$wind_speed<-as.factor(out$wind_speed)
  return(out)
}#end compute_group