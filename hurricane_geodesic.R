#
#
hurricane_geodesic<-function(data,x="longitude",y="latitude",r_in="wind_inner_rad",
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
    
    #This wasn't necessary to have two sequences, but I left the ability just incase.
    temp_s2<-temp_s1
    
    #Finally geocode arcs for plotting
    inner_pts<-data.frame(geosphere::destPoint(p=temp_p,
                                 d=temp_d1,
                                 b=temp_s1))
    inner_pts$angle<-temp_s1
    inner_pts$radtype<-"inner"
    inner_pts$rad<-temp_d1
    inner_pts<-inner_pts[order(inner_pts$angle),]
    
    
    outer_pts<-data.frame(geosphere::destPoint(p=temp_p,
                                 d=temp_d2,
                                 b=temp_s2))
    outer_pts$angle<-temp_s2
    outer_pts$radtype<-"outer"
    outer_pts$rad<-temp_d2
    outer_pts<-outer_pts[order(outer_pts$angle,decreasing = TRUE),]
    
    points<-dplyr::bind_rows(inner_pts,outer_pts)
    points[,names(arc_data)]<-arc_data[i,]
    if (i==1) out<-points
    if(i>1) out<-dplyr::bind_rows(out,points)
  }#end for loop
  
  out$quadrant<-factor(out$quadrant,levels=c("nw","sw","se","ne"))
  out$order<-out$angle
  out$order[out$radtype=="inner"]<- -out$order[out$radtype=="inner"]
  out<-dplyr::arrange_(out,~wind_speed,~radtype,~quadrant,~order)
  out$r_in<-NULL
  out$r_out<-NULL
  out$start_angle<-NULL
  out$end_angle<-NULL
  out$x<-NULL
  out$y<-NULL
  out$wind_speed<-as.factor(out$wind_speed)
  return(out)
}#end compute_group