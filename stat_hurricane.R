#'
#'
#'
#'
#'
#'
#'
#'
#'
StatHurricaneRadial<-ggproto("StatHurricaneRadial",Stat,
                  
              required_aes = c("x","y","r","quadrant","wind_speed"),

              compute_group=function(self,data,scales,params,arcRes=1){
                        
                #Transform the data into longitudinal
                #Renaming arguments to make referencing easier.
                  
                # Step 1: Merge data needed for geodesic inputs with each record
                arc_data<-dplyr::data_frame(quadrant=c("ne","se","sw","nw"),
                                            start_angle=c(0,90,180,270),
                                            end_angle=c(90,180,270,360))
                merged_data<-merge(data,arc_data,by="quadrant")
                
                
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
                out<-dplyr::rename_(out,.dots=setNames(list(~lon,~lat),c(x,y)))
                out<-dplyr::rename_(out,.dots=setNames(list(~x,~y),c("eye_lon","eye_lat")))
                #clear out extra variables
                out$start_angle<-NULL
                out$end_angle<-NULL
                
                
                return(out)
              }
                  
)#end StatHurricane ggproto() call

stat_hurricane_radial <- function (mapping = NULL, data = NULL, geom = "GeomHurricane", 
                                   position = "identity", 
                                   ..., show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = StatHurricaneRadial, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, 
                                                 ...))
}