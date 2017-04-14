#'
#'
#'
#'
#'
#'
#'
#'
#'
StatHurricane<-ggproto("StatHurricane",Stat,
                  
              required_aes = c("x","y","r","quadrant","wind_speed"),

              compute_panel=function(data,arcRes=1){
                        
                #Transform the data into longitudinal
                #Renaming arguments to make referencing easier.
                  
                hurricane_geodesic(storm_data=data,
                                         x="x",
                                         y="y",
                                         r="r",
                                         wind_speed="wind_speed")
                }
                  
)#end StatHurricane ggproto() call

stat_hurricane<- function (mapping = NULL, data = NULL, geom = "GeomHurricane", 
                                   position = "identity", 
                                   ..., show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = StatHurricane, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, 
                                                 ...))
}