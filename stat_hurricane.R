#'
#'
#'
#'
#'
#'
#'
#'
#'
StatHurricane<-ggproto("StatGeodesic",Stat,
            
              require_aes = c("x","y","r_in","r_out","quadrant","wind_speed"),
              
              default_aes=aes(x="longitude",y="latitude",r_in="wind_inner_rad",
                              r_out="wind_outer_rad",quadrant="quadrant",
                              wind_speed="wind_speed"),
               
              compute_group=function(data,scales,params,arcRes=1){
                          
                  #Give the tidy data points in general
                
                   

                               
)#end StatHurricane ggproto() call

#'
#'
#'
#'
stat_hurricane<-function(mapping = NULL, data = NULL,geom="GeomHurricane",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE,arcRes=1, ...){

  layer(
    stat = StatHurricane, data = data, mapping = mapping, 
    geom = GeomHurricane, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(arcRes=1,na.rm = na.rm, ...)
    )
}