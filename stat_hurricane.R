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
                  
              required_aes = c("x","y"),

              compute_group=function(self,data,scales,params,arcRes=1){
                          
                  #Transform the data into longitudinal
                  data<-hurricane_geodesic(data=data,x="x",y="y")
                  return(data)
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