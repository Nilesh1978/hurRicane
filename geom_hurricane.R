#'
#'
#'
#'
#'
#'
GeomHurricane<-ggproto("GeomHurricane",Geom,
        
        #required aesthetics are really just to pass along to the stat_ call.
        #the output is expected to be nearly identical to GeomPolygon
        required_aes = c("x","y"),               
        
        default_aes=aes(alpha=.4,arcRes=1),

        draw_group = function(data,params){
          
                        grid::grid.polygon(x=x,y=x,id=wind_speed,default.units="native")
          
                    }

)

geom_hurricane<-function(mapping = NULL, data = NULL, stat = stat,
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, ...){
  
  layer(
    stat = stat,geom=GeomHurricane, data = data, mapping = mapping, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  ) 
  
}