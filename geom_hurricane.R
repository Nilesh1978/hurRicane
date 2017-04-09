#'
#'
#'
#'
#'
#'
GeomHurricane<-ggproto("GeomHurricane",GeomPolygon,
                      required_aes = c("x","y","r","quadrant","wind_speed"),
                      default_aes = aes(color="yellow",
                                        fill="red",
                                        size=0.5,
                                        linetype=1,
                                        alpha=.6)
                    
                      
            
)
          

geom_hurricane<-function (mapping = NULL, 
                          data = NULL, 
                          stat = "identity",
                          position = "identity", 
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  
  layer(data = data, mapping = mapping, stat = stat, 
        geom = GeomHurricane, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}