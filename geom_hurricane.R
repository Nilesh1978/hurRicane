#'
#'
#'
#'
#'
#'
GeomHurricane<-ggproto("GeomHurricane",GeomPolygon,
                      required_aes = c("x","y","r_in","r_out","quadrant","wind_speed"),
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
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  #data=hurricane_geodesic(data)
  
  layer(data = data, mapping = mapping, stat = StatHurricaneRadial, geom = GeomHurricane, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}