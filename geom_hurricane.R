#'
#'
#'
#'
#'
#'
GeomHurricane<-ggproto("GeomHurricane",GeomPolygon,
        
          #Just creating an inherited Polygon geom with default overrides
          #geom_ call will have stat_hurricane as default to do the heavy lifing
          default_aes=aes(alpha=.4,arcRes=1)
)
          

geom_hurricane<-function(mapping = NULL, data = NULL, 
                         stat = "StatHurricaneRadial",geom="GeomHurricane",
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, ...){
  
  layer(
    stat = stat,geom=geom, data = data, mapping = mapping, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  ) 
  
}