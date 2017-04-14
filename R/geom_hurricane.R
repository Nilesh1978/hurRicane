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
                                        alpha=.6
                                        ),
                      
                    
                      setup_data=function(data,arcRes=1,coord){
                        
                        temp<-hurricane_geodesic(storm_data=data,
                            x="x",
                            y="y",
                            r="r",
                            wind_speed="wind_speed")
                        
                        #I am recoding the group, colour, and fill since I converted
                        #the whole data. If wanting to permit groups, need to find
                        #better workaround.
                        temp$group<-as.factor(temp$wind_speed)
                        temp$colour<-as.factor(temp$wind_speed)
                        temp$fill<-as.factor(temp$wind_speed)
                        temp
                        }
                      
                        
                      
                  
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