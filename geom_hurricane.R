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
                      
                      
              
                    draw_panel = function(data,panel_params,coord){
                      
                      GeomPolygon$draw_panel(data,panel_params,coord)
                      
                    },
                      
                      setup_data = function(data,params){
                        
                        #This isn't the most logical place to put the data
                        #transform, but it is literally the only place it doesn't
                        #cause an error. I have found no helpful documentaiton
                        #in regards to troubleshooting custom stats/geoms.
                        data<-hurricane_geodesic(storm_data=data,
                                                 x="x",
                                                 y="y",
                                                 r="r",
                                                 wind_speed="wind_speed")
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