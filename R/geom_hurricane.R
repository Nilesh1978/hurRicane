#' Geom to produce hurricane radii graph
#' 
#' This Geom is meant to satisfy a Coursera assignment to build a custom Geom for
#' ggplot2. This is really only suited for the data source outlined in the function
#' in this package. The point of the graph is to show a radius of distance of a 
#' certain wind speed as recorded in the data for a given hurricane.
#'
#' @import ggplot2
#' @import grid
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @export
#'
#'
#'
GeomHurricane<-ggplot2::ggproto("GeomHurricane",GeomPolygon,
                      
                       required_aes = c("x","y","r","quadrant","wind_speed"
                                        ),
                       
                       default_aes = aes(fill="red",
                                         colour="red",
                                         size=0.5,
                                         linetype=1,
                                         alpha=.5,
                                         arc_step=1,
                                         scale_radii=1
                                        ),
                  
                  #Transforming data as necessary to plot    
                  draw_panel = function(self,data,panel_scales,coord){
                   
                    extravars<-dplyr::select_(data,~-x,~-y,~-r)
                    
                    temp<-data
                    temp$r<-temp$r*temp$scale_radii
                   
                    
                    #Generate geocoded points
                    temp<-hurricane_geocode(storm_data=temp,
                                            x="x",
                                            y="y",
                                            r="r",
                                            wind_speed="wind_speed",
                                            quadrant="quadrant",
                                           arcRes=temp$arc_step[1])
                    
                    #Merge in original aes() mapping
                    #However, overwriting any attempt to fill or color by
                    #something other than wind_speed
                    
                    temp<-dplyr::select_(temp,~x,~y,~wind_speed,~quadrant)
                    
                    extravars$quadrant<-factor(extravars$quadrant,
                                               levels=c("ne","se","sw","nw"))
                    
                    extravars$wind_speed<-as.factor(extravars$wind_speed)
                    
                    temp$quadrant<-factor(temp$quadrant,
                                          levels=c("ne","se","sw","nw"))
                    
                    temp$wind_speed<-as.factor(temp$wind_speed)
                    
                    temp<-dplyr::left_join(temp,extravars,
                                           by=c("wind_speed","quadrant"))
                    
                    #force group by wind_speed to make polygons smooth                             
                    temp$group<-temp$wind_speed
                    
                    
                    ggplot2:::ggname("geom_polygon",
                                     GeomPolygon$draw_panel(temp, panel_scales, coord)) 
                  }
                  
                      
                        
                      
                  
)


#' This is the wrapper to the layer function for the GeomHurricane.
#' 
#' The wrapper is just to plot the object. See \code{ggplot2} package documenation
#' for more specifics on geom_* and layer_* functions.
#' 
#' @inheritParams ggplot2::geom_polygon
#' 
#' @param arc_step This regulates the resolution of the arcs in the wind_radii
#' graph. See \code{\link{hurricane_geocode}} function, specificall arcRes argument
#' as this is passed into that function to transform data in the underlying
#' GeomHurricane
#' 
#' @param scale_radii This is per the assignemnt instructions to implement a way
#' to scale the radius size. I'm honestly not sure why you would want to reduce all
#' the radii to 80%, 90% etc. of their original size, but the assignment wanted it.
#' I'm actually concerned this misrepresents the data since it appears that for a
#' given region near the eye, the wind speeds are slower than what the data records.
#' 
#' @note It is appropriate to always include a call to \code{fill} and \code{color}
#' within the mapping, set equal to \code{wind_speed} data to produce desirable
#' color scheme of assignment. Furthermore, you will need to set scale_fill_manual
#' to make the colors logical for your view.            
#'                                                             
#' @export                             
#'                                                   

geom_hurricane<-function (mapping = NULL, 
                          data = NULL, 
                          stat = "identity",
                          position = "identity",
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  
  layer(data = data, mapping = mapping, stat = stat, 
        geom = GeomHurricane, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,
                              ...))
}
