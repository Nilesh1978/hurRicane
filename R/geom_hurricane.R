#' Geom to produce hurricane radii graph
#' 
#' This Geom is meant to satisfy a Coursera assignment to build a custom Geom for
#' ggplot2. This is really only suited for the data source outlined in the function
#' in this package. The point of the graph is to show a radius of distance of a 
#' certain wind speed as recorded in the data for a given hurricane.
#'
#' @import ggplot2
#' @import grid
#' 
#'
#'
#'
GeomHurricane<-ggplot2::ggproto("GeomHurricane",ggplot2::GeomPolygon,
                      
                       required_aes = c("x","y","r","quadrant","wind_speed"),
                       
                       default_aes = ggplot2::aes(color="yellow",
                                        fill="red",
                                        size=0.5,
                                        linetype=1,
                                        alpha=.6
                                        ),
                  #Transforming data as necessary to plot    
                  setup_data=function(data,params,arc_step=1,scale_radii=1){
                       temp<-data
                       temp$r<-temp$r*scale_radii
                       temp<-hurricane_geodesic(storm_data=data,
                           x="x",
                            y="y",
                            r="r",
                            wind_speed="wind_speed",
                            arcRes=arc_step)
                        
                        #I am recoding the group, colour, and fill since I converted
                        #the whole data. If wanting to permit groups, need to find
                        #better workaround.
                        temp$group<-as.factor(temp$wind_speed)
                        temp$colour<-as.factor(temp$wind_speed)
                        temp$fill<-as.factor(temp$wind_speed)
                        
                        temp
                        
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
#'                               
#'                                                   

geom_hurricane<-function (mapping = NULL, 
                          data = NULL, 
                          stat = "identity",
                          position = "identity",
                          arc_step=1,scale_radii=1,
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){

  
  layer(data = data, mapping = mapping, stat = stat, 
        geom = GeomHurricane, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,
                      arc_step=arc_step,scale_radii=scale_radii,
                      ...))
}
