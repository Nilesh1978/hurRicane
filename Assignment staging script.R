#For this assessment, you will need to
#
#Build a custom geom for ggplot2 that can be used to add the hurricane wind
#radii chart for a single storm observation to a map (i.e., could be used to
#recreate the figure shown above). Use the geom to map the create a map showing
#the wind radii chart at one observation times for Hurricane Ike, which occurred
#in September 2008. Use an observation time when the storm was near or over the
#United States. More specifically, you will need to
#
#download the data on all storms in the Atlantic basin from 1988--2015 (extended
# best tracks) 
#tidy the dataset into "long" format 
#subset to the specific hurricane that you will be mapping (Hurricane Ike) 
#    and to a single observation time for that hurricane 

#Write the code for a geom named "geom_hurricane" that
#    can be used to add a hurricane wind radii chart to a base map The wind radii
#    give the maximum radial extent of winds of a certain direction in each
#    quadrant. 

#In some circumstances, you may want to plot a certain percentage of
#   the full wind radii, to give a better idea of average exposures in each
#   quadrant. For example, you might want to plot a chart based on radii that are
#   80% of these maximum wind radii. Include a parameter in your geom called
#   scale_radii that allows the user to plot wind radii charts with the radii
#   scaled back to a certain percent of the maximum radii. For example, if a user
#   set "scale_radii = 0.8", the geom should plot a wind radii chart mapping the
#   extent of 80% of the maximum wind radii in each quadrant. 

#If functions from other packages are called, you should use the appropriate 
#    :: notation to
#   reference those functions (e.g., "dplyr::mutate" rather than "mutate") Write
#   documentation in roxygen2 format that provides help file information for your
#   geom



#Review criterialess 
#To submit this assignment you must submit
#
#An R script containing the code implementing the geom 
#A plot file (in PNG or PDF format) containing a map that has the wind radii 
#  overlayed for Hurricane Ike.
#
#This assessment will ask reviewers the following questions:
#
#Is a map included that shows the wind radii for a single observation time for
#   Hurricane Ike? 
#Does the map show the correct hurricane? 
#Does the geom correctly map locations for the wind radii based on the center 
#   point, radii, and direction?
#Is the proper notation used for calling functions in other packages?
#Is there roxygen2-style documentation included in the R script? 
#Are all the parameters of the geom documented and described? 
#Does the geom include a scale_radii parameter that functions as described?
#
#libraries
library(ggmap)
library(dplyr)
library(geosphere)

#set working directory
pathname<-file.path(".","Week4Grob",fsep=.Platform$file.sep)
setwd(pathname)
#Import data
ext_tracks<-read_ext_tracks(file="ebtrk_atlc_1988_2015.txt")

#TIdy up the raw data for useful analysis.
tidy_ext_tracks<-tidy_tracks(ext_tracks=ext_tracks)

#Filter data for our assignment; Ike at a single point in time over land
hur_name<-"KATRINA"
hur_year<-2005
hur_month<-"08"
hur_day<-"29"
hur_hour<-"12"

hur_datehour<-lubridate::ymd_h(paste(hur_year,hur_month,hur_day,hur_hour))

storm_observation<-dplyr::filter_(tidy_ext_tracks,
                                  ~storm_name==hur_name,
                                  ~datehour==hur_datehour
                                  )
plot_data<-hurricane_geodesic(storm_observation,arcRes = .1)                                  



get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
ggmap(extent = "device") + geom_polygon(data=plot_data,mapping=aes(x=longitude,
                                                                   y=latitude,
                                                                   fill=wind_speed,
                                                                   #color=wind_speed,
                                                                   group=interaction(quadrant,wind_speed)
                                                                   ),
                                                      alpha=.7)+ 
                            scale_color_manual(name = "Wind speed (kts)", 
                                                         values = c("red", "orange", "yellow")) + 
                          scale_fill_manual(name = "Wind speed (kts)", 
                                                            values = c("red", "orange", "yellow"))
                   
get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%                     
ggmap(extent = "device") + geom_hurricane(data=plot_data,mapping=aes(x=longitude,
                                                                   y=latitude,
                                                                   fill=wind_speed,
                                                                   #color=wind_speed,
                                                                   group=interaction(quadrant,wind_speed)
                                                                    ),
alpha=.7)+ 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
