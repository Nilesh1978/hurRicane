#set working directory
pathname<-file.path(".","Week4Grob",fsep=.Platform$file.sep)
setwd(pathname)
#Import data
ext_tracks<-read_ext_tracks(file="ebtrk_atlc_1988_2015.txt")

#TIdy up the raw data for useful analysis.
tidy_ext_tracks<-tidy_tracks(ext_tracks=ext_tracks)

#Filter data for our assignment; Ike at a single point in time over land
hur_name<-"IKE"
hur_year<-2008
hur_month<-"09"
hur_day<-c("11")
hur_hour<-c("12")

hur_datehour<-lubridate::ymd_h(paste(hur_year,hur_month,hur_day,hur_hour))

storm_observation<-dplyr::filter_(tidy_ext_tracks,
                                  ~storm_name %in% hur_name,
                                  ~date %in% hur_datehour)
                               
myloc<-unique(c(storm_observation$longitude,storm_observation$latitude))

p<-get_map(location=myloc,
           zoom = 6, maptype = "toner-background") %>%
ggmap(extent = "device")+geom_hurricane(data=storm_observation,
                                        mapping=aes(x=longitude,y=latitude,
                                                    r=wind_radius,
                                                    wind_speed=wind_speed,
                                                    quadrant=quadrant),
                                        arc_step=45)+
                                                     
                            scale_color_manual(name = "Wind speed (kts)", 
                                                         values = c("red", "orange", "yellow")) + 
                          scale_fill_manual(name = "Wind speed (kts)", 
                                                            values = c("red", "orange", "yellow"))
p  
