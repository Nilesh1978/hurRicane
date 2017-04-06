#'
#'
#'
#'
read_ext_tracks<-function(file="ebtrk_atlc_1988_2015.txt",...,widths=NULL,
                          colnames=NULL,degW=TRUE){
    if (is.null(widths)){
                widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                           4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
    }
  
    if (is.null(colnames)){
                colnames <- c("storm_id", "storm_name", "month", "day",
                              "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                             "storm_type", "distance_to_land", "final"
                            )
    }
                          
    ext_tracks <- readr::read_fwf(file, 
                           readr::fwf_widths(widths, colnames),
                           na = "-99")
    
    #Change sign of longitude because it is in degrees W and in general coord
    #   are plotted with South/West as negative and North/East being positive.
    if (degW) {ext_tracks$longitude<- -ext_tracks$longitude }
    
    return(ext_tracks)
}