#' Read in ext_tracks data from Colorado State
#' 
#' This function is set to read in extended tracks hurricane data from Colorad
#' State NHC data compilation. This is simply a shell for \code{readr::read_fwf()}
#' with defaults set for the current data convenctions. Currently data is in a 
#' fixed with format and this function is dependent on that form.
#' 
#' @param file this is the filename (including path if necessary) to the txt file
#' containing the hurricane data.
#' 
#' @param widths This is a numeric vector of column widths for the fixed width
#' file. The default is set to the current convention.
#' 
#' @param colnames This is a character vector of column names we wish to apply to
#' the data being read in. The assumption is that there are no column names in 
#' the text file being read in. The default represents current convention.
#' 
#' @param degW This stands for "degrees West" and is a logical representing if the
#' text file data is coded such that the latitude is stated as "degrees West" and
#' thus be strictly positive. If TRUE (the default) the read in procedure will
#' transform the data by flipping the sign so that latitude is in universal
#' coordinates (like a number line). There is no corresponding argument for 
#' the longitude as hurricanes are only being track for the northern hemisphere
#' (always positive).
#' 
#' @param ... This can be used to pass alternative arguments to \code{readr::fwf()}
#' as seen in the following arguemnts.
#' 
#' @inheritParams readr::read_fwf
#' 
#' @return data returned per specifications in readr::fwf. By default this is a
#' data.frame
#'
#' @importFrom readr read_fwf
#' 
#' @export
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