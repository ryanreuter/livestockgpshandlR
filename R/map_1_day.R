#' Map GPS locations for 1 animal on 1 day
#'
#' This function plots points on a Google satellite image for 1 animal on
#' as selected day. Obtaining the Google image requires a Google API key.
#' You must obtain your key, and then register it using ggmap::register_key()
#' See: https://www.rdocumentation.org/packages/ggmap/versions/3.0.0
#'
#'
#' The function takes 3 arguments, first your Google API key, then the
#' date in format "YYYY-MM-DD", and the tag number of the animal. All of
#' these are quoted. Example: map_1_day("AIz...xI", "2018-07-15", "18082")
#' Optionally, the function can also take the name of the dataframe where
#' your data is stored in R. The function defaults to gps.data, where
#' the accumulate_raw_files() function saves the data it processes.
#'
#' @export

map_1_day <- function(googleAPIkey, date, tag, df=gps.data){

    mapping.data <- df
    mapping.data <- dplyr::filter(mapping.data, Date == date)
    mapping.data <- dplyr::filter(mapping.data, tag == tag)

    zoom <- min(RgoogleMaps::MaxZoom(range(mapping.data$Latitude), range(mapping.data$Longitude)))

    map <- ggmap::get_map(location = c(lon = mean(unique(mapping.data$Longitude)),
                            lat = mean(unique(mapping.data$Latitude))),
               api_key = googleAPIkey,
               zoom = zoom,
               maptype = "satellite",
               source = "google",
               messaging = TRUE)

    ggmap::ggmap(map) +
        geom_point(aes(x = Longitude, y = Latitude),
               data = mapping.data, alpha = 0.2, color="orange", size = 1) +
        ggtitle(date, tag)

}
