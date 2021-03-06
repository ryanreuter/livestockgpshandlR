% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_1_day.R
\name{map_1_day}
\alias{map_1_day}
\title{Map GPS locations for 1 animal on 1 day}
\usage{
map_1_day(googleAPIkey, date, tag, df = gps.data)
}
\description{
This function plots points on a Google satellite image for 1 animal on
as selected day. Obtaining the Google image requires a Google API key.
You must obtain your key, and then register it using ggmap::register_key()
}
\details{
The function takes 3 arguments, first your Google API key, then the
date in format "YYYY-MM-DD", and the tag number of the animal. All of
these are quoted. Example: map_1_day("AIz...xI", "2018-07-15", "18082")
Optionally, the function can also take the name of the dataframe where
your data is stored in R. The function defaults to gps.data, where
the accumulate_raw_files() function saves the data it processes.
}
