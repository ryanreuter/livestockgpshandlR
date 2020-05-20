#' Accumulates data from many raw files from iGotU
#'
#' This function accumulates data from many .csv files output by iGotU
#' loggers into a single dataframe named "gps.data". The function is
#' expecting the .csv files to be in a single
#' directory and to be named in the following convention:
#' "tagXXXXX_M.D.Y.csv" from the iGotU software. Use a 5 digit tag number,
#' i.e. fill in X's or 0's or something to make the tag number exactly 5
#' digits. The function takes one argument, which is the directory where
#' the files are located. Example: accumulate_raw_files("../RawData/")
#'


#' @export

accumulate_raw_files <- function(directory){
    data_dir <- paste(directory,
                      list.files(directory, pattern = "*.csv*"), sep="")

    tmp.data <- NULL

    for(file in data_dir) {
        this.data <- readr::read_csv(file)
        this.data$tag <- stringr::str_sub(file, 15, 19)
        tmp.data <- rbind(tmp.data, this.data)
    }

    gps.data <<- tmp.data


    rm(tmp.data, this.data, data_dir, file)
}
