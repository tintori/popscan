#' Covert a yy-mm-dd_HH-MM character into a time object
#'
#' This function takes a character with a specific format (yy-mm-dd_HH-MM), and converts it to a time format.
#' @param time.in Character representing time, in the format "yy-mm-dd_HH-MM".
#' @export
#' @examples
#' tm()

tm <- function(time.in){
    as.POSIXct(strptime(time.in, format="%y-%m-%d_%H-%M"))
}
