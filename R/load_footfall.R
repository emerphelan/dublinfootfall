#' Load in function for footfall data for the year 2021.
#'
#' This functions imports 2021 footfall data from the smart
#' Dublin website and cleans this up too be used later on.
#'
#'
#' @param date_format The default date_format is now in
#' day-month-year, this can be changed to have month as
#' January. This can be changed instead of it showing this
#' as numeric 01, one can replace the lowercase letter m to
#' the lowercase letter b.This can be seen below
#'  01 which can be
#' @param time_format The default for the time_format is in
#' specifying R which is the equivalent to hour - minutes. While
#' replacing this with T in the code below is adding seconds which
#' is equivalent to hours, minutes and seconds.
#' @param na.rm This argument is equal to TRUE whereby it removes
#' any NA values within the data itself.
#'
#'
#' @return An object of class footfall that has four columns
#' Date, Time, Location and Count.
#'
#' The date is in format day-month-year
#' The time column is in hours and minutes
#' The locations columns gives the locations
#' of various streets around the city.
#' The count column sums up the amount of
#' people in the area at dates and time as listed.
#'
#'
#' @export
#'
#'
#' @author Liam Padden - <\email{liam.padden.2020@@mumail.ie}
#'
#'
#' @importFrom readr "read_csv"
#' @importFrom dplyr "rename" "select" "ends_with" "mutate"
#' @importFrom tidyr "pivot_longer" "separate"
#'
#'
#' @seealso See \code{\link{strptime}} for the available conversion
#' specifications for the \code{date_format} and \code{time_format}
#'
#'
#' @examples
#' date_format1 <- load_footfall("%d-%m-%Y)
#'
#' date_format2 <- load_footfall("%d-%b-%Y)
#'
#' time_format1 <-load_footfall(time_format = "%R")
#'
#' time_format2 <- load_footfall(time_format = "%T")
#'
#'
#'
load_footfall <- function(date_format = "%d-%m-%Y",
                          time_format = "%R",
                          na.rm = TRUE) {

  #URL
  url <- paste0("https://data.smartdublin.ie/dataset/cc421859-1f4f-43f6-b349-f4ca0e1c60fa/resource/ef530dde-1511-4617-b783-c4a3ad1cd7dc/download/2021-jan-dec-dcc-footfall.csv")

  #Reading in the data using readr
  output  <- readr::read_csv(url,
                             col_types = paste(c("c", rep("d", 102)), collapse=""),
                             progress = FALSE)

  #Sort out Time and Date
  output <- output |> dplyr::rename(DateTime=`Date and Time`)

  #Removing in and out for the locations
  output <- output |> dplyr::select(-dplyr::ends_with("IN"),
                                    -dplyr::ends_with("OUT"))

  #Adding columns
  output <- tidyr::pivot_longer(output, -DateTime,
                                names_to = "Location",
                                values_to = "Count",
                                values_drop_na = na.rm)

  #Separating TimeDate away from each other
  output <- output |> tidyr::separate(DateTime,
                                      c("Date", "Time"),
                                      sep=" ")

  #Changing class of dates
  output <- output |> dplyr::mutate(Date=format(as.Date(Date, "%d-%m-%Y"), date_format),
                                    Time=format(strptime(Time, format="%T"), time_format))

  #Return object of class footfall
  class(output) <- c("footfall", class(output))
  return(output)

}
