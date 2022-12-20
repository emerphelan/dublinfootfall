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
load_footfall() 
