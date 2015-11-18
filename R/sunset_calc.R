
#' Creates a sunset calender
#'
#' @param date Date at which you want the calendar to start, in yyyy/mm/dd format.
#' @param lat  Latitude of location (for sunset time calculation)
#' @param long Longitude of location (for sunset time calculation, will be negative for continental US)
#' @param timezone Timezone of location (for sunset time calculation).
#' @param num.days Number of days you want sunset appointments for.
#' @param file Filename for outputted .CSV file (to be uploaded to Google Calendar).
#' @param location Location of sunset appointment. Will be input into Google Calendar event as the event location.
#'
#' @return .CSV file
#' @export
#' @examples
#'
create_sunset_cal <- function(date="2015/01/01",
                              lat = 49.2623,
                              long = -123.2619,
                              timezone = "UTC-8",
                              num.days = 365,
                              file="sunset.csv",
                              location = "Wreck Beach, Vancouver, BC V6T"){

  location <- gsub(",", "", location)

  dates <- seq(
    as.Date(date),
    by = "day",
    length.out = num.days
  )

  sunset_times <- sunrise.set(
    lat = lat,
    long = long,
    date = date,
    timezone = timezone,
    num.days = num.days
  )$sunset

  nms <- c(
    'Subject',
    'Start Date',
    'Start Time',
    'End Date',
    'End Time',
    'All Day Event',
    'Description',
    'Location',
    'Private'
  )
  mat <- matrix(
    nrow = length(dates),
    ncol = length(nms)
  )
  mat <- data.frame(mat)
  colnames(mat) <- nms

  mat$Subject <- "Sunset"
  mat$"Start Date" <- dates
  mat$"End Date" <- dates
  mat$"All Day Event" <- "False"
  mat$Description <- "Sunset Calendar"
  mat$Location <- location
  mat$Private <- "False"

  starts <- strftime(sunset_times, format="%H:%M:%S %p")
  ends <- strftime(sunset_times+60*30, format="%H:%M:%S %p")
  mat$"Start Time" <- starts
  mat$"End Time" <- ends

  write.csv(
    mat,
    file=file,
    quote=FALSE,
    row.names=FALSE
  )

}
