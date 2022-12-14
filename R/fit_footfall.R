fit <- function(dub = dub,
                data_type = c("yearly", "quarterly", "monthly"),
                fit_type = c("lm", "loess", "smooth.spline")) {
  UseMethod("fit")
}

fit.footfall <- function(location = c("Bachelors.walk.Bachelors.way", "Westmoreland.Street.West.Carrolls",
                                      "Westmoreland.Street.East.Fleet.street", "O.Connell.st.Princes.st.North",
                                      "O.Connell.St.Parnell.St.AIB", "Henry.Street.Coles.Lane.Dunnes",
                                      "Dawson.Street.Molesworth", "College.st.Westmoreland.st", "College.Green.Church.Lane"),
                         mod_type = c("daily", "weekly", "monthly")) {

  # list of possible locations
  location_list <- c("Bachelors.walk.Bachelors.way", "Westmoreland.Street.West.Carrolls",
                     "Westmoreland.Street.East.Fleet.street", "O.Connell.st.Princes.st.North",
                     "O.Connell.St.Parnell.St.AIB", "Henry.Street.Coles.Lane.Dunnes",
                     "Dawson.Street.Molesworth", "College.st.Westmoreland.st", "College.Green.Church.Lane")

  # find index of location
  location_type <- which(location_list == location)

  # create new data set with just the location and datetime
  dat <- subset(x = dub, select = c(1, location_type))

  # change the name of the location variable
  names(dat)[2] = "location"

  if(mod_type == "daily") {

    # for daily

    dat_day <- dat
    dat_day$datetime <- week(dat_day$datetime)

    mod <- stats::lm(location ~ datetime, data = dat_day)

  } else if (mod_type == "weekly") {

    # for weekly

    dat_week <- dat
    dat_week$datetime <- week(dat_week$datetime)

    mod <- stats::lm(location ~ datetime, data = dat_week)

  } else {

    # for monthly

    dat_month <- dat
    dat_month$datetime <- lubridate::month(dat_month$datetime)

    mod <- stats::lm(location ~ datetime, data = dat_month)
  }
}

