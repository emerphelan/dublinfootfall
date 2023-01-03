#' Linear regression model for Dublin 2021 footfall data.
#'
#' The function creates a linear regression model for either daily, weekly or
#' monthly data for the Dublin 2021 footfall data set. It returns an object
#' containing a variety of values of potential interest.
#'
#' @param x The Dublin 2021 footfall data.
#' @param data_type Whether the model should examine daily, weekly
#' or monthly data. Set to daily by default.
#'
#' @return An object of class \code{"lrmodel"} which contains the attributes:
#' mod.residuals: residual values,
#' mod.fitted: fitted values,
#' mod.coef: coefficients,
#' mod.summary: model summary,
#' mod.fstat: F-statistic,
#' mod.call: the call, ie. the formula used to generate the model,
#' mod.sigma: the sigma value.
#'
#' @export
#'
#' @author Emer Phelan - </{emer.phelan.2019@@mumail.ie}>
#' @seealso \code{\link{fit}}, \code{\link{load_footfall}}
#'
#' @examples
#' dublin <- load_footfall()
#' fit1 <- fit.footfall(dublin)
#' fit2 <- fit.footfall(dublin, mod_type = "weekly")
#' fit3 <- fit.footfall(dublin, mod_type = "monthly")
#'
#' plot(fit1$mod.fitted, fit1$mod.residuals)
#' fit2$mod.sigma
#' fit3$mod.summary

fit_footfall <- function(dat, data_type = "daily") {

  dub <- as.data.frame(dat)
  dub$Date <- lubridate::ymd(dub$Date)

  if(data_type == "daily") {

    # for daily
    mod <- stats::lm(Count ~ Location + lubridate::wday(Date), data = dub)

  } else if (data_type == "weekly") {

    # for weekly
    mod <- stats::lm(Count ~ Location + lubridate::week(Date), data = dub)

  } else {

    # for monthly
    mod <- stats::lm(Count ~ Location + lubridate::month(Date), data = dub)
  }

  s <- summary(mod)

  fit <- structure(list(mod.residuals = mod$residuals,
                        mod.fitted = mod$fitted.values,
                        mod.coef = mod$coefficients,
                        mod.summary = s,
                        mod.fstat = s$fstatistic,
                        mod.call = s$call,
                        mod.sigma = s$sigma),
                   class = "lrmodel")
  return(fit)
}
