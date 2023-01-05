#' Visualisation for Dublin 2021 footfall data.
#'
#' The function creates multiple plots relating to the Dublin 2021 footfall
#' data set. It creates one of three plots: a scatter plot with mean footfall
#' counts, a scatter plot with total footfall counts and a residual vs fitted
#' plot for a linear regression created from the data.
#'
#' @param dat The Dublin 2021 footfall data, from the \code{"load_footfall()"}
#' function.
#' @param mod An object of type \code{"lrmodel"}, from the
#' \code{"fit_footfall()"} function. If no object is provided, one is created
#' by default.
#' @param plot_type The type of plot produced. Can be "mean" for the mean
#' footfall plot, "total" for the total footfall plot or "res vs fit" for the
#' residual vs fitted plot. Set to residual vs fitted plot by default.
#'
#' @return One of three plots:
#' A scatter plot of mean footfall counts across each Dublin location,
#' A scatter plot of total footfall counts across each Dublin location,
#' A residual vs fitted plot from the linear model object.
#'
#' @export
#'
#' @author Obinna Njoku - </{Obinna.Njoku.2019@@mumail.ie}>
#' @seealso \code{\link{load_footfall}}, \code{\link{fit_footfall}}
#'
#' @importFrom dplyr "mutate" "group_by"
#' @importFrom ggplot2 "ggplot" "geom_point" "theme_bw" "labs"
#'
#' @examples
#' dublin <- load_footfall()
#' fit1 <- fit_footfall(dublin)
#'
#' plot_footfall(dublin, fit1)
#' plot(dublin, fit1, plot_type = "mean")

plot_footfall<- function(dat,
                         mod,
                         plot_type = "res vs fit"){

dat1 <- dat |>
    dplyr::group_by(Location) |>
    dplyr::mutate(mCount = mean(Count),
                  tCount = sum(Count))

if(plot_type == "mean") {

  ggplot2::ggplot(data = dat1, aes(x = mCount, y = Location))+
      geom_point()+
      theme_bw()+
      labs(title = "Mean Footfall for Each Dublin Location in 2021",
           x = "Mean Count")

} else if (plot_type == "total") {

  ggplot2::ggplot(data = dub_dat1, aes(x = tCount, y = Location))+
    geom_point()+
    theme_bw()+
    labs(title = "Total Footfall for Each Dublin Location in 2021",
         x = "Total Count")

  } else {

    x <- mod$mod.fitted
    y <- mod$mod.residuals
    plot(x,y)
  }
}

d <- load_footfall()
m <- fit_footfall(d)
plot_footfall(d,m)
