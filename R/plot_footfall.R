# plot  for Dublin 2021 footfall data.
#'
#' The function creates a plot for the Dublin 2021 footfall data
#' It returns a plot for the count with respect to location
#' It also returns a plot for the residuals with respect to fitted values
#' fit_footfall default function.
#' @example
#' dublin <- load_footfall()
#' fit1 <- fit.footfall(dublin)
#' plot_footfall(dublin,fit1)
#'
#' @param NA
#' @author Obinna Njoku - <\email{Obinna.Njoku.2019@@mumail.ie}


library(tidyverse)
plot_footfall<- function(dat,fit){
 x<- as.data.frame(dat)

fit1 <- as.data.frame(fit)

ggplot(x,aes(x=Location, y=Count))+
  geom_point()+
  geom_line(colour="red")+
  theme_bw()+
  labs(title = "Data visualisation")

ggplot(x,aes(x=Location, y=Count))+
  geom_boxplot()+
  geom_line(colour="red")+
  theme_bw()+
  labs(title = "Data visualisation")

plot(fit1$mod.fitted, fit1$mod.residuals, xlab = "Fitted" , ylab = "Residuals")
}


