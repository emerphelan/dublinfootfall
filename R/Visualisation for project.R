# Visualisation of data set
# first function helps us visualise the original data set
# It visualisese the footfall of INS and OUT through a select
##amout of locations
# my second function, visualises the linear model of the
# fitted data




install.packages("tidyverse")
library(tidyverse)
library(lubridate)

dublin <- read.csv("footfall.csv")

dublin$datetime <- dmy_hms(dublin$`Date.and.Time`)
dublin$`Date.and.Time` <- NULL # delete

glimpse(dublin)


# data set
dub <- subset(x = dublin, select = c(datetime, Bachelors.walk.Bachelors.way, Westmoreland.Street.West.Carrolls,
                                     Westmoreland.Street.East.Fleet.street, O.Connell.st.Princes.st.North,
                                     O.Connell.St.Parnell.St.AIB, Henry.Street.Coles.Lane.Dunnes,
                                     Dawson.Street.Molesworth, College.st.Westmoreland.st, College.Green.Church.Lane))

dub_in<- subset(x = dublin, select = c(datetime, Bachelors.walk.Bachelors.way.IN, Westmoreland.Street.West.Carrolls.IN,
                                       Westmoreland.Street.East.Fleet.street.IN, O.Connell.st.Princes.st.North.IN,
                                       O.Connell.St.Parnell.St.AIB.IN, Henry.Street.Coles.Lane.Dunnes.Pedestrian.IN,
                                       Dawson.Street.Molesworth.IN, College.st.Westmoreland.st.IN, College.Green.Church.Lane.IN))
dub_out<-subset(x = dublin, select = c(datetime, Bachelors.walk.Bachelors.way.OUT, Westmoreland.Street.West.Carrolls.OUT,
                                       Westmoreland.Street.East.Fleet.street.OUT, O.Connell.st.Princes.st.North.OUT,
                                       O.Connell.St.Parnell.St.AIB.OUT, Henry.Street.Coles.Lane.Dunnes.Pedestrian.OUT,
                                       Dawson.Street.Molesworth.OUT, College.st.Westmoreland.st.OUT, College.Green.Church.Lane.OUT))

install.packages("plotly")
library(plotly)


Plot.in.out<- function(){
  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$Bachelors.walk.Bachelors.way.IN, name = "Bachelors walk", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(7,40,89)',
                                                         outliercolor = 'rgba(7, 40, 89, 0.6)',
                                                         line = list(outliercolor = 'rgba(7, 40, 89, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,181,116)'))



  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$Westmoreland.Street.West.Carrolls.IN, name = "Westmoreland.Street.West", boxpoints = FALSE,
                                           marker = list(color = 'rgb(9,56,125)'),
                                           line = list(color = 'rgb(9,56,125)'))
  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$Westmoreland.Street.East.Fleet.street.IN, name = "Westmoreland.street.East", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(8,81,156)',
                                                         outliercolor = 'rgba(219, 64, 82, 0.6)',
                                                         line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,81,156)'))
  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$O.Connell.st.Princes.st.North.IN, name = "O.Connell.st.Princes", boxpoints = 'outliers',
                                           marker = list(color = 'rgb(107,174,214)'),
                                           line = list(color = 'rgb(107,174,214)'))

  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$O.Connell.St.Parnell.St.AIB.IN, name = "O.Connell.st.parnell", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(8,81,056)',
                                                         outliercolor = 'rgba(219, 64, 12, 0.6)',
                                                         line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,81,156)'))


  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$Henry.Street.Coles.Lane.Dunnes.Pedestrian.IN, name = "Henry.Street.Coles.Lane.Dunnes", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(100,81,056)',
                                                         outliercolor = 'rgba(119, 64, 12, 0.6)',
                                                         line = list(outliercolor = 'rgba(119, 64, 82, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,181,156)'))



  fig_box_in <- fig_box_in %>% add_boxplot(y = dub$Dawson.Street.Molesworth, name = "Dawson.Street.Molesworth", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(140,181,056)',
                                                         outliercolor = 'rgba(139, 24, 12, 0.6)',
                                                         line = list(outliercolor = 'rgba(169, 64, 82, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,181,156)'))


  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$College.st.Westmoreland.st.IN, name = "College.st.Westmoreland", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(140,101,156)',
                                                         outliercolor = 'rgba(140, 104, 112, 0.6)',
                                                         line = list(outliercolor = 'rgba(169, 64, 82, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,181,156)'))

  fig_box_in <- fig_box_in %>% add_boxplot(y = dub_in$College.Green.Church.Lane.IN, name = "College.Green.Church.Lane", boxpoints = 'suspectedoutliers',
                                           marker = list(color = 'rgb(140,101,156)',
                                                         outliercolor = 'rgba(140, 104, 112, 0.6)',
                                                         line = list(outliercolor = 'rgba(169, 64, 82, 1.0)',
                                                                     outlierwidth = 2)),
                                           line = list(color = 'rgb(8,181,156)'))

  fig_box_in <- fig_box_in %>% layout(title = "Box Plot For Each Variable")

  fig_box_in


  ######plot for out####
  fig_box_out <- plot_ly(type = 'box')



  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$Bachelors.walk.Bachelors.way.OUT, name = "Bachelor.Walk", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(7,40,89)',
                                                           outliercolor = 'rgba(7, 40, 89, 0.6)',
                                                           line = list(outliercolor = 'rgba(7, 40, 89, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,181,116)'))



  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$Westmoreland.Street.West.Carrolls.OUT, name = "Westmoreland.Street.West", boxpoints = FALSE,
                                             marker = list(color = 'rgb(9,56,125)'),
                                             line = list(color = 'rgb(9,56,125)'))
  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$Westmoreland.Street.East.Fleet.street.OUT, name = "Westmoreland.Street.East", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(8,81,156)',
                                                           outliercolor = 'rgba(219, 64, 82, 0.6)',
                                                           line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,81,156)'))
  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$O.Connell.st.Princes.st.North.OUT, name = "Oconnell.st.Princes", boxpoints = 'outliers',
                                             marker = list(color = 'rgb(107,174,214)'),
                                             line = list(color = 'rgb(107,174,214)'))

  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$O.Connell.St.Parnell.St.AIB.OUT, name = "Oconnell.st.Parnell", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(8,81,056)',
                                                           outliercolor = 'rgba(219, 64, 12, 0.6)',
                                                           line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,81,156)'))


  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$Henry.Street.Coles.Lane.Dunnes.Pedestrian.OUT, name = "Henry.street", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(100,81,056)',
                                                           outliercolor = 'rgba(119, 64, 12, 0.6)',
                                                           line = list(outliercolor = 'rgba(119, 64, 82, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,181,156)'))



  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$Dawson.Street.Molesworth.OUT, name = "Dawson.Street", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(140,181,056)',
                                                           outliercolor = 'rgba(139, 24, 12, 0.6)',
                                                           line = list(outliercolor = 'rgba(169, 64, 82, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,181,156)'))


  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$College.st.Westmoreland.st.OUT, name = "college.st.Westmoreland", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(140,101,156)',
                                                           outliercolor = 'rgba(140, 104, 112, 0.6)',
                                                           line = list(outliercolor = 'rgba(169, 64, 82, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,181,156)'))

  fig_box_out <- fig_box_out %>% add_boxplot(y = dub_out$College.Green.Church.Lane.OUT, name = "college.green", boxpoints = 'suspectedoutliers',
                                             marker = list(color = 'rgb(140,101,156)',
                                                           outliercolor = 'rgba(140, 104, 112, 0.6)',
                                                           line = list(outliercolor = 'rgba(169, 64, 82, 1.0)',
                                                                       outlierwidth = 2)),
                                             line = list(color = 'rgb(8,181,156)'))

  fig_box_out <- fig_box_out %>% layout(title = "Box Plot For Each Variable")

  fig_box_out




}
Plot.in.out()

#####function for lm####



plot.fit.footfall <- function(x,
                              data_type = "daily") {

  dub <- as.data.frame(x)

  if(data_type == "daily") {

    # for daily
    mod <- stats::lm(Count ~ Location + lubridate::day(date), data = dub)

  }else if (data_type == "weekly") {

    # for weekly
    mod <- stats::lm(Count ~ Location + lubridate::week(date), data = dub)

  } else {

    # for monthly
    mod <- stats::lm(Count ~ Location + lubridate::month(date), data = dub)
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



###plot##
ggplot(dub, aes(x=Location + lubridate::day(date), y= Count)) +
  geom_point(aes(colour=Count)) +
  theme_bw() +
  xlab("location date") +
  ylab("Count") +
  ggtitle(paste(x$fit_type, "based on", x$data_type, attr(x, "source"), "data")) +
  theme(legend.position = "None") +
  scale_color_viridis_c()

}

