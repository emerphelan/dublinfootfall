visuals <- function(fit, output) {

  df <- output

  model <- fit(load_footfall())

  ggplot(df, aes(x=x, y=temp)) +
    geom_point(aes(colour=temp)) +
    theme_bw() +
    xlab("Year") +
    ylab("Temperature Anomaly") +
    ggtitle(paste(x$fit_type, "based on", x$data_type, attr(x, "source"), "data")) +
    geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
    theme(legend.position = "None") +
    scale_color_viridis_c()

  plot(model)

  plot(fit(load_footfall()))


  }
