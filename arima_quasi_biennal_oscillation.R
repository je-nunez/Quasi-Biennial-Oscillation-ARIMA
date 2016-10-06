
required_packages <- c("forecast", "Hmisc")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if( 0 < length(missing_packages) ) {
  options(repos=c(CRAN="http://cran.cnr.Berkeley.edu/"))
  install.packages(missing_packages, dep=TRUE)
}

require(forecast)
require(Hmisc)

# Auxiliary functions on Arima and saving the forecast

run_arima_model_and_forecast <- function(univariate_time_series, confidence_level) {
  
  # ts_adjusted <- seasadj( stl(univariate_time_series, s.window = "periodic") )
  # tsdisplay( diff(ts_adjusted), main = "")
  
  truncateAfterMonths <- NULL     # do not truncate to build the ARIMA model

  arima_time_series_model <- auto.arima(univariate_time_series,
                                        truncate = truncateAfterMonths,
                                        seasonal = FALSE, stationary = FALSE,
                                        stepwise = FALSE,
                                        trace = TRUE)

  cat("Summary of the ARIMA model for the Quasi-biennial Oscillation of the Equatorial zonal Wind at 10hPa")
  
  summary(arima_time_series_model)

  Acf(residuals(arima_time_series_model))
  Ljung_Box_tests <- Box.test(x = residuals(arima_time_series_model), 
                              lag = 1, fitdf = 0, type = "Ljung")
  
  print(Ljung_Box_tests)
  
  number_of_periods_to_forecast <- 24      # number of months to forecast in the future

  arima_forecast <- forecast(object = arima_time_series_model,
                             h = number_of_periods_to_forecast,
                             level = confidence_level,
                             fan = FALSE)

  arima_forecast
}

plot_and_save_forecast_of_QSBO <- function(qsbo_forecast, confidence_level, save_plot_to_png_file) {

  png(save_plot_to_png_file, width=1600, height=1200)

  plot_title <- paste(c(c("Quasi-biennial Oscillation of the Equatorial zonal Wind at 10hPa (Singapur),",
                          "and its Forecast at confidence level percentages"),
                        confidence_level),
                      collapse = " ")

  old_par <- par(mar = c(5, 5, 5, 2) + 0.1)

  plot(qsbo_forecast, las=1, main = "")

  title(main = plot_title, col.main = "blue",
        xlab = "Time", ylab = "Equatorial-zone Wind Speed Average at 10hPa (0.1m/sec)",
        col.lab = "red", cex.lab = 1.75)

  minor.tick(nx=5)
  
  par(old_par)
  dev.off()
  
  cat("Plot of the Quasi-biennial Oscillation of the Equatorial zonal Wind at 10hPa saved to:",
      save_plot_to_png_file, sep = " ")
}

#### MAIN PROGRAM

windSpeed10hPa.monthly_observations <- c(-164.0,-167.0,-144.0,-35.0,116.0,82.0,45.0,116.0,140.0,144.0,116.0,145.0,66.0,-43.0,-89.0,-147.0,-210.0,-243.0,-245.0,-312.0,-346.0,-341.0,-180.0,-122.0,-244.0,-348.0,-366.0,-378.0,-329.0,-128.0,-17.0,34.0,35.0,84.0,157.0,212.0,149.0,157.0,182.0,154.0,64.0,48.0,-2.0,-75.0,-30.0,-133.0,-252.0,-284.0,-283.0,-302.0,-315.0,-312.0,-305.0,-318.0,-347.0,-332.0,-292.0,-169.0,91.0,209.0,161.0,170.0,173.0,161.0,101.0,42.0,56.0,143.0,167.0,175.0,216.0,169.0,145.0,138.0,68.0,-126.0,-268.0,-295.0,-308.0,-320.0,-340.0,-331.0,-329.0,-268.0,-348.0,-349.0,-351.0,-376.0,-218.0,57.0,85.0,115.0,137.0,157.0,184.0,185.0,196.0,102.0,64.0,-35.0,-238.0,-285.0,-322.0,-359.0,-335.0,-308.0,-278.0,-181.0,-277.0,-307.0,-322.0,-311.0,-307.0,-16.0,-16.0,41.0,96.0,160.0,228.0,186.0,187.0,198.0,205.0,206.0,175.0,131.0,18.0,-49.0,-149.0,-242.0,-245.0,-229.0,-176.0,-193.0,-247.0,-299.0,-292.0,-22.0,47.0,147.0,130.0,161.0,228.0,193.0,126.0,128.0,66.0,108.0,8.0,-194.0,-201.0,-221.0,-230.0,-259.0,-254.0,-255.0,-298.0,-307.0,-314.0,-343.0,-372.0,-358.0,-218.0,-238.0,-308.0,-280.0,-43.0,43.0,28.0,-69.0,-15.0,62.0,109.0,41.0,14.0,50.0,87.0,129.0,147.0,156.0,158.0,176.0,224.0,101.0,-100.0,-244.0,-257.0,-247.0,-222.0,-249.0,-262.0,-257.0,-201.0,-223.0,-287.0,-334.0,-263.0,4.0,139.0,145.0,129.0,142.0,182.0,246.0,180.0,132.0,171.0,132.0,93.0,51.0,22.0,0.0,-80.0,-132.0,-144.0,-170.0,-261.0,-313.0,-347.0,-351.0,-374.0,-380.0,-232.0,-201.0,1.0,121.0,177.0,188.0,171.0,127.0,116.0,6.0,69.0,19.0,-21.0,-28.0,-54.0,-137.0,-228.0,-200.0,-226.0,-318.0,-326.0,-338.0,-368.0,-208.0,-15.0,50.0,107.0,111.0,180.0,213.0,176.0,172.0,169.0,114.0,75.0,46.0,-34.0,-174.0,-242.0,-275.0,-262.0,-267.0,-320.0,-380.0,-337.0,-340.0,-342.0,-318.0,-182.0,-110.0,-218.0,-172.0,-47.0,-104.0,-171.0,-282.0,-259.0,-230.0,52.0,128.0,140.0,150.0,149.0,163.0,144.0,59.0,-109.0,-141.0,-262.0,-309.0,-306.0,-292.0,-244.0,-310.0,-310.0,-291.0,-177.0,-54.0,-261.0,-275.0,-333.0,-220.0,18.0,80.0,74.0,101.0,119.0,133.0,162.0,218.0,145.0,142.0,185.0,148.0,117.0,117.0,77.0,67.0,69.0,40.0,-154.0,-257.0,-251.0,-258.0,-280.0,-286.0,-285.0,-313.0,-334.0,-330.0,-320.0,-295.0,51.0,173.0,138.0,167.0,106.0,123.0,96.0,115.0,91.0,136.0,179.0,127.0,-55.0,-222.0,-231.0,-205.0,-148.0,-11.0,45.0,24.0,16.0,25.0,17.0)

windSpeed10hPa.ts <- ts(data = windSpeed10hPa.monthly_observations,
                        start = c(1987,1),
                        deltat = 1/12)

forecast_confidence_level <- c(50, 80)            # confidence level of the areas to predict

windSpeed10hPa.forecast <- run_arima_model_and_forecast(univariate_time_series = windSpeed10hPa.ts,
                                                        confidence_level = forecast_confidence_level)

new_png_filename <- file.path(getwd(), "arima_quasi_biennal_oscillation_at_10_hPa.png")

plot_and_save_forecast_of_QSBO(qsbo_forecast = windSpeed10hPa.forecast,
                               confidence_level = forecast_confidence_level,
                               save_plot_to_png_file = new_png_filename)

