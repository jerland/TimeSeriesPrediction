install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("timeSeries")
install.packages("ggfortify")
library("readxl")
library("dplyr")
library("ggplot2")
library("timeSeries")
library("ggfortify")
library("forecast")

export <- read_excel("export.xlsx")
mutate(export, datum=as.Date(datum, format = "%Y-%m-%d"), pos=as.numeric(pos)) 

ts_export <- ts(export[, 2], frequency = 365, start=c(2015))
mts_export <- msts(export[, 2], seasonal.periods = c(7, 91.3125, 365.25) , start=c(2015))
#Look at timeseries weekly and monthly
autoplot(ts_export) + ylab("Positions")

#Analyse autocorrelation and partial autocorrelation in the original time series
acf(mts_export)
pacf(export[, 2], lag.max=365)

seasonplot(mts_export)
#KPSS, Box-test, augmented dickey fueller tests
Box.test(ts_export)
kpss.test(ts_export)
adf.test(ts_export)

#Forecasting using STL
mts_export %>% stlf() %>% autoplot()

#Creating and testing dynamic harmonic regressors (using time-series object)
plots <- list()
for (i in seq(3)) {
  fit <- auto.arima(ts_export, xreg = fourier(ts_export, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(ts_export, K=i, h=30))) +
    xlab(paste("K=",i," AICC=",round(fit[["aicc"]],2))) +
    ylab("") + ylim(0,52000) + xlim(2019,2019.5)
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  nrow=3)
plots

#Creating and testing dynamic harmonic regressors (using multiseasonal object mts)
fit2 <- auto.arima(mts_export, xreg=fourier(mts_export, K=c(2, 44, 175)), seasonal=FALSE, lambda=0)

#Create and test harmonic regressors 
fit_tslm <- tslm(mts_export ~ fourier(mts_export, K = c(3,44, 170)), lambda = 0)

fc <- forecast(fit_tslm, newdata = data.frame(fourier(mts_export, K = c(3,44, 170), h = 30)))
autoplot(fc) + xlab("AIC=", AIC(fit_tslm))

#Plotting MSTL to see seasonality, trend, and remainder component
mts_export %>% mstl() %>%
  autoplot()

#Creating TBATS
mts_tbats <- tbats(mts_export)
ts_tbats <- tbats(ts_export)

fc2 <- forecast(mts_tbats, h=30)
fc3 <- forecast(ts_tbats, h=30)

plot(fc2)

?tbats
#Checking residuals

AIC(fit_tslm)
