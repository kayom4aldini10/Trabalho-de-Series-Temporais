library(forecast)
library(tseries)
library(ggplot2)

# CRIAÇÃO E PLOT DA SÉRIE TEMPORAL
y <- ts(dolar$dolar_fechamento, start = c(2010, 1), frequency = 12)
ts.plot(y)

# DECOMPOSIÇÃO E PLOT DA SÉRIE TEMPORAL
dec <- decompose(y)
autoplot(dec)

# TRANSFORMAR EM BOX-COX
lambda <- BoxCox.lambda(y)
y_bc <- BoxCox(y, lambda - lambda)
ggtsdisplay(y_bc)

# TESTE DE DIFERENCIAÇÃO
ndiffs(y_bc)
y_diff <- diff(y_bc,1)
ggtsdisplay(y_diff)
nsdiffs(y_diff)

# SEPARAÇÃO DE DADOS
y_treino <- window(y, start = c(2010,1), end = c(2018,12))
y_val <- window(y, start = c(2019,1))

# AJUSTE DO MODELO ARIMA
auto.arima(y_treino, lambda = lambda, stepwise = FALSE, approximation = FALSE, trace = TRUE )
flit <- Arima(y_treino, order = c(1,1,0), include.drift = TRUE,  lambda = 0.11229755280289 )
summary(flit)

# PREVISÃO DE 1 PASSO A FRENTE
previsao <- forecast(flit, h=1)
plot(previsao)

# CÁLCULO DOS ERROS
errores_1 <- y_val - previsao$mean
MAE_1  <- mean(abs(errores_1), na.rm=TRUE)
RMSE_1 <- sqrt(mean((errores_1)^2, na.rm=TRUE))
MAPE_1 <- mean(abs(errores_1 / y_val), na.rm=TRUE) * 100

# PREVISÃO DE 12 PASSOS A FRENTE
prev12 <- forecast(flit, h=12) 
plot(prev12)

# CÁLCULO DOS ERROS
errores_12 <- y_val - prev12$mean
MAE_12  <- mean(abs(errores_12), na.rm=TRUE)
RMSE_12 <- sqrt(mean((errores_12)^2, na.rm=TRUE))
MAPE_12 <- mean(abs(errores_12 / y_val), na.rm=TRUE) * 100

# COMPARAÇÃO DOS MÉTODOS DE PREVISÃO
erros <- data.frame(
  Metodo = c("1 passo", "12 passos"),
  MAE  = c(MAE_1, MAE_12),
  RMSE = c(RMSE_1, RMSE_12),
  MAPE = c(MAPE_1, MAPE_12)
)
