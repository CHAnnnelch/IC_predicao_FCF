library(tidyverse)

df <- read.csv('friedman_data.csv')

d_MAE <- df %>%
  select(knn_MAE, xgb_MAE, ARIMA_MAE, RF_MAE, Lasso_MAE) %>%
  rowid_to_column() %>%
  gather(key = "Modelo", value = "MAE", knn_MAE:Lasso_MAE) %>%
  group_by(Modelo) %>%
  ungroup()  

view(d_MAE)

library(ggstatsplot)

png("plot_MAE_friedman.png", width=1080, height=720)
ggwithinstats(
  data=d_MAE,
  x = Modelo,
  y = MAE,
  type='nonparametric',
  p.adjust.method = 'bonferroni',
)
dev.off()

library(effectsize)

interpret_kendalls_w(0.44) #mostra que existem diferenças significativas no valor dos RMSEs de treino dos modelos e essas diferenças são moderadas
?interpret_kendalls_w()


d_train_RMSE <- df %>%
  select(knn_train_RMSE, xgb_train_RMSE, ARIMA_train_RMSE, RF_train_RMSE, Lasso_train_RMSE) %>%
  rowid_to_column() %>%
  gather(key = "Modelo", value = "Train_RMSE", knn_train_RMSE:Lasso_train_RMSE) %>%
  group_by(Modelo) %>%
  ungroup()  

png("plot_RMSE_train_friedman.png", width=1080, height=720)
ggwithinstats(
  data=d_train_RMSE,
  x = Modelo,
  y = Train_RMSE,
  type='nonparametric',
  p.adjust.method = 'bonferroni'
)
dev.off()

interpret_kendalls_w(0.69) #mostra que existem diferenças significativas no valor dos RMSEs de treino dos modelos e essas diferenças são substanciais

?interpret_kendalls_w()