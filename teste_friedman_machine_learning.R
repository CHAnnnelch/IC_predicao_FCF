library(tidyverse)

df <- read.csv('friedman_data_FCLF.csv')

d_MAE <- df %>%
  select(knn_MAE, xgb_MAE, ARIMA_MAE, RF_MAE, Lasso_MAE) %>%
  rowid_to_column() %>%
  gather(key = "Modelo", value = "MAE", knn_MAE:Lasso_MAE) %>%
  group_by(Modelo) %>%
  ungroup()  

view(d_MAE)

library(ggstatsplot)
library(see)

ggwithinstats(
  data=d_MAE,
  x = Modelo,
  y = MAE,
  type='nonparametric',
  p.adjust.method = 'bonferroni',
)

library(effectsize)

interpret_kendalls_w(0.52) #mostra que existem diferenças significativas no valor dos RMSEs de treino dos modelos e essas diferenças são moderadas
?interpret_kendalls_w()


d_train_RMSE <- df %>%
  select(knn_train_RMSE, xgb_train_RMSE, ARIMA_train_RMSE, RF_train_RMSE, Lasso_train_RMSE) %>%
  rowid_to_column() %>%
  gather(key = "Modelo", value = "Train_RMSE", knn_train_RMSE:Lasso_train_RMSE) %>%
  group_by(Modelo) %>%
  ungroup()  


ggwithinstats(
  data=d_train_RMSE,
  x = Modelo,
  y = Train_RMSE,
  type='nonparametric',
  p.adjust.method = 'bonferroni'
)


interpret_kendalls_w(0.70) #mostra que existem diferenças significativas no valor dos RMSEs de treino dos modelos e essas diferenças são substanciais

?interpret_kendalls_w()

#teste com a rmse geral por empresa

d_rmse_final <- read.csv("rmse_final_FCLF.csv")

d_RMSE <- d_rmse_final %>%
  select(RMSE_knn, RMSE_xgb, RMSE_ARIMA, RMSE_RF, RMSE_Lasso) %>%
  rowid_to_column() %>%
  gather(key = "Modelo", value = "RMSE", RMSE_knn:RMSE_Lasso) %>%
  group_by(Modelo) %>%
  ungroup()  

view(d_RMSE)

ggwithinstats(
  data=d_RMSE,
  x = Modelo,
  y = RMSE,
  type='nonparametric',
  p.adjust.method = 'bonferroni',
)

interpret_kendalls_w(0.57) #mostra que existem diferenças significativas no valor dos RMSEs de treino dos modelos e essas diferenças são substanciais

?interpret_kendalls_w()
