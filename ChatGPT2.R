# Leitura dos dados (substitua 'seu_arquivo.csv' pelo nome do seu arquivo)
dados <- read.csv('Trabalho.csv')

# Forçando a conversão da variável de resposta Y para numérica
dados$Y <- as.numeric(as.character(dados$Y))

# 1 - Separe o conjunto em 70% de treino e 30% de teste
set.seed(123) # Define uma semente para reproducibilidade
amostra <- sample(1:nrow(dados), 0.7*nrow(dados))
dados_treino <- dados[amostra, ]
dados_teste <- dados[-amostra, ]

# 2 - Ajuste modelos KNN (com K = 1, 3 e 5) e regressão linear
library(class)

k <- c(1, 3, 5)
erros_knn <- numeric(length(k))

for (i in 1:length(k)) {
  knn_model <- knn(train = dados_treino[, c("X1", "X2", "X3", "X4")], 
                   test = dados_teste[, c("X1", "X2", "X3", "X4")], 
                   cl = dados_treino$Y, k = k[i])
  erros_knn[i] <- mean(abs(knn_model - dados_teste$Y))
}

regressao_linear <- lm(Y ~ X1 + X2 + X3 + X4, data = dados_treino)
erro_reg_linear <- mean(abs(predict(regressao_linear, newdata = dados_teste) - dados_teste$Y))

# 3 - Calcule o erro de previsão (em módulo) para cada elemento do teste
# Já calculado durante a etapa de ajuste dos modelos KNN

# 4 - Estime pontualmente o erro absoluto médio para cada método
erro_medio_knn <- mean(erros_knn)
erro_medio_reg_linear <- mean(abs(predict(regressao_linear, newdata = dados_teste) - dados_teste$Y))

# 5 - Calcule intervalos de confiança usando bootstrap para cada método
n_bootstrap <- 1000 # Número de amostras bootstrap

bootstrap_knn <- numeric(n_bootstrap)
bootstrap_reg_linear <- numeric(n_bootstrap)

for (i in 1:n_bootstrap) {
  # Amostras bootstrap
  amostra_ind <- sample(1:nrow(dados_teste), nrow(dados_teste), replace = TRUE)
  
  # Erro absoluto médio para KNN
  knn_bootstrap_model <- knn(train = dados_teste[amostra_ind, c("X1", "X2", "X3", "X4")], 
                             test = dados_teste[amostra_ind, c("X1", "X2", "X3", "X4")], 
                             cl = dados_teste[amostra_ind, "Y"], k = k[i])
  bootstrap_knn[i] <- mean(abs(knn_bootstrap_model - dados_teste[amostra_ind, "Y"]))
  
  # Erro absoluto médio para regressão linear
  reg_linear_bootstrap <- mean(abs(predict(regressao_linear, newdata = dados_teste[amostra_ind, ]) - dados_teste[amostra_ind, "Y"]))
  bootstrap_reg_linear[i] <- reg_linear_bootstrap
}

# Intervalo de confiança para erro absoluto médio (percentil 2.5 ao 97.5)
intervalo_conf_knn <- quantile(bootstrap_knn, c(0.025, 0.975))
intervalo_conf_reg_linear <- quantile(bootstrap_reg_linear, c(0.025, 0.975))

# Imprima os resultados
print(paste("Erro Médio KNN (K = 1, 3, 5): ", erro_medio_knn))
print(paste("Intervalo de Confiança para KNN: [", intervalo_conf_knn[1], ", ", intervalo_conf_knn[2], "]"))

print(paste("Erro Médio Regressão Linear: ", erro_medio_reg_linear))
print(paste("Intervalo de Confiança para Regressão Linear: [", intervalo_conf_reg_linear[1], ", ", intervalo_conf_reg_linear[2], "]"))
