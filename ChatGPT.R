# Carregar a biblioteca class para KNN
library(class)

# Carregar dados do arquivo "Trabalho.csv"
dados <- read.csv("Trabalho.csv")

# Converter a variável Y para numérica
dados$Y <- as.numeric(as.character(dados$Y))

# 1 - Separar o conjunto em 70% de treino e 30% de teste
set.seed(123)
indices <- sample(1:nrow(dados), 0.7 * nrow(dados))
dados_treino <- dados[indices, ]
dados_teste <- dados[-indices, ]

# 2 - Ajustar modelos KNN (com K = 1, 3 e 5) e regressão linear
modelo_knn1 <- knn(train = dados_treino[, c("X1", "X2", "X3", "X4")], 
                   test = dados_teste[, c("X1", "X2", "X3", "X4")], 
                   cl = dados_treino$Y, k = 1)
modelo_knn3 <- knn(train = dados_treino[, c("X1", "X2", "X3", "X4")], 
                   test = dados_teste[, c("X1", "X2", "X3", "X4")], 
                   cl = dados_treino$Y, k = 3)
modelo_knn5 <- knn(train = dados_treino[, c("X1", "X2", "X3", "X4")], 
                   test = dados_teste[, c("X1", "X2", "X3", "X4")], 
                   cl = dados_treino$Y, k = 5)
modelo_regressao <- lm(Y ~ X1 + X2 + X3 + X4, data = dados_treino)

modelo_knn1 <- as.numeric(as.character(modelo_knn1))
modelo_knn3 <- as.numeric(as.character(modelo_knn3))
modelo_knn5 <- as.numeric(as.character(modelo_knn5))

# 3 - Calcular o erro de previsão para cada modelo
erro_knn1 <- abs(dados_teste$Y - modelo_knn1)
erro_knn3 <- abs(dados_teste$Y - modelo_knn3)
erro_knn5 <- abs(dados_teste$Y - modelo_knn5)
erro_regressao <- abs(dados_teste$Y - predict(modelo_regressao, newdata = dados_teste))

# 4 - Estimar o erro absoluto médio para cada método
mae_pontual_knn1 <- mean(erro_knn1)
mae_pontual_knn3 <- mean(erro_knn3)
mae_pontual_knn5 <- mean(erro_knn5)
mae_pontual_regressao <- mean(erro_regressao)

# 5 - Conclusões sobre os resultados
print(paste("Erro Absoluto Médio para KNN (K=1):", mae_pontual_knn1))
print(paste("Erro Absoluto Médio para KNN (K=3):", mae_pontual_knn3))
print(paste("Erro Absoluto Médio para KNN (K=5):", mae_pontual_knn5))
print(paste("Erro Absoluto Médio para Regressão Linear:", mae_pontual_regressao))

# Bootstrap para estimar intervalo de confiança de 95% para o erro médio
n_simulacoes <- 10000  # Número de simulações bootstrap
resultados <- matrix(0, nrow = n_simulacoes, ncol = 4)

for (i in 1:n_simulacoes) {
  # Amostragem com reposição dos dados de teste
  indices_amostra <- sample(1:length(dados_teste$Y), replace = TRUE)
  Y_amostra <- dados_teste$Y[indices_amostra]
  
  # Calcular erro absoluto médio para cada método usando a amostra bootstrap
  erro_bootstrap_knn1 <- mean(abs(Y_amostra - modelo_knn1[indices_amostra]))
  erro_bootstrap_knn3 <- mean(abs(Y_amostra - modelo_knn3[indices_amostra]))
  erro_bootstrap_knn5 <- mean(abs(Y_amostra - modelo_knn5[indices_amostra]))
  erro_bootstrap_regressao <- mean(abs(erro_regressao[indices_amostra] - Y_amostra))
  
  # Armazenar resultados
  resultados[i,] <- c(erro_bootstrap_knn1, erro_bootstrap_knn3, erro_bootstrap_knn5, erro_bootstrap_regressao)
}

# Calcular intervalo de confiança de 95% para cada método
intervalo_confianca <- apply(resultados, 2, function(x) quantile(x, c(0.025, 0.975)))

print(intervalo_confianca)

# Imprimir estimativas pontuais e intervalos de confiança
print("Estimativa Pontual e Intervalo de Confiança para KNN (K=1):")
print(paste("Estimativa Pontual:", mae_pontual_knn1))
print(paste("Intervalo de Confiança: [", intervalo_confianca[1,1], ", ", intervalo_confianca[2,1], "]"))
print("Estimativa Pontual e Intervalo de Confiança para KNN (K=3):")
print(paste("Estimativa Pontual:", mae_pontual_knn3))
print(paste("Intervalo de Confiança: [", intervalo_confianca[1,2], ", ", intervalo_confianca[2,2], "]"))
print("Estimativa Pontual e Intervalo de Confiança para KNN (K=5):")
print(paste("Estimativa Pontual:", mae_pontual_knn5))
print(paste("Intervalo de Confiança: [", intervalo_confianca[1,3], ", ", intervalo_confianca[2,3], "]"))
print("Estimativa Pontual e Intervalo de Confiança para Regressão Linear:")
print(paste("Estimativa Pontual:", mae_pontual_regressao))
print(paste("Intervalo de Confiança: [", intervalo_confianca[1,4], ", ", intervalo_confianca[2,4], "]"))
