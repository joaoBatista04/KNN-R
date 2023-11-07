#Leitura dos dados
data <- read.csv("./Trabalho.csv", sep=",", header=TRUE)

#Definicao da coluna Y
row_labels = data[,2]

#Normalizacao dos parametros X1, X2, X3 e X4
data[,3:6] <- scale(data[,3:6])

#Definindo a seed
set.seed(123)

#Separando os dados em conjuntos de treino e de teste
size <- floor(0.7 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = size)

train_labels <- data[train_ind, 2]
test_labels <- row_labels[-train_ind]

data_train <- data[train_ind, 3:6]
data_test <- data[-train_ind, 3:6]

data_train_linear_regression <- data[train_ind, 2:6]
data_test_linear_regression <- data[-train_ind, 2:6]

#Utilizando o método KNN
library(class)
predictions1 <- knn(train = data_train,
                   test = data_test,
                   cl = train_labels,
                   k = 1)

predictions3 <- knn(train = data_train,
                   test = data_test,
                   cl = train_labels,
                   k = 3)

predictions5 <- knn(train = data_train,
                   test = data_test,
                   cl = train_labels,
                   k = 5)

#Calculando a regressão linear
modelo_regressao <- lm(Y ~ X1 + X2 + X3 + X4, data=data_train_linear_regression)

#Transformando os factors em itens numericos
predictions1 <- as.numeric(as.character(predictions1))
predictions3 <- as.numeric(as.character(predictions3))
predictions5 <- as.numeric(as.character(predictions5))

# 3 - Calcular o erro de previsão para cada modelo
erro_knn1 <- abs(test_labels - predictions1)
erro_knn3 <- abs(test_labels - predictions3)
erro_knn5 <- abs(test_labels - predictions5)
erro_regressao <- abs(test_labels - predict(modelo_regressao, newdata = data_test_linear_regression))

# 4 - Estimar o erro absoluto médio para cada método
mae_knn1 <- mean(erro_knn1)
mae_knn3 <- mean(erro_knn3)
mae_knn5 <- mean(erro_knn5)
mae_regressao <- mean(erro_regressao)

# 5 - Conclusões sobre os resultados
print(paste("Erro Absoluto Médio para KNN (K=1):", mae_knn1))
print(paste("Erro Absoluto Médio para KNN (K=3):", mae_knn3))
print(paste("Erro Absoluto Médio para KNN (K=5):", mae_knn5))
print(paste("Erro Absoluto Médio para Regressão Linear:", mae_regressao))

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