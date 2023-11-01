calcular_intervalo_confianca_media <- function(dados, nivel_confianca = 0.95) {
  # Calcula a média e o desvio padrão da amostra
  media <- mean(dados)
  desvio_padrao <- sd(dados)
 
  # Calcula o tamanho da amostra
  n <- length(dados)
 
  # Calcula o erro padrão da média
  erro_padrao <- desvio_padrao / sqrt(n)
 
  # Calcula o valor crítico para o intervalo de confiança
  valor_critico <- qt((1 + nivel_confianca) / 2, df = n - 1)
 
  # Calcula o intervalo de confiança
  limite_inferior <- media - valor_critico * erro_padrao
  limite_superior <- media + valor_critico * erro_padrao
 
  # Retorna o intervalo de confiança
  return(c(limite_inferior, limite_superior))
}



plot_intervalo_confianca <- function(intervalo_confianca_matrix) {
  # Obtém o número de linhas na matriz
  num_linhas <- nrow(intervalo_confianca_matrix)
 
  # Cria um vetor de posições para os pontos no eixo x
  x_posicoes <- 1:num_linhas
 
  # Cria o gráfico base
  plot(1, type = "n", xlim = c(0, num_linhas + 1), ylim = range(intervalo_confianca_matrix),
       xlab = "Amostra", ylab = "Valor", main = "Intervalo de Confiança")
 
  # Adiciona os intervalos de confiança ao gráfico
  arrows(x0 = x_posicoes, y0 = intervalo_confianca_matrix[, 1],
         x1 = x_posicoes, y1 = intervalo_confianca_matrix[, 2],
         angle = 90, code = 3, length = 0.05)
}

# Exemplo de uso
set.seed(123)  # Define uma semente para reproducibilidade
populacao = rnorm(30000,50,10)

n = 3000
rep= 80
intervalos = matrix(0,rep,2)

for(i in 1:rep) {

  dados <- sample(populacao,n)  # Amostra de uma distribuição normal
  intervalos[i,] <- calcular_intervalo_confianca_media(dados)
 
}

plot_intervalo_confianca(intervalos)
abline(h=mean(populacao))
mean(intervalos[,1] < 50 & intervalos[,2] > 50)