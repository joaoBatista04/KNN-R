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

data_train<- data[train_ind, 3:6]
data_test <- data[-train_ind, 3:6]

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

print(predictions5)
