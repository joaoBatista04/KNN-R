import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsRegressor
from sklearn.linear_model import LinearRegression

# Carregar dados do arquivo "Trabalho.csv"
dados = pd.read_csv("Trabalho.csv")

# Separar variáveis independentes (X) e variável dependente (Y)
X = dados[['X1', 'X2', 'X3', 'X4']]
Y = dados['Y']

# 1 - Separar o conjunto em 70% de treino e 30% de teste
X_treino, X_teste, Y_treino, Y_teste = train_test_split(X, Y, test_size=0.3, random_state=42)

# 2 - Ajustar modelos KNN (com K = 1, 3 e 5) e regressão linear
modelo_knn1 = KNeighborsRegressor(n_neighbors=1)
modelo_knn3 = KNeighborsRegressor(n_neighbors=3)
modelo_knn5 = KNeighborsRegressor(n_neighbors=5)
modelo_regressao = LinearRegression()

modelo_knn1.fit(X_treino, Y_treino)
modelo_knn3.fit(X_treino, Y_treino)
modelo_knn5.fit(X_treino, Y_treino)
modelo_regressao.fit(X_treino, Y_treino)

# 3 - Calcular o erro absoluto médio para cada modelo nos dados de teste
erro_knn1 = np.abs(Y_teste - modelo_knn1.predict(X_teste))
erro_knn3 = np.abs(Y_teste - modelo_knn3.predict(X_teste))
erro_knn5 = np.abs(Y_teste - modelo_knn5.predict(X_teste))
erro_regressao = np.abs(Y_teste - modelo_regressao.predict(X_teste))

# 4 - Estimar o erro absoluto médio para cada método usando bootstrap
num_bootstrap = 1000
resultados_bootstrap_knn1 = np.percentile(np.random.choice(erro_knn1, (len(erro_knn1), num_bootstrap)), [2.5, 97.5])
resultados_bootstrap_knn3 = np.percentile(np.random.choice(erro_knn3, (len(erro_knn3), num_bootstrap)), [2.5, 97.5])
resultados_bootstrap_knn5 = np.percentile(np.random.choice(erro_knn5, (len(erro_knn5), num_bootstrap)), [2.5, 97.5])
resultados_bootstrap_regressao = np.percentile(np.random.choice(erro_regressao, (len(erro_regressao), num_bootstrap)), [2.5, 97.5])

# 5 - Imprimir resultados
print("Erro Absoluto Médio para KNN (K=1):", np.mean(erro_knn1))
print("Erro Absoluto Médio para KNN (K=3):", np.mean(erro_knn3))
print("Erro Absoluto Médio para KNN (K=5):", np.mean(erro_knn5))
print("Erro Absoluto Médio para Regressão Linear:", np.mean(erro_regressao))

print("Intervalo de Confiança para KNN (K=1):", resultados_bootstrap_knn1)
print("Intervalo de Confiança para KNN (K=3):", resultados_bootstrap_knn3)
print("Intervalo de Confiança para KNN (K=5):", resultados_bootstrap_knn5)
print("Intervalo de Confiança para Regressão Linear:", resultados_bootstrap_regressao)
