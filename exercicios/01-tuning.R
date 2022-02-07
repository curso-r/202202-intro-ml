# A base de dados 'concrete' consiste em dados sobre a composição de diferentes
# misturas de concreto e a sua resistência à compressão - 'compressive_strength'
# (https://pt.wikipedia.org/wiki/Esfor%C3%A7o_de_compress%C3%A3o)
# O nosso objetivo é prever a resitência à compressão a partir dos ingredientes.
# A coluna 'compressive_strength' é a variável resposta. A coluna 'age' nos diz
# a idade do concreto na hora do teste (o concreto fica mais forte ao longo do tempo) e
# o resto das colunas como 'cement' e 'water' são componentes do concreto em
# kilogramas por metro cúbico.

# Pacotes -----------------------------------------------------------------

library(tidymodels)

# Base de dados -----------------------------------------------------------

data(concrete, package = "modeldata")
nrow(concrete)
glimpse(concrete)

# exercício 0 -------------------------------------------------------------
# Defina uma 'recipe' que normalize todas as variáveis explicativas.
# Dicas: recipe(), step_normalize(), all_numeric_predictors().

# exercício 1 -------------------------------------------------------------
# Defina uma especificação de f que caracterize uma regressão linear
# (mode 'regression'). Especifique também que você deseja tunar a 'penalty' e
# a 'mixture'.
# Dicas: linear_reg(), set_engine(), set_mode(), tune().

# exercício 2 -------------------------------------------------------------
# Defina um 'workflow' que junte a receita do ex. 0 e o modelo do ex. 1.
# Dicas: workflow(), add_model(), add_recipe().

# exercício 3 -------------------------------------------------------------
# Crie um objeto que represente a estratégia de reamostragem do tipo K-Fold cross
# validation com 5 folds.
# Dica: vfold_cv().

# exercício 4 -------------------------------------------------------------
# Defina um grid de hiperparâmetros que você irá testar tanto de 'penalty' quanto
# de 'mixture'.
# Dica: grid_regular(), penalty(), mixture().

# exercício 5 -------------------------------------------------------------
# Execute a tunagem do modelo usando os objetos criados nos exercícios anteriores.
# Dica: tune_grid()

# exercício 6 -------------------------------------------------------------
# Visualize os resultados dos modelos ajustados e atualize o workflow com os
# parâmetros do melhor modelo.
# Dica: autoplot(), collect_metrics(), show_best(), select_best(), finalize_workflow().

# desafio 1 ---------------------------------------------------------------
# Qual hiperparâmetro tem maior efeito no resultado do modelo? Justifique
# a sua afirmativa com um gráfico.

# exercício 7 -------------------------------------------------------------
# Ajuste o modelo na base de treino e verifique o desempenho na base de teste.
# Dica: last_fit(split = ______initial_split), collect_metrics()

# exercício 8 -------------------------------------------------------------
# Ajuste o modelo final para a base inteira salve-o em um arquivo .rds.
# Dica: fit(), saveRDS().

