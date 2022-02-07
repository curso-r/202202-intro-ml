# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(skimr)

# Exercício de Árvore de Decisão #########################################
# Objetivo: refazer o modelo feito no exercício de regressão logística para
# prever sobreviventes no desastre do Titanic (variável 'Survived') usando
# Árvore de Decisão.

# Questões interessantes:
# Valeu a pena mudar de regressão logística para árvore de decisão? Justifique.
# As variáveis importantes são as mesmas em ambos os modelos?

# Dicas:
# 1) Tente reaproveitar ao máximo o código usado na regressão logística.
# 2) Identifique quais partes do código devem mudar por causa da troca de
#    logistic_reg() para decision_tree().
# 3) Tome conhecimento sobre quais datapreps não precisa mais fazer
#    (scale, log, interação, etc).
# 3) Crie códigos para comparar os dois modelos. (duas curvas roc no mesmo gráfico)


