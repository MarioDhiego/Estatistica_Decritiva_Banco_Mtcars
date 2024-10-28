library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(magrittr)
library(DT)
library(corrgram)

# Carregar o conjunto de dados mtcars
data(mtcars)

# Remover linhas com valores ausentes
mtcars <- na.omit(mtcars)

# Verificar e remover duplicatas
mtcars <- unique(mtcars)

# Padronizar nomes de colunas (exemplo: converter para minúsculas)
colnames(mtcars) <- tolower(colnames(mtcars))
summary(mtcars)

datatable(mtcars, 
          options = list(pageLength = 5))

# Coeficiente de Correlação
correlacao <- cor.test(mtcars$wt, mtcars$mpg)
correlacao

# Diagrama de Disopersão Clássico
mtcars %>%
ggplot(aes(x = wt, y = mpg 
           #color = 
           ))+
  geom_point(color = "red", shape= 21)+
  geom_smooth(method = "loess", 
              se = TRUE, 
              color = "blue")+
  theme_gray(base_size = 15)+
  labs(
    title = "Diagrama de Dispersão",
    y = "Milhas por Galão (mpg)",
    x = "Peso (wt)",
    caption = "Fonte: mtcars dataset"
  )
  
# Disgrama de Disoersão c/ Correlação
mtcars %>%
  ggplot(aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(method = "lm", 
              se = TRUE)+
  theme_gray(base_size = 15)+
  labs(
    title = "Diagrama de Dispersão",
    ylab("Miles/(US) gallon"),
    xlab("Weight (1000 lbs)"),
    caption = "Fonte: mtcars dataset"
  )+
  annotate("text", x = 4.75, y = 30, 
           label = paste0("Correlação: ", 
                          round(correlacao$estimate, 3))
  )



# Matriz de Correlação
corrgram(mtcars, order=TRUE, 
         lower.panel=panel.shade,
         upper.panel=panel.pie, 
         text.panel=panel.txt,
         main="Matriz de Correlação - mtcars")


# Procurar Var correlacionadas
library(GGally)
ggpairs(mtcars,
        lower = list(continuous = "smooth")
        )


# Modelo Regressão Linear - Clássico
library(car) # vif
mtcars %>%


# Modelo com 11 variaveis X
Modelo1 <- lm(mpg ~ wt + cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, mtcars)
summary(Modelo1)
vif(Modelo1)

# Modelo com 10 variaveis (X = cyl)
Modelo2 <- lm(mpg ~ wt + disp + hp + drat + wt + qsec + vs + am + gear + carb, mtcars)
summary(Modelo2)
vif(Modelo2)


# Modelo com 9 variaveis (X = vs)
Modelo3 <- lm(mpg ~ wt + disp + hp + drat + wt + qsec + am + gear + carb, mtcars)
summary(Modelo3)
vif(Modelo3)


# Modelo com 8 variaveis (X = carb)
Modelo4 <- lm(mpg ~ wt + disp + hp + drat + wt + qsec + am + gear, mtcars)
summary(Modelo4)
vif(Modelo4)

# Modelo com 7 variaveis (X = gear)
Modelo5 <- lm(mpg ~ wt + disp + hp + drat + wt + qsec + am, mtcars)
summary(Modelo5)
vif(Modelo5)


# Modelo com 6 variaveis (X = drat)
Modelo6 <- lm(mpg ~ wt + disp + hp + qsec + am, mtcars)
summary(Modelo6)
vif(Modelo6)


# Modelo com 4 variaveis (X = disp, hp)
Modelo_Final <- lm(mpg ~ wt + qsec + am, mtcars)
summary(Modelo_Final)
vif(Modelo_Final)
plot(Modelo_Final)

# Modelo Regressão Linear - Machine Learning
library(tidyverse)
library(caret)

# Separação Dados
# 70% = Treino
# 30% = Teste

#
sample_n(mtcars, 10)
set.seed(123)

# Dividir os Dados
Amostra_Treino <- mtcars$mpg %>%
                  createDataPartition(p = 0.7, list = FALSE)

# Subconjuntos

Dados_Treino <- mtcars[Amostra_Treino,]
Dados_Teste  <- mtcars[-Amostra_Treino,]

# Modelo 1
options(scipen = 999)
Machine1 <- lm(mpg ~., data = Dados_Treino)
summary(Machine1)


# Tabela Modelo 1
library(sjPlot)
tab_model(Machine1)


# Predição Dados teste
Predicao_Machine1 <- predict(Machine1, Dados_Teste)
Predicao_Machine1

# Medidas
RMSE(Predicao_Machine1, Dados_Teste$mpg)
R2(Predicao_Machine1, Dados_Teste$mpg)
AIC(Machine1)


# Modelo 2
options(scipen = 999)
Machine2 <- lm(mpg ~ wt + qsec + am, data = Dados_Treino)

# Coeficientes
summary(Machine2)

# IC 95%
confint(Machine1)

# Tabela Modelo 2
library(sjPlot)
Tabela1 <- tab_model(Machine2,
          show.intercept = TRUE,
          show.est = TRUE,
          show.p = TRUE,
          show.stat = TRUE,
          show.ci = 0.95,
          show.se = TRUE,
          show.aic = TRUE,
          show.obs = TRUE)




# Predição 
Predicao_Machine2 <- predict(Machine2, Dados_Teste)
Predicao_Machine2

plot_model(Machine2, 
           type = "pred",
           show.legend = TRUE)














