###### Análise Estatística do Banco mtcars do R4.0 #####################################################################
###### Objetivo: Análise do Consumo Médio de Combustível de Carros Modelos 1973/1974
###### Descrição: o conjunto de dados, denominado de mtcars, foi obtido a partir das edições de março, abril, junho e julho de 1974 da revista Motor Trend para um estudo realizado por Hocking (1976) e posteriormente, reportado por Henderson e Velleman (1981)

####### INTRODUÇÃO #####################################################################################
# Na crise petrolífera de 1973, membros da Organização dos Países Árabes Exportadores de Petróleo (OPAEP) aplicaram sanções em protesto ao apoio dos Estados Unidos e outras nações à Israel durante a Guerra do Yom Kippur.
# O conflito resultou no aumento do preço do petróleo de três dólares por barril para cerca de 12 doláres no mundo inteiro, sendo que os preços fixados para os Estados Unidos foram ainda maiores.
# Como uma alternativa à alta do preço do petróleo no mercado mundial, os Estados Unidos iniciaram um programa de eficiência energética, conhecido como Corporate Average Fuel Economy (CAFE), com o propósito de reduzir o consumo de combustível de carros, pick-ups, minivans e SUVs (Almeida Filho, 2018)
# O conjunto de dados, denominado de mtcars,está disponível na biblioteca datasets do software R para consulta.

##########################################################################################################


###### Carregar os Pacotes ############################################################################
library(tidyverse)
library(GGally)
library(kableExtra)
library(ggplot2)
library(plotly)
library(gridExtra)
library(gtsummary)
library(xtable)
library(PerformanceAnalytics)
library(readxl)
#######################################################################################################


###### ANÁLISE EXAPLORATÓRIA DE DADOS #################################################################
###### As variáveis observadas no conjunto de dados são definidas como:
# Variável Resposta (Y): 
#  1) mpg: eficiência (milhas por galão de combustível).
# Variáveis Explicativas(X): 
#  1) cyl: número de cilindros.
#  2) disp: cilindradas (polegada cúbica).
#  3) hp: potência bruta (HP).
#  4) drat: relação de eixo traseiro.
#  5) wt: peso (1000 libras).
#  6) qsec: tempo no quarto de milha (segundos).
#  7) vs: formato do motor (0 = V e 1 = linha).
#  8) am: tipo de transmissão (0 = automático e 1 = manual).
#  9) gear: número de marchas para frente.
# 10) carb: número de carburadores.
######################################################################################################


############ Estatística Descritiva ##################################################################
head(mtcars)
summary(mtcars)

###### Resumo Tabular ################################################################################
resumo <- mtcars %>%
  select(-am,-vs) %>% 
  pivot_longer(everything()) %>%
  group_by(name) %>% 
  summarise_at("value", 
               list(Missing =~sum(is.na(.)),media=~mean(.),
                    desvPad=~sd(.), minimo=~min(.),
                    Q1=~quantile(.,0.25),med=~median(.),
                    Q3=~quantile(.,0.75),maxi=~max(.))) %>% 
  mutate_if(is.numeric, format, digits=3,nsmall = 2)

colnames(resumo) <- c('Variável', 'Missing', 'Média',
                      'Desvio padrão', 'Mínimo', 'Q1',
                      'Mediana', 'Q3', 'Máximo')
kbl(resumo, booktabs = T, caption = 'Estatísticas descritivas daS Características Quantitativa', longtable = T) %>% 
  kable_styling(position = 'center',latex_options = c("striped", "hold_position"))
###################################################################################################

###### Correlograma das variáveis explicativas #####################################
mtcars %>% 
  select(-vs,-am, -mpg) %>% 
  ggpairs() 
####################################################################################



###### Gráfico de Dispersão ##########################################
###### Gráfico 1 #####################################################
fig1 <- mtcars %>% 
  ggplot(aes(x=cyl,y=mpg)) +
  geom_point() +
  labs(x = 'Número de cilindros', y = 'Eficiência (mpg)')+ 
  geom_smooth(method = lm, se = FALSE)
fig1
######################################################################

###### Gráfico 2 #####################################################
fig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point() +
  labs(x = 'Cilindradas (in^3)', y = 'Eficiência (mpg)')+ 
  geom_smooth(method = lm, se = FALSE)
fig2
######################################################################

###### Gráfico 3 #####################################################
fig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point() +
  labs(x = 'Potência (HP)', y = 'Eficiência (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig3
######################################################################


###### Gráfico 4 #####################################################
fig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point() +
  labs(x = 'Relação de Eixo T', y = 'Eficiência (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig4
######################################################################

###### Gráfico 5 #####################################################
fig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point() +
  labs(x = 'Peso (1000 lb)', y = 'Eficiência (mpg)')+ 
  geom_smooth(method = lm, se = FALSE)
fig5
######################################################################


###### Gráfico 6 #####################################################
fig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point() +
  labs(x = 'Tempo (s)', y = 'Eficiência (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig6
######################################################################

###### Gráfico 7 #####################################################
fig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point() +
  labs(x = 'Número de Marchas', y = 'Eficiência (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig7
######################################################################

###### Gráfico 8 #####################################################
fig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point() +
  labs(x = 'Número de Carburadores', y = 'Eficiência (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig8
######################################################################

###### Gráfico Agrupado #####################################################

grid.arrange(fig2, fig3, fig4, fig5, fig6, ncol = 3, nrow = 2)
grid.arrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, ncol = 3, nrow = 3)
######################################################################



###### Gráfico de Dispersão com Pontos Estratificados ################################
###### Gráfico 2 #####################################################
ffig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Cilindradas (in^3)', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')
ffig2
######################################################################


###### Gráfico 3 #####################################################
ffig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Potência (HP)', y = 'Eficiência (mpg)') +
  theme(legend.position = 'none')
ffig3
######################################################################


###### Gráfico 4 #####################################################
ffig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Relação de eixo traseiro', y = 'Eficiência (mpg)') +
  theme(legend.position = 'none')
ffig4
######################################################################


###### Gráfico 5 #####################################################
ffig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Peso (1000 lb)', y = 'Eficiência (mpg)') +
  theme(legend.position = 'none')
ffig5
######################################################################

###### Gráfico 6 #####################################################
ffig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Tempo (s)', y = 'Eficiência (mpg)') +
  theme(legend.position = 'none')
######################################################################

###### Gráfico 7 #####################################################
ffig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Número de marchas', y = 'Eficiência (mpg)') +
  theme(legend.position = 'none')
ffig7
######################################################################


###### Gráfico 8 #####################################################
ffig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Número de carburadores', y = 'Eficiência (mpg)')
ffig8
######################################################################
####### Gráfico de dispersão com pontos estratificados pelo (Número de Cilindros)

grid.arrange(ffig2, ffig3, ffig4, ffig5, ffig6, ffig7, ffig8,
             ncol = 2, nrow = 4)
##################################################################################

###### Gráfico de Dispersão com pontos Estratificados ############################
ffig1 <- mtcars %>% 
  ggplot(aes(x=cyl,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Número de cilindros', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Cilindradas (in^3)', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')

ffig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Potência (HP)', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')

ffig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Relação de eixo traseiro', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')

ffig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Peso (1000 lb)', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')

ffig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Tempo (s)', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')

ffig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Número de marchas', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'none')

ffig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Número de carburadores', y = 'Eficiência (mpg)') + 
  theme(legend.position = 'top')

##################################################################################
# Gráfico de dispersão com pontos estratificados pelo (Formato do Motor)
grid.arrange(ffig1, ffig2, ffig3, ffig4, ffig5, ffig6, ffig7, ffig8,
             ncol = 2, nrow = 4)
####################################################################################


###### Gráfico de dispersão com pontos estratificados ################################
# Gráfico de dispersão com pontos estratificados pelo (Tipo de Transmissão)

ffig1 <- mtcars %>% 
  ggplot(aes(x=cyl,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Número de cilindros', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Cilindradas (in^3)', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Potência (HP)', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Relação de eixo traseiro', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Peso (1000 lb)', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Tempo (s)', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Número de marchas', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'none')

ffig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Número de carburadores', y = 'Eficiência (mpg)') +  
  theme(legend.position = 'top')

# Gráfico de dispersão com pontos estratificados pelo tipo de transmissão
grid.arrange(ffig1, ffig2, ffig3, ffig4, ffig5, ffig6, ffig7, ffig8,
             ncol = 2, nrow = 4)
#################################################################################


###### Boxplot ##################################################################
fig9 <- mtcars %>% 
  ggplot(aes(x=as.factor(cyl),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Número de cilindros', y = 'Eficiência (mpg)') 
fig9
################################################################################

fig10 <- mtcars %>% 
  ggplot(aes(x=as.factor(vs),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Formato do motor', y = 'Eficiência (mpg)') 

fig11 <- mtcars %>% 
  ggplot(aes(x=as.factor(am),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Tipo de transmissão', y = 'Eficiência (mpg)') 

fig12 <- mtcars %>% 
  ggplot(aes(x=as.factor(gear),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Número de marchas', y = 'Eficiência (mpg)') 

fig13 <- mtcars %>% 
  mutate(carb_novo=ifelse(carb<=2,0,1)) %>% 
  ggplot(aes(x=as.factor(carb_novo),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Número de carburadores', y = 'Eficiência (mpg)') 

grid.arrange(fig10, fig11, ncol = 2, nrow = 1)
############################################################################

####################################################################
######### A função select() é utilizada para selecionar as variáveis de interesse
novo=select(mtcars, mpg, cyl)

#No exemplo são selecionadas todas as variáveis excluindo mpg:
novo=select(mtcars, -c(mpg))

#É possível selecionar uma sequência de variáveis a partir de seus nomes
novo=select(mtcars, cyl:drat)


######## A Função filter() seleciona as variáveis da base de dados
novo=filter(mtcars, hp>146)

#mais de um critério de filtragem de
novo=filter(mtcars, hp>146 & am==1)

####### A função mutate() é utilizada para incluir informações ou variáveis na base de dados
novo=mutate(mtcars, novacol=(mpg*100))

####### A função summarise() é uma poderosa ferrarmenta para agregar sumarizações unindo diversos cálculos ao longo de uma base de dados
summarise(mtcars,
          media.hp=mean(hp),
          qtd.hp=length(hp),
          qtdunico.hp=length(unique(hp)))

###### Ainda, é possível agrupar as informações com a função group_by() ao mesmo tempo em que são efetuados cálculos adjacentes
summarise(group_by(mtcars, cyl.agrup=cyl),
          hp.medio=mean(hp),
          wt.medio=mean(wt),
          qtd=n())

###### A função count() é utilizada para sumarizar a contagem de determinados objetos dentro de uma variável do banco de dados
count(mtcars, cyl)

###### A função arrange() ordena a base de dados de acordo com o ordenamento da variável escolhida
novo=arrange(mtcars, cyl)

###### Ainda é possível indicar mais de uma variável para este ordenamento, bem como utilizar a função desc() para organizar em ordem descrescente:
novo=arrange(mtcars, mpg, desc(disp))
head
###################################################################

###### O operador pipe (símbolos %>%) #############################
#Abaixo segue um exemplo, onde o objetivo é filtrar os
#veículos com transmissão manual (am == 1), agrupando-os pela quantidade
#de cilindros ("cyl") e em seguida retomando a média das variáveis "drat" e
#"hp" para cada grupamento:

novo = mtcars %>%
  filter(am == 1) %>%
  group_by(cyl) %>%
  summarise(disp.drat=mean(drat),
            hp.media=mean(hp))
novo

###############################################################################


########### Pacote ggplot2 + plotly ##############################################
# Banco de Dados, Motor Trend Car Road Tests (mtcars)

#A função aes() descreve como as variáveis são mapeadas em aspectos visuais de formas geométricas definidas pelos geoms
ggplot(data = mtcars, aes(x = disp, y = mpg)) + 
  geom_point()

#Agora, a variável am (tipo de transmissão) foi mapeada à cor dos pontos, sendo que pontos vermelhos correspondem à transmissão automática (valor 0) e pontos azuis à transmissão manual (valor 1)
ggplot(data = mtcars, aes(x = disp, y = mpg, colour = as.factor(am))) + 
  geom_point()

# No entanto, tambem podemos mapear uma variável contínua à cor dos pontos
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl)) + 
  geom_point()


####### Também podemos mapear o tamanho dos pontos à uma variável de interesse:
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl, size = wt)) +
  geom_point()

###### Facetas ###############################################
fig1<- ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) + 
  geom_point() + 
  facet_grid(.~am)
ggplotly()
#############################################################





