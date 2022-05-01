###### An?lise Estat?stica do Banco mtcars do R4.0 #####################################################################
###### Objetivo: An?lise do Consumo M?dio de Combust?vel de Carros Modelos 1973/1974
###### Descri??o: o conjunto de dados, denominado de mtcars, foi obtido a partir das edi??es de mar?o, abril, junho e julho de 1974 da revista Motor Trend para um estudo realizado por Hocking (1976) e posteriormente, reportado por Henderson e Velleman (1981)

####### INTRODUÇÃO #####################################################################################
# Na crise petrol?fera de 1973, membros da Organiza??o dos Pa?ses ?rabes Exportadores de Petr?leo (OPAEP) aplicaram san??es em protesto ao apoio dos Estados Unidos e outras na??es ? Israel durante a Guerra do Yom Kippur.
# O conflito resultou no aumento do pre?o do petr?leo de tr?s d?lares por barril para cerca de 12 dol?res no mundo inteiro, sendo que os pre?os fixados para os Estados Unidos foram ainda maiores.
# Como uma alternativa ? alta do pre?o do petr?leo no mercado mundial, os Estados Unidos iniciaram um programa de efici?ncia energ?tica, conhecido como Corporate Average Fuel Economy (CAFE), com o prop?sito de reduzir o consumo de combust?vel de carros, pick-ups, minivans e SUVs (Almeida Filho, 2018)
# O conjunto de dados, denominado de mtcars,est? dispon?vel na biblioteca datasets do software R para consulta.

##########################################################################################################


###### Carregar os Pacotes ############################################################################
library(tidyverse)
library(GGally)
library(kableExtra)
library(ggplot2)
library(plotly)
library(gridExtra)
library(ggcorrplot)
library(gtsummary)
library(xtable)
library(PerformanceAnalytics)
library(readxl)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
#######################################################################################################


###### AN?LISE EXAPLORAT?RIA DE DADOS #################################################################
###### As vari?veis observadas no conjunto de dados s?o definidas como:
# Vari?vel Resposta (Y): 
#  1) mpg: efici?ncia (milhas por gal?o de combust?vel).
# Vari?veis Explicativas(X): 
#  1) cyl: n?mero de cilindros.
#  2) disp: cilindradas (polegada c?bica).
#  3) hp: pot?ncia bruta (HP).
#  4) drat: rela??o de eixo traseiro.
#  5) wt: peso (1000 libras).
#  6) qsec: tempo no quarto de milha (segundos).
#  7) vs: formato do motor (0 = V e 1 = linha).
#  8) am: tipo de transmiss?o (0 = autom?tico e 1 = manual).
#  9) gear: n?mero de marchas para frente.
# 10) carb: n?mero de carburadores.
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

colnames(resumo) <- c('Vari?vel', 'Missing', 'M?dia',
                      'Desvio padr?o', 'M?nimo', 'Q1',
                      'Mediana', 'Q3', 'M?ximo')
kbl(resumo, booktabs = T, caption = 'Estat?sticas descritivas daS Caracter?sticas Quantitativa', longtable = T) %>% 
  kable_styling(position = 'center',latex_options = c("striped", "hold_position"))
###################################################################################################


###### Correlograma das variaveis explicativas #####################################

# Matriz de Correlação
cor(mtcars)

# Gr?fico da Matriz de Correla??o
correl = cor(mtcars)
ggcorrplot(correl)


# Grafico Customizado
# method = "square" (padrão) 
# method = estilos (circle)

# hac.order = agrupamento hierqrquico
# type = esconder espelhamento
  #type = upper, lower
# lab = adicionar o valor da correlacao
# lab_size = ajustar a fonte ao tamanho
# p.mat = excluindo o coeiciente nao significante
# insig = "blank" deixar em branco coef não significante


ggcorrplot(correl, 
           method = "square",
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3.0,
           colors= c("firebrick", "white", "dodgerblue4"),
           outline.color = "white",
           title = "Matriz de Correlação",
           ggtheme = theme_gray()
)



# Teste t para Corelação
# P-valor das correlações
cor_pmat(mtcars)

# Grafico da Matriz de P-valor
ggcorrplot(correl,
           method = "square",
           hc.order = TRUE,
           type= "lower",
           lab = TRUE,
           lab_size = 3.0,
           p.mat = cor_pmat(mtcars),
           colors= c("firebrick", "white", "dodgerblue4"),
           outline.color = "white",
           title = "Matriz de Correlação",
           ggtheme = theme_gray
)




mtcars %>% 
  select(-vs,-am, -mpg) %>% 
  ggpairs() 
####################################################################################



###### Grafico de Dispers?o ##########################################
###### Grafico 1 #####################################################
fig1 <- mtcars %>% 
  ggplot(aes(x=cyl,y=mpg)) +
  geom_point() +
  labs(x = 'N?mero de cilindros', y = 'Efici?ncia (mpg)')+ 
  geom_smooth(method = lm, se = FALSE)
fig1
######################################################################

###### Gr?fico 2 #####################################################
fig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point() +
  labs(x = 'Cilindradas (in^3)', y = 'Efici?ncia (mpg)')+ 
  geom_smooth(method = lm, se = FALSE)
fig2
######################################################################

###### Gr?fico 3 #####################################################
fig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point() +
  labs(x = 'Pot?ncia (HP)', y = 'Efici?ncia (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig3
######################################################################


###### Gr?fico 4 #####################################################
fig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point() +
  labs(x = 'Rela??o de Eixo T', y = 'Efici?ncia (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig4
######################################################################

###### Gr?fico 5 #####################################################
fig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point() +
  labs(x = 'Peso (1000 lb)', y = 'Efici?ncia (mpg)')+ 
  geom_smooth(method = lm, se = FALSE)
fig5
######################################################################


###### Gr?fico 6 #####################################################
fig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point() +
  labs(x = 'Tempo (s)', y = 'Efici?ncia (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig6
######################################################################

###### Gr?fico 7 #####################################################
fig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point() +
  labs(x = 'N?mero de Marchas', y = 'Efici?ncia (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig7
######################################################################

###### Gr?fico 8 #####################################################
fig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point() +
  labs(x = 'N?mero de Carburadores', y = 'Efici?ncia (mpg)') + 
  geom_smooth(method = lm, se = FALSE)
fig8
######################################################################

###### Gr?fico Agrupado #####################################################

grid.arrange(fig2, fig3, fig4, fig5, fig6, ncol = 3, nrow = 2)
grid.arrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, ncol = 3, nrow = 3)
######################################################################



###### Gr?fico de Dispers?o com Pontos Estratificados ################################
###### Gr?fico 2 #####################################################
ffig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Cilindradas (in^3)', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')
ffig2
######################################################################


###### Gr?fico 3 #####################################################
ffig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Pot?ncia (HP)', y = 'Efici?ncia (mpg)') +
  theme(legend.position = 'none')
ffig3
######################################################################


###### Gr?fico 4 #####################################################
ffig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Rela??o de eixo traseiro', y = 'Efici?ncia (mpg)') +
  theme(legend.position = 'none')
ffig4
######################################################################


###### Gr?fico 5 #####################################################
ffig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Peso (1000 lb)', y = 'Efici?ncia (mpg)') +
  theme(legend.position = 'none')
ffig5
######################################################################

###### Gr?fico 6 #####################################################
ffig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'Tempo (s)', y = 'Efici?ncia (mpg)') +
  theme(legend.position = 'none')
######################################################################

###### Gr?fico 7 #####################################################
ffig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'N?mero de marchas', y = 'Efici?ncia (mpg)') +
  theme(legend.position = 'none')
ffig7
######################################################################


###### Gr?fico 8 #####################################################
ffig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_smooth(method = lm, aes(colour = factor(cyl)), se = FALSE) +
  labs(x = 'N?mero de carburadores', y = 'Efici?ncia (mpg)')
ffig8
######################################################################
####### Gr?fico de dispers?o com pontos estratificados pelo (N?mero de Cilindros)

grid.arrange(ffig2, ffig3, ffig4, ffig5, ffig6, ffig7, ffig8,
             ncol = 2, nrow = 4)
##################################################################################

###### Gr?fico de Dispers?o com pontos Estratificados ############################
ffig1 <- mtcars %>% 
  ggplot(aes(x=cyl,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'N?mero de cilindros', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Cilindradas (in^3)', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')

ffig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Pot?ncia (HP)', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')

ffig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Rela??o de eixo traseiro', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')

ffig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Peso (1000 lb)', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')

ffig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'Tempo (s)', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')

ffig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'N?mero de marchas', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'none')

ffig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point(aes(colour = factor(vs))) +
  geom_smooth(method = lm, aes(colour = factor(vs)), se = FALSE) +
  labs(x = 'N?mero de carburadores', y = 'Efici?ncia (mpg)') + 
  theme(legend.position = 'top')

##################################################################################
# Gr?fico de dispers?o com pontos estratificados pelo (Formato do Motor)
grid.arrange(ffig1, ffig2, ffig3, ffig4, ffig5, ffig6, ffig7, ffig8,
             ncol = 2, nrow = 4)
####################################################################################


###### Gr?fico de dispers?o com pontos estratificados ################################
# Gr?fico de dispers?o com pontos estratificados pelo (Tipo de Transmiss?o)

ffig1 <- mtcars %>% 
  ggplot(aes(x=cyl,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'N?mero de cilindros', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig2 <- mtcars %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Cilindradas (in^3)', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig3 <- mtcars %>% 
  ggplot(aes(x=hp,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Pot?ncia (HP)', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig4 <- mtcars %>% 
  ggplot(aes(x=drat,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Rela??o de eixo traseiro', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig5 <- mtcars %>% 
  ggplot(aes(x=wt,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Peso (1000 lb)', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig6 <- mtcars %>% 
  ggplot(aes(x=qsec,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  #geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'Tempo (s)', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig7 <- mtcars %>% 
  ggplot(aes(x=gear,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'N?mero de marchas', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'none')

ffig8 <- mtcars %>% 
  ggplot(aes(x=carb,y=mpg)) +
  geom_point(aes(colour = factor(am))) +
  geom_smooth(method = lm, aes(colour = factor(am)), se = FALSE) +
  labs(x = 'N?mero de carburadores', y = 'Efici?ncia (mpg)') +  
  theme(legend.position = 'top')

# Gr?fico de dispers?o com pontos estratificados pelo tipo de transmiss?o
grid.arrange(ffig1, ffig2, ffig3, ffig4, ffig5, ffig6, ffig7, ffig8,
             ncol = 2, nrow = 4)
#################################################################################


###### Boxplot ##################################################################
fig9 <- mtcars %>% 
  ggplot(aes(x=as.factor(cyl),y=mpg)) +
  geom_boxplot() +
  labs(x = 'N?mero de cilindros', y = 'Efici?ncia (mpg)') 
fig9
################################################################################

fig10 <- mtcars %>% 
  ggplot(aes(x=as.factor(vs),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Formato do motor', y = 'Efici?ncia (mpg)') 

fig11 <- mtcars %>% 
  ggplot(aes(x=as.factor(am),y=mpg)) +
  geom_boxplot() +
  labs(x = 'Tipo de transmiss?o', y = 'Efici?ncia (mpg)') 

fig12 <- mtcars %>% 
  ggplot(aes(x=as.factor(gear),y=mpg)) +
  geom_boxplot() +
  labs(x = 'N?mero de marchas', y = 'Efici?ncia (mpg)') 

fig13 <- mtcars %>% 
  mutate(carb_novo=ifelse(carb<=2,0,1)) %>% 
  ggplot(aes(x=as.factor(carb_novo),y=mpg)) +
  geom_boxplot() +
  labs(x = 'N?mero de carburadores', y = 'Efici?ncia (mpg)') 

grid.arrange(fig10, fig11, ncol = 2, nrow = 1)
############################################################################

####################################################################
######### A fun??o select() ? utilizada para selecionar as vari?veis de interesse
novo=select(mtcars, mpg, cyl)

#No exemplo s?o selecionadas todas as vari?veis excluindo mpg:
novo=select(mtcars, -c(mpg))

#? poss?vel selecionar uma sequ?ncia de vari?veis a partir de seus nomes
novo=select(mtcars, cyl:drat)


######## A Fun??o filter() seleciona as vari?veis da base de dados
novo=filter(mtcars, hp>146)

#mais de um crit?rio de filtragem de
novo=filter(mtcars, hp>146 & am==1)

####### A fun??o mutate() ? utilizada para incluir informa??es ou vari?veis na base de dados
novo=mutate(mtcars, novacol=(mpg*100))

####### A fun??o summarise() ? uma poderosa ferrarmenta para agregar sumariza??es unindo diversos c?lculos ao longo de uma base de dados
summarise(mtcars,
          media.hp=mean(hp),
          qtd.hp=length(hp),
          qtdunico.hp=length(unique(hp)))

###### Ainda, ? poss?vel agrupar as informa??es com a fun??o group_by() ao mesmo tempo em que s?o efetuados c?lculos adjacentes
summarise(group_by(mtcars, cyl.agrup=cyl),
          hp.medio=mean(hp),
          wt.medio=mean(wt),
          qtd=n())

###### A fun??o count() ? utilizada para sumarizar a contagem de determinados objetos dentro de uma vari?vel do banco de dados
count(mtcars, cyl)

###### A fun??o arrange() ordena a base de dados de acordo com o ordenamento da vari?vel escolhida
novo=arrange(mtcars, cyl)

###### Ainda ? poss?vel indicar mais de uma vari?vel para este ordenamento, bem como utilizar a fun??o desc() para organizar em ordem descrescente:
novo=arrange(mtcars, mpg, desc(disp))
head
###################################################################

###### O operador pipe (s?mbolos %>%) #############################
#Abaixo segue um exemplo, onde o objetivo ? filtrar os
#ve?culos com transmiss?o manual (am == 1), agrupando-os pela quantidade
#de cilindros ("cyl") e em seguida retomando a m?dia das vari?veis "drat" e
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

#A fun??o aes() descreve como as vari?veis s?o mapeadas em aspectos visuais de formas geom?tricas definidas pelos geoms
ggplot(data = mtcars, aes(x = disp, y = mpg)) + 
  geom_point()

#Agora, a vari?vel am (tipo de transmiss?o) foi mapeada ? cor dos pontos, sendo que pontos vermelhos correspondem ? transmiss?o autom?tica (valor 0) e pontos azuis ? transmiss?o manual (valor 1)
ggplot(data = mtcars, aes(x = disp, y = mpg, colour = as.factor(am))) + 
  geom_point()

# No entanto, tambem podemos mapear uma vari?vel cont?nua ? cor dos pontos
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl)) + 
  geom_point()


####### Tamb?m podemos mapear o tamanho dos pontos ? uma vari?vel de interesse:
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl, size = wt)) +
  geom_point()

###### Facetas ###############################################
fig1<- ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) + 
  geom_point() + 
  facet_grid(.~am)
ggplotly()
#############################################################





