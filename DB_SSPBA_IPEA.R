#####################################################################################################
#                                PROJETO DA DISSERTAÇÃO                                             #
#                                                                                                   #
#                                   IPEA  MPPPD - 4 Turma                                           # 
#                                                                                                   #                                    
#                                 Análise de Séries Temporais                                       #
#                                                                                                   #
#                                 Forecasting - HoltWinters                                         #
#####################################################################################################


##############################################
# Orientador:    Prof. Dr.  Bernardo Furtado #
# Coorientador:  Prof. Dr. Alexandre Cunha   #
# Orientando:    Ricardo Godoy               #
##############################################


#######################
# Versão: 02          #
# DAta:   06/10/2021  #
#######################


################################################################
# A EFICIÊNCIA DAS POLÍTICAS DE SEGURANÇA PÚBLICA NO COMBATE A #
# CRIMINALIDADE E A VIOLÊNCIA NA CIDADE DE SALVADOR NA BAHIA.  #
################################################################


# Instalação dos pacotes necessários no Script.

install.packages("fpp")
install.packages("fpp2")
install.packages("Quandl")
install.packages("xlsx")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("gapminder")
install.packages("Lahman")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("psych")
install.packages("lubridate")
install.packages("forecast")
install.packages("ggfortify")


library(Quandl)
library(fpp)
library(fpp2)
library(xlsx)
library(gridExtra)
library(tidyverse)
library(readr)
library(dplyr)
library(gapminder)
library(Lahman)
library(ggplot2)
library(tidyr)
library(psych)
library(lubridate)
library(forecast)
library(ggfortify)

#-------------------------------------------------------------------------------------

# Carregando os dados no R.

DB_SSPBA = read.csv2("C:/Users/User/Desktop/DB_SSPBA_IPEA.csv", stringsAsFactors=FALSE)
str(DB_SSPBA)
View(DB_SSPBA)

#-------------------------------------------------------------------------------------

# Legenda dos Eventos:

### Principais Delitos - Quantidade de Pessoas
# Homicidio Doloso                           = "H_D"
# Lesao Corporal Seguida de Morte            = "L_C_S_M"
# Roubo com Resultado Morte_Latrocinio       = "R_c_R_M_L"
# Tentativa de Homicidio                     = "T_d_H"
# Estupro                                    = "Estpr"
#
#**********************************************************
#
### Principais Delitos - Quantidade de Ocorrências
# Roubo a Onibus_Urbano e em Rodovia         = "R_a_O_U_R"
# Roubo de Veiculo                           = "R_d_V"
# Furto de Veiculo                           = "F_d_V"
# Uso_Porte Substancia Entorpecente_Usuarios = "U_P_S_E_U"

#-------------------------------------------------------------------------------------

# Legenda Cores:

# col = "steelblue4" = ts_DB_SSPBA2
# 
# col = "brown" = ts_DB_SSPBA_PES
# 
# col = "goldenrod4" = ts_DB_SSPBA_Oco

# https://vanderleidebastiani.github.io/tutoriais/Graficos_com_R.html

#-------------------------------------------------------------------------------------

DB_SSPBA1 = DB_SSPBA

colnames(DB_SSPBA1)[1]  <- "Ano"
colnames(DB_SSPBA1)[3]  <- "H_D"
colnames(DB_SSPBA1)[4]  <- "L_C_S_M"
colnames(DB_SSPBA1)[5]  <- "R_c_R_M_L"
colnames(DB_SSPBA1)[6]  <- "T_d_H"
colnames(DB_SSPBA1)[7]  <- "Estpr"
colnames(DB_SSPBA1)[8]  <- "R_a_O_U_R"
colnames(DB_SSPBA1)[9]  <- "R_d_V"
colnames(DB_SSPBA1)[10] <- "F_d_V"
colnames(DB_SSPBA1)[11] <- "U_P_S_E_U"

str(DB_SSPBA1)
View(DB_SSPBA1)
class(DB_SSPBA1)

#-------------------------------------------------------------------------------------

DB_SSPBA2 = DB_SSPBA1

DB_SSPBA2[,1] <- NULL
DB_SSPBA2[,1] <- NULL

#-------------------------------------------------------------------------------------
# ### Principais Delitos - Quantidade de Pessoas 
# ### & 
# ### Principais Delitos - Quantidade de Ocorrências
#-------------------------------------------------------------------------------------

DB_SSPBA2 = DB_SSPBA1

DB_SSPBA2[,1] <- NULL
DB_SSPBA2[,1] <- NULL

#-------------------------------------------------------------------------------------

ts_DB_SSPBA2 = ts(DB_SSPBA2, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_SSPBA2)
View(ts_DB_SSPBA2)
class(ts_DB_SSPBA2)

ts_DB_SSPBA2

#-------------------------------------------------------------------------------------

autoplot(ts_DB_SSPBA2,
         main = "Principais Delitos - Qtd de Pessoas e Ocorrências",
         xlab = "Anos",
         ylab = "Eventos") +
         labs(color = "Eventos") +
         geom_point() +
         theme_bw() +
         geom_hline(aes(yintercept = mean(ts_DB_SSPBA2[ts_DB_SSPBA2 != 0])), 
                   col = "black", lwd = 0.1)

#------------------------------------------------------------------------------------- 

boxplot(ts_DB_SSPBA2,
        main = "Principais Delitos - Qtd de Pessoas e Ocorrências",
        xlab = "Eventos",
        ylab = "Quantidade",
        col = "steelblue4",
        horizontal= FALSE,        
        pch = 16)
#------------------------------------------------------------------------------------- 

plot(ts_DB_SSPBA2, 
     type = "l", lwd = 2, 
     col = "steelblue4",
     main = "Principais Delitos - Qtd de Pessoas e Ocorrências",
     xlab = "Anos",
     ylab = "Eventos")

#-------------------------------------------------------------------------------------

summary(ts_DB_SSPBA2)
head(ts_DB_SSPBA2)
tail(ts_DB_SSPBA2)
 
decompose(ts_DB_SSPBA2)

#-------------------------------------------------------------------------------------

# Diferença e Logaritimo da Principais Delitos - 
# Qtd de Pessoas e Ocorrências & Qtd de Pessoas e Ocorrências

diff_ts_DB_SSPBA2 = diff(ts_DB_SSPBA2)

plot.ts(diff_ts_DB_SSPBA2,
        type = "l", lwd = 2, 
        col = "steelblue4",
        main = "Principais Delitos - (diff) Qtd de Pessoas e Ocorrências",
        xlab = "Anos",
        ylab = "Eventos") 
        
#-------------------------------------------------------------------------------------

log_ts_DB_SSPBA2 =log(ts_DB_SSPBA2)

plot.ts(log_ts_DB_SSPBA2,
        type = "l", lwd = 2, 
        col = "steelblue4",
        main = "Principais Delitos - (log) Qtd de Pessoas e Ocorrências",
        xlab = "Anos",
        ylab = "Eventos")

#-------------------------------------------------------------------------------------


#*******************************************************************************

#-------------------------------------------------------------------------------------
### Principais Delitos - Quantidade de Pessoas
#-------------------------------------------------------------------------------------

DB_SSPBA_PES = DB_SSPBA2

DB_SSPBA_PES[,6] <- NULL
DB_SSPBA_PES[,6] <- NULL
DB_SSPBA_PES[,6] <- NULL
DB_SSPBA_PES[,6] <- NULL

#-------------------------------------------------------------------------------------

ts_DB_SSPBA_PES = ts(DB_SSPBA_PES, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_SSPBA_PES)
View(ts_DB_SSPBA_PES)
class(ts_DB_SSPBA_PES)

ts_DB_SSPBA_PES

#-------------------------------------------------------------------------------------

autoplot(ts_DB_SSPBA_PES,
         main = "Principais Delitos - Quantidade de Pessoas",
         xlab = "Anos",
         ylab = "Eventos") +
         labs(color = "Eventos") +
         geom_point() +
         theme_bw() +
         geom_hline(aes(yintercept = mean(ts_DB_SSPBA_PES[ts_DB_SSPBA_PES != 0])), 
                   col = "black", lwd = 0.1)

#------------------------------------------------------------------------------------- 

boxplot(ts_DB_SSPBA_PES,
        main = "Principais Delitos - Quantidade de Pessoas",
        xlab = "Eventos",
        ylab = "Quantidade",
        col = "brown" ,
        horizontal= FALSE,        
        pch = 16)

#------------------------------------------------------------------------------------- 

plot(ts_DB_SSPBA_PES, 
     type = "l", lwd = 2, 
     col = "brown",
     main = "Principais Delitos - Quantidade de Pessoas",
     xlab = "Anos",
     ylab = "Eventos")

#-------------------------------------------------------------------------------------

summary(ts_DB_SSPBA_PES)
head(ts_DB_SSPBA_PES)
tail(ts_DB_SSPBA_PES)

decompose(ts_DB_SSPBA_PES)

#-------------------------------------------------------------------------------------

# Diferença e Logaritimo da Principais Delitos - Quantidade de Pessoas

diff_ts_DB_SSPBA_PES = diff(ts_DB_SSPBA_PES)

plot.ts(diff_ts_DB_SSPBA_PES,
        type = "l", lwd = 2, 
        col = "brown",
        main = "Principais Delitos - (diff) Quantidade de Pessoas",
        xlab = "Anos",
        ylab = "Eventos")

#-------------------------------------------------------------------------------------

log_ts_DB_SSPBA_PES = log(ts_DB_SSPBA_PES)

plot.ts(log_ts_DB_SSPBA_PES,
        type = "l", lwd = 2, 
        col = "brown",
        main = "Principais Delitos - (log) Quantidade de Pessoas",
        xlab = "Anos",
        ylab = "Eventos")

#-------------------------------------------------------------------------------------

#*******************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Pessoas" = "Homicidios Doloso"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd Evento H_D = "Homicidios Doloso":

ts_DB_PES_H_D = ts(DB_SSPBA_PES$H_D, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_PES_H_D)
View(ts_DB_PES_H_D)
class(ts_DB_PES_H_D)

ts_DB_PES_H_D

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_PES_H_D, 
        type = "l", lwd = 2, 
        col = "brown", 
        main = "Qtd de Homicidios Doloso - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_PES_H_D[ts_DB_PES_H_D != 0]), 
               col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_PES_H_D, 
         ts.geom = "point", shape = 3,
         ts.colour = "brown",
         main = "Qtd de Homicidios Doloso - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_PES_H_D[ts_DB_PES_H_D != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_PES_H_D, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Homicidios Doloso - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_PES_H_D = HoltWinters(ts_DB_PES_H_D, seasonal = "additive")

add_hw_PES_H_D


prev_add_hw_PES_H_D = forecast(add_hw_PES_H_D, h = 60, level = 80)

prev_add_hw_PES_H_D

autoplot(prev_add_hw_PES_H_D, 
         ts.colour = "brown",
         main = "Qtd de Homicidios Doloso - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

mult_hw_PES_H_D = HoltWinters(ts_DB_PES_H_D, seasonal = "multiplicative")

mult_hw_PES_H_D


prev_mult_hw_PES_H_D = forecast(mult_hw_PES_H_D, h = 60, level = 80)

prev_mult_hw_PES_H_D


autoplot(prev_mult_hw_PES_H_D, 
         ts.colour = "brown",
         main = "Qtd de Homicidios Doloso - Previsão Multiplicativa",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#*******************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Pessoas" = "Lesao Corporal Seguida de Morte"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd Evento L_C_S_M = "Lesao Corporal Seguida de Morte":

ts_DB_PES_L_C_S_M = ts(DB_SSPBA_PES$L_C_S_M, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_PES_L_C_S_M)
View(ts_DB_PES_L_C_S_M)
class(ts_DB_PES_L_C_S_M)

ts_DB_PES_L_C_S_M

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_PES_L_C_S_M, 
        type = "l", lwd = 2, 
        col = "brown", 
        main = "Qtd de Lesao Corporal Seguida de Morte - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_PES_L_C_S_M[ts_DB_PES_L_C_S_M != 0]), 
               col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_PES_L_C_S_M, 
         ts.geom = "point", shape = 3,
         ts.colour = "brown",
         main = "Qtd de Lesao Corporal Seguida de Morte - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_PES_L_C_S_M[ts_DB_PES_L_C_S_M != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_PES_L_C_S_M, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Lesao Corporal Seguida de Morte - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_PES_L_C_S_M = HoltWinters(ts_DB_PES_L_C_S_M, seasonal = "additive")

add_hw_PES_L_C_S_M


prev_add_hw_PES_L_C_S_M = forecast(add_hw_PES_L_C_S_M, h = 60, level = 80)

prev_add_hw_PES_L_C_S_M

autoplot(prev_add_hw_PES_L_C_S_M, 
         ts.colour = "brown",
         main = "Qtd de Lesao Corporal Seguida de Morte - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

# mult_hw_PES_L_C_S_M = HoltWinters(ts_DB_PES_L_C_S_M, seasonal = "multiplicative")
# 
# mult_hw_PES_L_C_S_M
# 
# 
# prev_mult_hw_PES_L_C_S_M = forecast(mult_hw_PES_L_C_S_M, h = 60, level = 80)
# 
# prev_mult_hw_PES_L_C_S_M
# 
# 
# autoplot(prev_mult_hw_PES_L_C_S_M, 
#          ts.colour = "brown",
#          main = "Qtd de Lesao Corporal Seguida de Morte - Previsão Multiplicativa",
#          xlab = "Anos",
#          ylab = "Quantidade") +
#         theme_bw() +
#         geom_hline(yintercept = 0, 
#                    col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#*******************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Pessoas" = "Roubo com Resultado Morte_Latrocinio"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd Evento R_c_R_M_L = "Roubo com Resultado Morte_Latrocinio":

ts_DB_PES_R_c_R_M_L = ts(DB_SSPBA_PES$R_c_R_M_L, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_PES_R_c_R_M_L)
View(ts_DB_PES_R_c_R_M_L)
class(ts_DB_PES_R_c_R_M_L)

ts_DB_PES_R_c_R_M_L

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_PES_R_c_R_M_L, 
        type = "l", lwd = 2, 
        col = "brown", 
        main = "Qtd de Roubo com Resultado Morte_Latrocinio - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_PES_R_c_R_M_L[ts_DB_PES_R_c_R_M_L != 0]), 
               col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_PES_R_c_R_M_L, 
         ts.geom = "point", shape = 3,
         ts.colour = "brown",
         main = "Qtd de Roubo com Resultado Morte_Latrocinio - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = (ts_DB_PES_R_c_R_M_L[ts_DB_PES_R_c_R_M_L != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_PES_R_c_R_M_L, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Roubo com Resultado Morte_Latrocinio - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_PES_R_c_R_M_L = HoltWinters(ts_DB_PES_R_c_R_M_L, seasonal = "additive")

add_hw_PES_R_c_R_M_L


prev_add_hw_PES_R_c_R_M_L = forecast(add_hw_PES_R_c_R_M_L, h = 60, level = 80)

prev_add_hw_PES_R_c_R_M_L

autoplot(prev_add_hw_PES_R_c_R_M_L, 
         ts.colour = "brown",
         main = "Qtd de Roubo com Resultado Morte_Latrocinio - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

# mult_hw_PES_R_c_R_M_L = HoltWinters(ts_DB_PES_R_c_R_M_L, seasonal = "multiplicative")
# 
# mult_hw_PES_R_c_R_M_L
# 
# 
# prev_mult_hw_PES_R_c_R_M_L = forecast(mult_hw_PES_R_c_R_M_L, h = 60, level = 80)
# 
# prev_mult_hw_PES_R_c_R_M_L
# 
# 
# autoplot(prev_mult_hw_PES_R_c_R_M_L, 
#          ts.colour = "brown",
#          main = "Qtd de Roubo com Resultado Morte_Latrocinio - Previsão Multiplicativa",
#          xlab = "Anos",
#          ylab = "Quantidade") +
#         theme_bw() +
#         geom_hline(yintercept = 0, 
#                    col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#**********************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Pessoas" = "Tentativa de Homicidio"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd Evento "Tentativa de Homicidio" = "T_d_H":

ts_DB_PES_T_d_H = ts(DB_SSPBA_PES$T_d_H, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_PES_T_d_H)
View(ts_DB_PES_T_d_H)
class(ts_DB_PES_T_d_H)

ts_DB_PES_T_d_H

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_PES_T_d_H, 
        type = "l", lwd = 2, 
        col = "brown", 
        main = "Qtd de Tentativa de Homicidio - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = (ts_DB_PES_T_d_H[ts_DB_PES_T_d_H != 0]), col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_PES_T_d_H, 
         ts.geom = "point", shape = 3,
         ts.colour = "brown",
         main = "Qtd de Tentativa de Homicidio - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_PES_T_d_H[ts_DB_PES_T_d_H != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_PES_T_d_H, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Tentativa de Homicidio - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_PES_T_d_H = HoltWinters(ts_DB_PES_T_d_H, seasonal = "additive")

add_hw_PES_T_d_H


prev_add_hw_PES_T_d_H = forecast(add_hw_PES_T_d_H, h = 60, level = 80)

prev_add_hw_PES_T_d_H

autoplot(prev_add_hw_PES_T_d_H, 
         ts.colour = "brown",
         main = "Qtd de Tentativa de Homicidio - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# # Forecasting - HoltWinters - multiplicative
# 
# mult_hw_PES_T_d_H = HoltWinters(ts_DB_PES_T_d_H, seasonal = "multiplicative")
# 
# mult_hw_PES_T_d_H
# 
# 
# prev_mult_hw_PES_T_d_H = forecast(mult_hw_PES_T_d_H, h = 60, level = 80)
# 
# prev_mult_hw_PES_T_d_H
# 
# 
# autoplot(prev_mult_hw_PES_T_d_H, 
#          ts.colour = "brown",
#          main = "Qtd Tentativa de Homicidio - Previsão Multiplicativa",
#          xlab = "Anos",
#          ylab = "Quantidade") +
#         theme_bw() +
#         geom_hline(yintercept = 0, 
#                    col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#**********************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Pessoas" = "Estupro"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd Evento Estpr = "Estupro":

ts_DB_PES_Estpr = ts(DB_SSPBA_PES$Estpr, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_PES_Estpr)
View(ts_DB_PES_Estpr)
class(ts_DB_PES_Estpr)

ts_DB_PES_Estpr

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_PES_Estpr, 
        type = "l", lwd = 2, 
        col = "brown", 
        main = "Qtd de Estupro - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = (ts_DB_PES_Estpr[ts_DB_PES_Estpr != 0]), col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_PES_Estpr, 
         ts.geom = "point", shape = 3,
         ts.colour = "brown",
         main = "Qtd de Estupro - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
         theme_bw() +
         geom_hline(aes(yintercept = mean(ts_DB_PES_Estpr[ts_DB_PES_Estpr != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_PES_Estpr, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Estupro - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_PES_Estpr = HoltWinters(ts_DB_PES_Estpr, seasonal = "additive")

add_hw_PES_Estpr


prev_add_hw_PES_Estpr = forecast(add_hw_PES_Estpr, h = 60, level = 80)

prev_add_hw_PES_Estpr

autoplot(prev_add_hw_PES_Estpr, 
         ts.colour = "brown",
         main = "Qtd de Estupro - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
         theme_bw() +
         geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

# mult_hw_PES_Estpr = HoltWinters(ts_DB_PES_Estpr, seasonal = "multiplicative")
# 
# mult_hw_PES_Estpr
# 
# 
# prev_mult_hw_PES_Estpr = forecast(mult_hw_PES_Estpr, h = 60, level = 80)
# 
# prev_mult_hw_PES_Estpr
# 
# 
# autoplot(prev_mult_hw_PES_Estpr, 
#          ts.colour = "brown",
#          main = "Qtd de Estupro - Previsão Multiplicativa",
#          xlab = "Anos",
#          ylab = "Quantidade") +
#         theme_bw() +
#         geom_hline(yintercept = 0, 
#                    col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#**********************************************************************************
#**********************************************************************************

#-------------------------------------------------------------------------------------
### Principais Delitos - Quantidade de Ocorrências
#-------------------------------------------------------------------------------------

DB_SSPBA_Oco = DB_SSPBA2

DB_SSPBA_Oco[,1] <- NULL
DB_SSPBA_Oco[,1] <- NULL
DB_SSPBA_Oco[,1] <- NULL
DB_SSPBA_Oco[,1] <- NULL
DB_SSPBA_Oco[,1] <- NULL

#-------------------------------------------------------------------------------------

ts_DB_SSPBA_Oco = ts(DB_SSPBA_Oco, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_SSPBA_Oco)
View(ts_DB_SSPBA_Oco)
class(ts_DB_SSPBA_Oco)

ts_DB_SSPBA_Oco

#-------------------------------------------------------------------------------------

autoplot(ts_DB_SSPBA_Oco,  
         main = "Principais Delitos - Quantidade de Ocorrências",
         xlab = "Anos",
         ylab = "Quantidade")+
         labs(color = "Eventos") +
         geom_point() +
         theme_bw() +
         geom_hline(aes(yintercept = mean(ts_DB_SSPBA_Oco[ts_DB_SSPBA_Oco != 0])), 
                   col = "black", lwd = 0.1)

#------------------------------------------------------------------------------------- 

boxplot(ts_DB_SSPBA_Oco,
        main = "Principais Delitos - Quantidade de Ocorrências",
        xlab = "Eventos",
        ylab = "Quantidade",
        col = "goldenrod4",
        horizontal= FALSE,        
        pch = 16)

#------------------------------------------------------------------------------------- 

plot(ts_DB_SSPBA_Oco, 
     type = "l", lwd = 2, 
     col = "goldenrod4",
     main = "Principais Delitos - Quantidade de Ocorrências",
     xlab = "Anos",
     ylab = "Eventos")

#-------------------------------------------------------------------------------------

summary(ts_DB_SSPBA_Oco)
head(ts_DB_SSPBA_Oco)
tail(ts_DB_SSPBA_Oco)

decompose(ts_DB_SSPBA_Oco)

#-------------------------------------------------------------------------------------

# Diferença e Logaritimo da Principais Delitos - Quantidade de Ocorrências

diff_ts_DB_SSPBA_Oco = diff(ts_DB_SSPBA_Oco)

plot.ts(diff_ts_DB_SSPBA_Oco,
        type = "l", lwd = 2, 
        col = "goldenrod4",
        main = "Principais Delitos - (diff) Quantidade de Ocorrências",
        xlab = "Anos",
        ylab = "Eventos")

#-------------------------------------------------------------------------------------

log_ts_DB_SSPBA_Oco = log(ts_DB_SSPBA_Oco)

plot.ts(log_ts_DB_SSPBA_Oco,
        type = "l", lwd = 2, 
        col = "goldenrod4",
        main = "Principais Delitos - (log) Quantidade de Ocorrências",
        xlab = "Anos",
        ylab = "Eventos")

#-------------------------------------------------------------------------------------

#*******************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Ocorrências" = "Roubo a Onibus_Urbano e em Rodovia"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd de Evento R_a_O_U_R = "Roubo a Onibus_Urbano e em Rodovia":

ts_DB_Oco_R_a_O_U_R = ts(DB_SSPBA_Oco$R_a_O_U_R, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_Oco_R_a_O_U_R)
View(ts_DB_Oco_R_a_O_U_R)
class(ts_DB_Oco_R_a_O_U_R)

ts_DB_Oco_R_a_O_U_R

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_Oco_R_a_O_U_R, 
        type = "l", lwd = 2, 
        col = "goldenrod4", 
        main = "Qtd de Roubo a Onibus_Urbano e em Rodovia - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_Oco_R_a_O_U_R[ts_DB_Oco_R_a_O_U_R != 0]), col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_Oco_R_a_O_U_R, 
         ts.geom = "point", shape = 3,
         ts.colour = "goldenrod4",
         main = "Qtd de Roubo a Onibus_Urbano e em Rodovia - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_Oco_R_a_O_U_R[ts_DB_Oco_R_a_O_U_R != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_Oco_R_a_O_U_R, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Roubo a Onibus_Urbano e em Rodovia - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_Oco_R_a_O_U_R = HoltWinters(ts_DB_Oco_R_a_O_U_R, seasonal = "additive")

add_hw_Oco_R_a_O_U_R


prev_add_hw_Oco_R_a_O_U_R = forecast(add_hw_Oco_R_a_O_U_R, h = 60, level = 80)

prev_add_hw_Oco_R_a_O_U_R

autoplot(prev_add_hw_Oco_R_a_O_U_R, 
         ts.colour = "goldenrod4",
         main = "Qtd de Roubo a Onibus_Urbano e em Rodovia - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

mult_hw_Oco_R_a_O_U_R = HoltWinters(ts_DB_Oco_R_a_O_U_R, seasonal = "multiplicative")

mult_hw_Oco_R_a_O_U_R


prev_mult_hw_Oco_R_a_O_U_R = forecast(mult_hw_Oco_R_a_O_U_R, h = 60, level = 80)

prev_mult_hw_Oco_R_a_O_U_R


autoplot(prev_mult_hw_Oco_R_a_O_U_R, 
         ts.colour = "goldenrod4",
         main = "Qtd de Roubo a Onibus_Urbano e em Rodovia - Previsão Multiplicativa",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#*******************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Ocorrências" = "Roubo de Veiculo"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd de Evento R_d_V = "Roubo de Veiculo":

ts_DB_Oco_R_d_V = ts(DB_SSPBA_Oco$R_d_V, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_Oco_R_d_V)
View(ts_DB_Oco_R_d_V)
class(ts_DB_Oco_R_d_V)

ts_DB_Oco_R_d_V

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_Oco_R_d_V, 
        type = "l", lwd = 2, 
        col = "goldenrod4", 
        main = "Qtd de Roubo de Veiculo - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_Oco_R_d_V[ts_DB_Oco_R_d_V != 0]), col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_Oco_R_d_V, 
         ts.geom = "point", shape = 3,
         ts.colour = "goldenrod4",
         main = "Qtd de Roubo de Veiculo - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_Oco_R_d_V[ts_DB_Oco_R_d_V != 0])),
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_Oco_R_d_V, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Roubo de Veiculo - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_Oco_R_d_V = HoltWinters(ts_DB_Oco_R_d_V, seasonal = "additive")

add_hw_Oco_R_d_V


prev_add_hw_Oco_R_d_V = forecast(add_hw_Oco_R_d_V, h = 60, level = 80)

prev_add_hw_Oco_R_d_V

autoplot(prev_add_hw_Oco_R_d_V, 
         ts.colour = "goldenrod4",
         main = "Qtd de Roubo de Veiculo - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

mult_hw_Oco_R_d_V = HoltWinters(ts_DB_Oco_R_d_V, seasonal = "multiplicative")

mult_hw_Oco_R_d_V


prev_mult_hw_Oco_R_d_V = forecast(mult_hw_Oco_R_d_V, h = 60, level = 80)

prev_mult_hw_Oco_R_d_V


autoplot(prev_mult_hw_Oco_R_d_V, 
         ts.colour = "goldenrod4",
         main = "Qtd de Roubo de Veiculo - Previsão Multiplicativa",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#**********************************************************************************

#-------------------------------------------------------------------------------------
# "Principais Delitos - Quantidade de Ocorrências" = "Furto de Veiculo"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd de Evento F_d_V = "Furto de Veiculo":

ts_DB_Oco_F_d_V = ts(DB_SSPBA_Oco$F_d_V, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_Oco_F_d_V)
View(ts_DB_Oco_F_d_V)
class(ts_DB_Oco_F_d_V)

ts_DB_Oco_F_d_V

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_Oco_F_d_V, 
        type = "l", lwd = 2, 
        col = "goldenrod4", 
        main = "Qtd de Furto de Veiculo - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_Oco_F_d_V[ts_DB_Oco_F_d_V != 0]), col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_Oco_F_d_V, 
         ts.geom = "point", shape = 3,
         ts.colour = "goldenrod4",
         main = "Qtd de Furto de Veiculo - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_Oco_F_d_V[ts_DB_Oco_F_d_V != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_Oco_F_d_V, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Furto de Veiculo - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_Oco_F_d_V = HoltWinters(ts_DB_Oco_F_d_V, seasonal = "additive")

add_hw_Oco_F_d_V


prev_add_hw_Oco_F_d_V = forecast(add_hw_Oco_F_d_V, h = 60, level = 80)

prev_add_hw_Oco_F_d_V

autoplot(prev_add_hw_Oco_F_d_V, 
         ts.colour = "goldenrod4",
         main = "Qtd de Furto de Veiculo - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative

mult_hw_Oco_F_d_V = HoltWinters(ts_DB_Oco_F_d_V, seasonal = "multiplicative")

mult_hw_Oco_F_d_V


prev_mult_hw_Oco_F_d_V = forecast(mult_hw_Oco_F_d_V, h = 60, level = 80)

prev_mult_hw_Oco_F_d_V


autoplot(prev_mult_hw_Oco_F_d_V, 
         ts.colour = "goldenrod4",
         main = "Qtd de Furto de Veiculo - Previsão Multiplicativa",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#**********************************************************************************

#-------------------------------------------------------------------------------------
# "PPrincipais Delitos - Quantidade de Ocorrências" = "Uso_Porte Substancia Entorpecente_Usuarios"
#-------------------------------------------------------------------------------------

# TS somente com o Qtd de Evento U_P_S_E_U = "Uso_Porte Substancia Entorpecente_Usuarios":

ts_DB_Oco_U_P_S_E_U = ts(DB_SSPBA_Oco$U_P_S_E_U, start = c(2014,1), end = c(2021,6), freq = 12)

str(ts_DB_Oco_U_P_S_E_U)
View(ts_DB_Oco_U_P_S_E_U)
class(ts_DB_Oco_U_P_S_E_U)

ts_DB_Oco_U_P_S_E_U

#-------------------------------------------------------------------------------------

plot.ts(ts_DB_Oco_U_P_S_E_U, 
        type = "l", lwd = 2, 
        col = "goldenrod4", 
        main = "Qtd de Uso_Porte Substancia Entorpecente_Usuarios - Por Ano",
        xlab = "Anos",
        ylab = "Quantidade") +
        abline(h = mean(ts_DB_Oco_U_P_S_E_U[ts_DB_Oco_U_P_S_E_U != 0]), 
               col = "green", lwd = 1)

#-------------------------------------------------------------------------------------

autoplot(ts_DB_Oco_U_P_S_E_U, 
         ts.geom = "point", shape = 3,
         ts.colour = "goldenrod4",
         main = "Qtd de Uso_Porte Substancia Entorpecente_Usuarios - Por Ano",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(aes(yintercept = mean(ts_DB_Oco_U_P_S_E_U[ts_DB_Oco_U_P_S_E_U != 0])), 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

ggseasonplot(ts_DB_Oco_U_P_S_E_U, polar = T) + 
        ylab("Quantidade") + 
        xlab ("Mês") +
        ggtitle("Qtd de Uso_Porte Substancia Entorpecente_Usuarios - Por Ano/Mês") +
        labs(color = "Anos") +
        geom_point() +
        theme_bw()

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - additive

add_hw_Oco_U_P_S_E_U = HoltWinters(ts_DB_Oco_U_P_S_E_U, seasonal = "additive")

add_hw_Oco_U_P_S_E_U


prev_add_hw_Oco_U_P_S_E_U = forecast(add_hw_Oco_U_P_S_E_U, h = 60, level = 80)

prev_add_hw_Oco_U_P_S_E_U

autoplot(prev_add_hw_Oco_U_P_S_E_U, 
         ts.colour = "goldenrod4",
         main = "Qtd de Uso_Porte Substancia Entorpecente_Usuarios - Previsão Aditiva",
         xlab = "Anos",
         ylab = "Quantidade") +
        theme_bw() +
        geom_hline(yintercept = 0, 
                   col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

# Forecasting - HoltWinters - multiplicative


# mult_hw_Oco_U_P_S_E_U = HoltWinters(ts_DB_Oco_U_P_S_E_U, seasonal = "multiplicative")
#  
# mult_hw_Oco_U_P_S_E_U
#  
#  
# prev_mult_hw_Oco_U_P_S_E_U = forecast(mult_hw_Oco_U_P_S_E_U, h = 60, level = 80)
#  
# prev_mult_hw_Oco_U_P_S_E_U
#  
#  
# autoplot(prev_mult_hw_Oco_U_P_S_E_U, 
#           ts.colour = "goldenrod4",
#           main = "Qtd de Uso_Porte Substancia Entorpecente_Usuarios - Previsão Multiplicativa",
#           xlab = "Anos",
#           ylab = "Quantidade") +
#           theme_bw() +
#           geom_hline(yintercept = 0, 
#                     col = "green", lwd = 0.1)

#-------------------------------------------------------------------------------------

#**********************************************************************************











#-------------------------------------------------------------------------------------
# stemp1 e stemp2
#-------------------------------------------------------------------------------------

# lungDeaths <- cbind(stemp1, stemp2)
# 
# autoplot(lungDeaths)



