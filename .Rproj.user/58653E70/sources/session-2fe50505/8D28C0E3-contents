
setwd("C:/Users/Cerqueira/Documents/rstudio_projects/vendas_carros/")

dados <- read.csv("dados/vendas_carros.csv", 
                  sep = ";",
                  dec = ",",
                  fileEncoding = "latin1")

# Verificando os dados
View(dados)

# Verificando dados no terminal
head(dados)

# Importando biblioteca dplyr 
library(dplyr)

glimpse(dados)

# Convertando colunas de caracteres em factor
dados$Marca <- as.factor(dados$Marca)
dados$Modelo <- as.factor(dados$Modelo)
dados$Cor <- as.factor(dados$Cor)
dados$Vendedor <- as.factor(dados$Vendedor)
dados$Local_Venda <- as.factor(dados$Local_Venda)
dados$Forma_Pagamento <- as.factor(dados$Forma_Pagamento)



