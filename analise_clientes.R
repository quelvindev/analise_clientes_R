
# Definindo pasta de trabalho
setwd("C:/Users/Cerqueira/r_projects/analise_clientes/")

# Importando dataset
df <- read.csv("data/clientes_8000-1.csv",
                 sep=";",
                 dec=",",
                 fileEncoding = "latin1")

# Verificando primeiras linhas
head(df)

# Verificando colunas nulas ou em branco
colSums(is.na(df)|df=="")

# Vistualizar dados 
View(df)

# Estrutura dos dados 
str(df)
# idade             - quantitativa discreta
# classe_trabalho   - qualitativa nominal
# escolaridade      - qualitativa ordinal
# anos_estudo       - quantitativa discreta
# Estado_civil      - qualitativa nominal
# raça              - qualitativa nominal
# sexo              - qualitativa nominal
# uf                - qualitativa nominal
# regiao            - qualitativa nominal
# qtde_filho        - quantitativa discreta
# salário           - quantitativa continua 

# Verificando valores unicos
print(unique(df$classe_trabalho))

# Convertendo em factor 
df$classe_trabalho <- factor(df$classe_trabalho)


# Verificando valores unicos
print(unique(df$escolaridade))

# Convertendo em factor 
df$escolaridade <- factor(df$escolaridade,levels 
                          = c("Analfabeto",
                              "Ensino Fundamental Incompleto",
                              "Ensino Fundamental Completo",
                              "Ensino Médio Incompleto",
                              "Ensino Médio Completo",
                              "Superior Incompleto",
                              "Superior Completo",
                              "Mestrado",
                              "Pós-Graduado"),
                          ordered = TRUE)

levels(df$escolaridade)


# Verificando valores unicos
print(unique(df$Estado_civil))

# Convertendo em factor 
df$Estado_civil <- factor(df$Estado_civil)


# Verificando valores unicos
print(unique(df$raça))

# Convertendo em factor 
df$raça <- factor(df$raça)


# Verificando valores unicos
print(unique(df$sexo))

# Convertendo em factor 
df$sexo <- factor(df$sexo)


# Verificando valores unicos
print(unique(df$região))

# Convertendo em factor 
df$região <- factor(df$região)


# Verificando valores unicos
print(unique(df$UF))

# Convertendo em factor 
df$UF <- factor(df$UF)

# Verificando níveis 
levels(df$escolaridade)


# Verificando ultimas linhas
tail(df)

# Visualizando dados modo planilha
View(df)

# Análise macro dos dados
summary(df)

# ------------------ESCOLARIDADE-----------------------------------------------#

# Frequência absoluta
fa_escolaridade <- table(df$escolaridade)
fa_escolaridade

# Frequência relativa
fr_escolaridade <- prop.table(table(df$escolaridade))
fr_escolaridade

# Frequência percentual
fp_escolaridade <- fr_escolaridade*100
fp_escolaridade
round(fp_escolaridade,digits = 2)

# Moda
names(fa_escolaridade)[which.max(fa_escolaridade)]

barplot(fa_escolaridade,
        main="Frequência Absoluta",
        xlab="Níveis Escolaridade",
        ylab="Quantidade",
        cex.names =1,
        las = 2)



barplot(sort(fa_escolaridade,decreasing = TRUE),
        main="Frequência Absoluta",
        xlab="Níveis Escolaridade",
        ylab="Quantidade")

# ------------------CLASSE DE TRABALHO-----------------------------------------#


levels(df$classe_trabalho)

# Frequência absoluta
fa_classe_trabalho = table(df$classe_trabalho)
fa_classe_trabalho
sort(fa_classe_trabalho)

# Frequência relativa
fr_classe_trabalho <- prop.table(table(df$classe_trabalho))
sort(fr_classe_trabalho)

# Frequência rercentual
fp_classe_trabalho <- fr_classe_trabalho*100
sort(round(fp_classe_trabalho,digits = 2))

# Moda
names(fa_classe_trabalho)[which.max(fa_classe_trabalho)]

# Plotagem
barplot(sort(fa_classe_trabalho),
        main = "Frequência Absoluta",
        ylab = "Quantidade",
        xlab = "Classe Trabalho",
        cex.names = 0.8,
        las=2)

# ------------------Raça-------------------------------------------------------#

levels(df$raça)

# Frequência absoluta
fa_raca = table(df$raça)
sort(fa_raca, decreasing = TRUE)

# Frequência relativa
fr_raca <- prop.table(table(df$raça))
fr_raca

# Frquência percentual
fp_raca <- fr_raca*100
sort(round(fp_raca,digits = 2),decreasing = TRUE)


barplot(sort(fa_raca,decreasing = TRUE),
        main = "Frequencia Absoluta",
        ylab = "Quantidade",
        xlab = "Raça",
        col = c("red", "green", "blue", "yellow"))



barplot(sort(fr_raca,decreasing = TRUE),
        main = "Frequencia Relativa",
        ylab = "Quantidade",
        xlab = "Raça",
        col = c("red", "green", "blue", "yellow"))


# ------------------Estado Civil-----------------------------------------------#

levels(df$Estado_civil)

fa_estado_civil <- table(df$Estado_civil)
fa_estado_civil

barplot(sort(fa_estado_civil,decreasing = TRUE),
        main = "Frequencia Relativa",
        ylab = "Raça",
        xlab = "Quantidade",
        density = 50,
        horiz = TRUE,
        las=2)

# ------------------Região-----------------------------------------------------#

levels(df$região)

fa_regiao <- table(df$região)
fa_regiao

barplot(sort(fa_regiao,decreasing = TRUE),
        main = "Frequencia Relativa",
        ylab = "Quantidade",
        xlab = "Regição",
        col = c("red", "green", "blue", "yellow","purple"))


fp_regiao  <- prop.table(table(df$região))*100
pie(fp_regiao,
    main= "Frequência absoluta",
    labels  = paste( names(fp_regiao), as.character(round(fp_regiao,digits = 2))))


# ------------------UF---------------------------------------------------------#

levels(df$UF)

fa_uf  <-table(df$UF)
fa_uf

barplot(sort(fa_uf,decreasing = TRUE),
        main = "Frequencia Relativa",
        ylab = "Quantidade",
        xlab = "UF",
        col = heat.colors(length(fa_uf)),
        density = 90)

################### QUATITATIVAS DISCRETAS #####################################
# ------------------IDADE------------------------------------------------------#

fa_idade <- table(df$idade)
fr_idade <- prop.table(table(df$idade))
fp_idade <- fr_idade*100

fa_idade
fr_idade
fp_idade

barplot(sort(fa_idade,decreasing = TRUE),las=2)

# MOda 
names(fa_idade)[which.max(fa_idade)]

# Mediana
median(df$idade)
median(df$idade,na.rm = TRUE)

# Media
mean(df$idade)
mean(df$idade,na.rm = TRUE)

# Quartis
quantile(df$idade)
quantile(df$idade,na.rm = TRUE)


max(df$idade)
min(df$idade)

# Amplitude 
diff(range(df$idade))
diff(range(df$idade,na.rm = TRUE))

# Variância
var(df$idade)

# Desvio Padrão
sd(df$idade)

# Coeficiente de variação
sd(df$idade)/mean(df$idade)

# Amplitude interquartilica
quartis_idade <-quantile(df$idade)
quartis_idade[4]-quartis_idade[2]

# Frequência acumulada 
faa_idade <- cumsum(df$idade)
faa_idade2 <- cumsum(head(df$idade,n = 10))

head
barplot(faa_idade)
pie(faa_idade)
plot(fa_idade,
     main="Frequencia absoluta",
     ylab = "Quantidade",
     xlab="Idades")

plot(faa_idade2,
     main="Frequencia absoluta",
     ylab = "Quantidade",
     xlab="Idades",
     type = "s")

