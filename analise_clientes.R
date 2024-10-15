
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
df$escolaridade <- factor(df$escolaridade)


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
        ylab="Quantidade")


