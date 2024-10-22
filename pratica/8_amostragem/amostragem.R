knitr::opts_chunk$set(collapse = TRUE)
suppressPackageStartupMessages(library(tidyverse))

## # Instalando o pacote
##   install.packages("survey")

  # Chamando a biblioteca survey
  library(survey)
  library(tidyverse) # Para manipular os dados
  library(readxl) # Para ler arquivos .xls e .xlsx

  # Lendo os microdados do Vigitel 2023
  vigitel <- read_xlsx("Data/Vigitel-2023-peso-rake.xlsx")

  BH <- vigitel |> filter(cidade == 3)
  
  # Tamanho da amostra
  nrow(BH)

  # Olhar o dicionário de variáveis!
  # Variável sexo - q7
  BH$sexo <- factor(x = BH$q7, levels = 1:2, 
               labels = c("Masculino", "Feminino"))

  table(BH$sexo)
  

# Tabagismo por sexo (ignorando o desenho)
by(data = BH$fumante, INDICES = BH$sexo, FUN = function(x) 
      c(mean = mean(x), 
        sd = sqrt( mean(x) * (1-mean(x)) / length(x) )
       )  )

# Faixa etaria
BH$fet = factor( BH$fet, labels = c("18 a 24", "25 a 34",
                                   "35 a 44", "45 a 54", 
                                   "55 a 64", "65 +"))
BH$fet <- relevel(BH$fet, ref = "25 a 34")


# Escolaridade
BH$fesc = factor( BH$fesc, labels = c("0 a 8 anos", 
                                      "9 a 11 anos",
                                      "12 anos e mais"))

# Atividade fisica
BH$af150 <- factor(BH$af3dominios, labels = c("AF < 150", "AF >= 150"))

# Alimentacao saudavel
BH$alimsau <- factor( BH$flvreg, labels = c("AS Nao", "AS Sim"))

# Comportamente Saudavel
BH$compsaude <- factor( BH$af3dominios + 2*BH$flvreg, labels = c("Nenhum", "Ativ. Fis.", "Alim. Sau.", "Ambos"))

aux <- table(BH$fet, BH$sexo)
pyramid::pyramid(data.frame(M = aux[,1], F = aux[,2], Ages = rownames(aux)), Llab = "Homens", Rlab = "Mulheres")

# Definindo o desenho
BH.svy <- svydesign( id=~1, strata =NULL, fpc=NULL, 
                     weights = ~pesorake, data=BH)
# id -- variavel que define os clusters
#      ~1 significa que que não tem clusters
# strata -- variável que define os estratos
# fpc -- correção de população finita, aponta para a
#       variável do banco com o tamanho da população
# weights -- pesos amostrais
# data -- data frame com os dados gerados

# Estimando o total de fumantes de BH em 2016
svytotal(~fumante, BH.svy)

# Estimando prevalência de tabagismo na capital
svymean(~fumante, BH.svy)

# Tabagismo por sexo
svyby(formula = ~fumante, by = ~sexo, design = BH.svy, 
      FUN = svymean)

# Tabagismo por escolaridade
svyby(formula = ~fumante, by = ~fesc, design = BH.svy, 
      FUN = svymean)

# Tabagismo por faixa etaria
svyby(formula = ~fumante, by = ~fet, design = BH.svy, 
      FUN = svymean)[,-1]

P1 <- svyby(formula = ~fumante, by = ~fet, design = BH.svy, FUN = svymean)

arm::coefplot(P1[,2], P1[,3], varnames=as.character(P1[,1]), main = "Prevalencias")

# Modelo
modelo <- fumante ~ fet

# Ajuste sem pesos
output0 <- glm(modelo, data = BH, family = binomial)

# Ajuste com pesos
output <- svyglm(formula = modelo, 
                 family = binomial, 
                 design = BH.svy)


# Coeficientes estimados
cbind( Sem_Pesos = coef(output0), Com_Pesos = coef(output))

summary(output)

output.aux <- output
class(output.aux) <- "glm"
arm::coefplot(output.aux, varnames=levels(BH$fet))


output3 <- output0

arm::coefplot(output.aux, varnames=levels(BH$fet), )
arm::coefplot(output3, col=2, add=T)
legend("bottomleft", c("glm", "svyglm"), col=2:1, lty=1, pch=20)


P1 <- svyby(formula = ~fumante, by = ~fet, 
            design = BH.svy, FUN = svymean)

P2 <- predict( output, type = "response" ,se.fit = T, 
               newdata = data.frame( 
                 fet = levels(BH$fet) 
                 ) 
               )

P3 <- predict( output0, type = "response" ,se.fit = T, 
               newdata = data.frame( 
                 fet = levels(BH$fet) 
                 ) 
               )

P2 <- data.frame(P2)
P3 <- data.frame(P3)

arm::coefplot(P1[,2], P1[,3], varnames=as.character(P1[,1]), main = "Prevalencias")
arm::coefplot(P2[,1], P2[,2], add = T, col=2)
arm::coefplot(P3[,1], P3[,2], add = T, col=3, offset = 0.2)
legend("topright", c("glm", "svyglm", "svymean"), col=3:1, lty=1, pch=20)


# Modelo
modelo2 <- fumante ~ fet + sexo + fesc

# Ajuste
output2 <- svyglm(formula = modelo2, 
                 family = binomial, 
                 design = BH.svy)



class(output2) <- "glm"
arm::coefplot(output.aux, varnames = levels(BH$fet))
arm::coefplot(output2, col=2, add=T)
legend("topright", c("svyglm + covariaveis", "svyglm"), col=2:1, lty=1, pch=20)

