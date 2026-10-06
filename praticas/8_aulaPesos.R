
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------------
  # Chamando a biblioteca survey
  library(tidyverse) # Para manipular os dados
  library(readxl) # Para ler arquivos .xls e .xlsx


## ----warning=FALSE, message=FALSE----------------------------------------------
  # Chamando a biblioteca survey
  library(survey)

  # Lendo os microdados do Vigitel 2023
  vigitel <- read_xlsx("Data/Vigitel-2023-peso-rake.xlsx")

  BH <- vigitel |> filter(cidade == 3)
  
  # Tamanho da amostra
  nrow(BH)


## ----echo =FALSE, message=FALSE------------------------------------------------
# Faixa etaria
BH$fet = factor( BH$fet, labels = c("18 a 24", "25 a 34",
                                   "35 a 44", "45 a 54", 
                                   "55 a 64", "65 +"))
BH$fet <- relevel(BH$fet, ref = "18 a 24")

BH$sexo <- factor(x = BH$q7, levels = 1:2, 
               labels = c("Masculino", "Feminino"))

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
pyramid::pyramid(data.frame(M = aux[,1], F = aux[,2], 
                            Ages = rownames(aux)), 
                 Llab = "Homens", Rlab = "Mulheres", Clab = "Faixa etária")


## ----echo =FALSE, message=FALSE------------------------------------------------
# FOnte: DATASUS, RIPSA
# https://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvs2024br.def
aux2 <- data.frame(ages = c("18 a 24", "25 a 34", "35 a 44", "45 a 54", 
                            "55 a 64", "65 +"),
                   males = c(117149,	182940,	186749,	152770,
                             126657,	134336)/1000,
                   females = c(116918,	192023,	207169,	179339,
                               161554,	209018)/1000
                   )

pyramid::pyramid(aux2[c(2,3,1)], 
                 Llab = "Homens (x1000)", Rlab = "Mulheres (x1000)", Clab = "Faixa etária"
                 )



## ------------------------------------------------------------------------------
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


## ----echo=T, message=T, warning=T----------------------------------------------
# Desenho iid
BH.svy.0 <- svydesign( id=~1, strata =NULL, fpc=NULL, 
                     data=BH)



## ------------------------------------------------------------------------------
# Estimando prevalência de tabagismo na capital
svymean(~fumante, BH.svy.0)

# Estimando prevalência de tabagismo na capital
svymean(~fumante, BH.svy)

# Estimando o total de fumantes de BH em 2016
svytotal(~fumante, BH.svy)



## ------------------------------------------------------------------------------
# Tabagismo por sexo (ignorando o desenho)
svyby(formula = ~fumante, by = ~sexo, design = BH.svy.0, 
      FUN = svymean)[,-1]

# Tabagismo por sexo
svyby(formula = ~fumante, by = ~sexo, design = BH.svy, 
      FUN = svymean)[,-1]



## ------------------------------------------------------------------------------

# Tabagismo por escolaridade
svyby(formula = ~fumante, by = ~fesc, design = BH.svy, 
      FUN = svymean)


## ----echo = T------------------------------------------------------------------
# Tabagismo por faixa etaria
svyby(formula = ~fumante, by = ~fet, design = BH.svy, 
      FUN = svymean)[,-1]


## ----echo = F------------------------------------------------------------------
P1 <- svyby(formula = ~fumante, by = ~fet, design = BH.svy, FUN = svymean)
P2 <- svyby(formula = ~fumante, by = ~fet, design = BH.svy.0, FUN = svymean)

arm::coefplot(P1[,2], P1[,3], varnames=as.character(P1[,1]), 
              main = "Prevalencia de tabagismo em BH")
arm::coefplot(P2[,2], P2[,3], varnames=as.character(P2[,1]), 
              add=T, col.pts = "red")
legend("topright", c("Sem desenho", "Com desenho"), col=2:1, lty=1, pch=20)



## ----echo = F------------------------------------------------------------------
P3 <- svyby(formula = ~fumante, by = ~fet+sexo, design = BH.svy, FUN = svymean)
arm::coefplot(P3[P3$sexo=="Masculino",3], P3[P3$sexo=="Masculino",4], varnames=as.character(P3[P3$sexo=="Masculino",1]), 
              main = "Prevalencia de tabagismo em BH", )
arm::coefplot(P3[P3$sexo=="Feminino",3], P3[P3$sexo=="Feminino",4],              add=T, col.pts = "red")
legend("topright", c("Sexo: Feminino", "Sexo: Masculino"), col=2:1, lty=1, pch=20)



## ------------------------------------------------------------------------------
# Modelo
modelo <- fumante ~ fet

# Ajuste sem pesos
output0 <- glm(modelo, data = BH, family = binomial)

# Ajuste com pesos
output <- svyglm(formula = modelo, 
                 family = binomial, 
                 design = BH.svy)



## ------------------------------------------------------------------------------
# Coeficientes estimados
cbind( Sem_Pesos = coef(output0), Com_Pesos = coef(output))


## ------------------------------------------------------------------------------
summary(output)


## ----echo=F--------------------------------------------------------------------
output.aux <- output
class(output.aux) <- "glm"
arm::coefplot(output.aux, varnames=levels(BH$fet))



## ----echo=F--------------------------------------------------------------------
output3 <- output0

arm::coefplot(output.aux, varnames=levels(BH$fet), xlim = c(-2.5,1.5) )
arm::coefplot(output3, col=2, add=T)
legend("bottomleft", c("glm", "svyglm"), col=2:1, lty=1, pch=20)



## ----echo=T--------------------------------------------------------------------
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


## ----echo=F--------------------------------------------------------------------
P2 <- data.frame(P2)
P3 <- data.frame(P3)

arm::coefplot(P1[,2], P1[,3], varnames=as.character(P1[,1]), main = "Prevalencias")
arm::coefplot(P2[,1], P2[,2], add = T, col=2)
arm::coefplot(P3[,1], P3[,2], add = T, col=3, offset = 0.2)
legend("topright", c("glm", "svyglm", "svymean"), col=3:1, lty=1, pch=20)



## ----warning=FALSE-------------------------------------------------------------
# Modelo
modelo2 <- fumante ~ fet + sexo + fesc

# Ajuste
output2 <- svyglm(formula = modelo2, 
                 family = binomial, 
                 design = BH.svy)



## ----echo=F--------------------------------------------------------------------

class(output2) <- "glm"
arm::coefplot(output.aux, varnames = levels(BH$fet))
arm::coefplot(output2, col=2, add=T)
legend("topright", c("svyglm + covariaveis", "svyglm"), col=2:1, lty=1, pch=20)


