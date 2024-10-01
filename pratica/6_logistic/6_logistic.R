## ----echo=FALSE, results=hide----------------------------------------
  
suppressPackageStartupMessages(library(tidyverse))
library(xtable)


## --------------------------------------------------------------------
# dados <- read.csv("Aula6_binary/DUsifilis.csv")
dados <- read.csv("DUsifilis.csv")
head(dados)


## ----echo=FALSE,results=tex------------------------------------------
m0Sex <- glm(sifilis ~ sexo, dados, family=binomial())
xtable(m0Sex)


## ----echo=FALSE,results=tex------------------------------------------
dados$sexo = relevel(factor(dados$sexo), ref = "masculino")
m0Sex <- glm(sifilis ~ sexo, dados, family=binomial())
m1Sex <- glm(sifilis ~ sexo + faixaetaria, dados, family=binomial())
xtable(m1Sex)


## ----echo=FALSE,results=tex------------------------------------------
anova(m1Sex) %>% xtable()


## ----echo=FALSE, results=tex-----------------------------------------
aux = cbind(OR = exp(c(
  m0Sex$coefficients[2], 
  m1Sex$coefficients[2])
), 
exp(
  rbind(
    confint(m0Sex)[2,], 
    confint(m1Sex)[2,]
    )
  )
)
rownames(aux) = c("sexo", "sexo + faixaetaria")
aux %>%  xtable()


## ----echo=T----------------------------------------------------------
summary(influence.measures(m1Sex))

