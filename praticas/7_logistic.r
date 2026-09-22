### R code from vignette source '7_logistic/7_logistic.Rnw'

###################################################
### code chunk number 1: 7_logistic.Rnw:84-87
###################################################
  
suppressPackageStartupMessages(library(tidyverse))


###################################################
### code chunk number 2: 7_logistic.Rnw:139-142
###################################################
# dados <- read.csv("Aula6_binary/DUsifilis.csv")
dados <- read.csv("../dados/DUsifilis.csv")
head(dados)


###################################################
### code chunk number 3: 7_logistic.Rnw:150-152
###################################################
m0Sex <- glm(sifilis ~ sexo, dados, family=binomial())
(m0Sex)


###################################################
### code chunk number 4: 7_logistic.Rnw:181-186
###################################################
dados$sexo = relevel(factor(dados$sexo), 
                     ref = "masculino")
m0Sex <- glm(sifilis ~ sexo, 
             dados, family=binomial())
 (m0Sex)


###################################################
### code chunk number 5: 7_logistic.Rnw:200-203
###################################################
m1Sex <- glm(sifilis ~ sexo + faixaetaria, 
             dados, family=binomial())
 (m1Sex)


###################################################
### code chunk number 6: 7_logistic.Rnw:218-221
###################################################
m2Sex <- glm(sifilis ~ sexo + idade, 
             dados, family=binomial())
 (m2Sex)


###################################################
### code chunk number 7: 7_logistic.Rnw:232-233
###################################################
anova(m1Sex) %>%  ()


###################################################
### code chunk number 8: 7_logistic.Rnw:254-255
###################################################
AIC(m0Sex, m1Sex, m2Sex)


###################################################
### code chunk number 9: 7_logistic.Rnw:258-259
###################################################
glmtoolbox::adjR2(m0Sex, m1Sex, m2Sex)


###################################################
### code chunk number 10: 7_logistic.Rnw:273-275
###################################################
car::vif( glm(sifilis ~ sexo + faixaetaria, 
             dados, family=binomial()) )


###################################################
### code chunk number 11: 7_logistic.Rnw:288-290
###################################################
summary( glm(sifilis ~ sexo + idade + faixaetaria, 
             dados, family=binomial()) )


###################################################
### code chunk number 12: 7_logistic.Rnw:306-308
###################################################
car::vif( glm(sifilis ~ sexo + idade + faixaetaria, 
             dados, family=binomial()) )


###################################################
### code chunk number 13: 7_logistic.Rnw:319-334
###################################################
aux = cbind(OR = exp(c(
  m0Sex$coefficients[2], 
  m1Sex$coefficients[2], 
  m2Sex$coefficients[2])
), 
exp(
  rbind(
    confint(m0Sex)[2,], 
    confint(m1Sex)[2,], 
    confint(m2Sex)[2,]
    )
  )
)
rownames(aux) = c("Bruto", "Ajustado por faixaetaria", "Ajustado por idade" )
aux
# aux %>%  xtable(caption = "Efeito do sexo (Base: Masculino) no log da chance de infecção por sífilis")


###################################################
### code chunk number 14: 7_logistic.Rnw:394-395
###################################################
glmtoolbox::hltest(m1Sex)


###################################################
### code chunk number 15: 7_logistic.Rnw:439-440
###################################################
summary(influence.measures(m1Sex))


