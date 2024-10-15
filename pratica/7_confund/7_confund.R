#############################
# Script para a aula 8 da
# Disciplina Bioestatístia II
# VigiFronteiras
# Leo Bastos e Aline Nobre
# Novembro de 2022
#############################

# Regressão Logística - Interação e Confundimento

#Dicionário de dados:
#saruim - avaliação de saúde ruim (0=Não ou 1=Sim).
#q6 - idade.
#q7 - sexo (1=Masculino, 2=Feminino).
#inativo - inatividade física (0=Não ou 1=Sim).
#fesc - faixas de escolaridade (1=“0 a 8 anos”,2=“9 a 11 anos”,3=“12 anos ou mais”).

# Importando o banco de dados VIGITEL:
vigitelSL <- read.csv2("Vigitel2019_SaoLuis.csv")

### Renomeando as variáveis e transformando em fator 
vigitelSL$saruim <- factor( vigitelSL$saruim, labels=c("Não","Sim"))
table(vigitelSL$saruim)
vigitelSL$idade <- vigitelSL$q6
summary( vigitelSL$idade )
vigitelSL$sexo<-factor(vigitelSL$q7,labels=c("Masculino","Feminino"))
table(vigitelSL$sexo)
vigitelSL$fesc<-factor(vigitelSL$fesc,labels=c("0 a 8 anos","9 a 11 anos","12 anos e mais"))
table(vigitelSL$fesc)
vigitelSL$inativo<-factor(vigitelSL$inativo,labels=c("Não","Sim"))
table(vigitelSL$inativo)

### Suponha a variável auto avaliação de saúde (saruim) como variável
#dependente e as demais variáveis como independentes

### Construindo a Tabela 1
library(tableone)
vars=c("idade","sexo","fesc","inativo")
tabela=CreateTableOne(data=vigitelSL,vars=vars,strata="saruim")
print(tabela,showAllLevels = TRUE)

## Agora suponha que a variável inatividade física seja a exposição 
#principal de interesse e as demais covariáveis

### Vamos ajustar o modelo bruto
modbruto<- glm(saruim ~ inativo, family=binomial(logit), data=vigitelSL)
summary(modbruto)
#Calculando OR e IC(95%)
cbind(exp(coef(modbruto)),exp(confint(modbruto)))


### Será que a idade é uma variável de confundimento?
modajust1<- glm(saruim ~ inativo + idade, family=binomial(logit), data=vigitelSL)
summary(modajust1)
#Calculando OR e IC(95%)
cbind(exp(coef(modajust1)),exp(confint(modajust1)))

### E a escolaridade?
modajust2<- glm(saruim ~ inativo + idade + fesc, family=binomial(logit), data=vigitelSL)
summary(modajust2)
#Calculando OR e IC(95%)
cbind(exp(coef(modajust2)),exp(confint(modajust2)))


### Comparando as deviances
anova(modajust2, test="LRT")


### Será que o sexo é uma variável modificadora de efeito?
modint<- glm(saruim ~ inativo*sexo, family=binomial(logit), data=vigitelSL)
summary(modint)

### Comparando as deviances
anova(modint, test="LRT")

#Temos 2 possibilidades: estratificar por sexo ou calcular as OR's 
#considerando a interação

### Estratificando por sexo
vigitelSL1<-subset(vigitelSL, subset=sexo=="Masculino")
modsexo1<- glm(saruim ~ inativo, family=binomial(logit), data=vigitelSL1)
summary(modsexo1)

vigitelSL2<-subset(vigitelSL, subset=sexo=="Feminino")
modsexo2<- glm(saruim ~ inativo, family=binomial(logit), data=vigitelSL2)
summary(modsexo2)
#Calculando OR e IC(95%)
cbind(exp(coef(modsexo1)),exp(confint(modsexo1)))
cbind(exp(coef(modsexo2)),exp(confint(modsexo2)))

### Calculando a OR na presença de interação
sexo<-c(0,1)
or<-exp(modint$coef[2]+modint$coef[4]*sexo)
or
# Agora vamos calcular o IC
vcov(modint)
var.or<- 0.2622040+sexo^2*0.3289436+2*sexo*(-0.2622040)
var.or
li<-exp(modint$coef[2]+modint$coef[4]*sexo-1.96*sqrt(var.or))
ls<-exp(modint$coef[2]+modint$coef[4]*sexo+1.96*sqrt(var.or))
cbind(or,li,ls)

### Extra - Usando weights###
table(vigitelSL$inativo, vigitelSL$saruim)

inativo<-c(1,1,0,0)
saruim<-c(1,0,1,0)
n<-c(31,338,63,1633)

banco<-as.data.frame(cbind(inativo,saruim,n))

mod<- glm(saruim ~ inativo, family=binomial(logit), 
          weights = n, data=banco)
summary(mod)

#Comparando com o banco original
summary(modbruto)
