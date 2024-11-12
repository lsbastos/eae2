library(tidyverse)

n <- 1000

set.seed(1234)

dados <- tibble(Hipertensao = rbinom(n, 1,.5))

# A funcao logit e inversa logit nao estao implementadas no R
# Vamos aproveitar e ilustrar como se cria uma funcao.
inv.logit <- function(x){
  ex <- exp(x)
  return(ex / (1 + ex))
}
inv.logit(0)
inv.logit(c(0,2,-1))


dados <- dados %>% mutate(
  Aterosclerose = rbinom(n, 1, inv.logit(2*Hipertensao)),
  AVC = rbinom(n, 1, inv.logit(-1+0.05*Hipertensao+ 2*Aterosclerose)),
  Aterosclerose = factor(Aterosclerose),
  Hipertensao = factor(Hipertensao)
)

dados %>% group_by(AVC,Aterosclerose,Hipertensao) %>% 
  summarise(n = n())

ggplot(dados %>% group_by(Aterosclerose, Hipertensao) %>% 
         summarise(n = n(), Prop = mean(AVC)), 
       aes(x=Aterosclerose, fill=Hipertensao, y = Prop)) + 
  geom_col(position = position_dodge()) + ylab("Probabilidade de AVC")
# Nao ficou muito realista né?

# Como podemos alterar os coeficientes para que seja? 

mod.simp <- glm(AVC ~ Hipertensao, 
                family = binomial(), data = dados)
coef( mod.simp )

mod.full <- glm(AVC ~ Hipertensao + Aterosclerose, 
                family = binomial(), data = dados)
coef( mod.full )



library(medflex)

expDados <- neImpute(object = mod.full)

head(expDados)

output <- neModel(formula = AVC ~ Hipertensao0 + Hipertensao1, family = binomial("logit"), 
                  expData = expDados)


summary(output)




###########################################
# Tentando gerar um cenário um pouco mais 
# realista
############################################

library(tidyverse)

n <- 1000

set.seed(12345)

dados <- tibble(Hipertensao = rbinom(n, 1,.5))

dados <- dados %>% mutate(
  Aterosclerose = rbinom(n, 1, inv.logit(2*Hipertensao)),
  AVC = rbinom(n, 1, inv.logit(-2+ 1*Hipertensao + (3-log(1.2))*Aterosclerose + (log(1.2)-1) * Aterosclerose * Hipertensao )),
  Aterosclerose = factor(Aterosclerose),
  Hipertensao = factor(Hipertensao)
)


ggplot(dados %>% group_by(Aterosclerose, Hipertensao) %>% 
         summarise(n = n(), Prop = mean(AVC)), 
       aes(x=Aterosclerose, fill=Hipertensao, y = Prop)) + 
  geom_col(position = position_dodge()) + ylab("Probabilidade de AVC")


mod.simp <- glm(AVC ~ Hipertensao, 
                family = binomial(), data = dados)
coef( mod.simp )

mod.full <- glm(AVC ~ Hipertensao * Aterosclerose, 
                family = binomial(), data = dados)
coef( mod.full )



library(medflex)

expDados <- neImpute(object = mod.full)

head(expDados)

output <- neModel(formula = AVC ~ Hipertensao0 + Hipertensao1, family = binomial("logit"), 
                  expData = expDados)


summary(output)

summary(neEffdecomp(output))

plot(neEffdecomp(output))


