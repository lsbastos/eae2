## ----echo=FALSE, results=hide-------------------------------------------------
birth <- read.table("birthweight.data", header=T)
birth$smoke = factor(birth$smoke, labels=c("nao","sim"))
birth$ht = factor(birth$ht, labels=c("nao","sim"))
birth$ui = factor(birth$ui, labels=c("nao","sim"))
birth$lwt = round(birth$lwt * 0.45359237) # Peso em Kilos


## ----echo=FALSE, results=tex--------------------------------------------------
library(xtable)
set.seed(1234)
xtable(data.frame(Y = rbinom(10,1,0.5),
                  X1 = rbinom(10,1,0.5),
                  X2 = sample(c("Controle", "Tratamento 1", "Tratamento 2"),size = 10, replace = T)
                  )
       )


## ----echo=FALSE, results=tex--------------------------------------------------
xtable(data.frame(Y = c(3,6,12,3,20,1),
                  n = c(10,30,45,20,50,15),
                  X1 = c(0,0,0,1,1,1),
                  X2 = rep(c("Controle", "Tratamento 1", "Tratamento 2"),2)
                  ), digits = 0
       )


## ----echo=F,results=tex-------------------------------------------------------
library(arm)
library(tidyverse)
aux <- tibble(Prob = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) %>% mutate( 
  Odds = Prob / (1-Prob),
  'log(Odds)' = logit(Prob))
xtable(aux)


## ----echo=F, fig=T,  width=6, height=4----------------------------------------
teste <- tibble(X = seq(-3, 3, by = 0.1)) %>%
  mutate(
    Probp1p2 = invlogit(1 + 2*X),
    Probm1p2 = invlogit(-1 + 2*X),
    Probp1m2 = invlogit(1 - 2*X),
    Probm1m2 = invlogit(-1 - 2*X)
    ) %>% gather(Beta, Prob, -X)
  
aux.eq <- c(
  "+1+2x",
  "-1 + 2x",
  "+1 - 2x",
  "-1 - 2x")

ggplot(teste, aes(x = X, y = Prob, col = Beta)) + geom_line(show.legend = T ) + theme_bw(base_size = 18) + 
  scale_color_discrete(name = expression(logit(theta)), 
                     breaks = unique(teste$Beta),
                     labels = aux.eq
                     ) #+ theme(legend.position="bottom")
  


## -----------------------------------------------------------------------------
saida <- glm(low ~ smoke, 
             family=binomial(link = 'logit'), 
             data=birth)


## ----echo=F-------------------------------------------------------------------
saida


## -----------------------------------------------------------------------------
exp(0.7041)


## ----echo=F-------------------------------------------------------------------
summary(saida)


## -----------------------------------------------------------------------------
cbind(saida$coef, confint(saida) )


## -----------------------------------------------------------------------------
exp(cbind(OR=saida$coef, confint(saida) ))[-1,]


## -----------------------------------------------------------------------------
birth.pred <- data.frame(smoke = c("sim", "nao"))
prev <- predict(saida, type = 'response',
                newdata = birth.pred)
prev


## -----------------------------------------------------------------------------
prev2 <- predict(saida, type = 'link',
                newdata = birth.pred,
                se.fit = T)


## -----------------------------------------------------------------------------
birth.pred <- birth.pred %>% 
  bind_cols(
    Prob = prev, 
    Link = prev2$fit, 
    Link.sd = prev2$se.fit
  )


## ----echo=F, results=tex------------------------------------------------------
xtable(birth.pred, digits = 3)


## ----echo=F, results=tex------------------------------------------------------
aux <- birth.pred %>% mutate(
  LI = Link - 1.96 * Link.sd,
  LS = Link + 1.96 * Link.sd
) %>% select(-Prob)
xtable(aux, digits = 3)


## ----echo=F, results=tex------------------------------------------------------
inv.fun <- saida$family$linkinv
aux <- aux %>% mutate(
  Prob = inv.fun( Link ),
  LI = inv.fun(LI),
  LS = inv.fun(LS)
) %>% select(smoke, Prob, LI, LS)
xtable(aux, digits = 3)

