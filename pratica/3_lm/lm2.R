## ----echo=FALSE,fig=T---------------------------------------------------------
library(tidyverse, quietly = T )
# Carregando dados da aula 1
load("../1_lm/pratica_lm.RData")
racao$Racao <- factor(racao$Racao, levels = unique(racao$Racao), labels = c("NovaIr", "NovaAuto","Controle")) 
racao$Racao <- relevel(racao$Racao, ref = "Controle")
p <- ggplot(racao, aes(x=Racao, y=Peso))
p + geom_boxplot(fill = "lightblue") + theme_bw(base_size = 18)


## -----------------------------------------------------------------------------
summary(aov(Peso ~ Racao, data = racao))


## ----echo=FALSE,fig=T---------------------------------------------------------
p2 <- ggplot(racao, aes(x=Linhagem, y=Peso))
p2 + geom_boxplot(fill = "lightgreen") + theme_bw(base_size = 18)


## -----------------------------------------------------------------------------
summary(aov(Peso ~ Linhagem, data = racao))


## ----echo=FALSE,fig=T---------------------------------------------------------
p <- ggplot(racao, aes(x=Linhagem, y=Peso, fill=Racao))
p + geom_boxplot() + theme_bw(base_size = 18)  + theme(legend.position = c(.2, 0.8))


## -----------------------------------------------------------------------------
summary(aov(Peso ~ Racao + Linhagem, data = racao))


## ----echo=T-------------------------------------------------------------------
modelo <- lm(Peso ~ Racao + Linhagem, data = racao)
summary(modelo)


## ----fig=TRUE-----------------------------------------------------------------
hist(modelo$residuals, col = 2)


## ----fig=TRUE-----------------------------------------------------------------
qqnorm(modelo$residuals )


## -----------------------------------------------------------------------------
shapiro.test(modelo$residuals)


## ----fig=TRUE-----------------------------------------------------------------
plot(modelo$residuals)


## ----echo=T-------------------------------------------------------------------
modeloR <- lm(Peso ~ Racao, data = racao)
modeloL <- lm(Peso ~ Linhagem, data = racao)
modelo <- lm(Peso ~ Racao + Linhagem, data = racao)
modeloRL <- lm(Peso ~ Racao * Linhagem, data = racao)


## -----------------------------------------------------------------------------
summary(modeloRL)


## -----------------------------------------------------------------------------
anova(modeloRL)


## -----------------------------------------------------------------------------
anova(lm(Peso ~ Linhagem*Racao, data = racao))


## -----------------------------------------------------------------------------
anova(modeloR, modeloRL)


## -----------------------------------------------------------------------------
AIC(modelo)
BIC(modelo)


## ----echo=F,results=tex-------------------------------------------------------

xtable::xtable(
  cbind(AIC(modeloR, modeloL, modelo, modeloRL),
        BIC(modeloR, modeloL, modelo, modeloRL)
        )[,-3]
)


## -----------------------------------------------------------------------------
(modelo1.shhs <- lm(BMI ~ Sex, data = SHHS))


## -----------------------------------------------------------------------------
(modelo2.shhs <- lm(BMI ~ Smoking, data = SHHS))


## -----------------------------------------------------------------------------
(modelo3.shhs <- lm(BMI ~ Sex + Smoking, data = SHHS))


## -----------------------------------------------------------------------------
(modelo4.shhs <- lm(BMI ~ Sex * Smoking, data = SHHS))


## -----------------------------------------------------------------------------
anova(modelo4.shhs)


## ----echo=F,results=tex-------------------------------------------------------

xtable::xtable(
  cbind(AIC(modelo1.shhs, modelo2.shhs, modelo3.shhs, modelo4.shhs),
        BIC(modelo1.shhs, modelo2.shhs, modelo3.shhs, modelo4.shhs)
        )[,-3]
)


## ----echo=T, fig=T------------------------------------------------------------
hist(modelo3.shhs$residuals)


## ----echo=T, fig=T------------------------------------------------------------
plot(modelo3.shhs$residuals)


## ----echo=T-------------------------------------------------------------------
summary(modelo3.shhs)


## ----echo=F, fig=T------------------------------------------------------------
dmft <- dmft %>%  mutate(Pais = factor(Pais, levels= 1:2, labels = c("Industrializado", "Em desenvolvimento")))
p <- ggplot(dmft, aes(y = DMFT, x = Consumo, colour = Pais))
p <- p + geom_point() + xlab("Consumo de açucar (kg per capita/ano)") + theme_bw(base_size = 18) + theme(legend.position = c(.25, 0.8)) + labs(colour = "País") + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1))
p


## -----------------------------------------------------------------------------
# Modelo ignorando o tio de país
modelo1 <- lm(DMFT ~ Consumo, data = dmft)
#
# Modelo variando intercepto
modelo2 <- lm(DMFT ~ Consumo + Pais, data = dmft)
#
# Modelo variando slope
modelo3 <- lm(DMFT ~ Consumo + Consumo:Pais, 
              data = dmft)
#
# Modelo variando intercepto e slope
modelo4 <- lm(DMFT ~ Consumo + Pais + Consumo:Pais,
              data = dmft)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo1)
p + geom_abline(slope = aux[2], intercept = aux[1])


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo1)


## ----echo=T-------------------------------------------------------------------
AIC(modelo1)
BIC(modelo1)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo2)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2], intercept = aux[1]+aux[3], color = "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo2)


## ----echo=T-------------------------------------------------------------------
AIC(modelo2)
BIC(modelo2)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo3)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2]+aux[3], intercept = aux[1], color = "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo3)


## ----echo=T-------------------------------------------------------------------
AIC(modelo3)
BIC(modelo3)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo4)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2]+aux[4], intercept = aux[1]+aux[3], color = "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo4)


## ----echo=T-------------------------------------------------------------------
AIC(modelo4)
BIC(modelo4)


## ----echo=F, fig=T------------------------------------------------------------
hist(modelo4$residuals)


## ----echo=F, fig=T------------------------------------------------------------
p <- ggplot( dmft %>% bind_cols(Resid = modelo4$residuals, ID = 1:length(modelo4$residuals)), aes(x = ID, y = Resid))
ppp <- p + geom_point() + theme_bw(base_size = 18)
ppp


## ----echo=F, fig=T------------------------------------------------------------
p + geom_point(aes(color = Pais), show.legend = F) + theme_bw(base_size = 18)


## -----------------------------------------------------------------------------
# Modelo ignorando o tio de país
modelo1 <- lm(log(DMFT) ~ Consumo, data = dmft)
#
# Modelo variando intercepto
modelo2 <- lm(log(DMFT) ~ Consumo + Pais, data = dmft)
#
# Modelo variando slope
modelo3 <- lm(log(DMFT) ~ Consumo + Consumo:Pais, 
              data = dmft)
#
# Modelo variando intercepto e slope
modelo4 <- lm(log(DMFT) ~ Consumo + Pais + Consumo:Pais,
              data = dmft)


## ----echo=F, fig=T------------------------------------------------------------
p <- ggplot(dmft, aes(y = DMFT, x = Consumo, colour = Pais))
p <- p + geom_point() + xlab("Consumo de açucar (kg per capita/ano)") + theme_bw(base_size = 18) + theme(legend.position = c(.25, 0.8)) + labs(colour = "País") + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1))
p + geom_line(data = dmft %>% bind_cols(Fit = exp(modelo1$fitted.values)) %>% arrange(Consumo), aes(x = Consumo, y = Fit))


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo1)


## ----echo=T-------------------------------------------------------------------
AIC(modelo1)
BIC(modelo1)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo2)
p + geom_line(data = dmft %>% bind_cols(Fit = exp(aux[1] + dmft$Consumo * aux[2])) %>% arrange(Consumo), aes(x = Consumo, y = Fit), color = "#F8766D") + 
  geom_line(data = dmft %>% bind_cols(Fit = exp(aux[1] + aux[3] + dmft$Consumo * aux[2])) %>% arrange(Consumo), aes(x = Consumo, y = Fit), color =  "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo2)


## ----echo=T-------------------------------------------------------------------
AIC(modelo2)
BIC(modelo2)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo3)
p + geom_line(data = dmft %>% bind_cols(Fit = exp(aux[1] + dmft$Consumo * aux[2])) %>% arrange(Consumo), aes(x = Consumo, y = Fit), color = "#F8766D") + 
  geom_line(data = dmft %>% bind_cols(Fit = exp(aux[1] + dmft$Consumo * (aux[2]+ aux[3]))) %>% arrange(Consumo), aes(x = Consumo, y = Fit), color =  "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo3)


## ----echo=T-------------------------------------------------------------------
AIC(modelo3)
BIC(modelo3)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modelo4)
p + geom_line(data = dmft %>% bind_cols(Fit = exp(aux[1] + dmft$Consumo * aux[2])) %>% arrange(Consumo), aes(x = Consumo, y = Fit), color = "#F8766D") + 
  geom_line(data = dmft %>% bind_cols(Fit = exp(aux[1] + aux[3] + dmft$Consumo * (aux[2]+ aux[4]))) %>% arrange(Consumo), aes(x = Consumo, y = Fit), color =  "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modelo4)


## ----echo=T-------------------------------------------------------------------
AIC(modelo4)
BIC(modelo4)


## ----echo=F, fig=T------------------------------------------------------------
hist(modelo4$residuals)


## ----echo=F, fig=T------------------------------------------------------------
p <- ggplot( dmft %>% bind_cols(Resid = modelo4$residuals, ID = 1:length(modelo4$residuals)), aes(x = ID, y = Resid))
p + geom_point() + theme_bw(base_size = 18)


## ----echo=F, fig=T------------------------------------------------------------
ppp

