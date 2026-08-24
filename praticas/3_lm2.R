## ----echo=FALSE---------------------------------------------------------------
# Lendo os bancos utilizados e carregando tidyverse
library(tidyverse, quietly = T, warn.conflicts = F)
SHHS <- read_rds("../dados/shhs.rds")
racao <- read_rds("../dados/racao.rds")
dmft <- read_rds("../dados/dmft.rds")
# dieta <- read_rds("../dados/dieta.rds")


## -----------------------------------------------------------------------------
summary(racao)


## ----echo=FALSE,fig=T---------------------------------------------------------
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
p + geom_boxplot() + theme_bw(base_size = 18)  + theme(legend.position = c(.8, 0.8))


## -----------------------------------------------------------------------------
summary(aov(Peso ~ Racao + Linhagem, data = racao))


## ----echo=T-------------------------------------------------------------------
modelo <- lm(Peso ~ Racao + Linhagem, data = racao)
summary(modelo)



## ----echo=T-------------------------------------------------------------------
modelo.coef <- cbind(Est = coef(modelo), confint(modelo)) 


## ----echo=F,results=tex-------------------------------------------------------
modelo.coef


## ----echo=FALSE,fig=T---------------------------------------------------------
modelo.coef  |> 
  as_tibble(rownames = "term", ) |> 
  filter(term != "(Intercept)") |> 
  # reorder the coefficients so that the largest is at the top of the plot
  mutate(term = fct_reorder(term, Est)) %>%
  ggplot(aes(Est, term)) +
  geom_point() +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimativa do efeito da variável no peso esperado (em gramas)",
    y = NULL,
    title = "Coefficient plot"
  ) + theme_bw()


## ----fig=TRUE-----------------------------------------------------------------
hist(rstandard(modelo), col = 2)


## ----fig=TRUE-----------------------------------------------------------------
qqnorm(rstandard(modelo) )
qqline(rstandard(modelo) )


## -----------------------------------------------------------------------------
shapiro.test(rstandard(modelo))


## ----fig=TRUE-----------------------------------------------------------------
plot(rstandard(modelo))


## ----echo=T-------------------------------------------------------------------
modeloR <- lm(Peso ~ Racao, data = racao)
modeloL <- lm(Peso ~ Linhagem, data = racao)
modeloRL <- lm(Peso ~ Racao + Linhagem, data = racao)
modeloRL2 <- lm(Peso ~ Racao * Linhagem, data = racao)


## -----------------------------------------------------------------------------
summary(modeloRL2)


## -----------------------------------------------------------------------------
#anova(modeloRL2)
anova(lm(Peso ~ Racao*Linhagem, data = racao))


## -----------------------------------------------------------------------------
anova(lm(Peso ~ Linhagem*Racao, data = racao))


## -----------------------------------------------------------------------------
anova(modeloR, modeloRL2)


## -----------------------------------------------------------------------------
AIC(modelo)
BIC(modelo)


## ----echo=F,results=tex-------------------------------------------------------

  cbind(AIC(modeloR, modeloL, modeloRL, modeloRL2),
        BIC(modeloR, modeloL, modeloRL, modeloRL2),
        R2 = 100*c(summary(modeloR)$r.squared, summary(modeloL)$r.squared, summary(modeloRL)$r.squared, summary(modeloRL2)$r.squared),
        R2.adj = 100*c(summary(modeloR)$adj.r.squared, summary(modeloL)$adj.r.squared, summary(modeloRL)$adj.r.squared, summary(modeloRL2)$adj.r.squared)
        )[,-3]


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

mm <- cbind(AIC(modelo1.shhs, modelo2.shhs, modelo3.shhs, modelo4.shhs),
            BIC(modelo1.shhs, modelo2.shhs, modelo3.shhs, modelo4.shhs),
            R2 = 100*c(summary(modelo1.shhs)$r.squared, 
                       summary(modelo2.shhs)$r.squared, 
                       summary(modelo3.shhs)$r.squared, 
                       summary(modelo4.shhs)$r.squared),
            R2.adj = 100*c(summary(modelo1.shhs)$adj.r.squared, 
                           summary(modelo2.shhs)$adj.r.squared, 
                           summary(modelo3.shhs)$adj.r.squared, 
                           summary(modelo4.shhs)$adj.r.squared)
)[,-3]
rownames(mm) = paste("BMI ~", c("Sex", "Smoke", "Sex + Smoke", "Sex * Smoke")) 
mm

## ----echo=T-------------------------------------------------------------------
summary(modelo3.shhs)


## ----echo=T, fig=T------------------------------------------------------------
hist(rstandard(modelo3.shhs))


## ----echo=T, fig=T------------------------------------------------------------
plot(rstandard(modelo3.shhs))


## ----echo=F, results=tex------------------------------------------------------
summary(modelo3.shhs) |> xtable::xtable()


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
#arm::coefplot(modelo3.shhs)
tibble(term = modelo3.shhs$coefficients |> names(), 
       Est = coef(modelo3.shhs))  |> 
  bind_cols(confint(modelo3.shhs)) |> 
  filter(term != "(Intercept)") |> 
  ggplot(aes(Est, term)) +
  geom_point() +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Estimativa do efeito da variável no IMC",
    y = NULL,
    title = "Coefficient plot"
  ) + theme_light()



## ----echo=F, fig=T------------------------------------------------------------
dmft <- dmft |>   
  mutate(Pais = factor(Pais, levels= 1:2, labels = c("Industrializado", "Em desenvolvimento")))
p <- ggplot(dmft, aes(y = DMFT, x = Consumo, colour = Pais, fill = Pais))
p <- p + 
  geom_point() + 
  xlab("Consumo de açucar (kg per capita/ano)") + 
  theme_bw(base_size = 18) + 
  theme(legend.position = c(.25, 0.8)) + 
  labs(colour = "País", fill = "País") + 
  theme(legend.background = element_rect(linetype = 1, colour = 1))
p


## -----------------------------------------------------------------------------
# Modelo ignorando o tipo de país
modeloC <- lm(DMFT ~ Consumo, data = dmft)
#
# Modelo variando intercepto
modeloCP <- lm(DMFT ~ Consumo + Pais, data = dmft)
#
# Modelo variando slope
modeloC.int <- lm(DMFT ~ Consumo + Consumo:Pais, 
              data = dmft)
#
# Modelo variando intercepto e slope
modeloCP.int <- lm(DMFT ~ Consumo + Pais + Consumo:Pais,
                   data = dmft)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modeloC)
p + geom_abline(slope = aux[2], intercept = aux[1])


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modeloC)


## ----echo=T-------------------------------------------------------------------
AIC(modeloC)
BIC(modeloC)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modeloCP)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2], intercept = aux[1]+aux[3], color = "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modeloCP)


## ----echo=T-------------------------------------------------------------------
AIC(modeloCP)
BIC(modeloCP)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modeloC.int)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2]+aux[3], intercept = aux[1], color = "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
xtable::xtable(modeloC.int)


## ----echo=T-------------------------------------------------------------------
AIC(modeloC.int)
BIC(modeloC.int)


## ----echo=F, fig=T------------------------------------------------------------
aux <- coef(modeloCP.int)
p + 
  geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") +
  geom_abline(slope = aux[2]+aux[4], intercept = aux[1]+aux[3], color = "#00BFC4")


## ----echo=F, results=tex------------------------------------------------------
modeloCP.int


## ----echo=T-------------------------------------------------------------------
AIC(modeloCP.int)
BIC(modeloCP.int)


## ----echo=F, results=tex------------------------------------------------------

tibble(
  Modelo = c("Consumo", "Consumo + Pais", "Consumo + Consumo:Pais","Consumo*Pais"),
  AIC = AIC(modeloC, modeloCP, modeloC.int, modeloCP.int)$AIC,
  BIC = BIC(modeloC, modeloCP, modeloC.int, modeloCP.int)$BIC,
  R2 = 100*c(summary(modeloC)$r.squared, summary(modeloCP)$r.squared, 
         summary(modeloC.int)$r.squared, summary(modeloCP.int)$r.squared),
  R2.adj = 100*c(summary(modeloC)$adj.r.squared, 
             summary(modeloCP)$adj.r.squared, 
             summary(modeloC.int)$adj.r.squared, 
             summary(modeloCP.int)$adj.r.squared)
)

## ----echo=F, fig=T------------------------------------------------------------
hist(rstandard(modeloCP.int))


## ----echo=F, fig=T------------------------------------------------------------
qqnorm(rstandard(modeloCP.int))
qqline(rstandard(modeloCP.int))


## ----echo=F, fig=T------------------------------------------------------------
pp <- ggplot( dmft %>% 
                bind_cols(
                  Resid = rstandard(modeloCP.int), 
                  ID = 1:nrow(dmft)), 
              aes(x = ID, y = Resid, color = Pais))
ppp <- pp + geom_point(show.legend = F) + theme_bw(base_size = 18)
ppp + 
  geom_hline(yintercept = c(0,2,-2), 
             linetype = c("solid","dashed","dashed"))


## ----echo=T-------------------------------------------------------------------
output2 <- lm(log(DMFT) ~ Consumo*Pais,  data = dmft)
summary(output2)


## ----echo=F, fig=T------------------------------------------------------------
limits.Ind <- dmft$Consumo[dmft$Pais == "Industrializado"] |> range()
limits.Dev <- dmft$Consumo[dmft$Pais == "Em desenvolvimento"] |> range()

pred <- data.frame(Consumo = seq.int(limits.Ind[1], limits.Ind[2]),
                       Pais = "Industrializado") |> 
  bind_rows(data.frame(Consumo = seq.int(limits.Dev[1], limits.Dev[2]),
                       Pais = "Em desenvolvimento"))
                     
predictions <- predict(output2, se.fit = T,
                     newdata = pred)

pred <- pred |> 
  bind_cols(pred = exp(predictions$fit), se = predictions$se.fit) |> 
  mutate(LI = pred*exp(- 1.96 * se),
         LS = pred*exp(+ 1.96 * se))


p +  
  # geom_ribbon(data = pred,
              # aes(y = pred, ymin = LI, ymax = LS), alpha = 0.25, show.legend = F) +
  geom_line(data = pred, aes(y = pred))


## ----echo=F, fig=T------------------------------------------------------------
p +  
  geom_ribbon(data = pred,
              aes(y = pred, ymin = LI, ymax = LS), alpha = 0.25, 
              show.legend = F) +
  geom_line(data = pred, aes(y = pred))


## ----echo=F, fig=T------------------------------------------------------------
hist(rstandard(output2))


## ----echo=F, fig=T------------------------------------------------------------
qqnorm(rstandard(output2))
qqline(rstandard(output2))


## ----echo=F, fig=T------------------------------------------------------------
pp2 <- ggplot( dmft %>% bind_cols(Resid = rstandard(output2), ID = 1:nrow(dmft)), aes(x = ID, y = Resid, color = Pais))
pp2 + geom_point(show.legend = F) + geom_hline(yintercept = c(0,2,-2), linetype = c("solid","dashed","dashed")) + theme_light(base_size = 18)


## ----echo = F, fig=T----------------------------------------------------------
cowplot::plot_grid(nrow = 1,
ppp + geom_hline(yintercept = c(0,2,-2), linetype = c("solid","dashed","dashed")) + theme_light() + labs(title = "Antes"),
pp2 + geom_point(show.legend = F) + geom_hline(yintercept = c(0,2,-2), linetype = c("solid","dashed","dashed")) + theme_light() + labs(title = "Depois")
)

