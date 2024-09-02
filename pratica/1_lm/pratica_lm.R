## ----echo=FALSE, results=tex---------------------------------------------
library(tidyverse, quietly = T, warn.conflicts = F)
library(xtable)
dieta <- read.table("../../Dados/K11828 supplements/Datasets/Table 9.1.DAT")
names(dieta) <- c("Dieta", "Colesterol")
dieta$Dieta <- factor(dieta$Dieta,levels = 1:3, labels = c("Onívoros", "Vegetarianos", "Veganos"))

dieta %>% mutate(Subject = rep(1:6,each=3)) %>% spread(Dieta, Colesterol) %>% select( -Subject) 

## ----echo=FALSE, fig = T-------------------------------------------------
ggplot(data = dieta, mapping = aes(x = Dieta, y = Colesterol, fill = Dieta)) + geom_boxplot(show.legend = F) + theme_bw(base_size = 18) + xlab("") + ylab("Colesterol (mmol/L)")

## ----echo = F, results=tex-----------------------------------------------
dieta %>% group_by(Dieta) %>% summarise( n = n(), Media = mean(Colesterol), Variancia = var(Colesterol)) 

## ----echo=T, results=verbatim, eval=FALSE--------------------------------
## aov(Colesterol ~ Dieta, dados)

## ----echo=F, results=tex-------------------------------------------------
xtable(anova(lm(Colesterol ~ Dieta, dados )))

## ----echo=TRUE, results=verbatim-----------------------------------------
pairwise.t.test(x = dieta$Colesterol, 
                g = dieta$Dieta )

## ----echo=FALSE, results=tex---------------------------------------------
dados %>% mutate(Subject = rep(1:6,each=3)) %>% spread(Dieta, Colesterol) %>% select( -Subject) %>% xtable()

## ----echo=FALSE, results=tex---------------------------------------------
dados %>% mutate(Subject = rep(1:6,each=3), Colesterol = rank(Colesterol)) %>% spread(Dieta, Colesterol) %>% select( -Subject) %>% xtable()

## ----echo = T, results=verbatim------------------------------------------
kruskal.test(Colesterol ~ Dieta, data = dados)

## ----echo = T, results=verbatim------------------------------------------
pairwise.wilcox.test(x = dados$Colesterol,
                     g = dados$Dieta)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## # O primeiro valor do vetor levels é a referencia
## # default: ordem alfabetica
## factor(x, levels)
## #
## # Redefinindo a categoria de referencia
## relevel(x, ref)

## ----echo=FALSE, fig=TRUE------------------------------------------------
x <- rexp(100, 1)
y <- 2 + 0.5 * x + rnorm(100, 0, .3)
ggplot(data = data.frame(x=x, y=y), aes(x,y)) + geom_abline(slope = 0.5, intercept = 2) + xlim(range(x)) + ylim(range(y)) + theme_bw(base_size = 18)

## ----echo=F, fig=T-------------------------------------------------------
dmft <- read.table("../../Dados/K11828 supplements/Datasets/Table 9.8.DAT")
names(dmft) <- c("Pais", "Consumo", "DMFT")
dmft2 <- dmft %>% filter(Pais == 2)
p <- ggplot(dmft2, aes(y = DMFT, x = Consumo))
p <- p + geom_point() + xlab("Consumo de açucar (kg per capita/ano)") + theme_bw(base_size = 18)
p

## ----echo=T--------------------------------------------------------------
output <- lm(DMFT ~ Consumo, data = dmft2)
output

## ----echo=F, fig=T-------------------------------------------------------
p + geom_abline(slope = output$coef[2],
                intercept = output$coef[1])

## ----echo=T--------------------------------------------------------------
summary(output)

## ------------------------------------------------------------------------
(COV <- vcov(output))
#
# A variancia da previsao para o valor esperado 
COV[1,1] + 46.98^2*COV[2,2] + 2*46.98*COV[1,2] 
#
# Variancia para o novo valor 
COV[1,1] + 36.98^2*COV[2,2] + 2*36.98*COV[1,2] 

## ------------------------------------------------------------------------

# Usando a funcao predict
previsao <- predict(output, se.fit = T,
                    newdata = data.frame(
                      Consumo = c(46.98, 36.98)
                      )
                    )
previsao$fit
previsao$se.fit^2

## ----echo=F, fig=T-------------------------------------------------------
previsao <- predict(output, se.fit = T,
                    newdata = data.frame(
                      Consumo = 0:64
                      )
                    )
previsao.df <- data.frame(Prev = previsao$fit,
                          sd = previsao$se.fit) %>%
  mutate(LI = Prev - 1.96 * sd,
         LS = Prev + 1.96 * sd,
         ID = 0:64)
  

p +  geom_ribbon(data = previsao.df, aes(x = ID, y = Prev, ymin = LI, ymax = LS), alpha = 0.25 ) + geom_line(data = previsao.df, aes(x = ID, y = Prev))

## ----echo=F, fig=T-------------------------------------------------------
toy <- data.frame(x = seq(-3,3,by=0.1)) %>% 
  mutate(
    y1 = x^2, 
    y2= x^3 + x^2 + 1, 
    y3 = exp(x), 
    y4 = log(x+4)
    )


p1 <- ggplot(toy, aes(x=x,y=y1)) + geom_line() + ylab("y") + 
  theme(  axis.line=element_line(arrow = arrow()),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(), 
          panel.background = element_blank()) 

p2 <- ggplot(toy, aes(x=x,y=y2)) + geom_line() + ylab("y")  + theme(
  axis.line=element_line(arrow = arrow()),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(), 
          panel.background = element_blank()) 

p3 <- ggplot(toy, aes(x=x,y=y3)) + geom_line() + ylab("y")  + theme(
  axis.line=element_line(arrow = arrow()),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(), 
          panel.background = element_blank()) 

p4 <- ggplot(toy, aes(x=x,y=y4)) + geom_line() + ylab("y")  + theme(axis.line=element_line(arrow = arrow()),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(), 
          panel.background = element_blank()) 

source("../../R/multiplot.R")
multiplot(p1,p3,p2,p4, cols = 2)

## ----echo=T--------------------------------------------------------------
output2 <- lm(log(DMFT) ~ Consumo,  data = dmft2)
summary(output2)

## ----echo=F, fig=T-------------------------------------------------------
previsao2 <- predict(output2, se.fit = T,
                    newdata = data.frame(
                      Consumo = 0:64
                      )
                    )
previsao2.df <- data.frame(Prev = exp(previsao2$fit),
                           sd = previsao2$se.fit) %>%
  mutate(LI = Prev*exp(- 1.96 * sd),
         LS = Prev*exp(+ 1.96 * sd),
         ID = 0:64)
  

p +  geom_ribbon(data = previsao2.df, aes(x = ID, y = Prev, ymin = LI, ymax = LS), alpha = 0.25 ) + geom_line(data = previsao2.df, aes(x = ID, y = Prev))

## ----echo=F, fig=T-------------------------------------------------------
dmft <- dmft %>%  mutate(Pais = factor(Pais, levels= 1:2, labels = c("Industrializado", "Em desenvolvimento")))
p <- ggplot(dmft, aes(y = DMFT, x = Consumo, colour = Pais))
p <- p + geom_point() + xlab("Consumo de açucar (kg per capita/ano)") + theme_bw(base_size = 18) + theme(legend.position = c(.25, 0.8)) + labs(colour = "País") + theme(legend.background = element_rect(linetype = 1, size = 0.25, colour = 1))
p

## ------------------------------------------------------------------------
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

## ----echo=F, fig=T-------------------------------------------------------
aux <- coef(modelo1)
p + geom_abline(slope = aux[2], intercept = aux[1])

## ----echo=F, fig=T-------------------------------------------------------
aux <- coef(modelo2)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2], intercept = aux[1]+aux[3], color = "#00BFC4")

## ----echo=F, fig=T-------------------------------------------------------
aux <- coef(modelo3)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2]+aux[3], intercept = aux[1], color = "#00BFC4")

## ----echo=F, fig=T-------------------------------------------------------
aux <- coef(modelo4)
p + geom_abline(slope = aux[2], intercept = aux[1], color = "#F8766D") + geom_abline(slope = aux[2]+aux[4], intercept = aux[1]+aux[3], color = "#00BFC4")

## ----echo = F------------------------------------------------------------
SHHS <- read.table("../../Dados/K11828 supplements/Datasets/Table 9.15.DAT")
names(SHHS) <- c("BMI", "Sex", "Smoking")
SHHS <- SHHS %>% mutate(
  Sex = factor(Sex, 
                levels = 1:2, 
                labels = c("M","F")),
  Smoking = factor(Smoking,
                   levels = 1:3,
                   labels = c("current", "ex", "never"))
  )

## ------------------------------------------------------------------------
(modelo1.shhs <- lm(BMI ~ Sex, data = SHHS))

## ------------------------------------------------------------------------
(modelo2.shhs <- lm(BMI ~ Smoking, data = SHHS))

## ------------------------------------------------------------------------
(modelo3.shhs <- lm(BMI ~ Sex + Smoking, data = SHHS))

## ------------------------------------------------------------------------
(modelo4.shhs <- lm(BMI ~ Sex * Smoking, data = SHHS))

