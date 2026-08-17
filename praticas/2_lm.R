## ----echo=FALSE, results=tex-------------------------------------------------------------------------
library(tidyverse, quietly = T, warn.conflicts = F)

# Os dados estao em https://github.com/lsbastos/eae2/dados/

dados <- read_rds("dieta.rds")
# dados <- read.table("../Dados/K11828 supplements/Datasets/Table 9.1.DAT")
names(dados) <- c("Dieta", "Colesterol")
dados$Dieta <- factor(dados$Dieta,levels = 1:3, labels = c("Onívoros", "Vegetarianos", "Veganos"))

dados %>% mutate(Subject = rep(1:6,each=3)) %>% spread(Dieta, Colesterol) %>% select( -Subject) 


## ----echo=FALSE, fig = T-----------------------------------------------------------------------------
ggplot(data = dados, mapping = aes(x = Dieta, y = Colesterol, fill = Dieta)) + geom_boxplot(show.legend = F) + theme_bw(base_size = 18) + xlab("") + ylab("Colesterol (mmol/L)")


## ----echo = F, results=tex---------------------------------------------------------------------------
dados %>% group_by(Dieta) %>% summarise( n = n(), Media = mean(Colesterol), Variancia = var(Colesterol)) 


## ----echo=T, results=verbatim, eval=FALSE------------------------------------------------------------
## aov(Colesterol ~ Dieta, dados)


## ----echo=F, results=tex-----------------------------------------------------------------------------
(anova(lm(Colesterol ~ Dieta, dados )))


## ----echo=TRUE, results=verbatim---------------------------------------------------------------------
pairwise.t.test(x = dados$Colesterol, 
                g = dados$Dieta)


## ----echo=FALSE, results=tex-------------------------------------------------------------------------
dados %>% mutate(Subject = rep(1:6,each=3)) %>% spread(Dieta, Colesterol) %>% select( -Subject) 


## ----echo=FALSE, results=tex-------------------------------------------------------------------------
dados %>% mutate(Subject = rep(1:6,each=3), Colesterol = rank(Colesterol)) %>% spread(Dieta, Colesterol) %>% select( -Subject) 


## ----echo = T, results=verbatim----------------------------------------------------------------------
kruskal.test(Colesterol ~ Dieta, data = dados)


## ----echo = T, results=verbatim----------------------------------------------------------------------
pairwise.wilcox.test(x = dados$Colesterol,
                     g = dados$Dieta)


## ----echo=TRUE, eval=FALSE---------------------------------------------------------------------------
## # O primeiro valor do vetor levels é a referencia
## # default: ordem alfabetica
## factor(x, levels)
## #
## # Redefinindo a categoria de referencia
## relevel(x, ref)


## ----echo=FALSE, fig=TRUE----------------------------------------------------------------------------
x <- rexp(100, 1)
y <- 2 + 0.5 * x + rnorm(100, 0, .3)
ggplot(data = data.frame(x=x, y=y), aes(x,y)) + 
  geom_point() +
  geom_abline(slope = 0.5, intercept = 2) + 
  xlim(range(x)) + ylim(range(y)) + 
  theme_bw(base_size = 18)


## ----echo=F, fig=T-----------------------------------------------------------------------------------
dmft <- read_rds("dmft.rds")
# dmft <- read.table("../Dados/K11828 supplements/Datasets/Table 9.8.DAT")
names(dmft) <- c("Pais", "Consumo", "DMFT")
dmft2 <- dmft %>% filter(Pais == 2)
p <- ggplot(dmft2, aes(y = DMFT, x = Consumo))
p <- p + geom_point() + xlab("Consumo de açucar (kg per capita/ano)") + theme_bw(base_size = 18)
p


## ----echo=T------------------------------------------------------------------------------------------
output <- lm(DMFT ~ Consumo, data = dmft2)
output


## ----echo=F, fig=T-----------------------------------------------------------------------------------
p + geom_abline(slope = output$coef[2],
                intercept = output$coef[1])


## ----echo=T------------------------------------------------------------------------------------------
summary(output)


## ----------------------------------------------------------------------------------------------------
(COV <- vcov(output))
#
# A variancia da previsao para o valor esperado 
COV[1,1] + 50^2*COV[2,2] + 2*50*COV[1,2] 
#
# Variancia para o cenário de redução de consumo 
COV[1,1] + 25^2*COV[2,2] + 2*25*COV[1,2] 


## ----------------------------------------------------------------------------------------------------

# Usando a funcao predict
previsao <- predict(output, se.fit = T,
                    newdata = data.frame(
                      Consumo = c(50, 25)
                      )
                    )

previsao$se.fit^2


## ----echo=F, fig=T-----------------------------------------------------------------------------------
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
  

p + 
  geom_ribbon(data = previsao.df, aes(x = ID, y = Prev, ymin = LI, ymax = LS), alpha = 0.25 ) +
  geom_line(data = previsao.df, aes(x = ID, y = Prev))



## ----echo=FALSE, results=tex-------------------------------------------------------------------------
dados %>% mutate(Subject = rep(1:6,each=3)) %>% spread(Dieta, Colesterol) %>% select( -Subject) %>% xtable()


## ----echo=FALSE, fig = T-----------------------------------------------------------------------------
ggplot(data = dados, mapping = aes(x = Dieta, y = Colesterol, fill = Dieta)) + geom_boxplot(show.legend = F) + theme_bw(base_size = 18) + xlab("") + ylab("Colesterol (mmol/L)")


## ----echo=T------------------------------------------------------------------------------------------
output_dieta <- lm(Colesterol ~ Dieta, data = dados)
output_dieta


## ----echo=T------------------------------------------------------------------------------------------
summary(output_dieta)

