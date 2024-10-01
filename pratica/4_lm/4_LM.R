## ----echo=TRUE, results=hide--------------------------------------------------
n = 200
p = 3
X1 <- runif(n)
X2 <- rbinom(n, 1, 0.5)
dados <- data.frame(x1 = X1,
                    x2 = X2,
                    y = 1 - 2*X1 + 4*X2 + rnorm(n,0,.5)
                    )
dados$y[200] = 5

# Modelo com a funcao lm
lm.ex = lm(y ~ x1 + x2, data = dados)

# Modelo com a funcao glm
glm.ex = glm(y ~ x1 + x2, family=gaussian(), data = dados)


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
library(tidyverse)
library(xtable)
ggplot(dados, aes(x=x1,y=y, color = factor(x2))) + 
  geom_point(show.legend = F) + 
  theme_bw(base_size = 18)


## ----echo=FALSE---------------------------------------------------------------
summary(lm.ex)


## ----echo=FALSE---------------------------------------------------------------
summary(glm.ex)


## ----echo=T-------------------------------------------------------------------
anova(lm.ex)


## ----echo=T-------------------------------------------------------------------
anova(glm.ex, test="F")


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
res <- rstandard(lm.ex)
dados2 <- data.frame(res=res, Ordem = 1:n, Fit = lm.ex$fitted.values)
ggplot(data = dados2, aes(x=Ordem, y = res)) + 
  geom_point() + theme_bw(base_size = 18) + xlab("Ordem dos dados") + 
  geom_hline(yintercept = c(-2,2,0), linetype = c(2,2,1)) +
  ylab("Residuos padronizados")


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
ggplot(data = dados2, aes(x=Fit, y = res)) + 
  geom_point() + theme_bw(base_size = 18) + ylab("Resíduos padronizados") + 
  geom_hline(yintercept = c(-2,2,0), linetype = c(2,2,1)) +
  xlab("Fitted values")


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
hist(dados$y, col = "red")


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
aaa <- shapiro.test(dados$y)
qqnorm(dados$y)
qqline(dados$y)
text(x = 1, y = min(dados$y), label = paste(aaa$method, ": p-value=", round(aaa$p.value,3)))


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
bbb <- shapiro.test(dados2$res)
qqnorm(dados2$res)
qqline(dados2$res)
text(x = 1, y = min(dados2$res), label = paste(bbb$method, ": p-value=", round(bbb$p.value,3)))


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
hist(dados2$res, col = "red")


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
ggplot(dados2 %>% bind_cols(Z = rnorm(n)), aes(y = res, x = Z)) + 
  geom_point() + theme_bw(base_size = 18) + ylab("Resíduos padronizados") + 
  geom_hline(yintercept = c(-2,2,0), linetype = c(2,2,1)) +
  xlab("Nova variável Z")


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
dados2 <- dados2 %>% mutate(hii = hatvalues(lm.ex), 
                            hipn = hii*nrow(dados2)/2,
                            dffits = dffits(lm.ex),
                            Dcook = cooks.distance(lm.ex))
ggplot( dados2, aes(x=Ordem, y=hipn)) + geom_point() + 
  theme_bw(base_size = 18) + ylab(expression(h[ii]/(p/n))) + 
  geom_hline(yintercept = c(2,3), linetype = 1:2) +
  xlab("Data index") 


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
ggplot( dados2, aes(x=Ordem, y=dffits)) + geom_point() + 
  theme_bw(base_size = 18) + ylab("DFFITS") + 
  geom_hline(yintercept = 2*sqrt(p/n), linetype = 2) +
  xlab("Data index") 


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
# signif(cooks.distance(lm.ex), 3)
ggplot( dados2, aes(x=Ordem, y=Dcook)) + geom_point() + 
  theme_bw(base_size = 18) + ylab("Cook's distance") + 
  geom_hline(yintercept = qf(c(0.5, 0.75),df1 = p, df2 = n-p), linetype = 2:3) +
  xlab("Data index") 


## ----echo=T-------------------------------------------------------------------
summary(influence.measures(lm.ex))


## ----echo=FALSE, fig=TRUE-----------------------------------------------------
teste <- influence.measures(lm.ex)
index <- which(apply(teste$is.inf, 1, FUN = function(x)any(x) ))

ggplot(dados, aes(x=x1,y=y, color = factor(x2))) + 
  geom_point(show.legend = F) + 
  geom_text(data = dados[index,] %>% rownames_to_column(), 
            mapping = aes(label = rowname), color = "black", 
            check_overlap = T) +
  theme_bw(base_size = 18)

