### R code from vignette source 'Slides/Aula5_bayes/5_bayes.Rnw'

###################################################
### code chunk number 1: 5_bayes.Rnw:211-257
###################################################
# curve(dbeta(x,24,74), lwd=2, xlab=expression(theta), ylab="")
# lines(c(0,1),c(1,1), col=2, lwd=2)
# legend("topright",c("Priori uniforme", "Posteriori"), lwd=2, lty=1, col=2:1)
require(ggplot2)
# x <- seq(0, 1, len = 200)
# 
# p <- qplot(x, geom = "blank")
# 
# stat <- stat_function(fun = dbeta, colour="red", args = list(shape1 = 24, shape2 = 74))
# stat2 <- stat_function(fun = dbeta, colour="blue", args = list(shape1 = 1, shape2 = 1))
# 
# p + stat + stat2
x <- seq(0,1,length=200)

# posterior <- dbeta(x, 2, 2)
prior <- dbeta(x, 1, 1)

# bimodal <- dbeta(x, .75, .75)
# a1 <- dbeta(x, .75, 1)
# a2 <- dbeta(x, 1, .75)

a1 <- dbeta(x, 1, 4)
a2 <- dbeta(x, 2, 4)
a3 <- dbeta(x, 4, 1)

db = data.frame(
  # x=rep(x,5),
  # density=c(prior, posterior, bimodal, a1, a2), 
  # type=rep(c("Beta(1,1)","Beta(2,2)", 
  #            "Beta(3/4,3/4)", "Beta(3/4,1)",
  #            "Beta(1,3/4)"), 
  x=rep(x,4),
  density=c(prior, a1, a2, a3), 
  type=rep(c("Beta(1,1): media = 1/2; Sem moda", 
             "Beta(1,4): media = 1/5; Moda = 0", 
             "Beta(2,4): media = 1/3; Moda: 1/4", 
             "Beta(4,1): media = 4/5; Moda: 1"), 
           each=200))

ggplot(db, aes(x=x, y=density, group=type, colour=type)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.5, .8)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta))) 


###################################################
### code chunk number 2: 5_bayes.Rnw:303-314
###################################################
x <- seq(0,1,length=100)
posterior <- dbeta(x, 21, 81)
prior <- dbeta(x, 1, 1)
db = data.frame(x=rep(x,2),density=c(prior, posterior), type=rep(c("Priori: Beta(1,1)","Posteriori: Beta(21,81)"), each=100))
ggplot(db, aes(x=x, y=density, group=type, colour=type)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.7, .8)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta ~ "|" ~ y ))) 


###################################################
### code chunk number 3: 5_bayes.Rnw:339-351
###################################################
x <- seq(0,1,length=100)
posterior <- dbeta(x, 21, 84)
prior <- dbeta(x, 1, 4)

db = data.frame(x=rep(x,2),density=c(prior, posterior), type=rep(c("Priori: Beta(1,4)","Posteriori: Beta(21,84)"), each=100))
ggplot(db, aes(x=x, y=density, group=type, colour=type)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.7, .8)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta ~ "|" ~ y ))) 


###################################################
### code chunk number 4: 5_bayes.Rnw:362-374
###################################################
x <- seq(0,1,length=100)
posterior <- dbeta(x, 24, 81)
prior <- dbeta(x, 4, 1)

db = data.frame(x=rep(x,2),density=c(prior, posterior), type=rep(c("Priori: Beta(4,1)","Posteriori: Beta(24,81)"), each=100))
ggplot(db, aes(x=x, y=density, group=type, colour=type)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.7, .8)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta ~ "|" ~ y ))) 


###################################################
### code chunk number 5: 5_bayes.Rnw:433-474
###################################################
# curve(dbeta(x,24,74), lwd=2, xlab=expression(theta), ylab="")
# lines(c(0,1),c(1,1), col=2, lwd=2)
# legend("topright",c("Priori uniforme", "Posteriori"), lwd=2, lty=1, col=2:1)
require(ggplot2)
# x <- seq(0, 1, len = 200)
# 
# p <- qplot(x, geom = "blank")
# 
# stat <- stat_function(fun = dbeta, colour="red", args = list(shape1 = 24, shape2 = 74))
# stat2 <- stat_function(fun = dbeta, colour="blue", args = list(shape1 = 1, shape2 = 1))
# 
# p + stat + stat2
x <- seq(0,1,length=200)

p1 <- dbeta(x, 1, 1)
p2 <- dbeta(x, 1, 4)
p3 <- dbeta(x, 2, 4)
p4 <- dbeta(x, 4, 1)

db = data.frame(
  # x=rep(x,5),
  # density=c(prior, posterior, bimodal, a1, a2), 
  # type=rep(c("Beta(1,1)","Beta(2,2)", 
  #            "Beta(3/4,3/4)", "Beta(3/4,1)",
  #            "Beta(1,3/4)"), 
  x=rep(x,4),
  density=c(p1, p2, p3, p4), 
  type=rep(c("Beta(1,1): media = 1/2; Sem moda", 
             "Beta(1,4): media = 1/5; Moda = 0", 
             "Beta(2,4): media = 1/3; Moda: 1/4", 
             "Beta(4,1): media = 4/5; Moda: 1"), 
           each=200),
  pp = "priori")

ggplot(db, aes(x=x, y=density, group=type, colour=type)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.5, .8)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta))) 


###################################################
### code chunk number 6: 5_bayes.Rnw:513-543
###################################################

x <- seq(0.12,0.25,length=300)
n = 2120
Y = 371
pp1 <- dbeta(x, Y+1, n-Y+1)
pp2 <- dbeta(x, Y+1, n-Y+4)
pp3 <- dbeta(x, Y+2, n-Y+4)
pp4 <- dbeta(x, Y+4, n-Y+1)

db.pp = 
  data.frame(
  x=rep(x,4),
  density=c(pp1, pp2, pp3, pp4), 
  type=rep(c("Beta(1,1): media = 1/2; Sem moda", 
             "Beta(1,4): media = 1/5; Moda = 0", 
             "Beta(2,4): media = 1/3; Moda: 1/4", 
             "Beta(4,1): media = 4/5; Moda: 1"), 
           each=300),
  pp = "posteriori")

ggplot(data = db.pp |> dplyr::mutate( type = substr(type,1,9)), 
       aes(x=x, y=density, 
           colour=type)) +
  coord_cartesian(xlim = c(0.14,0.22)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.8, .8)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta ~ "|" ~ y ))) 


###################################################
### code chunk number 7: 5_bayes.Rnw:553-582
###################################################

x <- seq(0,1,length=200)
n = 2120
Y = 371
pp1 <- dbeta(x, Y+1, n-Y+1)
pp2 <- dbeta(x, Y+1, n-Y+4)
pp3 <- dbeta(x, Y+2, n-Y+4)
pp4 <- dbeta(x, Y+4, n-Y+1)

db.pp = 
  data.frame(
  x=rep(x,4),
  density=c(pp1, pp2, pp3, pp4), 
  type=rep(c("Beta(1,1): media = 1/2; Sem moda", 
             "Beta(1,4): media = 1/5; Moda = 0", 
             "Beta(2,4): media = 1/3; Moda: 1/4", 
             "Beta(4,1): media = 4/5; Moda: 1"), 
           each=200),
  pp = "posteriori")

ggplot(data = rbind(db,db.pp) |> dplyr::mutate( type = substr(type,1,9)), 
       aes(x=x, y=density, 
           colour=type, linetype = pp)) + 
  geom_line(lwd=2) + 
  theme_bw(base_size = 16) + 
  theme(legend.title=element_blank(), legend.position = c(0.5, .6)) + 
  labs(
    x = expression(theta), 
    y = expression(p(theta ~ "|" ~ y))) 


###################################################
### code chunk number 8: 5_bayes.Rnw:645-659
###################################################
# require(ggplot2)
x <- seq(-3,3,length=100)
v <- c(0.5,1,2,10)
dens = NULL
for(s in v){
  dens = c(dens, dnorm(x,0,s))
}
db = data.frame(x=rep(x,4),density=dens, Var=rep(v^2, each=100))
ggplot(db, aes(x=x, y=density, group=Var, colour=as.factor(Var))) + 
  geom_line(lwd=2) + 
  theme_bw() +
  theme(legend.position=c(0.8,0.8), 
        axis.title=element_blank()) +
  labs(title = expression(N(0,sigma^2)), color = expression(sigma^2))


###################################################
### code chunk number 9: 5_bayes.Rnw:716-723
###################################################
 library(arm)
 n = 30
 x = runif(n)
 y = rnorm(n, mean = 1 + 2 * x, sd = 0.5)
 bayesglm(y ~ x, family=gaussian, prior.df=Inf, 
          prior.mean = 0, prior.mean.for.intercept = 0,
          prior.scale=10, prior.scale.for.intercept = 10)


###################################################
### code chunk number 10: 5_bayes.Rnw:732-736
###################################################
m1 = bayesglm(y ~ x, family=gaussian, prior.df=1, 
              prior.mean = 0, prior.mean.for.intercept = 0,
              prior.scale=10, prior.scale.for.intercept = 10)
xtable::xtable( cbind(Mean=coef(m1), confint(m1)) )


###################################################
### code chunk number 11: 5_bayes.Rnw:741-743
###################################################
m2 = lm(y ~ x)
xtable::xtable( cbind(Mean=coef(m2), confint(m2)) )


###################################################
### code chunk number 12: 5_bayes.Rnw:753-766
###################################################
prev = predict(m1, newdata=data.frame(x=seq(0,1,by=0.1)), se=T)
prev2 = data.frame(x = seq(0,1,by=0.1), mean=prev$fit, "LI"=prev$fit - 1.96*prev$se.fit, "LS"=prev$fit + 1.96*prev$se.fit)

h <- ggplot(prev2, aes(x=x))
h + 
  geom_ribbon(aes(ymin=LI, ymax=LS), fill="red", alpha=0.2) + 
  geom_line(aes(y=mean), size=2) + 
  geom_point(data = data.frame(x=x, y=y), aes(x=x, y=y)) + 
  theme_bw(base_size = 16) + 
  labs(
    x = "X",
    y = "Y"
  ) 


###################################################
### code chunk number 13: 5_bayes.Rnw:793-807
###################################################
library(tidyverse, quietly = T, warn.conflicts = F)
life_exp_oms <- read.csv("../dados/data_oms.csv")

life_exp_2015 <- life_exp_oms |> 
  filter(
    Indicator == "Life expectancy at birth (years)",
    # Indicator == "Life expectancy at age 60 (years)", 
    Dim1 != "Both sexes",
    Period == 2015
  ) |> 
  # Selecionando e renomeando apenas as variaveis que eu vou usar
  transmute( 
    Ano = Period, Sexo = factor(Dim1), RegiaoOMS = factor(ParentLocation), 
    Pais = Location, ExpVida = FactValueNumeric)


###################################################
### code chunk number 14: 5_bayes.Rnw:818-831
###################################################
library(tidyverse, quietly = T, warn.conflicts = F)

life_exp_2015$RegiaoOMS = relevel( life_exp_2015$RegiaoOMS, ref = "Americas")

life_exp_2015  |> ggplot(aes(x = RegiaoOMS, y = ExpVida, fill = Sexo)) + 
  geom_boxplot() + 
  theme_bw(base_size = 16) +
  labs(
    y = "Expectativa de vida ao nascer (em anos)",
    x = "Região da OMS"
  ) + 
  theme(legend.position = "bottom")



###################################################
### code chunk number 15: 5_bayes.Rnw:839-842
###################################################
output <- bayesglm(formula = ExpVida ~ Sexo + RegiaoOMS, 
                   family = gaussian, data = life_exp_2015)
output


###################################################
### code chunk number 16: 5_bayes.Rnw:851-852
###################################################
coefplot(output)


###################################################
### code chunk number 17: 5_bayes.Rnw:862-884
###################################################
life_exp_2021 <- life_exp_oms |> 
  filter(
    Indicator == "Life expectancy at birth (years)",
    # Indicator == "Life expectancy at age 60 (years)", 
    Dim1 != "Both sexes",
    Period == 2021
  ) |> 
  # Selecionando e renomeando apenas as variaveis que eu vou usar
  transmute( 
    Ano = Period, Sexo = factor(Dim1), RegiaoOMS = factor(ParentLocation), 
    Pais = Location, ExpVida = FactValueNumeric)

life_exp_2021$RegiaoOMS = relevel( life_exp_2021$RegiaoOMS, ref = "Americas")

life_exp_2021  |> ggplot(aes(x = RegiaoOMS, y = ExpVida, fill = Sexo)) + 
  geom_boxplot() + 
  theme_bw(base_size = 16) +
  labs(
    y = "Expectativa de vida ao nascer (em anos)",
    x = "Região da OMS"
  ) + 
  theme(legend.position = "bottom")


###################################################
### code chunk number 18: 5_bayes.Rnw:892-895
###################################################
output2 <- bayesglm(formula = ExpVida ~ Sexo + RegiaoOMS, 
                   family = gaussian, data = life_exp_2021)
output2


###################################################
### code chunk number 19: 5_bayes.Rnw:904-906
###################################################
coefplot(output2, col.pts = "red", xlim = c(-15, 9))
coefplot(output, add =T)


###################################################
### code chunk number 20: 5_bayes.Rnw:912-914
###################################################
xtable::xtable( as_tibble(cbind(Est.2015 = coef(output),
                                Est.2021 = coef(output2)), rownames = " " ))


###################################################
### code chunk number 21: 5_bayes.Rnw:934-947
###################################################
life_exp_BR <- life_exp_oms |> 
  filter(Location == "Brazil", 
         Indicator == "Life expectancy at birth (years)", 
         Dim1 == "Both sexes")


modelo <- bayesglm( FactValueNumeric ~ Period, family = gaussian, 
                    data = life_exp_BR |> filter(Period < 2020))

g1 <- life_exp_BR |> 
  ggplot( aes(x=Period, y=FactValueNumeric)) + 
            geom_point() +
            theme_bw()


###################################################
### code chunk number 22: 5_bayes.Rnw:955-978
###################################################
aaa <- predict(modelo, 
               newdata = data.frame(Period = c(2020,2021)), 
               se.fit = T)

aaa <- data.frame(Period = c(2020,2021),
                  pred = aaa$fit, 
                  pred.se = aaa$se.fit) |> 
  mutate(
    LI = pred - 1.96*pred.se,
    LS = pred + 1.96*pred.se,
  )

# aaa |> 
#   add_column( Obs = rev(life_exp_BR$FactValueNumeric[life_exp_BR$Period >= 2020])) 

g1 + 
  geom_pointrange(aes(y = pred, ymin = LI, ymax = LS), data = aaa ) +
  geom_abline(slope = coef(modelo)[2], intercept = coef(modelo)[1]) + 
  theme_bw( base_size = 14) + 
  labs(
    x = "Ano",
    y = "Expectativa de vida ao nascer"
  )


