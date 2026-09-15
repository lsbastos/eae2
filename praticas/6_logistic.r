
## ----echo=FALSE, results=hide--------------------------------------------------
birth <- read.table("birthweight.data", header=T)
birth$smoke = factor(birth$smoke, labels=c("nao","sim"))
birth$ht = factor(birth$ht, labels=c("nao","sim"))
birth$ui = factor(birth$ui, labels=c("nao","sim"))
birth$lwt = round(birth$lwt * 0.45359237) # Peso em Kilos




## ----echo=F,results=tex--------------------------------------------------------
library(arm)
library(tidyverse)
aux <- tibble(Prob = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99)) %>% mutate( 
  Odds = Prob / (1-Prob),
  'log(Odds)' = logit(Prob))
(aux)


## ----echo=F, fig=T,  width=6, height=4-----------------------------------------
teste <- tibble(X = seq(-3, 3, by = 0.1))  |> 
  mutate(
    Probp1p2 = invlogit(1 + 2*X),
    Probm1p2 = invlogit(-1 + 2*X),
    Probp1m2 = invlogit(1 - 2*X),
    # Probm1m2 = invlogit(-1 - 2*X)
    )  |>  gather(Beta, Prob, -X)
  
aux.eq <- c(
  "+1+2x",
  "-1 + 2x",
  "+1 - 2x"#,  "-1 - 2x"
  )

ggplot(teste, aes(x = X, y = Prob, col = Beta)) + geom_line(show.legend = T ) + theme_bw(base_size = 18) + 
  scale_color_discrete(name = expression(logit(theta)), 
                     breaks = unique(teste$Beta),
                     labels = aux.eq
                     ) #+ theme(legend.position="bottom")
  


## ------------------------------------------------------------------------------
saida <- glm(low ~ smoke, 
             family=binomial(link = 'logit'), 
             data=birth)


## ----echo=F--------------------------------------------------------------------
saida



## ----echo=F--------------------------------------------------------------------
summary(saida)


## ------------------------------------------------------------------------------
cbind(saida$coef, confint(saida) )


## ------------------------------------------------------------------------------
exp(cbind(OR=saida$coef, confint(saida) ))[-1,]


## ------------------------------------------------------------------------------
birth.pred <- data.frame(smoke = c(1,0))
prev <- predict(saida, type = 'response',
                newdata = birth.pred)
prev


## ------------------------------------------------------------------------------
prev2 <- predict(saida, type = 'link',
                newdata = birth.pred,
                se.fit = T)


## ------------------------------------------------------------------------------
birth.pred <- birth.pred %>% 
  bind_cols(
    Prob = prev, 
    Link = prev2$fit, 
    Link.sd = prev2$se.fit
  )


## ----echo=F, results=tex-------------------------------------------------------
(birth.pred)


## ----echo=F, results=tex-------------------------------------------------------
aux <- birth.pred  |> 
  mutate(
    LI = Link - 1.96 * Link.sd,
    LS = Link + 1.96 * Link.sd
  ) |>  dplyr::select(-Prob)
(aux)


## ----echo=F, results=tex-------------------------------------------------------
inv.fun <- saida$family$linkinv
aux <- aux %>% mutate(
  Prob = inv.fun( Link ),
  LI = inv.fun(LI),
  LS = inv.fun(LS)
) %>% dplyr::select(smoke, Prob, LI, LS)
aux


## ------------------------------------------------------------------------------
data("Titanic")
Titanic2 <- Titanic |> as_tibble() |> 
  pivot_wider(names_from = Survived, values_from = n)

head(Titanic2, n = 5)


## ------------------------------------------------------------------------------
Titanic2 <- Titanic2 |> 
  mutate(
      Sex = factor(Sex) |>  relevel(ref = "Male"),
      Age = factor(Age) |> relevel(ref = "Adult"),
      Class = factor(Class) |> relevel(ref = "Crew")
  )


## ----fig=TRUE, echo=FALSE------------------------------------------------------
gather(Titanic2,Survived,n,-Sex,-Age,-Class) %>%
ggplot(aes(x=Survived, y = n, color = Sex, fill=Sex)) + geom_col(position = position_dodge()) + theme_bw(base_size = 18)


## ----echo=FALSE, results=tex---------------------------------------------------
Titanic2 |> group_by(Sex) |> summarise(`Surv:Yes` = sum(Yes), `Surv:No` = sum(No),) |> xtable(digits = 0)


## ----fig=TRUE, echo=FALSE------------------------------------------------------
gather(Titanic2,Survived,n,-Sex,-Age,-Class) %>%
ggplot(aes(x=Survived, y = n, color = Age, fill=Age)) + geom_bar(position = position_dodge(), stat = "identity") + theme_bw(base_size = 18)


## ----echo=FALSE, results=tex---------------------------------------------------
Titanic2 |> group_by(Age) |> summarise(`Surv:Yes` = sum(Yes), `Surv:No` = sum(No),) |> xtable(digits = 0)


## ------------------------------------------------------------------------------
head(Titanic2)


## ----echo=TRUE-----------------------------------------------------------------
m0Sex <- glm(cbind(Yes, No) ~ Sex, 
             data = Titanic2, 
             family=binomial())


m0Age <- glm(cbind(Yes, No) ~ Age, 
             data = Titanic2, 
             family=binomial())


## ----echo=FALSE, results=tex---------------------------------------------------
summary(m0Sex)


## ----echo=FALSE,results=tex----------------------------------------------------
summary(m0Age)


## ----echo=FALSE,results=tex----------------------------------------------------
m0SexpAge <- glm(cbind(Yes, No)~ Sex + Age, Titanic2, family=binomial())
summary(m0SexpAge)


## ----fig=TRUE, echo=FALSE------------------------------------------------------
Titanic2 |>   
  group_by(Sex,Age) |>  
  summarise(Yes = sum(Yes), No = sum(No))  |> 
  mutate(Prop = Yes / (Yes + No)) |> 
  ggplot(aes(x=Sex, y = Prop, color = Age, fill=Age)) + 
  geom_col(position = position_dodge()) + 
  theme_bw(base_size = 18) + 
  ylab("Observed survival proportion") + 
  theme(legend.position = c(.15,.8))
  


## ----echo=FALSE,results=tex----------------------------------------------------
summary(glm(cbind(Yes, No)~ Age, filter(Titanic2, Sex == "Male"), family=binomial()))


## ----echo=FALSE,results=tex----------------------------------------------------
summary(glm(cbind(Yes, No)~ Age, filter(Titanic2, Sex == "Female"), family=binomial()))


## ----echo=FALSE,results=tex----------------------------------------------------
Titanic2 <- Titanic2 |> 
  mutate( 
    AgeSex = paste(Age,Sex,sep=":") |> 
      factor() |> relevel(ref = "Adult:Male") 
  )  |> filter( !(Class == "Crew" & Age == "Child") )

output <- glm(cbind(Yes, No)~ AgeSex, Titanic2, family=binomial())
summary(output)


## ----echo=F,results=tex--------------------------------------------------------
aux <- predict(output, newdata = data.frame(AgeSex = output$xlevels$AgeSex), se.fit = T)

mat <- data.frame(
  Levels = output$xlevels$AgeSex,
  OR = c(1, exp(output$coefficients[-1])),
  SurvProb=output$family$linkinv(aux$fit), 
  LI=output$family$linkinv(aux$fit-1.96*aux$se.fit),
  LS=output$family$linkinv(aux$fit+1.96*aux$se.fit)
)
rownames(mat) <- NULL

mat


## ----echo=F, results=tex-------------------------------------------------------
outputSex <- glm(cbind(Yes, No)~ Sex, Titanic2, family=binomial())

outputAge <- glm(cbind(Yes, No)~ Age, Titanic2, family=binomial())

outputSexpAge <- glm(cbind(Yes, No)~ Sex + Age, Titanic2, family=binomial())
XX <- c("Age", "Sex", "Age + Sex", "AgeSex")
mat <- data.frame(
  Modelos = paste("1 +", XX),
  AIC = c(AIC(outputAge), AIC(outputSex), AIC(outputSexpAge), AIC(output))
)
(mat)


## ----fig=TRUE, echo=FALSE------------------------------------------------------
gather(Titanic2, Survived, n, -Class, -Sex, - Age, -AgeSex) %>%
ggplot(aes(x=Survived, y = n, color = Class, fill=Class)) + geom_bar(position = position_dodge(), stat = "identity") + theme_bw(base_size = 18) + 
    theme(legend.position = c(.75, .8))


## ----echo=FALSE,results=tex----------------------------------------------------
output2 <- glm(cbind(Yes, No)~ AgeSex + Class, Titanic2, family=binomial())

summary(output2)


## ----echo=FALSE,results=tex----------------------------------------------------
or.int = confint(output2)
(
  data.frame(
    OR = exp(coef(output2)[-(1:4)]), 
    LI = exp(or.int[5:7,1]),
    LS = exp(or.int[5:7,2])
  )
)


## ----echo=FALSE,results=tex----------------------------------------------------
mat2 <- data.frame(OR.crude = exp(coef(output)[-1]),
                   OR.adj = exp(coef(output2)[2:4]))
(mat2)


## ----echo=FALSE,fig=TRUE, width=8----------------------------------------------
Titanic2 <- Titanic2 %>% bind_cols(Probab = output2$fitted.values)

pos1 <- position_dodge(.9)
ggplot(Titanic2  |>  mutate(n = paste0(Yes,"/",Yes + No)), aes(x=AgeSex, y=Probab, fill=Class)) + 
  geom_col(position = pos1) + 
  theme_bw(base_size = 18) + 
  labs(
    x = "Age:Sex",
    y = "Fitted survival probability"
  ) + 
  ylim(0,1) +
  geom_text(aes(label = n), position = pos1, vjust = -.5) + 
  theme(legend.position = c(.1, .8))

