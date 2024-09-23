### R code from vignette source 'Aula5_Logistic.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Aula5_Logistic.Rnw:83-86
###################################################

suppressPackageStartupMessages(library(tidyverse))


###################################################
### code chunk number 2: Aula5_Logistic.Rnw:143-144
###################################################
data(Titanic)


###################################################
### code chunk number 3: Aula5_Logistic.Rnw:148-152
###################################################
Titanic2 <- as_tibble(Titanic) %>% 
  spread(Survived, n)

head(Titanic2)


###################################################
### code chunk number 4: Aula5_Logistic.Rnw:162-163
###################################################
apply(Titanic,MARGIN = c(2,4), sum)


###################################################
### code chunk number 5: Aula5_Logistic.Rnw:167-168
###################################################
apply(Titanic,MARGIN = c(3,4), sum)


###################################################
### code chunk number 6: Aula5_Logistic.Rnw:177-179
###################################################
gather(Titanic2,Survived,n,-Sex,-Age,-Class) %>%
ggplot(aes(x=Survived, y = n, color = Sex, fill=Sex)) + geom_col(position = position_dodge()) + theme_bw(base_size = 18)


###################################################
### code chunk number 7: Aula5_Logistic.Rnw:184-186
###################################################
gather(Titanic2,Survived,n,-Sex,-Age,-Class) %>%
ggplot(aes(x=Survived, y = n, color = Age, fill=Age)) + geom_bar(position = position_dodge(), stat = "identity") + theme_bw(base_size = 18)


###################################################
### code chunk number 8: Aula5_Logistic.Rnw:223-224
###################################################
head(Titanic2)


###################################################
### code chunk number 9: Aula5_Logistic.Rnw:236-244
###################################################
Titanic2 <- Titanic2 %>% 
  mutate(
    Class = factor(Class,levels = unique(Class)[c(3,2,1,4)]),
    Sex = factor(Sex, levels = unique(Sex)[c(2,1)]),
    Age = factor(Age, levels = unique(Age))
  )

glm(cbind(Yes, No)~ Sex, Titanic2, family=binomial())


###################################################
### code chunk number 10: Aula5_Logistic.Rnw:248-249
###################################################
glm(cbind(Yes, No)~ Age, Titanic2, family=binomial())


###################################################
### code chunk number 11: Aula5_Logistic.Rnw:262-263
###################################################
glm(cbind(Yes, No)~ Sex + Age, Titanic2, family=binomial())


###################################################
### code chunk number 12: Aula5_Logistic.Rnw:271-274
###################################################
Titanic3 <- Titanic2 %>%  group_by(Sex,Age) %>% summarise(Yes = sum(Yes), No = sum(No)) %>% mutate(Prop = Yes / (Yes + No)) 

ggplot(Titanic3, aes(x=Sex, y = Prop, color = Age, fill=Age)) + geom_col(position = position_dodge()) + theme_bw(base_size = 18) + ylab("Probabilidade de Sobrevivencia")


###################################################
### code chunk number 13: Aula5_Logistic.Rnw:291-292
###################################################
(glm(cbind(Yes, No)~ Age, filter(Titanic2, Sex == "Male"), family=binomial()))


###################################################
### code chunk number 14: Aula5_Logistic.Rnw:296-297
###################################################
(glm(cbind(Yes, No)~ Age, filter(Titanic2, Sex == "Female"), family=binomial()))


###################################################
### code chunk number 15: Aula5_Logistic.Rnw:309-316
###################################################
Titanic2 <- mutate(Titanic2, 
                   AgeSex = paste(Age,Sex,sep=":"),
                   AgeSex = factor(AgeSex, 
                                   levels = unique(AgeSex)[c(3,4,2,1)])
) %>% filter( !(Class == "Crew" & Age == "Child") )
output <- glm(cbind(Yes, No)~ AgeSex, Titanic2, family=binomial())
xtable(output)


###################################################
### code chunk number 16: Aula5_Logistic.Rnw:329-340
###################################################
aux <- predict(output, newdata = data.frame(AgeSex = output$xlevels$AgeSex), se.fit = T)


mat <- data.frame(
  Levels = output$xlevels$AgeSex,
  OR = c(1, exp(output$coefficients[-1])),
  SurvProb=output$family$linkinv(aux$fit), 
  LI=output$family$linkinv(aux$fit-1.96*aux$se.fit),
  LS=output$family$linkinv(aux$fit+1.96*aux$se.fit)
)

rownames(mat) <- NULL




###################################################
### code chunk number 17: Aula5_Logistic.Rnw:349-360
###################################################
outputSex <- glm(cbind(Yes, No)~ Sex, Titanic2, family=binomial())

outputAge <- glm(cbind(Yes, No)~ Age, Titanic2, family=binomial())

outputSexpAge <- glm(cbind(Yes, No)~ Sex + Age, Titanic2, family=binomial())

XX <- c("Age", "Sex", "Age + Sex", "AgeSex")
mat <- data.frame(
    Modelos = paste("1 +", XX),
    AIC = c(AIC(outputAge), AIC(outputSex), AIC(outputSexpAge), AIC(output))
)

(mat)


###################################################
### code chunk number 18: Aula5_Logistic.Rnw:378-380
###################################################
gather(Titanic2, Survived, n, -Class, -Sex, - Age, -AgeSex) %>%
ggplot(aes(x=Survived, y = n, color = Class, fill=Class)) + geom_bar(position = position_dodge(), stat = "identity") + theme_bw(base_size = 18)


###################################################
### code chunk number 19: Aula5_Logistic.Rnw:388-393
###################################################
output2 <- glm(cbind(Yes, No)~ AgeSex + Class, Titanic2, family=binomial())

output3 <- glm(cbind(Yes, No)~ AgeSex + AgeSex:Class, Titanic2, family=binomial())

(output2)


###################################################
### code chunk number 20: Aula5_Logistic.Rnw:403-404
###################################################
(data.frame(OR = exp(coef(output2)[-(1:4)])))


###################################################
### code chunk number 21: Aula5_Logistic.Rnw:409-412
###################################################
mat2 <- data.frame(OR.crude = exp(coef(output)[-1]),
                   OR.adj = exp(coef(output2)[2:4]))
(mat2)


###################################################
### code chunk number 22: Aula5_Logistic.Rnw:427-428
###################################################
(output3)


###################################################
### code chunk number 23: Aula5_Logistic.Rnw:436-440
###################################################
Titanic2 <- Titanic2 %>% bind_cols(Probab = output3$fitted.values)

pos1 <- position_dodge(.9)
ggplot(Titanic2 %>% mutate(n = Yes + No), aes(x=AgeSex, y=Probab, fill=Class)) + geom_col(position = pos1) + theme_bw(base_size = 18) + xlab("Age:Sex") + ylab("Fitted survival probability") + geom_text(aes(label = n), position = pos1, vjust = -.5)


