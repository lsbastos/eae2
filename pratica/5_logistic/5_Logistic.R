### R code from vignette source 'Aula6_Logistic.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Aula6_Logistic.Rnw:90-93
###################################################

suppressPackageStartupMessages(library(tidyverse))


###################################################
### code chunk number 2: Aula6_Logistic.Rnw:157-160
###################################################
load("Aula6_Logistic.RData")
tbl0 <- table(dados2$PARTO, dados2$RACACORMAE )
tbl0


###################################################
### code chunk number 3: Aula6_Logistic.Rnw:172-174
###################################################
ggplot(dados2, aes(x = RACACORMAE, fill = PARTO)) + geom_bar(position = "dodge" ) + 
  theme_bw(base_size = 18) + xlab("Raça/cor da mãe") + ylab("Nascimentos")  + theme(legend.position = c(0.85, 0.8))


###################################################
### code chunk number 4: Aula6_Logistic.Rnw:183-185
###################################################
output <- glm(I(PARTO == "Cesareo") ~ RACACORMAE, data = dados2, family = binomial())
summary(output)


###################################################
### code chunk number 5: Aula6_Logistic.Rnw:189-190
###################################################
exp(cbind(OR = coef(output), confint(output)))[-1,]


###################################################
### code chunk number 6: Aula6_Logistic.Rnw:203-205
###################################################
tbl0 <- table(dados2$PARTO, dados2$ESCMAE )
tbl0


###################################################
### code chunk number 7: Aula6_Logistic.Rnw:215-216
###################################################
ggplot(dados2, aes(x = ESCMAE, fill = PARTO)) + geom_bar(position = "dodge") + theme_bw(base_size = 18) + xlab("Escolaridade da mãe (anos de estudo)") + ylab("Nascimentos") + theme(legend.position = c(0.2, 0.8))


###################################################
### code chunk number 8: Aula6_Logistic.Rnw:225-227
###################################################
outputA <- glm(I(PARTO == "Cesareo") ~ ESCMAE, data = dados2, family = binomial())
summary(outputA)


###################################################
### code chunk number 9: Aula6_Logistic.Rnw:231-234
###################################################
tblA <- exp(cbind(OR = coef(outputA), 
                 confint(outputA))[-1,]) 
tblA


###################################################
### code chunk number 10: Aula6_Logistic.Rnw:248-256
###################################################
tbl1 <- dados2 %>% group_by(PARTO) %>% 
  summarise( n = n(),
             Media = mean(IDADEMAE),
             sd = sd(IDADEMAE),
             LI = Media - 1.96 * sd / sqrt(n),
             LS = Media + 1.96 * sd / sqrt(n)
             )
tbl1


###################################################
### code chunk number 11: Aula6_Logistic.Rnw:268-269
###################################################
ggplot(dados2, aes(x = PARTO, y = IDADEMAE, fill=PARTO)) + geom_boxplot(position = "dodge", show.legend = F) + theme_bw(base_size = 18) + xlab("Tipo de parto") + ylab("Idade da mãe")


###################################################
### code chunk number 12: Aula6_Logistic.Rnw:282-284
###################################################
output <- glm(I(PARTO == "Cesareo") ~ IDADEMAE, data = dados2, family = binomial())
output


###################################################
### code chunk number 13: Aula6_Logistic.Rnw:288-289
###################################################
exp(cbind(OR = coef(output), confint(output)))[-1,]


###################################################
### code chunk number 14: Aula6_Logistic.Rnw:379-384
###################################################
names(dados2)[4] <- "IDGEST"
outputB <- glm(I(PARTO == "Cesareo") ~ ESCMAE +
                RACACORMAE + IDGEST + IDADEMAE,
              data = dados2,
              family = binomial())

summary(outputB)

###################################################
### code chunk number 15: Aula6_Logistic.Rnw:394-397
###################################################
tblB <- cbind(OR = coef(outputB), confint(outputB))[-1,]
tblB <- exp(tblB)
tblB[1:2,]



###################################################
### code chunk number 18: Aula6_Logistic.Rnw:425-428
###################################################
tbl.aux <- table(dados2$RACACORMAE, dados2$ESCMAE)
colnames(tbl.aux) <- c("7 ou -", "8 a 11", "12 ou +")
tbl.aux


###################################################
### code chunk number 19: Aula6_Logistic.Rnw:433-436
###################################################
tbl.aux <- table(dados2$IDGEST, dados2$ESCMAE)[c(2,3,1,4),]
colnames(tbl.aux) <- c("7 ou -", "8 a 11", "12 ou +")
tbl.aux


###################################################
### code chunk number 20: Aula6_Logistic.Rnw:446-447
###################################################
ggplot(dados2, aes(x = ESCMAE, fill = RACACORMAE)) + geom_bar(position = "dodge") + theme_bw(base_size = 18) + theme(legend.position = c(0.2, 0.8)) + labs(fill = "Raça/Cor") + xlab("Escolaridade da mãe")


###################################################
### code chunk number 21: Aula6_Logistic.Rnw:455-458
###################################################
dados2$IDGEST2 <- factor(dados2$IDGEST, 
                         levels = levels(dados2$IDGEST)[c(2,3,1,4)] )
ggplot(dados2, aes(x = ESCMAE, fill = IDGEST2)) + geom_bar(position = "dodge") + theme_bw(base_size = 18)  + theme(legend.position = c(0.2, 0.8)) + labs(fill = "Idade gestacional") + xlab("Escolaridade da mãe")


###################################################
### code chunk number 22: Aula6_Logistic.Rnw:465-466
###################################################
ggplot(dados2, aes(x = ESCMAE, y = IDADEMAE, fill = ESCMAE)) + geom_boxplot(show.legend = F) + theme_bw(base_size = 18)


###################################################
### code chunk number 23: Aula6_Logistic.Rnw:557-571
###################################################
malaria <- tibble(
  Sex = rep(rep(c("Males","Females"), each=2),2),
  Malaria = rep(c("Cases","Controls"),4),
  Ocupacao = rep(c("Outdoor","Indoor"),each=4),
  n = c(53,15,10,3,35,53,52,79)
  ) %>% mutate(
    Ocupacao = factor(Ocupacao, levels = c("Outdoor","Indoor")),
    Sex = factor(Sex, levels = c("Females","Males"))
    )

malaria %>% group_by(Sex, Malaria) %>% 
  summarise(N = sum(n)) %>% spread(Malaria, N) %>% 
  xtable(digits = 0) %>% 
  print( include.rownames=FALSE )


###################################################
### code chunk number 24: Aula6_Logistic.Rnw:585-589
###################################################
output <- glm(formula = I(Malaria == "Cases") ~ Sex, family = binomial(), weights = n, data = malaria)

summary(output)


out2 <- round(exp(cbind(coef(output),  confint(output)))[2,],2)
out2

###################################################
### code chunk number 25: Aula6_Logistic.Rnw:604-608
###################################################
malaria %>%  group_by(Ocupacao, Malaria) %>% 
  summarise(N = sum(n)) %>% spread(Malaria, N) 


###################################################
### code chunk number 26: Aula6_Logistic.Rnw:612-617
###################################################
malaria <- malaria %>% mutate(Ocupacao = relevel(Ocupacao, ref = "Indoor"))
output2 <- glm(formula = I(Malaria == "Cases") ~ Ocupacao, family = binomial(), weights = n, data = malaria)

summary(output2)

out3 <- round(exp(cbind(coef(output2),  confint(output2)))[2,],2)
out3

###################################################
### code chunk number 27: Aula6_Logistic.Rnw:622-628
###################################################
malaria %>%  group_by(Ocupacao, Sex) %>% 
  summarise(N = sum(n)) %>% spread(Ocupacao, N) %>%
  arrange(desc(Sex)) %>%
  select(Sex, Outdoor, Indoor) 


###################################################
### code chunk number 28: Aula6_Logistic.Rnw:632-636
###################################################
output2 <- glm(formula = I(Sex == "Males") ~ Ocupacao, family = binomial(), weights = n, data = malaria)

summary(output2)

out3 <- round(exp(cbind(coef(output2),  confint(output2)))[2,],2)
out3

###################################################
### code chunk number 29: Aula6_Logistic.Rnw:680-690
###################################################
malaria %>% filter(Ocupacao == "Outdoor") %>% 
  spread(Malaria, n) %>% select(-Ocupacao) %>% 
  arrange(desc(Sex)) 

output <- glm(I(Malaria=="Cases") ~ Sex, 
              family = binomial(),
              data = filter(malaria, Ocupacao == "Outdoor"),
              weights = n)
out <- round(exp(cbind(coef(output), confint(output))[2,]),2)
out

###################################################
### code chunk number 30: Aula6_Logistic.Rnw:695-705
###################################################
malaria %>% filter(Ocupacao == "Indoor") %>% 
  spread(Malaria, n) %>% select(-Ocupacao) %>% 
  arrange(desc(Sex)) 


output <- glm(I(Malaria=="Cases") ~ Sex, 
              family = binomial(),
              data = filter(malaria, Ocupacao == "Indoor"),
              weights = n)
out <- round(exp(cbind(coef(output), confint(output))[2,]),2)
out

###################################################
### code chunk number 31: Aula6_Logistic.Rnw:716-721
###################################################
output <- glm(I(Malaria=="Cases") ~ Sex, 
              family = binomial(),
              data = malaria,
              weights = n)
summary(output)


###################################################
### code chunk number 32: Aula6_Logistic.Rnw:724-729
###################################################
output2 <- glm(I(Malaria=="Cases") ~ Sex + Ocupacao, 
              family = binomial(),
              data = malaria,
              weights = n)
summary(output2)


