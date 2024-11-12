library(tidyverse)
library(read.dbc)

DNRJ2016.full <- read.dbc("caminho/DNRJ2016.dbc", as.is = T)

# Selecionando Rio de Janeiro, e excluindo parto e escolaridade não especificados
DNRJ2016 <- DNRJ2016.full %>%  
  filter( 
    CODMUNNASC == "330455",  
    PARTO != "9", 
    ESCMAE != "9") %>%  
  dplyr::select(
    PARTO, ESCMAE, 
    RACACORMAE, CODESTAB) %>% 
  mutate(
    PARTO = ifelse(PARTO == "2", 
                   "Cesario", "Vaginal")
  )


# Lista dos 76 estabelecimentos (CNES) que tiveram algum parto em 2016 e total de partos 
CNESRio0 <- DNRJ2016.cleaned %>% group_by(CODESTAB) %>% summarise(n = n())
View(CNESRio0)


# O objeto CNESRio0 tem a lista dos CNES segundo o SINASC
# Esse objeto foi exportado, e as colunas AtendeSUS e Natureza do estabelecimento
# foram obtidas manualmente por mim (Leo) em 
# http://cnes.datasus.gov.br/pages/estabelecimentos/consulta.jsp?search=247
# E o arquivo resultante salvo no formato csv
CNESRio <- read_csv("cnesRio.csv", col_types = "ciic")

# A Perinatal de Laranjeiras (CNES = 2814188) é a unica 
# Entidade empresarial da lista que diz atender SUS
# E existem 4 estabelicimentos da administração pública que 
# nao atendem SUS (militares)
# Então criei a variavel SUS abaixo
CNESRio <- CNESRio %>% mutate(
  SUS = ifelse(AtendeSUS == 1 & Natureza == "ADMINISTRAÇÃO PÚBLICA", "SIM", "NAO")
)
head(CNESRio)

# Unindo os dois bancos usando o left_join
DNRJ2016 <- DNRJ2016 %>% 
  left_join(y = CNESRio, 
            by = c("CODESTAB" = "CODESTAB") 
            ) %>% 
  na.exclude()

# Escolaridade, em anos de estudo concluídos: 
#   1 – Nenhuma; 
#   2 – 1 a 3 anos; 
#   3 – 4 a 7 anos; 
#   4 – 8 a 11 anos; 
#   5 – 12 e mais.
ggplot(DNRJ2016 %>% group_by(SUS, ESCMAE) %>%
         summarise(n = n(), Prop = mean(I(PARTO == "Cesario"))), 
       aes(x=SUS, fill=ESCMAE, y = Prop)) + 
  geom_col(position = position_dodge()) + 
  ylab("Proporção de cesarias") + 
  theme_bw(base_size = 18)

# Tipo de raça e cor da mãe:
#   1 – Branca;
#   2 – Preta;
#   3 – Amarela;
#   4 – Parda;
#   5 – Indígena.
ggplot(DNRJ2016 %>% group_by(SUS, RACACORMAE) %>%
         summarise(n = n(), Prop = mean(I(PARTO == "Cesario"))), 
       aes(x=SUS, fill=RACACORMAE, y = Prop)) + 
  geom_col(position = position_dodge()) + 
  ylab("Proporção de cesarias") + 
  theme_bw(base_size = 18)


########################################
# Analise de mediacao
########################################

library(medflex)
# Vamos dicotomizar a variavel escolaridade da mae
# Duas formas distintas, escolaridade alta e sem escolaridade

# Redefinindo fatores e variaveis de base
DNRJ2016 <- DNRJ2016 %>% mutate(
  ESCMAE = factor(ESCMAE, levels = c("4","1", "2", "3", "5")),
  RACACORMAE = factor(RACACORMAE),
  SUS = factor(SUS, levels = c("NAO", "SIM")),
  ESCMAEALTA = ifelse(ESCMAE == "5", 1, 0),
  ESCMAEALTA = factor(ESCMAEALTA),
  ESCMAENENHUMA = ifelse(ESCMAE == "1", 1, 0),
  ESCMAENENHUMA = factor(ESCMAENENHUMA)
)

# Escolaridade alta  

modelo.semmed <- glm(I(PARTO == "Cesario") ~ ESCMAEALTA + RACACORMAE,
                     family = binomial(),
                     data = DNRJ2016)
summary(modelo.semmed)

modelo.completo <- glm(I(PARTO == "Cesario") ~ ESCMAEALTA + SUS + RACACORMAE,
                     family = binomial(),
                     data = DNRJ2016)
summary(modelo.completo)


expDados <- neImpute(object = modelo.completo)

head(expDados)

output <- neModel(formula = PARTO ~ ESCMAEALTA0 + ESCMAEALTA1 + RACACORMAE, 
                  family = binomial("logit"), 
                  expData = expDados,
                  se = "robust")


summary(neEffdecomp(output))

plot(neEffdecomp(output))

# Repita incluindo a interação entre Escolaridade alta e SUS
# Repita para sem escolaridade
