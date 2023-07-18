#Questão 2A Q46
#importando bibliotecas -----------------------------------------------

library(writexl)
library(pROC)
library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(glmnet)
library(margins)
library(lmtest)
library(ResourceSelection)
library(car)
library(lfe)
#AUmentando o tamanho do View -----------------------------------------------
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
#Manipulação dos data frames base -----------------------------------------------
df <- WVS_Cross.National_Wave_7_csv_v5_0
df9822 <- WVS_TimeSeries_4_0
df2 <- data.frame(df$B_COUNTRY,df$Q260, df$Q261, df$Q262, df$G_TOWNSIZE, df$hdi, df$H_URBRURAL,df$Q4, df$Q98, df$I_WOMPOL, df$Q29, df$Q46, df$Q119, df$Q233, df$S007, df$S018, df$Q275)
colnames(df2) <- c("codPais","Sex","Year of Birth","Age","tamCidade","IDH", "Urban/Rural","InLPolitics","AC/INAC in Political Party", "womPol","Q29", "Q46", "Q119", "Q233","id","S018", "Escolaridade")
df9822_clean <- data.frame(df9822$S007,df9822$S024)
names(df9822_clean)<- c("id","Strata")

#Booleana Q46 -----------------------------------------------

df2$Q46[df2$Q46 < 0] <- NA
typeof(df2$Q46)
df2<-na.omit(df2)


df2$Q46Bool <- ifelse(df2$Q46 <= 2,1,0)
typeof(df2$Q46Bool)
df2$Q46Bool <- as.logical(df2$Q46Bool)

summary(df2$Q46Bool)

df2 %>% count(df2$Q46Bool == 1)
# Transformando COD em Nome -----------------------------------------------
names(states) <- c("codPais", "Name")
df2 <- merge(x = states, y = df2, by="codPais")

df2 <- merge(x=df2, y = df9822_clean, by="id")

# Limpeza básica ----------------------------------------------------------
#Removendo valores não respondidos, valores outros ou desconhecidos
df2 <- na.omit(df2)
df2 <- df2[complete.cases(df2), ]
View(df2)

# Individual --------------------------------------------------------------

#Codificando os questionários para peso

df2$treated_Q29 <- ifelse(df2$Q29 == 1, 0,
                    ifelse(df2$Q29 == 2, 0.25,
                           ifelse(df2$Q29 == 3, 0.75,
                                  ifelse(df2$Q29 == 5, 1, 0))))

action_Mean <- (sum(df2$treated_Q29)/(length(df2$treated_Q29)-1))

df2$treated_Q119 <- ifelse(df2$Q119 == 0, 0,
                           ifelse(df2$Q119 == 1, 0.25,
                                  ifelse(df2$Q119 == 2, 0.5,
                                         ifelse(df2$Q119 == 3, 0.75,
                                                ifelse(df$Q119 == 4, 0.1, 0)))))

df2$treated_Q233 <- ifelse(df2$Q29 == 4, 0,
                          ifelse(df2$Q29 == 3, 0.25,
                                 ifelse(df2$Q29 == 2, 0.75,
                                        ifelse(df2$Q29 == 1, 1, 0))))

df2$treated_Q275 <- ifelse(df2$Escolaridade > 0, df2$Escolaridade * 0.125,
                           ifelse(df2$Escolaridade == 0, 0.125, 0))
summary(df2$treated_Q275)

P <- (mean(df2$treated_Q119) + mean(df2$treated_Q233))/2

#Criando a variável média wom pool
dfwomPOL_country <- aggregate(df2$womPol, by = list(df2$Name), FUN = mean)
colnames(dfwomPOL_country) <- c("Name", "Media_womPol")
df2 <- merge(x = dfwomPOL_country, y = df2, by="Name")

#Calculando I
df2$I <-  df2$treated_Q29 - (df2$treated_Q29) + df2$Sex + P + df2$Media_womPol + (1-(df2$Age/100)) + df2$treated_Q275
hist(df2$I)

#Retirando valores negativos e NA
df2 <- df2[df2$I >= 0, ]
df2 <- na.omit(df2)

View(df2)

# Transformando variáveis -------------------------------------------------
df2 <- df2 %>%
  mutate(Sex = factor(Sex, levels = c(1,2,-2,-4,-5),
                      labels = c("Male","Female","No answer","Not asked","Missing")))

df2 <- df2 %>%
  mutate(tamCidade = factor(tamCidade, levels = c(1,2,3,4,5,6,7,8,-5),
                      labels = c("Under 2,000","2,000-5,000","5,000-10,000","10,000-20,000","20,000-50,000","50,000FALSE,000","100,000-500,000","500,000 or more",FALSE)))

df2 <- df2 %>%
  mutate(`Urban/Rural` = factor(`Urban/Rural`, levels = c(1,2,-5),
                      labels = c("Urban","Rural",FALSE)))

df2 <- df2 %>%
  mutate(`InLPolitics` = factor(`InLPolitics`, levels = c(1,2,3,4,-1, -2, -5),
                      labels = c("Very Important","Rather Important", "Not Very Important","Not at all important",FALSE,FALSE,FALSE)))

df2 <- df2 %>%
  mutate(`AC/INAC in Political Party` = factor(`AC/INAC in Political Party`, levels = c(0,1,2,-1,-2,-5),
                      labels = c("Not a Member",  "Inactive member","Active Member",FALSE,FALSE,FALSE)))

df2 <- df2 %>%
  mutate(Q29 = factor(Q29, levels = c(1,2,3,4,-1,-2,-5),
                      labels = c("Agree Strongly", "Agree","Disagree","Strongly Disagree",FALSE,FALSE,FALSE)))
df2 <- df2 %>%
  mutate(Q46 = factor(Q46, levels = c(1,2,3,4,-1),
                      labels = c("Very Happy","Quite Happy","Not Very Happy","Not at all happy",FALSE)))

df2 <- df2 %>%
  mutate(Q119 = factor(Q119, levels = c(0,1,2,3,4,-1,-2,-4,-5),
                      labels = c("Hard to say", "Strongly agree","Agree","Disagree","Strongly Disagree",FALSE,FALSE,FALSE,FALSE)))

df2 <- df2 %>%
  mutate(Q233 = factor(Q233, levels = c(1,2,3,4,-1,-2,-3,-4,-5),
                      labels = c("Very Often","Fairly often","Not Often","Not at all often",FALSE,FALSE,FALSE,FALSE,FALSE)))

df2 <- df2[!apply(df2==FALSE,1,any), ]
View(df2)
summary(df2$InLPolitics)
# Survey ------------------------------------------------------------------
#Foi necessário baixar a df que abrange todas as waves, importar as variaveis de ID e strata e por fim dar merge nas duas
library(survey)

svy <- svydesign(id = ~1, strata = df2$Strata, weights = df2$S018, data = df2, keep.names = TRUE)
summary(svy)

stats_mean <- svymean(Q46Bool ~ Name + `Year of Birth` + tamCidade + `Urban/Rural` + InLPolitics + `AC/INAC in Political Party` + Q46+ Q119 + Q233 + I,
                      design = svy,
                      na.rm = TRUE, parms = NA)

print(stats_mean)

# Modelo PL ---------------------------------------------------------------

mpl <- svyglm(Q46Bool ~ Name + `Year of Birth` + tamCidade + `Urban/Rural` + InLPolitics + `AC/INAC in Political Party` + Q119 + Q233 + I,
              design = svy,
              family = stats::gaussian(link = "identity"))


summary(mpl)
plot(mpl)
#Histograma do predict

hist(predict(mpl, type = 'response', design=svy), freq= FALSE)
#Probabilidades que não fazem sentido
sum(count_mpl=ifelse(predict(mpl,
                             type='response', design=svy)<0 |
                       predict(mpl,type='response', design=svy)>1,1,0),
    na.rm=TRUE)

#Efeitos marginais
library(margins)

margins_mpl <- margins(mpl, design = svy)

length(coef(mpl))
length(attributes(mpl)$names)
length(attributes(svy)$variables)

#Não podemos calcular as margens do modelo utilizando o método mpl pois as dimensões são diferentes, para ajustar isso é necessário uma restruturação do modelo ou do design utilizado

# MPL com país EF ---------------------------------------------------------
#install.packages("stargazer")
library(stargazer)

mpl_lfe <- lfe::felm(Q46Bool ~ `Year of Birth` + tamCidade + `Urban/Rural` + InLPolitics + `AC/INAC in Political Party` + Q119 + Q233 + I |Name,
                     weights = df2$s018, data = df2)
summary(mpl_lfe)

stargazer(mpl_lfe, type = 'text')

# Modelo Probit -----------------------------------------------------------

typeof(df2$Q46Bool)

probit <- svyglm(Q46Bool ~ Name + `Year of Birth` + tamCidade + `Urban/Rural` + InLPolitics + `AC/INAC in Political Party` + Q46+ Q119 + Q233 + I, design = svy, family=quasibinomial(link="probit"))

summary(probit)
plot(probit)
#Histograma Probit

hist(predict(probit, type = 'response', design=svy), freq=FALSE)

#Probs inuteis
sum(count_probit=ifelse(predict(probit,
                             type='response', design=svy)<0 |
                       predict(probit,type='response', design=svy)>1,1,0),
    na.rm=TRUE)



# Modelo logit ------------------------------------------------------------
logit <- svyglm(Q46Bool ~ Name + `Year of Birth` + tamCidade + `Urban/Rural` + InLPolitics + `AC/INAC in Political Party` + Q46+ Q119 + Q233 + I, design = svy, family=quasibinomial(link="logit"))

summary(logit)
plot(logit)

#Histograma logit
hist(predict(logit, type = 'response', design=svy), freq= FALSE)
#Probs inuteis NÃO FUNCIONA
sum(count_logit = ifelse(predict(logit, type = 'response', newdata = as.data.frame(svy)) < 0 | 
                           predict(logit, type = 'response', newdata = as.data.frame(svy)) > 1, 1, 0), 
    na.rm = TRUE)
# Teste de hipotese -------------------------------------------------------



#Variancia/Covariancia probit
coeftest(probit)
library(pROC)
library(ROCit)

#Curva ROC
#Método 1

probs <- predict(mpl, type = "response")
df2$Q46Bool <- as.numeric(df2$Q46Bool)

roc_curva <- roc(df2$Q46Bool, probs)
auc <- auc(roc_curva)

plot(roc_curva, main = "Curva ROC", xlab = "Taxa de falso positivo", ylab = "Taxa de verdadeiro positivo")

#Método 2
roc <- roc(df2$Q46Bool, probs)

plot(roc)

#Acurácia
roc <- roc(df2$Q46Bool, probs)
score <- roc$predictions
class <- factor(df2$Q46Bool, levels = c(0, 1))
class <- as.numeric(levels(class))[class]
score <- score[order(class)]
class <- class[order(class)]

measure <- measureit(roc, measure = c("ACC", "SENS", "SPEC"), class = class, score = score)

#Total
plot(measure$Cutoff, measure$ACC)
#Sensibilidade
plot(measure$Cutoff,measure$SENS)
#Especificidade
plot(measure$Cutoff,measure$SPEC)


#Teste hoslem.test
probs_logit <- predict(logit, type = "response")
HL_tab <- cbind(fitted.values = probs_logit, obs = df2$Q46Bool, strata = df2$Strata)
hoslem.test(x=probs_logit,y = df2$Q46Bool, g = 10 )



# Descritiva Gráfica --------------------------------------------------------------
# Utilizando df originais por que a ordem do trabalho foi feita diferente do escopo
df2 <- data.frame(df$B_COUNTRY,df$Q260, df$Q261, df$Q262, df$G_TOWNSIZE, df$hdi, df$H_URBRURAL,df$Q4, df$Q98,df$I_WOMJOB, df$I_WOMPOL, df$Q29, df$Q46, df$Q71, df$Q80, df$Q119, df$Q199, df$Q233, df$Q249, df$S007, df$S018, df$Q275)
colnames(df2) <- c("codPais","Sex","Year of Birth","Age","tamCidade","IDH", "Urban/Rural","InLPolitics","AC/INAC in Political Party", "womJob", "womPol","Q29", "Q46", "Q71", "Q80", "Q119", "Q199", "Q233", "Q249","id","S018", "Escolaridade")
#Adicionando o fator I
df2$I <- ifelse(df2$Sex == 1, mean_treatedQ29 + df2$IDH + df2$Sex + wompol_Mean, mean_treatedQ29 - (1-mean_treatedQ29) + df2$IDH + df2$Sex + wompol_Mean) 

df2 <- merge(x = states, y = df2, by="codPais")
df2 <- merge(x=df2, y = df9822_clean, by="id")

length(df2$Strata)

#Count groups
country_groups <- df2 %>%
  count(Name) %>%
  view()


#Gráfico sexo

dataSex <- data.frame(qnt=c(length(df2$Sex[df2$Sex == "2"]),length(df2$Sex[df2$Sex == "1"]),-length(df2$Sex)+length(df2$Sex[df2$Sex == "2"])+length(df2$Sex[df2$Sex == "2"])), desc = c("Female", "Male", "Others") )
View(dataSex)

ggplot(data=dataSex, aes(x=qnt, y=desc)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=qnt), vjust=1.3, color="black", size=3.5)+
  theme_minimal()

#Gráfico Idade
ggplot(df2, aes(x=Age)) + 
  geom_histogram(bins=10, color="black", fill="black") +
  labs(title="Faixas Etárias", x="Idade", y="Frequência")

#Gráfico tamanho cidade
# Nomeando as categorias
df2$tamCidade[df2$tamCidade == 1] <- "Under 2,000"
df2$tamCidade[df2$tamCidade == 2] <- "2,000-5,000"
df2$tamCidade[df2$tamCidade == 3] <- "5,000-10,000"
df2$tamCidade[df2$tamCidade == 4] <- "10,000-20,000"
df2$tamCidade[df2$tamCidade == 5] <- "20,000-50,000"
df2$tamCidade[df2$tamCidade == 6] <- "50,000FALSE,000"
df2$tamCidade[df2$tamCidade == 7] <- "100,000-500,000"
df2$tamCidade[df2$tamCidade == 8] <- "500,000 or more"
summary(df2$tamCidade)

# Criando o gráfico
ggplot(df2, aes(x = tamCidade)) + 
  geom_bar() +
  xlab("Faixa de População") +
  ylab("Quantidade") +
  ggtitle("Distribuição de Cidades por População")

#Gráfico Urban/Rural
summary(df2$`Urban/Rural`)
df2 <- subset(df2, `Urban/Rural` >= 0)
counts <- table(df2$`Urban/Rural`)
counts
# vetor de categorias
categorias <- c("Urban", "Rural")
categorias
df_urban_rural <- data.frame(categorias, counts)

# Criando o gráfico de barras
ggplot(df_urban_rural, aes(x=categorias, y=counts, fill=categorias)) +
  geom_bar(stat="identity") +
  xlab("Categoria") +
  ylab("Contagem") +
  ggtitle("Distribuição das categorias Urban, Rural e Outros") +
  theme_minimal() +
  theme(plot.title = element_text(size=16, face="bold"))

#Gráfico de importância para política

categorias <- c("Very Important", "Rather Important", "Not Very Important", "Not Important")

df2$InLPolitics <- ifelse(df2$InLPolitics == 1, "Very Important",
                          ifelse(df2$InLPolitics == 2, "Rather Important",
                                 ifelse(df2$InLPolitics == 3, "Not Very Important", "Not Important")))

counts <- table(df2$InLPolitics)
counts
barplot(counts, main="Influence of Politics on Life", xlab="Importance", ylab="Frequency", names.arg=categorias)

#Criando um gráfico para Names (países)

freq_names <- table(df2$Name)
freq_names
# plotar gráfico de barras
barplot(freq_names, main="Frequência de nomes", xlab="Nome", ylab="Frequência")


View(df2)

