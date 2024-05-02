############################################## 
#Data: 03/05/24
#Prof. Adriana Andrade
#EstatÍstica Não Paramétrica
############################################## 

setwd("G:/Meu Drive/_UFRRJ/Post Graduation/Solos/Não Paramétrico/2024/DataR")

#Pacotes
library(openxlsx) 


# Exemplo 1 - Mann-Whitney ----------------------------------------------------

Peso_Mann <- read.xlsx("Peso_Mann.xlsx")

str(Peso_Mann)
View(Peso_Mann)
attach(Peso_Mann)

#Estatística Descritiva
tapply(Peso, Tratamento,median)
tapply(Peso, Tratamento,range)
tapply(Peso, Tratamento,IQR)

boxplot(Peso~Tratamento,notch=TRUE)


#Teste Mann-Whitney

wilcox.test(Peso~Tratamento,paired = FALSE, data=Peso_Mann)


# Exemplo 2 - Wilcoxon --------------------------------------------------------


data <- read.xlsx("colinesterase_Wilcoxon.xlsx")
View (data)

data$Tempo<-factor(data$Tempo, labels = c("Antes", "Depois"))

# Análise Descritiva


tapply(data$colinesterase , data$Tempo, median)
tapply(data$colinesterase , data$Tempo, range)
tapply(data$colinesterase , data$Tempo, IQR)

library(ggplot2)
ggplot(data,aes(y=colinesterase,x=Tempo,fill=Tempo))+
  geom_boxplot()+
  geom_jitter()

boxplot(data$colinesterase~ data$Tempo,notch=TRUE)

par(mfrow=c(1,2))
hist(data$colinesterase[data$Tempo=="Antes"],main="Antes")
hist(data$colinesterase[data$Tempo=="Depois"],main="Depois")

# Estatística de Wilcoxon

wilcox.test(colinesterase ~ Tempo, paired=T, data=data)


# Exemplo 3 - Kruskal-Wallis --------------------------------------------------

Cultura_Kruskal <- read.xlsx("Cultura_Kruskal.xlsx")
str(Cultura_Kruskal)

Cultura_Kruskal$grupo<-as.factor(Cultura_Kruskal$grupo)


# Estatísticas Descritivas

tapply(Cultura_Kruskal$cultura,Cultura_Kruskal$grupo,median)
tapply(Cultura_Kruskal$cultura,Cultura_Kruskal$grupo,range)
tapply(Cultura_Kruskal$cultura,Cultura_Kruskal$grupo,IQR)


boxplot(Cultura_Kruskal$cultura~Cultura_Kruskal$grupo,
        col="gray", 
        xlab="Grupo",
        ylab="Peso_Cultura",
        main="Peso da Cultura segundo Grupo de Tratamento")


#Teste KW para verificar se grupos são iguais
kruskal.test(Cultura_Kruskal$cultura,Cultura_Kruskal$grupo)


#Teste Dunn - posthoc KW: comparações múltiplas

install.packages("dunn.test")
library(dunn.test)
dunn.test(Cultura_Kruskal$cultura,
          Cultura_Kruskal$grupo,
          method="bonferroni",
          list=T)



# Exemplo 4 - Teste de Friedman----------------------------------------------

VacasFriedman <- read.xlsx("VacasFriedman.xlsx")
str(VacasFriedman)


VacasFriedman$Blocos=factor(VacasFriedman$Blocos,labels=c('Gir','Holandesa','Jersey','Nelore','Guzerá'))
VacasFriedman$Trat=factor(VacasFriedman$Trat,labels=c('S','M','A','BD'))
View(VacasFriedman)


#Comparação entre os tratamentos

tapply(VacasFriedman$Producao,VacasFriedman$Trat,median)
tapply(VacasFriedman$Producao,VacasFriedman$Trat,range)
tapply(VacasFriedman$Producao,VacasFriedman$Trat,IQR)

boxplot(VacasFriedman$Producao~VacasFriedman$Trat,
        col="gray",
        main="Produção de leite",
        xlab="Tratamento",
        ylab = "Produção")


##Teste de Friedman

friedman.test(VacasFriedman$Producao,
              groups = VacasFriedman$Trat,
              blocks = VacasFriedman$Blocos)


## PostHoc - comparações múltiplas

library(PMCMRplus)
frdAllPairsConoverTest(VacasFriedman$Producao,
                       groups = VacasFriedman$Trat,
                       blocks = VacasFriedman$Blocos,
                       p.adjust.method = "bonferroni")


# Exemplo 5 - Qui quadrado --------------------------------------------------

x<-matrix(c(14,6,22,46),nrow=2)
rownames(x)<-c("Oniscus","Armadilidium")
colnames(x)<-c("Argiloso","Calcáreo")
x

barplot(x,font=3,beside=TRUE,col=c(2,7));abline(h=0)
legend(1,35,c("Oniscus","Armadilidium"),fill=c(2,7))


chisq.test(x, correct=F)





# Exemplo 6 - Spearman -------------------------------------------------------

feijao <- read.xlsx("Feijao_Spearman.xlsx")
str(feijao)
View(feijao)


#Correlação 

plot(feijao$FFPA,feijao$FFSR, main="Diagrama de Dispersão - FFPA x FFSR",cex=1.5,pch=16,xlab="FFPA",ylab="FFRS")


cor.test(feijao$FFPA,feijao$FFSR,method = c("spearman"))
