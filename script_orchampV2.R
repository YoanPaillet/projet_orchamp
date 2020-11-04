library(doBy)
library(questionr)
library(publish)
library(ggplot2)
library(lme4)
source("C:/Users/yoan.paillet/Dropbox/Stats/HighstatLibV10.R")

#Chargement des données
d.trems.raw <- read.csv(file="D:/Documents/GitHub/projet_orchamp/donnees_reelles.csv", sep=";", dec=".", na.strings = c("NA", ""))

#d.trems.raw <- read.csv(file="E:/stage alpes/Donnees_Trems/donnees_reelles.csv", sep=";", dec=".", na.strings = c("NA", ""))
str(d.trems.raw)
head(d.trems.raw)

#Elimination des NAs (à vérifier pour le nombre d'arbres')
d.trems <- droplevels(subset(d.trems.raw, !is.na(d.trems.raw$microhabitats)))


#Tableau disjoint microhabitats
#permet d'avoir un tableau avec chaque catégorie de mh en colonne
list.trem <- c("CV11","CV12","CV13","CV14","CV21","CV22","CV23","CV24","CV25","CV26","CV31","CV41","CV42","CV43","CV44",
               "IN11","IN12","IN13","IN14","IN21","IN22","IN23","IN24","IN25",
               "DE11","DE12","DE13",
               "GF11","GF12",
               "GR21","GR22",
               "FU11","FU21","FU22","FU23","FU24",
               "EP11","EP12","EP13","EP14","EP15","EP21","EP22","EP31","EP32",
               "EX11","EX12")

for (i in 1:length(list.trem)){ list.trem.searched<-list.trem[i]
presence.microhab<-as.double(regexpr(list.trem.searched,d.trems$microhabitat)>0)
d.trems.new<-cbind.data.frame(d.trems,presence.microhab)
names(d.trems.new)<-c(names(d.trems),paste("N.",list.trem.searched,sep=""))
d.trems<-d.trems.new}

#Nombre de DMH/arbre
d.trems$N.tree <- apply(d.trems[,12:50], 1, sum)
print(d.trems$N.tree)

#nombre moyen de types de mh par arbre pour une placette donnée = N.alpha
list.m = levels(as.factor(d.trems.raw$codeplot))
N.alpha = data.frame(list.m)
N.alpha$mean = 0
a = 1
for (i in list.m){
  N.alpha$mean[a] = sum(d.trems.raw$N.tree[which(d.trems.raw$codeplot == i)], na.rm = T)/
    length(d.trems.raw$N.tree[which(d.trems.raw$codeplot == i)])
  a = a +1
}
print(N.alpha)



#création d'un dataframe qui donne le N.gamma
div_gamma <- data.frame(list.m)
list.N.gamma <- c(7, 11, 0, 4, 10, 10, 6, 5, 2, 14, 7, 0, 9, 8, 10, 3, 0, 0, 5, 3, 0, 13, 14, 14, 13, 5, 2, 8, 3, 0, 0, 0, 4, 9, 4, 11, 7, 9, 11, 0, 0, 0, 0, 0, 0)
div_gamma$N.gamma = list.N.gamma
print(div_gamma)


##Même chose en plus simple (/!\ il y a des différences)
n.dmh.plot <- summaryBy(N.tree ~ codeplot, data = d.trems, FUN = mean)

## Avec summaryBy en 2 étapes
n.dmh.plot2 <- summaryBy (N.CV11+N.CV12+N.CV13+N.CV14+N.CV21+N.CV22+N.CV23+N.CV24+N.CV25+N.CV26+N.CV31+N.CV41+N.CV42+N.CV43+N.CV44+
               N.IN11+N.IN12+N.IN13+N.IN14+N.IN21+N.IN22+N.IN23+N.IN24+N.IN25+
               N.DE11+N.DE12+N.DE13+
               N.GF11+N.GF12+
               N.GR21+N.GR22+
               N.FU11+N.FU21+N.FU22+N.FU23+N.FU24+
               N.EP11+N.EP12+N.EP13+N.EP14+N.EP15+N.EP21+N.EP22+N.EP31+N.EP32+
               N.EX11+N.EX12 ~ codeplot, data= d.trems, FUN = sum, keep.names = T)

sup.1 <- function(x) { x[x>0] <-1; return(x) }

n.dmh.plot2[,2:ncol(n.dmh.plot2)] <- sup.1(n.dmh.plot2[,2:ncol(n.dmh.plot2)])

n.dmh.plot2$n.dmh.gamma <- apply(n.dmh.plot2[,2:ncol(n.dmh.plot2)],1, sum)

n.dmh.plot.fin <- merge(n.dmh.plot, n.dmh.plot2[,c("codeplot","n.dmh.gamma") ] , by.x="codeplot", by.y="codeplot", all.x=T, all.y=T)

alti <- unique(d.trems[,c("codeplot", "alt")])
n.dmh.plot.fin <- merge(n.dmh.plot.fin, alti, by.x="codeplot", by.y="codeplot", all.x=T, all.y=T)


#Data exploration
par(mfrow=c(1,2))
plot(n.dmh.plot.fin$N.tree.mean~n.dmh.plot.fin$alt, col = n.dmh.plot.fin$codeplot)
plot(n.dmh.plot.fin$n.dmh.gamma~n.dmh.plot.fin$alt, col = n.dmh.plot.fin$codeplot)

plot(n.dmh.plot.fin$n.dmh.gamma~n.dmh.plot.fin$N.tree.mean, col = n.dmh.plot.fin$codeplot)

MyVar <- c("N.tree.mean","n.dmh.gamma","alt")
pairs(n.dmh.plot.fin[,MyVar], lower.panel = panel.smooth,upper.panel = panel.cor, diag.panel = panel.hist)

boxplot(d.trems$N.tree ~ d.trems$codeplot)

lm1 <- lm(n.dmh.plot.fin$n.dmh.gamma~n.dmh.plot.fin$alt)
summary(lm1)

glm1 <- glm(n.dmh.plot.fin$n.dmh.gamma~n.dmh.plot.fin$alt, family= gaussian)
summary(glm1)

#l'equation du modèle est donc n.dmh.gamma = 9.135220  + (-0.001135) * alt

glm2 <- glm(n.dmh.plot.fin$n.dmh.gamma~n.dmh.plot.fin$alt, family = poisson)
summary(glm2)
#dans la distribution de poisson, log(Y) = X
#l'equation du modèle est donc log(n.dmh.gamma) = 2.2220714 + (-0.0001449) * alt

n.dmh.plot2 <- merge(n.dmh.plot2, alti, by.x="codeplot", by.y="codeplot", all.x=T, all.y=T)
glm3 <- glm(n.dmh.plot2$N.EP11~n.dmh.plot2$alt, family = binomial)
summary(glm3)

#dans la loi binomiale, logit(Y) = X <=> log(Y+1)/log(Y) = X
#l'équation du modèle est :   logit(N.EP11) = 14.468189 +(-0.009331)*alt

glm4 <- glm(n.dmh.plot2$N.EP12~n.dmh.plot2$alt, family = binomial)
summary(glm4)


#creation d'un code transect
n.dmh.plot.fin$transect <- substr(n.dmh.plot.fin$codeplot, 1, 3)

glmm <- glmer(n.dmh.gamma~alt + (1|transect), family = poisson, data = n.dmh.plot.fin ) 
summary(glmm)



#représentation de l'évolution du N.gamma en fonction de l'alt
alt <- levels(as.factor(d.trems.raw$alt))
plot(div_gamma$N.gamma ~ alt)

#étude statistique de l'effet de l'alt sur la diversité gamma
cor(div_gamma$N.gamma, as.numeric(alt))
cor.test(div_gamma$N.gamma, as.numeric(alt))

#étude statistique de l'évolution du N.alpha en fonction de l'alt
cor.test(N.alpha$mean, as.numeric(alt))
#représentation de N.alpha en fonction de l'altitude
plot(N.alpha$mean ~ list.alt)

#création liste des altitudes
list.alt = levels ( as.factor((d.trems.raw$alt)))
print(list.alt)


#Un peu d'explo
table(d.trems.raw$N.CV44 )
plot(d.trems.raw$N.CV44 ~ d.trems.raw$codeplot, col = d.trems.raw$N.CV44)
#permet de donner le nombre d'arbres par essence
table(d.trems.raw$lb_nom)
#donne le nombre d'arbres par placettes
nbe_arbre_placette <- table(d.trems.raw$codeplot)
print(nbe_arbre_placette)
#donne le nombre de mh par essence
boxplot(d.trems.raw$N.tree ~ d.trems.raw$lb_nom)
#permet de voir le nbe moyen de mh par placette
boxplot(d.trems.raw$N.tree ~ d.trems.raw$codeplot)
#donne le nombre de mh en fonction du diamètre, une couleur par essence
plot(d.trems.raw$N.tree ~ d.trems.raw$diam, col = d.trems.raw$lb_nom )


d.plot <- summaryBy(N.tree ~ d.trems.raw$codeplot, data=d.trems.raw, FUN = mean)
print(d.plot)

# densité d'arbres (N/ha)
arbre_15<-subset(d.trems.raw, distance == 15 & diam >= 30) #sélection des arbres dans les 15m
print(arbre_15)
arbre_5<-subset(d.trems.raw, (distance == 1.5 | 5 & distance != 15) & diam >= 7.5) #sélection des arbres dans les 5m + transect
print(arbre_5)
#nombre d'arbres de plus de 30cm de diamètre dans les 300m2
nbe_300<-table(arbre_15$codeplot)
print(nbe_300)
#nombre d'arbres de plus de 7.5cm dans les 150m2
nbe_150<-table(arbre_5$codeplot)
print(nbe_150)
#densité d'arbre 1
dens_300<-nbe_300/300
print(dens_300)
#desnité d'arbres 2
dens_150<-nbe_150/150
print(dens_150)
#densité d'arbre par placette
dens_tot<-dens_150+dens_300
print(dens_tot)



       

#test 
table(d.trems.raw$N.CV44, d.trems.raw$codeplot)
list.nbe_CV44 <- c(table(d.trems.raw$N.CV44, d.trems.raw$codeplot)[2,])
print(list.nbe_CV44)
       
#courbes de distribution pour les différents types de mh (nbe d'apparition du type de mh par rapport à l'altitude)
plot(table(d.trems.raw$N.CV44, d.trems.raw$alt)[2,] ~ alt)
plot(table(d.trems.raw$N.EP12, d.trems.raw$alt)[2,] ~ alt)
plot(table(d.trems.raw$N.EX12, d.trems.raw$alt)[2,] ~ alt)
plot(table(d.trems.raw$N.EP11, d.trems.raw$alt)[2,] ~ alt)
plot(table(d.trems.raw$N.EP13, d.trems.raw$alt)[2,] ~ alt)

#donne le nbe total d'apparition des mh de type epiphytes
epiphytes <- apply(d.trems.raw[,48:50], 1, sum)
print(epiphytes)
#courbe de distribution du nbe d'épiphytes en fonction de l'alt
plot(table(epiphytes, d.trems.raw$alt)[2,] ~ alt)
#idem pour les mh de type cavités
cavites <- apply(d.trems.raw[,12:26], 1, sum)
plot(table(cavites, d.trems.raw$alt)[2,] ~ alt)
injuries <- apply(d.trems.raw[,27:35], 1, sum)
plot(table(injuries, d.trems.raw$alt)[2,] ~ alt)
#idem pour les crown deadwood
deadwood <- apply(d.trems.raw[,36:38], 1, sum)
plot(table(deadwood, d.trems.raw$alt)[2,] ~ alt)
plot(table(excroissances, d.trems.raw$alt)[2,] ~ alt)
#idem pour les champi
champi <- apply(d.trems.raw[,43:47], 1, sum)
plot(table(champi, d.trems.raw$alt)[2,] ~ alt)
       
       