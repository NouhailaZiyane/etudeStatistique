
#=> MCD: l'etudiant est caract�ris� par son sexe, Age, Ecole,Fili�re, Ann�e d'etude.
#les 2 autres entit�s March� de travail et difficult�s

#==>Etape 1: Collecte des donn�es
#=>Importation des donn�es
library(readxl)
e <- read_excel("C:/Users/nouhaila ziyane/Desktop/R/Livrables 2/e.xlsx")
View(e)

#Suppression des caract�res sp�ciaux
#Renommer les items(m1,m2,...,d1,d2,..) 
#m=March� de travail 
#d=Difficult�s

#=>Etape2-pr�traitement
#1.conversion des donn�es
#la seule var quanti est l'age
e$age=as.integer(e$age)
#variables  Quali
e$Sexe=as.factor(e$Sexe)
e$Ann�e=as.factor(e$Ann�e)
e$Ecole=as.factor(e$Ecole)
e$Fili�re=as.factor(e$Fili�re)
e$m1=as.factor(e$m1)
e$m2=as.factor(e$m2)
e$m1=as.factor(e$m1)
e$m3=as.factor(e$m3)
e$m4=as.factor(e$m4)
e$m5=as.factor(e$m5)
e$m6=as.factor(e$m6)
e$m7=as.factor(e$m7)
e$m8=as.factor(e$m8)
e$m9=as.factor(e$m9)
e$d1=as.factor(e$d1)
e$d2=as.factor(e$d2)
e$d3=as.factor(e$d3)
e$d4=as.factor(e$d4)
e$d5=as.factor(e$d5)
e$d6=as.factor(e$d6)
e$d7=as.factor(e$d7)
e$d8=as.factor(e$d8)
e$d9=as.factor(e$d9)
e$d10=as.factor(e$d10)
e$d11=as.factor(e$d11)
e$d12=as.factor(e$d12)


#2.Nettoyage des donn�es
#on etudie les etudiants de genie informatique a ENSA-KENITRA
#donc on supprime les valeurs des etudiants des autres fili�res et au sein des autres ecoles


#les valeurs aberrantes 
boxplot(e$age)

#on les visualise
boxplot(e$age)$out


#on trouve 6  valeurs aberrantes donc on vas les transformer en des val NA
e$age[e$age==56]<-NA
e$age[e$age==400]<-NA
e$age[e$age==220]<-NA
e$age[e$age==26]<-NA
e$age[e$age==340]<-NA
e$age[e$age==269]<-NA

#les valeurs manquantes
#on visualise les NA 
summary(e)

# On a trouv� 6 vals manquantes
#on calcule le pourc des NA =>6/57*100= 10.52%>5%
#Donc on fait l'estimation par la moyenne de la variable age
mean(e$age,na.rm = TRUE)
# on a trouv� la moy=20.92 donc on remplace les na par la val 20
#======>On remplace les valeurs NA par la moyenne en utilisant une boucle
for(i in seq(length(e$age)))
{
  if(is.na(e$age[i]))
  {
    e$age[i]=20;
  }
}

#On verifie qu'il n y a pas de valeurs manquantes
summary(e)



#=> etape 3 : Analyse des donn�es 
# Analyse unvari�
hist(e$age) #Age var quanti

#variables  quali
plot(e$Sexe)
plot(e$Ecole)
plot(e$Fili�re)
plot(e$Ann�e)
plot(e$m1)
plot(e$m2)
plot(e$m3)
plot(e$m4)
plot(e$m5)
plot(e$m6)
plot(e$m7)
plot(e$m8)
plot(e$m9)
plot(e$d1)
plot(e$d2)
plot(e$d3)
plot(e$d4)
plot(e$d5)
plot(e$d6)
plot(e$d7)
plot(e$d8)
plot(e$d9)
plot(e$d10)
plot(e$d11)
plot(e$d12)

# 1- test de normalit�: 
shapiro.test(e$age)
#var Age ne suit pas la loi normale

#Test de quasi normalit�
library(moments)
skewness(e$age)
kurtosis(e$age)
# y pas de quasi normalit� 
#puisqu'ils ne sont pas inclus dans l'intervalle [-2.5,2.5]

#on peut visualiser l'age  par le graphique
qqnorm(e$age)
qqline(e$age)

# analyse bivarie 


#Test non parametrique
#*****************************Entit� Etudiant*****************************
#Sexe et Age 
plot(e$Sexe,e$age)
wilcox.test(e$age~e$Sexe)
#on a p-value=1.66%<5% 
#donc il y a une difference entre age et sexe

#=>Ann�e et age
plot(e$Ann�e,e$age)
#Ann�e a plusieurs modalit�s c pour cela on doit 
#faire un test de moyenne et variance
kruskal.test(e$age,e$Ann�e)
#on a p-value<5% donc il y a une difference significative

#=>on ne peut pas faire le test d'assoc entre quanti et quanti
#=>quali et quali

table(e$Sexe,e$Ecole)


#on ne peut pas faire ces tests chisq.test(e$Ecole,e$Sexe)et chisq.test(e$Sexe,e$Fili�re)
#parceque les 2 variables Ecole et fili�re ont une seule modalit�

table(e$Sexe,e$Ann�e)
chisq.test(e$Sexe,e$Ann�e)
#on a p-value>5%
#Donc il y pas une association entre le sexe et l'ann�e d'etude

#*************************2�me Entit� "March� de travail"************************

#puisqu'on a les items sont des variables quali donc on fait chisq.test
#=> TEST d'assoc avec m1
chisq.test(e$m1,e$m2)#Donc il y pas une association
chisq.test(e$m1,e$m3)#Donc il y pas une association
chisq.test(e$m1,e$m4)#on a egale a 5.04 on consid�re qu'il y a une association
chisq.test(e$m1,e$m5)#Donc il y pas une association
chisq.test(e$m1,e$m6)#il y a une association
chisq.test(e$m1,e$m7)#Donc il y pas une association
chisq.test(e$m1,e$m8)#il y a une associaltion
chisq.test(e$m1,e$m9)#il y a une associaltion

#=> TEST d'assoc avec m2

chisq.test(e$m2,e$m3)#il y a pas une association
chisq.test(e$m2,e$m4)#Donc il y pas une association




#=> TEST d'assoc avec m3

chisq.test(e$m3,e$m4)#Donc il y pas une association
chisq.test(e$m3,e$m5)#Donc il y pas une association

#=> TEST d'assoc avec m4
chisq.test(e$m4,e$m7)#Donc il y a une association
chisq.test(e$m4,e$m8)#Donc il y a une association


#=> TEST d'assoc avec m5

chisq.test(e$m5,e$m6)#Donc il y a une association
chisq.test(e$m5,e$m7)#Donc il y pas une association

#=> TEST d'assoc avec m6
chisq.test(e$m6,e$m7)#Donc il y a une association
chisq.test(e$m6,e$m8)#Donc il y a une association


#=> TEST d'assoc avec m7
chisq.test(e$m7,e$m8)#Donc il y pas une association


#=> TEST d'assoc avec m8

chisq.test(e$m8,e$m9)#Donc il y a une association



#*************3eme Entit� Difficult�***********************************
#puisqu'on a les items sont des variables quali donc on fait chisq.test
#=> TEST d'assoc avec d1
chisq.test(e$d1,e$d2)#Donc il ya une association

chisq.test(e$d1,e$d5)#Donc il y a une association
chisq.test(e$d1,e$d6)#il y a une association
chisq.test(e$d1,e$d7)#Donc il y a une association


#=> TEST d'assoc avec d2

chisq.test(e$d2,e$d3)#il y a  une association
chisq.test(e$d2,e$d4)#Donc il y a une association
chisq.test(e$d2,e$d9)#Donc il y a une association



#=> TEST d'assoc avec d3

chisq.test(e$d3,e$d4)#Donc il y pas une association
chisq.test(e$d3,e$d5)#Donc il y pas une association

#=> TEST d'assoc avec m4
chisq.test(e$m4,e$d5)#Donc il y a une association


#=> TEST d'assoc avec d5

chisq.test(e$d5,e$d6)#Donc il y a une association
chisq.test(e$d5,e$d7)#Donc il y a une association

chisq.test(e$d5,e$d9)#Donc il y a une association

#=> TEST d'assoc avec d6
chisq.test(e$d6,e$d7)#Donc il y a une association
chisq.test(e$d6,e$d9)#Donc il y a une association

#=> TEST d'assoc avec d7
chisq.test(e$d7,e$d9)#Donc il y a une association

#=> TEST d'assoc avec d8

chisq.test(e$d8,e$d9)#Donc il y a une association

#************************************************************


#==> On ne peut pas faire la regression lineaire
# puisqu'on n'a pas 2 variables quanti an a juste l'age quanti


