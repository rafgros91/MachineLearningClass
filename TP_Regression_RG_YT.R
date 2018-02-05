#TP-reg.R

# 1. Etudier les données de pollution airquality

# a- Commenter les graphiques et synthèses numériques

	# Chargement du dataset:
	data(airquality)
	
	# Dimension du dataset :
	dim(airquality)
	# 153 enregistrements, 6 variables explicatives

	# Noms des variables explicatives
	names(airquality)
	# 6 variables explicatives : Ozone, Solar.R, Wind, Temp, Month et Day	

	# Synthèse du dataset:
	summary(airquality)
	# Ozone : Min 1, Max 168, Valeurs manquantes 37
	# Solar R : Min 7, Max 334, Valeurs manquantes 7
	# Wind : Min 1.7, Max 20.7
	# Temp : Min 56, Max 97
	# Month : Min 5, Max 9 (Mai-Septembre)
	# Day : Min 1, Max 31

	# Graphs des variables explicatives 2 à 2
	pairs(airquality)

# b- Calculer les corrélations des variables numériques.
#    [extension passer à la corrélation partielle]

	# Corrélations des variables explicatives 2 à 2
	cor(airquality)
	
# c- Etudier les données manquantes

	# On constate que les coefficients de corrélation concernant des variables explicatives
	# qui ont des valeurs manquantes ne s'affichent pas
	# On peut ainsi créer un nouveau dataset similaire mais qui ignorera les 
	# enregistrements pour lesquels de valeurs sont manquantes
	DS = na.omit(airquality)
	dim(DS)
	n = nrow(DS)
	p = ncol(DS)
	# On obtient ainsi un dataset avec 111 enregistrements complets

# 2. Regresser l'ozone sur la température par une régression linéaire simple
	
	# Regression linéaire de l'ozone sur la température
	lm(Ozone ~ Temp, data=DS)
	# On obtient Ozone = 2.439xTemp-147.646

# 3. Les grandes valeurs sont mal modélisées passer à une régression par un
#    polynôme de degré 2 ou bien faire une modélisation par zones. Dans ce dernier
#    cas, sépare les jours de pollution inférieure à 80 microgrammes/m3 des autres
#    jours. Comparer les coefficients des deux régressions. Utiliser le summary.

	# Regression par un polynome de degré 2
	lm(Ozone ~ I(Temp^2), data=DS)
	# Ce qui nous donne Ozone = 0.01619xTemp-57.32414

	# Modélisation par zones (jours de pollution inférieure à 80 microgramme/m3)
	DS2 = DS[DS$Ozone<=80,]
	lm(Ozone ~ Temp, data= DS2)
	# Ce qui nous donne Ozone = 1.662xTemp-94.646

	# Ces deux modèles semblent un peu plus pertinents que la regression linéaire simple
	# qui nous donnait un intercept avec une très grande valeur négative (-147.646)
	# Le cas de la regression linéaire par un polynome de degré 2 donne un intercept
	# de -57.32414 qui est surement plus acceptable que l'intercept de la modélisation 
	# par zones avec une valeur de -94.646

# 4. Régression linéaire sur toutes les variables. Faire les modèles, étudier et
#    commenter les résultats. Faire les représentations visuelles possibles.

	# Modèle 1 : Regression linéaire de l'Ozone sur toutes les variables
	mod1 = lm(Ozone ~ ., data=DS)
	plot(mod1)
	# Ozone = 0.05027xSolar.R - 3.31844xWind + 1.89579xTemp - 3.03996xMonth + 0.27388xDay
	#		  - 64.11632

	# Modèle 2 : Regression linéaire de Solar.R sur toutes les variables
	mod2 = lm(Solar.R ~ ., data=DS)
	plot(mod2)
	# Solar.R = 0.8363xOzone + 3.4449xWind + 2.0642xTemp - 11.0806xMonth - 0.4570xDay
	#		  + 42.0222

	# Modèle 3 : Regression linéaire de Wind sur toutes les variables
	mod3 = lm(Wind ~ ., data=DS)
	plot(mod3)
	# Wind = -0.060746xOzone + 0.003791xSolar.R - 0.036604xTemp - 0.159671xMonth 
	#        + 0.017353xDay + 15.519460

	# Modèle 4 : Regression linéaire de Temp sur toutes les variables
	mod4 = lm(Temp ~ ., data=DS)
	plot(mod4)
	# Temp = 0.16528xOzone + 0.01082xSolar.R - 0.17433xWind + 2.04246xMonth - 0.08919xDay
	#		  + 57.25183

	# IL ne semble pas pertinent de faire une regression pour Month et Day

	# Ces résultats nous montrent que Wind et Temp ont une forte influence sur 
	# Ozone et Solar.R.
	# Si Wind augmente : Ozone diminue plus de 3 fois plus vite et Solar.R augmente plus
	# de 3 fois plus vite
	# Si Temp augmente : Ozone augmente presque 2 fois plus vite et Solar.R augmente 
	# plus de 2 fois plus vite

# 5. Extensions éventuelles

# a- Vers la régression quantile. Utiliser le package rq.

	# Installation du package rq
	install.packages("quantreg")
	library(quantreg)

	# Regression quantile sur les quartiles (exemple de regression de l'ozone)
	quartiles = c(0.25,0.5,0.75)
	rquartiles = rq(Ozone ~ ., data=DS, tau=quartiles)
	rquartiles
	summary(rquartiles)

	# Regression quantile sur les déciles (On étudiera cette option)
	deciles = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
	ozonerdeciles = rq(Ozone ~ ., data=DS, tau=deciles)
	solarrdeciles = rq(Solar.R ~ ., data=DS, tau=deciles)
	windrdeciles = rq(Wind ~ ., data=DS, tau=deciles)
	temprdeciles = rq(Temp ~ ., data=DS, tau=deciles)
	# Afficher les coefficients moyens pour chaque décile
	ozonerdeciles
	solarrdeciles
	windrdeciles
	temprdeciles
	# Afficher les bornes inférieures et supérieures des coeffs de chaque décile
	summary(ozonerdeciles)
	summary(solarrdeciles)
	summary(windrdeciles)
	summary(temprdeciles)

	# En se focalisant sur les coeffs moyens pour chaque décile, on peut analyser :
	# (La variable Day ne semble pas significative pour les autres variables)

	# Regression de l'Ozone
	# - coeff de Solar.R : Entre 0.02 et 0.081 sur tous les déciles (pas très significatif)
	# - coeff de Wind : Entre -3.5 et -1.9 sur tous les déciles
	#					Entre -3.25 et -2.4 sur les déciles 2 à 7
	# - coeff de Temp : Entre 1.39 et 2.1 sur tous les déciles
	#					Entre 1.78 et 2.1 sur les déciles 3 à 9
	# - coeff de Month : Entre -9.28 et -0.18 sur tous les déciles
	#					 Entre -2.09 et -0.18 sur les déciles 1 à 4
	#					 Entre -5.27 et -2.97 sur les déciles 5 à 8

	# Regression de Solar.R
	# - coeff de Ozone : Entre -0.068 et 1.33 sur tous les déciles
	#					 Entre 1.05 et 1.325 sur les déciles 1 à 5
	#					 Entre -0.068 et 0.57 sur les déciles 6 à 9 (Séparation notable)
	# - coeff de Wind : Entre -0.34 et 7.79 sur tous les déciles (gros écart)
	#					Entre 4.25 et 7.79 sur les déciles 2 à 5
	#					Entre 1.12 et 3.23 sur les déciles 6 à 9
	# - coeff de Temp : Entre 0.06 et 4.56 sur tous les déciles
	#					Entre 3.25 et 4.55 sur les déciles 1 à 4
	#					Entre 0.06 et 1.60 sur les déciles 5 à 9
	# - coeff de Month : Entre -18.53 et 0.36 sur tous les déciles
	#					 Entre -18.53 et -11.841 sur les déciles 1 à 4
	#					 Entre -17.66 et -12.37 sur les déciles 6 à 9

	# Regression de Wind
	# - coeff de Ozone : Entre -0.079 et -0.037 sur tous les déciles
	# - coeff de Solar.R : Entre -0.001 et 0.01 sur tous les déciles
	# - coeff de Temp : Entre -0.086 et -0.003 sur tous les déciles
	# - coeff de Month : Entre -0.25 et 0.22 sur tous les déciles

	# Regression de Temp
	# - coeff de Ozone : Entre 0.08 et 0.21 sur tous les déciles
	# - coeff de Solar.R : Entre -0.002 et 0.02 sur tous les déciles
	# - coeff de Wind : Entre -0.93 et 0.18 sur tous les déciles
	# - coeff de Month : Entre 0.98 et 2.51 sur tous les déciles
	#					 Entre 2.12 et 2.50 sur les déciles 1 à 5
	#					 Entre 1.24 et 1.46 sur les déciles 6 à 8

	# Interprétations :
	# - Influence significative de Wind et Temp sur Ozone
	# - Influence significative de Month sur Ozone pour certains déciles (notamment 5 à 8)
	# - Forte influence de Wind sur Solar.R pour les déciles 2 à 5
	# - Influence notable de Wind sur Solar.R sur les déciles 6 à 9
	# - Forte influence de Temp sur Solar.R pour les déciles 1 à 4
	# - Très forte influence de Month sur Solar.R pour tous les déciles sauf le 5
	# - Wind ne semble pas être significativement influencé par Ozone, Solar.R, Temp et Month
	# - Influence notable de Month sur Temp notamment sur les déciles 1 à 5 (modéré sur 6 à 8)

# b- Vers la régression robuste. Utiliser robustReg.

	# Installation du package robustreg
	install.packages("robustreg")
	library(robustreg)

	# Regression Robuste en utilisant la fonction Huber Psi

	# Regression de l'ozone
	ozonerrh = robustRegH(Ozone ~ ., data=DS, tune=1345,m=TRUE,max.it=1000,tol=1e-5,anova.table=TRUE)
	# Convergence après 5 itérations
	# (arrondi) Ozone = 0.05xSolar.R - 3.32xWind + 1.90xTemp - 3.04xMonth + 0.27xDay - 64.12

	# Regression de Solar.R
	solarrrh = robustRegH(Solar.R ~ ., data=DS, tune=1345,m=TRUE,max.it=1000,tol=1e-5,anova.table=TRUE)
	# Convergence après 5 itérations
	# (arrondi) Solar.R = 0.84xOzone + 3.44xWind + 2.06xTemp - 11.08xMonth - 0.46xDay + 42.02

	# Regression de Wind
	windrrh = robustRegH(Wind ~ ., data=DS, tune=1345,m=TRUE,max.it=1000,tol=1e-5,anova.table=TRUE)
	# Convergence après 5 itérations
	# (arrondi) Wind = -0.06xOzone + O.004xSolar.R - 0.04xTemp - 0.16xMonth + 0.02xDay + 15.52

	# Regression de Temp
	temprrh = robustRegH(Temp ~ ., data=DS, tune=1345,m=TRUE,max.it=1000,tol=1e-5,anova.table=TRUE)
	# Convergence après 5 itérations
	# (arrondi) Temp = 0.165xOzone + 0.01xSolar.R - 0.17xWind + 2.04xMonth - 0.09xDay + 57.25

	# Interprétations :
	# - Influence significative de Wind et Temp sur Ozone et Solar.R
	# - Influence significative de Month sur Temp
	# - Et donc influence de Month sur Ozone et Solar.R

# c- Vers l’analyse de la variance.

	# Analyse de variance de l'Ozone sur les autres variables
	ozoneaov = aov(Ozone ~ Solar.R + Wind + Temp + Month + Day, data=DS)
> 	summary (ozoneaov)
	# F value et Pr(>F)
	# - Solar.R : 33.97 et 6.22e-08
	# - Wind : 91.868 et 5.24e-16
	# - Temp : 43.785 et 1.58e-09
	# - Month : 3.910 et 0.0506
	# - Day : 1.422 et 0.2358
	# On constate donc que Solar.R, Temp et surtout Wind sont significatifs dans le cas d'une 
	# regression de Ozone, faible significativité mais notable de Month aussi

	# Analyse de variance de Solar.R sur les autres variables
	solaraov = aov(Solar.R ~ Ozone + Wind + Temp + Month + Day, data=DS)
> 	summary (solaraov)
	# F value et Pr(>F)
	# - Ozone : 15.323 et 0.000161
	# - Wind : 1.501 et 0.223294
	# - Temp : 0.911 et 0.342048
	# - Month : 3.309 et 0.071764
	# - Day : 0.235 et 0.628615
	# On constate que dans le cas d'une regression de Solar.R, seul Ozone semble vraiment significatif
	# faible significativité mais notable de Month aussi

	# Analyse de variance de Wind sur les autres variables
	windaov = aov(Wind ~ Ozone + Solar.R + Temp + Month + Day, data=DS)
> 	summary (windaov)
	# F value et Pr(>F)
	# - Ozone : 65.584 et 1.06e-12
	# - Solar.R : 1.477 et 0.227
	# - Temp : 41.896 et 0.171
	# - Month : 0.553 et 0.459
	# - Day : 0.309 et 0.580
	# On constate donc que dans le cas d'une regression de Wind, seul Ozone semble très significatif

	# Analyse de variance de l'Ozone sur les autres variables
	tempaov = aov(Temp ~ Ozone + Solar.R + Wind + Month + Day, data=DS)
> 	summary (tempaov)
	# F value et Pr(>F)
	# - Ozone : 128.522 et <2e-16
	# - Solar.R : 0.772 et 0.382
	# - Wind : 2.360 et 0.128
	# - Month : 24.997 et 2.31e-06
	# - Day : 1.735 et 0.191
	# On constate donc que Temp semble significativement influencé par Ozone mais aussi le mois (Month)
	# Ce qui parait logique, la température évolue bien selon le mois

# 6. Commenter l’intérêt de l’ensemble de l’étude.

	# Cette étude nous a permis d'étudier un jeu de données sur de la "météorologie" en utilisant
	# divers techniques de statistique relatives à la regression pour étudier les influences entre 
	# les divers variables. 
	# Surtout dans la partie 5, on remarque que les diverses techniques utilisées nous donnent des 
	# informations qui convergent, notamment :
	# - Influence de Wind, Temp et Month sur Ozone
	# - Infuence de Wind, Temp et Month sur Solar.R
	# - Infuence de Month sur Temp
