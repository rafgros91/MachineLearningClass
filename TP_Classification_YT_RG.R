# TP-Class.R
# Experiment 4 classification models over the iris data set
# Logistic, CART, random Forest and knn.
	data(iris)
# Let's look at the data
	summary(iris)
# Variables
	pairs(iris[,-5],col=iris[,5])
	dim(iris)
	names(iris)
	n = nrow(iris)
	p = ncol(iris)
	
# Split the data in two subsamples, n/3 2n/3 approximately
	Ind.test = c(sample(1:50,n/9),sample(51:100,n/9),sample(101:150,n/9))
	Learn = iris[-Ind.test,]
	Test = iris[Ind.test,]

# Check dimensions
	print(dim(Learn))
	print(dim(Test))
###############################
# Q1 - Poser le problème que le programme résout
#
#  Ce probleme explore quatre modeles de classification permettant
# d'identifier une fleur qui appartient à une des trois espèces:
# (setosa, veriscolor ou virginica). Les modèles de classification
# utilisent les largeurs et longeurs des tiges et pétales de ces fleur
# afin de répondre à cette problématique.
###############################
# Q2 - Exécuter le programme
###############################
# The logisitic model - Original parameters
# 
# Learn the model using the learning sample & predict over the test sample
	library(nnet)

	mod1 = multinom(Species ~ ., data=Learn) #Q1 >> Q3
	#mod1 = multinom(Species ~ Sepal.Length + Sepal.Width, data=Learn) #Q4
	summary(mod1)	

	#Passage des coeeficients au carré
	exp(coef(mod1))

	prev = predict(mod1,newdata=Test)
# Analyze predictions 
	table(prev,Test$Species)
# compute the missclassification error
	err.logistic = 100 * mean(prev != Test$Species)
	err.logistic #print error
###############################
# Q3 - multinomial logistic model - commentaires sur résultats
# 
# Ce model de classification utilise comme base de comparaison la première classe du dataset, c'est à dire "setosa"
# En regardant le summary de notre model on obtenient de nombreuses informations.
#		- Une matrice de Coefficient qui représente, pour chaque ligne, une equation modèle.
#		- Une matrice d'Erreurs standards
#	La première ligne compare notre classe "versicolor" à notre classe de référence "setosa"
#		On voit que si la longueur de la tige diminue d'une unitée, la probabilité logarithmique que la fleur soit une "versicolor" baisse de 8.05
#		De même, si la longeur des pétals augmente d'une unitée, la probabilité logarithmique que la fleur soit une versicolor monte de 20.41!
#	La seconde ligne compare comme la première mais la classe "virginica" avec la calsse "setosa"
#
#	Ainsi, en élevant au carré les coefficients on obtient le ratio du risque relatif (relative risk ratio)
#	Cela permet de comprendre quelles caractéristiques sont plus significatives pour différencier une classe d'une autre.
#______________________________
#
#	Outre les deux matrices, le programme nous donne également une table qui compare les résultats prédits contre les résultats attendus.
#	La diagonale montre le nombre de "match" entre les résultats prédits et les résultats attendus.
#	Les autres valeurs représentent les "mismatch" avec lequel nous pouvons déduire le taux d'erreur.
#
#	En exécutant le modèle de regression logistique on obtient cette liste d'erreurs:
#		[2.08 ; 4.16 ; 6.25 ; 2.08 ; 4.16 ; 0 ; 8.33] 
#	Qui correspond à un taux d'erreur moyen de 4% environ
#
###############################
# Q4 Modifier l’ensemble des variables pour déterminer les variables utiles et indispensables.
# 
#	En regardant le summary des données, on voit que Sepal.Length + Sepal.Width ne permet pas de bien distinguer
#	la séparation entre deux des trois classes. Ainsi, faire une classification en fonction de ces deux
#	paramètres ne pourra qu'induire de plus grandes erreures.
#
#	Nous changeons le parametre pour n'appliquer au modèle que les paramètres de la tige (Sepal)
#		commande:	`mod1 = multinom(Species ~ Sepal.Width + Sepal.Length, data=Learn)`
#
# 	Nous obtenons la matrice de "match" && "mismatch" et répétons le processus avec une repartition différente
#	des données d'apprentissage et de test et nous obtenons les résultats suivants:
#		[27.08 ; 16.66 ; 20.83 ; 18.75 ; 22.92 ; 20.8]
#	Qui correspond à un taux d'arreur moyen de 21.6%. Ce qui est incroyablement plus élevé.
#	Nous comprenons donc l'importance des paramètres donnés au modèle de classification.
#
###############################
# CART

	library(rpart)
	mod2 = rpart(Species ~ Petal.Length+Petal.Length,data=iris)
	#mod2 = rpart(Species ~ Sepal.Length+Sepal.Width,data=iris)`
	summary(mod2)
# png("Iris.tree.png")
	plot(mod2,branch=.5)
	text(mod2,cex=0.8)
	title("Classification tree for Iris data set",cex.main=0.8)
# dev.off()

# Now learn the tree using the learning sample and compute prediction for test sample
	mod2 = rpart(Species ~.,data=Learn)
	prev = predict(mod2,newdata=Test)
# See the content of prev... For each observation in the test
# it returns probabilities for each possible output label
	prev = predict(mod2,newdata=Test,type="class")
	table(prev,Test$Species)	
	err.cart = 100 * mean(prev != Test$Species)
	err.cart
###############################
# Q3 - CART model - commentaires sur résultats
# 
#	Le principe du model de CART est d'utiliser les caractéristiques des classes afin de diviser 
#	l'espace occupé par toutes les valeurs des classes.
#	
#	En utilisants les caractéristiques des classes d'entrainement, le model crée des "splits" qui permettent de
#	créer un arbre de décision en fonction de ces caractéristiques.
#
#	Nous nous trouvons donc avec un arbre qui divise en fonction de la taille des pétales:
#		- Si la longeur est inférieure à 2.45, nous nous trouvons avec des "setosa"
#		- Si la largeur est inférieure à 1.75, nous nous trouvons avec des "versicolor"
#		- Sinon, nous avons des "virginic"
#
#		Cependant, lorsque l'on change les donnée prises en entrainement, les valeurs qui constituent
#		les séparations changes beaucoup. L'interprétation d'un arbre n'est donc pas très consistente. 
#
#	Ce model nous permet aussi de voir le tableau de "match" / "mismatch".
#		[8.33, 6.25, 4.16, 4.16, 6.25, 8.33]
#	Ce qui donne un taux moyen d'erreur de 6.25%
###############################
# Q4 Modifier l’ensemble des variables pour déterminer les variables utiles et indispensables.
# 
#	On change les caractéristiques utilisées pour créer notre arbre CART:
#		`mod2 = rpart(Species ~ Sepal.Length+Sepal.Width,data=iris)`
#
#	L'arbre résultant est plus complexe que celui fait précedemment. Les "splits" se font suivant les caractéristiqyues suivantes:
#		- Si longeur tige < 5.45
#			- Si largeur tige >= 2.8 --> setosa
#			- Sinon --> versicolor
#		- Sinon, Si longeur tige < 6.15
#			-Si largeur >= 3.1 -->setosa
#			-Sinon --> versicolor
#		-Sinon --> virginica
#
#	En executant plusieurs fois le scripts on obtient les résultats suivants:
#		[10.42 ; 6.25 ; 4.16 ; 6.25 ; 4.16]
#	Ce qui donne un taux moyen d'erreur de 6% environ, ce qui est assez similair aux résultats avec la taille des pétales.
###############################
# Lets Try random forests

	library(randomForest)
	mod3 = randomForest(Species ~.,data=Learn)
	#mod3 = randomForest(Species ~ Petal.Length+Petal.Width,data=Learn)
	prev = predict(mod3,newdata=Test)
	#summary(mod3) #print model info
# See the content of prev... For each observation in the test
# it returns probabilities for each possible output label
	prev = predict(mod3,newdata=Test,type="class")
	table(prev,Test$Species)	
	err.rf = 100 * mean(prev != Test$Species)
	err.rf #print
###############################
# Q3 - Random Forest model - commentaires sur résultats
# 
#	La méthode Random Forsest présente de nombreux avantages:
#		- C'est une des méthodes les plus préconisées du à sa précision inégalable
# 		- (Donne une idée de quelles variables sont utile pour la classification)
#		
# 	En exécutant le code tel qu'il est donné, le summary de notre modèle ne nous
#	donne que le tableau des "match" / "mismatch", ainsi que le taux d'errueur
# 		[6.25 2.08 2.08 0.00 2.08 6.25]
#		Ce qui donne un taux d'erreur d'environ 3.12%
#
###############################
# Q4 Modifier l’ensemble des variables pour déterminer les variables utiles et indispensables.
# 
#	Il existe de nombreux paramètres à la fonction `randomForest` (40+)	
#	Nous avons décidé d'utiliser deux arguments qui nous semblaient utiles: `importance` && `proximity`
#	Ces parametres permettent de calculer la proximité des inputs ainsi que l'importance des variables d'entrée
#
#	command:	`mod3 = randomForest(Species ~.,data=Learn,importance=TRUE,proximity=TRUE)`
#
#	En exécutant le code tel qu'il est donné, le summary de notre modèle ne nous
#	donne que le tableau des "match" / "missmatch", ainsi que le taux d'erreur
# 		[6.25 2.08 8.08 0.00 2.08 6.25 2.08 0.00 0.00]
#		Ce qui donne un taux d'erreur d'environ 2.08%
#
#	La méthode "RandomForest" est connu pour être la plus précisse, sur un dataset aussi petit et peu complexe
#	il nous parait difficile d'améliorer les résultats avec ce dataset.
###############################
# KNN
	Ind.test = c(sample(1:50,n/9),sample(51:100,n/9),sample(101:150,n/9))
	Learn = iris[-Ind.test,]
	Test = iris[Ind.test,]
	library(class)
	prev = knn(Learn[,-5],Test[,-5],Learn[,5],k=3)
	summary(prev)
	table(prev,Test$Species)	
	err.knn = 100 * mean(prev != Test$Species)
	err.knn	#print

###############################
# Q3 - KNN model - commentaires sur résultats
# 
#	Le modèle KNN est très différent des autres modèles. En effet, il regarde les
#	plus proches voisins et décide de la classe en fonction de celles ci.
#	
#	schéma représentatif:
#
#	|	x x
#	|  x x  x
#	|   . x
#	|  x   o  o
#	|	o   o
#	|_____o__________
#
#	Dans le schéma ci-dessus, on peut voir que en prennant les trois plus proches voisin du "."
#	nous pouvons estimer sa classe à "x".
#
# 	En exécutant le code tel qu'il est donné, le summary de notre modèle ne nous
#	donne que le tableau des "match" / "missmatch", ainsi que le taux d'errueur
# 		[6.25 4.08 2.08 6.25 2.08 0.00]
#		Ce qui donne un taux d'erreur d'environ 4.08%
#
###############################
# Q4 Modifier l’ensemble des variables pour déterminer les variables utiles et indispensables.
# 
#	Deux paramètres sont "k", le nombre de voisins pris en compte et "cl" les facteur de vraie classification du set d'entrainement.
#		
#	En prenant k = 5 on obtient les résultats suivants:
#		[4.16 ; 4.16 ; 6.24 ; 2.08 ; 2.08 ; 6.25]
#	Ce qui donne un résultat moyen d'environ 4.08%
#
###############################
# Toutes les erreurs
	erreurs = c(err.logistic,err.cart, err.rf,err.knn)
###############################
#	SOURCES
###############################
#	https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
#	https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/
#	http://www.stat.wisc.edu/~loh/treeprogs/guide/wires11.pdf
#	http://www.itgo.me/a/x4100995879482653700/how-does-the-function-multinom-from-r-package-nnet-compute-the-multinomial-proba
#	https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#features
#	https://math.usu.edu/adele/RandomForests/ENAR.pdf
#	https://www.analyticsvidhya.com/blog/2014/10/introduction-k-neighbours-algorithm-clustering/	
###############################
