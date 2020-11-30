## Librerías necesarias

library(dplyr)
library(sampling)
library(DescTools)
library(arules)
library(VIM)
library(DMwR)
library(missForest)
library(car)
library(ISwR)
library(survival)
library(survminer)
library(rminer)
library(caret)
library(cluster)
library(fpc)


##
authors <- data.frame( surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  retired = c("yes", rep("no", 4)))
books <- data.frame( name = c("Tukey", "Venables", "Tierney", "Ripley", "Ripley", "McNeil"),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA))

example1<-merge(authors, books, by.x="surname", by.y="name")


###

data1 <- data.frame(CustomerId = c(1:6), Product = c(rep("Oven", 3), rep("Television", 3)))
data2 <- data.frame(CustomerId = c(4:7), Product = c(rep("Television", 2), rep("Air conditioner", 2)))
example2<-rbind(data1,data2)
example2

##

unique(example1)
example1 %>% distinct(surname, .keep_all = TRUE)

###

mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale = TRUE)
summary(mtcars.pca)
###

mtcars.pca$rotation

###

par(mfrow=c(1,2))
hist(AirPassengers,breaks=100)
hist(AirPassengers,breaks=10)

###

data(swissmunicipalities)
cl=cluster(swissmunicipalities,clustername=c("REG"),size=3,method="srswor")
getdata(swissmunicipalities, cl)

####
mtcars.norm<-scale(mtcars[,c(1:7,10,11)])


#####

x <- rlnorm(500, 3, 2)
bx <- BoxCox(x, lambda = BoxCoxLambda(x) )

par(mfrow=c(2,2))
qqnorm(x, main="Lognormal")
qqline(x,col=2)

qqnorm(bx, main="Box-Cox")
qqline(bx,col=2)

hist(x,main="Lognormal")
hist(bx, main="Box-Cox")


####

data(iris)
x <- iris[,1]

par(mfrow=c(2,2))

# equal frequency
hist(x, breaks = 20, main = "Equal Frequency")
abline(v = discretize(x, breaks = 3, onlycuts = TRUE), col = "red")

# equal interval width
hist(x, breaks = 20, main = "Equal Width")
abline(v = discretize(x, method = "interval", breaks = 3, onlycuts = TRUE), col = "red")

# k-means clustering 
hist(x, breaks = 20, main = "Clustering")
abline(v = discretize(x, method = "cluster", breaks = 3, onlycuts = TRUE), col = "red")

# user-specified (with labels)
hist(x, breaks = 20, main = "User-specified")
abline(v = discretize(x, method = "fixed", breaks = c(-Inf, 6, Inf),labels=c("small", "large"), onlycuts = TRUE), col = "red")


#####
data(iris)
iris.mis <- prodNA(iris, noNA = 0.1)
aggr(iris.mis, numbers=TRUE, sortVars=TRUE, labels=names(iris.mis), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

##kNN 1
kNN1.imp<-kNN(iris.mis, k=3)

##kNN 2
kNN2.imp <- knnImputation(iris.mis, 3)

##missForest
missForest.imp<-missForest(iris.mis, variablewise = TRUE)

#########
iris.bp<-boxplot(iris$Sepal.Width,main="Sepal Width")
iris.bp$out

####
ap <- data.frame(Altura.cm=c(164, 167, 168, 169, 169, 170, 170, 170, 171, 172, 172, 173, 173, 175, 176, 178),
                 Peso.kg=c( 54,  57,  58,  60,  61,  60,  61,  62,  62,  64,  62,  62,  64,  56,  66,  70)) 

#Criterio +/-2SD
Altura.outlier <- abs(scale(ap$Altura.cm)) > 2
Peso.outlier <- abs(scale(ap$Peso.kg)) > 2
pch <- (Altura.outlier | Peso.outlier) * 16
par(mfrow=c(1,2))
plot(ap, pch=pch) 

#Criterio distancia Mahalanobis (los dos outliers más extremos)
n.outliers   <- 2
m.dist.order <- order(mahalanobis(ap, colMeans(ap), cov(ap)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(ap))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
plot(ap, pch=pch) 


####
summary(iris.mis)
str(iris.mis)

###
ks.test(iris$Sepal.Length, pnorm, mean(iris$Sepal.Length), sd(iris$Sepal.Length))
shapiro.test(iris$Sepal.Length)

####

leveneTest(count ~ spray, data = InsectSprays)

fligner.test(count ~ spray, data = InsectSprays)

####

shapiro.test(sleep$extra)
leveneTest(extra ~ group, data = sleep)
t.test(extra ~ group, data = sleep)

###
shapiro.test(airquality$Ozone)
fligner.test(Ozone ~ Month, data = airquality)
wilcox.test(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))

###

men = c(100, 120, 60)
women = c(350, 200, 90)
ice.cream.survey = as.data.frame(rbind(men, women))
names(ice.cream.survey) = c('chocolate', 'vanilla', 'strawberry')

chisq.test(ice.cream.survey)


#####

shapiro.test(iris$Sepal.Width)
leveneTest(Sepal.Width ~ Species, data = iris)
res.aov <- aov(Sepal.Width ~ Species, data = iris)
summary(res.aov)

###
shapiro.test(airquality$Ozone)
fligner.test(Ozone ~ Month, data = airquality)
kruskal.test(Ozone ~ Month, data = airquality)


##
plot(trees)

m1 = lm(Volume~Girth,data=trees)
summary(m1)


m2 = lm(Volume~Girth+Height,data=trees)
summary(m2)

####

m3 = lm(Volume~Girth+I(Girth^2),data=trees)
summary(m3)

###

pred.frame<-data.frame(Girth=seq(10,16,2))
predict(m3,newdata=pred.frame)

###
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer),]
m4<-glm(Class ~ Cell.size+Cell.shape,data=bc, family="binomial")
summary(m4)

###
cor(trees)
cor.test(trees$Volume,trees$Height)
cor.test(trees$Volume,trees$Height, method="spearman")

####
data(melanom)
attach(melanom)
names(melanom)

Surv(days, status==1)

surv.all <- survfit(Surv(days,status==1)~1)
ggsurvplot(surv.all,Surv(days,status==1),pval=TRUE)

surv.bysex <- survfit(Surv(days,status==1)~sex)
ggsurvplot(surv.bysex,Surv(days,status==1),pval=TRUE)

###
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer),]
bc<-bc[,-1]

h<-holdout(bc$Class,ratio=2/3,mode="stratified")
data_train<-bc[h$tr,]
data_test<-bc[h$ts,]

print(table(data_train$Class))
print(table(data_test$Class))


####

train_control1<- trainControl(method="LOOCV")
train_control2<- trainControl(method="cv", number=10)
train_control3<- trainControl(method="repeatedcv", number=4, repeats=10)

####
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer),-1]

h<-holdout(bc$Class,ratio=2/3,mode="stratified")
data_train<-bc[h$tr,]
data_test<-bc[h$ts,]

train_control<- trainControl(method="cv", number=4)
mod<-train(Class~., data=data_train, method="rf", trControl = train_control)

pred <- predict(mod, newdata=data_test)
confusionMatrix(pred,data_test$Class,positive="malignant")

####
iris.cl<-iris
iris.cl$Species<-NULL
kmeans.res<-kmeans(iris.cl,3)

table(iris$Species,kmeans.res$cluster)

###

kmedoids.res1<-pam(iris.cl,3)
table(iris$Species,kmedoids.res1$cluster)

kmedoids.res2<-pamk(iris.cl)
table(iris$Species,kmedoids.res2$pamobject$clustering)

####
id<-sample(1:dim(iris.cl)[1],40)
irisSample<-iris.cl[id,]
hc<-hclust(dist(irisSample),method="ave")
plot(hc, hang = -1, labels = iris$Species[id])
rect.hclust(hc, k = 3)

#####

ds <- dbscan(iris.cl, eps = 0.42, MinPts = 5)
table(ds$cluster, iris$Species)

######

par(mfrow=c(2,2))
hist(iris$Sepal.Width)

hist(airquality$Ozone)

qqnorm(iris$Sepal.Width, main="Sepal width")
qqline(iris$Sepal.Width,col=2)

qqnorm(airquality$Ozone, main="Ozone")
qqline(airquality$Ozone,col=2)

###

boxplot(iris[,-5])

##

titanic.data<-table(TitanicSurvival[,c(1,4)])
barplot(titanic.data, main = "Supervivientes del Titanic", xlab = "Clase",col = c("cadetblue4","aquamarine"))
legend("topleft", c("No sobrevive","Superviviente"), fill = c("cadetblue4","aquamarine"))

###

corr.res<-cor(mtcars)
corrplot(corr.res,method="circle")

####

corrplot.mixed(corr.res,upper="circle",number.cex=.7,tl.cex=.8)

###

shapiro.test(ToothGrowth$len)
leveneTest(ToothGrowth$len ~ ToothGrowth$supp)
t.test(ToothGrowth$len ~ ToothGrowth$supp)


##
shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)
shapiro.test(iris$Petal.Length)
shapiro.test(iris$Petal.Width)

corr.res<-cor(iris[,-5], method="spearman")
corr.res
corrplot.mixed(corr.res,upper="ellipse",number.cex=.9,tl.cex=.8)
