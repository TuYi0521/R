mtcars
install.packages("psych")
library(psych)
summary(mtcars)
describe(mtcars)
install.packages("gmodels")
CrossTable(x=mtcars$cyl,y=low)
low<-mtcars$carb %in% c(1,2,3)
low
mtcars.df<-as.data.frame(mtcars)
mtcars.kc<-kmeans(mtcars.df,3)
mtcars.kc
library(flexclust)
install.packages("flexclust")
library(flexclust)
iclust(mtcars, nclusters=2)
mtcars

install.packages("sqldf")
library(sqldf)
set.seed("14792638")
mtdf<-as.data.frame(mtcars)
newdf<-sqldf("select * from mtdf where cyl=8")
newdf
plot(mtcars$disp,mtcars$mpg)
mt.pam<-pam(x=mt.df,k=3,metric="euclidean",stand=FALSE)
install.packages("pam")

mtpr2<-principal(mtcars, nfactors=3, rotate = "none")
mtpr2

mtsc<-scree(mtcars,factors = TRUE)
