data()
data(package = .packages(all.available = TRUE))

#apply()

m1 <- matrix(c<-(1:10),nrow=5,ncol=6)
m1
a_m1 <- apply(m1,1,sum)#Manipulation on rows
a_m1
a_m1 <- apply(m1,2,sum)#Manipulation on columns
a_m1

#l in  lapply()
movies <- c("RED NOTICE","AVENGERS","FROZEN","BATMAN")
movies_lower <- lapply(movies,tolower)
str(movies_lower)

#sapply()
dt <- iris
head(dt)
sapply(dt,class)

#tapply()
data(iris)
tapply(iris$Sepal.Width,iris$Species,median)

#Visualization#Histogram
require(ggplot2)
data(diamonds)
head(diamonds)

hist(diamonds$carat,main="Carat Histogram", xlab = "Carat")

plot(price~carat,data = diamonds)
plot(diamonds$carat,diamonds$price)
boxplot(diamonds$carat)


ggplot(data = diamonds)+geom_histogram(aes(x=carat),stat ="bin",binwidth=1)
g <- ggplot(diamonds,aes(x=carat,y=price))
g+geom_point(aes(color=color))
g+geom_point(aes(color=color))+facet_wrap(~color)
g+geom_point(aes(color=color))+facet_grid(cut~clarity)

#Boxplot&Violin
ggplot(diamonds,aes(y=carat,x=cut))+geom_boxplot()
require(gridExtra)
p1 <- ggplot(diamonds,aes(y=carat,x=cut))+geom_point()+
geom_violin()
p2 <- ggplot(diamonds,aes(y=carat,x=cut))+geom_violin()+geom_point()
grid.arrange(p1,p2,ncol=2)

#Line Graph
data(economics)
head(economics)
ggplot(economics,aes(x=date,y=pop))+geom_line()

require(lubridate)##Create month and year columns
economics$year <- year(economics$date)
economics$month <- month(economics$date)##Subset the data
econ2000 <- economics[which(economics$year<=2000),]
head(econ2000)

g <- ggplot(econ2000,aes(x=month,y=pop))
g <- g + geom_line(aes(color=factor(year),group=year))
g <- g + scale_color_discrete(name="Year")
g <- g + labs(title = "Population Growth", x="Month",y="Population")
g


library(caret)
data("iris")
head(iris)
nrow(iris)
ncol(iris)

df <- data("iris")
df <- iris
dim(df)
names(df)
validation_index <- createDataPartition(df$Species,p=0.80,list=FALSE)
validation <- df[-validation_index,]
df <- df[validation_index,]
dim(df)
sapply(df,class)
head(df)
levels(df$Species)
percentage <- prop.table(table(df$Species))*100
cbind(freq=table(df$Species),percentage=percentage)
summary(df)
x <- df[,1:4]
y <- df[,5]
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(x[,i],main=names(iris)[i])
}
#barplot for class breakdown
plot(y)
featurePlot(x=x,y=y,plot="ellipse")

#box and whisker plots for each attribute
featurePlot(x=x,y=y,plot="box")
#density plots for each attribute by class value
scales <- list(x=list(relation="free"),
y=list(relation="free"))
featurePlot(x=x,y=y,plot="density",scales=scales)
control <- trainControl(method="cv",number=10)
metric <- "Accuracy"
set.seed(7)
fit.lda <- train(Species~.,data=df,method="lda",
                 metric=metric,trControl=control)
set.seed(7)
fit.cart <- train(Species~.,data=df,method="rpart",
                 metric=metric,trControl=control)
set.seed(7)
fit.knn<- train(Species~.,data=df,method="knn",
                 metric=metric,trControl=control)
set.seed(7)
fit.svm <- train(Species~.,data=df,method="svmRadial",
                 metric=metric,trControl=control)
set.seed(7)
fit.rf <- train(Species~.,data=df,method="rf",
                 metric=metric,trControl=control)
set.seed(7)
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn,
                          
                          svm=fit.svm, rf=fit.rf))

summary(results)
dotplot(results)

# summarize Best Model

print(fit.lda)

predictions <-predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
