## -------------------------------------------------------------------------------
knitr::purl("Mushrooms.Rmd")
library(randomForest)
library(Boruta)
library(corrplot)
library(tidyverse)
library(caret)
library(ggplot2)
mushrooms = read.csv("mushrooms.csv")
glimpse(mushrooms)


## -------------------------------------------------------------------------------
#Rename variable for the class variable
colnames(mushrooms)[1] = "edibility"

#Change each variable to a factor
mushrooms = mushrooms %>% map_df(function(.x) as.factor(.x))

#Set levels to each categorical variable
levels(mushrooms$edibility) = c("edible","poisonous")
levels(mushrooms$cap.shape) = c("bell", "conical", "flat","knobbed", "sunken", "convex")
levels(mushrooms$cap.surface) = c("fibrous", "grooves", "smooth", "scaly")
levels(mushrooms$cap.color) = c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(mushrooms$bruises) = c("no", "yes")
levels(mushrooms$odor) = c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushrooms$gill.attachment) = c("attached", "descending")
levels(mushrooms$gill.spacing) = c("close", "crowded")
levels(mushrooms$gill.size) = c("broad", "narrow")
levels(mushrooms$gill.color) = c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow")
levels(mushrooms$stalk.shape) = c("enlarging", "tapering")
levels(mushrooms$stalk.root) = c("missing", "bulbous", "club", "equal", "rooted")
levels(mushrooms$stalk.surface.above.ring) = c("fibrous", "silky", "smooth", "scaly")
levels(mushrooms$stalk.surface.below.ring) = c("fibrous", "silky", "smooth", "scaly")
levels(mushrooms$stalk.color.above.ring) =  c("buff", "cinnamon", "red", "gray", "brown", "orange", "pink", "white", "yellow")
levels(mushrooms$stalk.color.below.ring) = c("buff", "cinnamon", "red", "gray", "brown", "orange", "pink", "white", "yellow")
levels(mushrooms$veil.type) = ("partial")
levels(mushrooms$veil.color) = c("brown", "orange", "white", "yellow")
levels(mushrooms$ring.number) = c("none", "one", "two")
levels(mushrooms$ring.type) = c("evanescent", "flaring", "large", "none", "pendant")
levels(mushrooms$spore.print.color) = c("buff", "chocolate", "black", "brown", "orange", "green", "purple", "white", "yellow")
levels(mushrooms$population) = c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushrooms$habitat)= c("woods", "grasses", "leaves", "meadows", "paths", "urban", "waste")

#Check levels are properly changed
glimpse(mushrooms)



## -------------------------------------------------------------------------------
map_dbl(mushrooms, function(.x) {sum(is.na(.x))})


## -------------------------------------------------------------------------------
dt = sort(sample(nrow(mushrooms), nrow(mushrooms)*.7))
train = mushrooms[dt,]
test = mushrooms [-dt,]
set.seed(123)
boruta_train = Boruta(edibility~., train, doTrace =2)


## -------------------------------------------------------------------------------
plot(boruta_train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_train$ImpHistory),function(i)
boruta_train$ImpHistory[is.finite(boruta_train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(boruta_train$ImpHistory), cex.axis = 0.7)


## -------------------------------------------------------------------------------
#plot of the two most important variables
ggplot(mushrooms, aes(x = odor, y = spore.print.color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("dark green", "red"))


## -------------------------------------------------------------------------------
set.seed(100)
rf = randomForest(edibility ~ ., data=train, ntree=100, keep.forest=FALSE, importance=TRUE)

#OOB error vs. number of trees
plot(rf)

