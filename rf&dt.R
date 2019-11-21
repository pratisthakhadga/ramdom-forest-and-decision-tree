---
title: "Week 5 Assingment"
author: "Pratistha Khadga"
date: "October 28, 2019"
output: 
word_document: default
html: default
---

##Part 1 building a classifier using neural network
# Installing all the required packages   
install.packages("neuralnet")
install.packages("network")
install.packages("e1071")
library(network)
library(neuralnet)

# Removes all objects
rm(list=ls())

##importing the required data
shrooms <- read.csv('C:/Users/Pratistha/Documents/Data Science/Machine Learning/week 5/mushrooms.csv', 
                    header = FALSE, stringsAsFactors = TRUE)



colnames(shrooms) <- c('cap.shape', 'cap.surface', 'cap.color', 'bruises', 'odor', 
                       'gill.attachment', 'gill.spacing', 'gill.size', 'gill.color', 
                       'stalk.shape', 'stalk.root', 'stalk.surface.above.ring', 
                       'stalk.surface.below.ring', 'stalk.color.above.ring', 
                       'stalk.color.below.ring', 'veil.type', 'veil.color', 'ring.number', 
                       'ring.type', 'spore.print.color', 'population', 'habitat', 'class')

##Error : The factor function is not working
shrooms <- factor(shrooms)

shrooms <- data.frame(lapply(shrooms, as.numeric))
str(shrooms)
# Spliting data to traning set and test set. Traning set is 70% and test set is 30%
ind = sample(2, nrow(shrooms), replace = TRUE, prob=c(0.7, 0.3))
trainset = shrooms[ind == 1,]
testset = shrooms[ind == 2,]
# Difining classes of mushroom edible and poisonous
trainset$edible <- trainset$class == 'e'
trainset$poisonous <- trainset$class == 'p'

network.plus <- neuralnet(edible + poisonous ~ cap.shape + cap.surface + cap.color + bruises + odor + 
                            gill.attachment + gill.spacing + gill.size + gill.color + 
                            stalk.shape + stalk.root + stalk.surface.above.ring +
                            stalk.surface.below.ring + stalk.color.above.ring +
                            stalk.color.below.ring + veil.type + veil.color + ring.number +
                            ring.type + spore.print.color + population + habitat, data = trainset, hidden = 3, 
                          algorithm = "rprop+")

network.plus

#network$result.matrix
network.plus$result.matrix
#plot(network5h) 
plot(network.plus)
network.plus.1.5 <- neuralnet(edible + poisonous ~ cap.shape + cap.surface + cap.color + bruises + odor + 
                                gill.attachment + gill.spacing + gill.size + gill.color + 
                                stalk.shape + stalk.root + stalk.surface.above.ring +
                                stalk.surface.below.ring + stalk.color.above.ring +
                                stalk.color.below.ring + veil.type + veil.color + ring.number +
                                ring.type + spore.print.color + population + habitat, data = trainset, hidden = 1,
                              learningrate = 5, algorithm = "rprop+")

network.plus.1.5
plot(network.plus.1.5)


## Part 2 Building classifiers using SMV

library(e1071)
# Removes all objects so that there is no confusion in this process
rm(list=ls()) 
# As all object has been removed we need to import the data again
shrooms <- read.csv('C:/Users/Pratistha/Documents/Data Science/Machine Learning/week 5/mushrooms.csv', 
                    header = FALSE)
# Exploring and preparing the data 
shroom1 <- data.frame(lapply(shrooms[, 1:22], as.numeric))
shroom1 <- scale(shroom1)
shroom2 <- data.frame(shrooms[, 23])
shrooms <- cbind(shroom1, shroom2)

colnames(shrooms) <- c('cap.shape', 'cap.surface', 'cap.color', 'bruises', 'odor', 
                       'gill.attachment', 'gill.spacing', 'gill.size', 'gill.color', 
                       'stalk.shape', 'stalk.root', 'stalk.surface.above.ring', 
                       'stalk.surface.below.ring', 'stalk.color.above.ring', 
                       'stalk.color.below.ring', 'veil.type', 'veil.color', 'ring.number', 
                       'ring.type', 'spore.print.color', 'population', 'habitat', 'class')
rm(shroom1, shroom2)
write.csv(shrooms, 'D:/shrooms7.csv')
# As the variable vile type is not requires so its removed
shrooms$veil.type <- NULL
str(shrooms)
# Splliting the data into traning set 70% and test set 30%
ind = sample(2, nrow(shrooms), replace = TRUE, prob=c(0.7, 0.3))
trainset = shrooms[ind == 1,]
testset = shrooms[ind == 2,]

#### Using SVM Radial kernel as classifier
#formulae for radial kernel
model <- svm(class ~ ., data = trainset, kernel="radial", cost=1,
             gamma = 1/ncol(trainset))
##how the formule is being used
summary(model)
#Testing the data using radical kernel
predictions.radial <- predict(model, testset)
conf.matrix.radial <- table(testset$class, predictions.radial)
# display the confusion matrix
conf.matrix.radial

#### Using SVM Polynomial kernel as classifier
#formulae for polynomial kernel
model <- svm(class ~ ., data = trainset, kernel="polynomial", cost=1,
             gamma = 1/ncol(trainset))
# how the formulae is being used
summary(model)
# Testing the data using polynomial kernel as classifier
predictions.polynomial <- predict(model, testset)
conf.matrix.polynomial <- table(testset$class, predictions.polynomial)
# display the confusion matrix
conf.matrix.polynomial

#### Testing data using Sigmoid kernel
#formulae for sigmoid kernel
model <- svm(class ~ ., data = trainset, kernel="sigmoid", cost=1,
             gamma = 1/ncol(trainset))
# how the formulae is being used
summary(model)
# Testing the data using sigmoid kernel
predictions.sigmoid <- predict(model, testset)
conf.matrix.sigmoid <- table(testset$class, predictions.sigmoid)
# display the confusion matrix
conf.matrix.sigmoid
