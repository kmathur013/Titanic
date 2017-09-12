
library(ggplot2)
library(magrittr)
library(dplyr)
library(mice)
library(randomForest)

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

str(train)

summary(train)

train$title <- gsub('(.*, )|(\\..*)', '', train$Name)


table(train$Sex, train$title)

Rare_title <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady",
                "Major", "Rev", "Sir", "the Countess")

train$title[train$title == "Mlle"] <- "Miss"
train$title[train$title == "Mme"] <- "Mrs"
train$title[train$title == "Ms"] <- "Miss"

train$title[train$title %in% Rare_title] <- 'Rare_title'

strsplit(train$Name, split = '[,.]')[[1]][1]

train$Surname <- sapply(train$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

table(train$title, train$Survived)

ggplot(train, aes(x=title, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')+
  labs(x = 'Title') 

mosaicplot(table(train$title, train$Survived), main='Title by Survival', shade=TRUE)


#ggplot(train, aes(x=Surname, fill = factor(Survived))) +
#  geom_bar(stat='count', position='dodge')

#table(train$Surname, train$Survived >= 4)

strsplit(train$Cabin, NULL)[[2]][1]

train$deck = factor(sapply(train$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

str(train)
summary(train)

table(train$deck, train$Survived)

ggplot(train, aes(x=deck, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')+
  labs(x = 'Deck') 

table(train$Pclass, train$Survived)

ggplot(train, aes(x=Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Passenger Class') 

table(train$Pclass, train$deck)

train$family_size <- (train$SibSp + train$Parch +1)

table(train$family_size, train$Survived)


ggplot(train, aes(x=family_size, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Famiy Size') 


train$Age_Pclass <- (train$Age * train$Pclass)


table(train$Age_Pclass, train$Survived)


ggplot(train, aes(x=Age_Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Age Pclass Interaction') 

train$fare_per_pax <- (train$Fare/train$family_size)


ggplot(train, aes(x=fare_per_pax, fill = factor(Survived))) +
geom_line(aes( y = Survived, group = Pclass, color = "red"))



ggplot(train, aes(x=Embarked, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Embarkment') 


table(train$PassengerId, train$Embarked)

unlist(lapply(train, function(x) anyNA(x)))

colnames(train)[colSums(is.na(train)) > 0]

tmpmat <- as.matrix(table(train$PassengerId, train$Embarked))

which(tmpmat[,1] == 1)

train$Fare[c(62,830)]

train$Pclass[c(62,830)]

#Remove Passenger ID which contains missing Embarked station

#embark_fare <- train %>% filter(PassengerId !=62 & PassengerId !=830)

ggplot(train, aes(x= Embarked, y=Fare, fill=factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 80), color="red", linetype='dashed', lwd=2) 

train$Embarked[c(62,830)] <- 'C'


tmpmat1 <- as.matrix(table(train$PassengerId, train$Fare))

which(tmpmat1[,1] == 1)

summary(train$Fare)

ggplot(train[train$Pclass=='3'& train$Embarked =='S',],
       aes(x=Fare)) + 
  geom_density(fill= '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1)

train$family <- paste(train$Surname, train$family_size, sep='_')


sum(is.na(train$Age))
summary(train)
str(train)
factors_vars <- c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'title', 'Surname',
                  'family', 'family_size')

train[factors_vars] <- lapply(train[factors_vars], function(x) as.factor(x))

set.seed(129)

mice_mod <- mice(train[, !names(train) %in% 
            c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')

mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(train$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
train$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(train$Age))


pairs(~PassengerId +Pclass+ Sex+title+deck+Embarked, data=train)

ggplot(train, aes(Age, fill=factor(Survived))) +
  geom_histogram()+
  facet_grid(.~Sex)

train$child[train$Age < 18] <- 'Child'
train$child[train$Age >=18] <- 'Mother'

table(train$child, train$Survived)

train$mother <- 'Not Mother'

train$mother[train$Sex == 'female' & train$Parch > 0 & train$Age >=18 & train$title !='Miss'] <- 'Mother'

table(train$mother, train$Survived)

train$child <- as.factor(train$child)
train$mother <- as.factor(train$mother)

md.pattern(train)

set.seed(754)

rf_model <- randomForest(factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+
                           Fare+Embarked+title+family_size+child+mother, 
                         data=train)

plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


importance <- importance(rf_model)

varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x=reorder(Variables, Importance),
                           y=Importance, fill=Importance)) +
  geom_bar(stat = 'identity')+
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 


#Preprocessing Test Data

test$title <- gsub('(.*, )|(\\..*)', '', test$Name)

table(test$Sex, test$title)

Rare_title <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady",
                "Major", "Rev", "Sir", "the Countess")

test$title[test$title == "Mlle"] <- "Miss"
test$title[test$title == "Mme"] <- "Mrs"
test$title[test$title == "Ms"] <- "Miss"

test$title[test$title %in% Rare_title] <- 'Rare_title'


factors_vars_test <- c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'title')

test[factors_vars_test] <- lapply(test[factors_vars_test], function(x) as.factor(x))

set.seed(1219)

mice_mod_test <- mice(test[, !names(test) %in% 
                        c('PassengerId','Name','Ticket','Cabin') ], method='rf')

mice_output_test <- complete(mice_mod_test)


par(mfrow=c(1,2))
hist(test$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output_test$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
test$Age <- mice_output_test$Age

# Show new number of missing Age values
sum(is.na(test$Age))

# Repeat for Fare

test$Fare <- mice_output_test$Fare


par(mfrow=c(1,2))
hist(test$Fare, freq=F, main='Fare: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output_test$Fare, freq=F, main='Fare: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))


sum(is.na(test$Fare))

rf_model1 <- randomForest(factor(Survived) ~ title+Fare+Sex+Age+Pclass, 
                          data=train)

prediction <- predict(rf_model1, newdata=test)

str(test)
summary(test)

nlevels(train)
nlevels(test)


for(attr in colnames(train))
{
  if (is.factor(train[[attr]]))
  {
    new.levels <- setdiff(levels(train[[attr]]), levels(test[[attr]]))
    if ( length(new.levels) == 0 )
    { print(paste(attr, '- no new levels')) }
    else
    {
      print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
      levels(test[[attr]]) <- union(levels(test[[attr]]), levels(train[[attr]]))
    }
  }
}


str(train)
str(test)

test$Surname <- sapply(test$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
test$Surname <- as.factor(test$Surname)

test$deck = factor(sapply(test$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
test$family_size <- (test$SibSp + test$Parch +1)
test$Age_Pclass <- (test$Age * test$Pclass)































