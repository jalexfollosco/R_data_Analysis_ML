#codice unito


library("tidyr")
library("ggplot2")
library(dplyr)
library(magrittr)

#procedura logica

print(paste("Per questo IDE, la cartella di lavoro è impostata all' indirizzo: 
            ",getwd(),sep=''))

heart <- read.csv(paste(getwd(),"\\heart.csv",sep = ''))

#heart <- read.csv("C:/Users/JalexFollosco/Desktop/Its_RmachineL/esame/heart.csv"
#                  ,header = TRUE, stringsAsFactors = FALSE)

str(heart)
View(heart)

# TRAFORMAZIONE DELLE VARIABILI IN FORMALMENTE CORRETTI

# TRASFORMO SEX DA CHR IN FACTOR

heart$sex <- factor(heart$sex)
str(heart$sex) #-- notiamo che come livelli spunta unspecified
heart$sex[heart$sex == 'unspecified' ] <- NA # trasformiamo unspecified in NA
levels(heart$sex) # sui livelli è rimasto Unspecified, quindi lo rimuoviamo:
heart$sex <- factor(heart$sex)
levels(heart$sex) <- c('femmina', 'maschio')

# TRASFORMO CP DA INT A FACTOR

heart$cp <- factor(heart$cp)
levels(heart$cp) <- c('asintomatico', 'angina atipica', 'non anginale',
                      'angina tipica')

#TRASORMO CHOL DA CHR A INT

heart$chol[heart$chol == "undefined"] <- NA
heart$chol <- as.integer(heart$chol)

# TRASFORMO FBS DA INT A FACTOR

heart$fbs <- factor(heart$fbs)
levels(heart$fbs) <- c('<=120', '>120')

#TRASFORMO RESTECG DA INT A FACTOR

heart$restecg <- factor(heart$restecg)
levels(heart$restecg) <- c('ipertrofia', 'normale', 'anomalia ST-T')

#TRASFORMO EXANG DA INT IN FACTOR

heart$exang <- factor(heart$exang)
levels(heart$exang) <- c('no', 'si')

#TRASFORMO SLOPE DA INT IN FACTOR

heart$slope <- factor(heart$slope)
levels(heart$slope) <- c('in discesa', 'piatto', 'in salita')

#TRASFORMO CA DA INT IN FACTOR
heart$ca <- factor(heart$ca)

#TRASFORMO THAL DA INT IN FACTOR 
heart$thal <- factor(heart$thal)

#TRASFORMO TARGET DA INT IN FACTOR 
heart$target <- factor(heart$target)
levels(heart$target) <- c('malattia', 'no malattia')


str(heart)
summary(heart)

#notiamo che i livelli di ca vanno da 0-4 anziche 0-3, quindi i valori 4 lo 
#traformiamo in NA
# per thal 0 = valore eliminato dal precedente set di dati quindi lo traformiamo
#in NA

heart$ca[heart$ca == 4] <- NA
heart$ca <- factor(heart$ca)

heart$thal[heart$thal == 0] <- NA
heart$thal <- factor(heart$thal)
levels(heart$thal) <- c('difetto corretto', 'flusso sanguigno normale',
                        'difetto reversibile')

str(heart)
summary(heart)




#elimino tutte le na
apply(is.na(heart),2, sum)
heart <- na.omit(heart)
apply(is.na(heart),2, sum)

#elimino le colonne inutili

heart$x <- NULL
dim(heart)

#rinomino le colonne

names(heart)[c(3, 4, 5, 6, 7, 8, 9, 12)] <- c('chest_pain_type', 
'rest_blood_pressure', 'cholesterol','fbs', 'rest_electrocardio_result', 
                      'maximum_heart_rate', 'exercise_angina','n_ vessels' )

#RENDERE I DATI CONSISTENTI -------------------------------------


summary(heart)

#filtro ad age>0 e cancellare

heart <- heart[heart$age > 0,]

# filto a 'maximum_heart_rate' < 222 sostituiti dalla media delle
#frequenze cardiache

heart$maximum_heart_rate[heart$maximum_heart_rate > 222 ] <- 
  mean(heart$maximum_heart_rate)

#PULIZIA OUTLIERS -------------------------------------


#rest pressure con outliers

ggplot(heart, aes(y=rest_blood_pressure)) + geom_boxplot(outlier.colour="red", 
                                              outlier.shape=8,outlier.size=4)

heart %>% ggplot(mapping = aes(y=rest_blood_pressure,x=1)) + geom_boxplot(alpha
      = 0) + geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)

#1.5xIQR Rule.

q3 <- quantile(heart$rest_blood_pressure, .75)
q1 <- quantile(heart$rest_blood_pressure, .25)
iqr <- (q3 - q1) # differenza interquartile
minimum <- (q1 - (1.5 * iqr))
maximum <- (q3 + (1.5 * iqr))

# conto quante osservazioni hanno un valore non consistente
sum(heart$rest_blood_pressure < minimum | 
      heart$rest_blood_pressure > maximum)

# rimuovo gli outlier attraverso un filtro
heart <- heart[heart$rest_blood_pressure > minimum & 
                               heart$rest_blood_pressure < 
                               maximum, ]

# visualizzo il boxplot senza outliers
ggplot(heart, aes(y=rest_blood_pressure)) + geom_boxplot(outlier.colour="red", 
                                             outlier.shape=8,outlier.size=4)
# con vista della distribuzione
heart %>% ggplot(mapping = aes(y=rest_blood_pressure,x=1)) + geom_boxplot(alpha
       = 0) + geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)

#-----------------------------------------------------

#Cholesterol con outlieers
ggplot(heart, aes(y=cholesterol)) + geom_boxplot(outlier.colour="red", 
                                             outlier.shape=8,outlier.size=4)
# con vista della distribuzione
heart %>% ggplot(mapping = aes(y=cholesterol,x=1)) + geom_boxplot(alpha 
  = 0) + geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)

#1.5xIQR Rule.

q3_chol <- quantile(heart$cholesterol, .75)
q1_chol <- quantile(heart$cholesterol, .25)
iqr_chol <- (q3_chol - q1_chol) # differenza interquartile
minimum_chol <- (q1_chol - (1.5 * iqr_chol))
maximum_chol <- (q3_chol + (1.5 * iqr_chol))

# conto quante osservazioni hanno un valore non consistente
sum(heart$cholesterol < minimum_chol | heart$cholesterol > 
      maximum_chol)

# rimuovo gli outlier attraverso un filtro
heart <- heart[heart$cholesterol > minimum_chol & 
                               heart$cholesterol < maximum_chol, ]

# visualizzo il boxplot senza outliers
ggplot(heart, aes(y=cholesterol)) + geom_boxplot(outlier.colour="red", 
                                                 outlier.shape=8,outlier.size=4)
# con vista della distribuzione
heart %>% ggplot(mapping = aes(y=cholesterol,x=1)) + geom_boxplot(alpha 
  = 0) + geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)

#-------------------------------------

#oldpeak con outlieers
ggplot(heart, aes(y=oldpeak)) + geom_boxplot(outlier.colour="red", 
                                                 outlier.shape=8,outlier.size=4)
# con vista della distribuzione
heart %>% ggplot(mapping = aes(y=oldpeak,x=1)) + geom_boxplot(alpha 
  = 0) + geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)


#1.5xIQR Rule.

q3_oldpeak <- quantile(heart$oldpeak, .75)
q1_oldpeak <- quantile(heart$oldpeak, .25)
iqr_oldpeak <- (q3_oldpeak - q1_oldpeak) # differenza interquartile
minimum_oldpeak <- (q1_oldpeak - (1.5 * iqr_oldpeak))
maximum_oldpeak <- (q3_oldpeak + (1.5 * iqr_oldpeak))

# conto quante osservazioni hanno un valore non consistente
sum(heart$oldpeak < minimum_oldpeak | heart$oldpeak > 
      maximum_oldpeak)

# rimuovo gli outlier attraverso un filtro
heart <- heart[heart$oldpeak > minimum_oldpeak & 
                               heart$oldpeak < maximum_oldpeak, ]

# visualizzo il boxplot senza outliers
ggplot(heart, aes(y=oldpeak)) + geom_boxplot(outlier.colour="red", 
                                             outlier.shape=8,outlier.size=4)
# con vista della distribuzione
heart %>% ggplot(mapping = aes(y=oldpeak,x=1)) + geom_boxplot(alpha 
   = 0) + geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)


# controllo che i dati siano consistenti
summary(heart)


#-------analisi del dataset

#distribuzione di frequenza per variabili qualitative e distribuzione delle 
#variabili quantitative per avere una prima visione del dataset

plot(density(heart$age))

gender <- table(heart$sex)
barplot(gender, col = c("pink", "cyan"))

chest_pain <- table(heart$chest_pain_type)
barplot(chest_pain, col = c("green", "blue", "red", "orange"))

plot(density(heart$rest_blood_pressure))

plot(density(heart$cholesterol))

blood_sugar <- table(heart$fbs)
barplot(blood_sugar, col = c("blue", "cyan"))

ecg_result <- table(heart$rest_electrocardio_result)
barplot(ecg_result, col =  c("orange", "green", "red"))

plot(density(heart$maximum_heart_rate))

effort_angina <- table(heart$exercise_angina)
barplot(effort_angina, col =  c("green", "red"))

plot(density(heart$oldpeak))

tab_slope <- table(heart$slope)
barplot(tab_slope)

tab_vessel <- table(heart$`n_ vessels`)
barplot(tab_vessel)

tab_thal <- table(heart$thal)
barplot(tab_thal, col =  c("green", "white", "orange"))

tab_target <- table(heart$target)
barplot(tab_target, col =  c("orange", "green")) #--

#-------- ansalisi desccrittiva CON GGPLOT --------

# per sesso



ggplot(heart,aes(target,fill=sex)) + geom_bar(position = 'dodge') +
  scale_x_discrete(labels = heart$sex)


ggplot(heart, aes(y=rest_blood_pressure,x=sex)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)



ggplot(heart, aes(y=oldpeak,x=sex)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)


#-----------per target------


ggplot(heart, aes(y=age,x=target)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)


ggplot(heart,aes(target,fill=chest_pain_type)) + geom_bar(position = 'dodge') +
  scale_x_discrete(labels = heart$chest_pain_type)


ggplot(heart, aes(y=cholesterol,x=target)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)


ggplot(heart,aes(fbs,fill=target)) + geom_bar(position = 'dodge') +
  scale_x_discrete(labels = heart$target)


ggplot(heart, aes(y=maximum_heart_rate,x=target)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)

ggplot(heart, aes(y=oldpeak,x=target)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.5,color = "tomato",width = 0.2,height = 0.2)


#--- altre variabili---

plot(heart$age, heart$maximum_heart_rate, xlab = "età", 
     ylab = "frequenza cardiaca massima")#regressione
title(main = 'frequenza cardiaca massima per età')


plot(heart$cholesterol ~ heart$age, xlab = "età", 
     ylab = "cholesterolo") #regressione e fare 
#le predizioni
title(main = 'colesterolo per età')





#-------------REGRESSIONE LINEARE-----


eta<- heart$age
blood_pressure <- heart$rest_blood_pressure

plot(eta, blood_pressure, xlab = "eta", ylab = "pressione sanguigna")
# invertire l'ordine!
reg <- lm(blood_pressure ~ eta)
# per disegnare la retta di regressione lineare
abline (reg, col = "red")

segments(eta, fitted(reg), eta, blood_pressure, col = "blue", lty = 2)
title(main = "Regr.lin tra pressione sanguigna e eta")


summary(reg)
#b0 <- (Intercept)      105.80482 
#b1 <- age               0.43744
#blood_pressure=b0+b1*age

#Multiple R-squared:  0.07266

#--analisi dei residui e della distribuzione in quantili-----

# analisi dei residui
plot(reg$fitted, reg$residuals,
     main = "Residui")
abline(0, 0)
# distribuzione in quantili
qqnorm(reg$residuals)
qqline(reg$residuals)



#predizione---------------------

previsioni <- data.frame("eta" = c(30 ,40, 50, 60, 80, 70, 55, 45, 56, 77))
predict(reg, previsioni, interval = "confidence")


## CREAZIONE DEL DATASET PER IL TRAINING
# create a matrix of 80% of the rows in the original dataset that we can use for training
library(caret)

set.seed(2021)

# create a matrix of 80% of the rows in the original dataset that we can use for training
training_index <- createDataPartition(heart$target, p = .80, list = FALSE)
# select 80% of data to training the models
training_set <- heart[training_index, ]
nrow(training_set)
# use the remaining 20% of the data for test
test_set <- heart[-training_index, ]
nrow(test_set)

seed = set.seed(2021)
control <- trainControl(method = "cv", number = 10, seed = seed)
metric <- "Accuracy"


fit_lda <- train(target ~ ., data = training_set, metric = metric, trControl = 
                   control, 
                 method = "lda")

# nonlinear algorithms


## kNN
fit_knn <- train(target ~ ., data = training_set, metric = metric, trControl =
                   control, 
                 method = "knn")
## MLP
fit_mlp <- train(target ~ ., data = training_set, metric = metric, trControl = 
                   control, 
                 method = "mlp")

# advanced algorithms
## Random Forest
fit_rf <- train(target ~ ., data = training_set, metric = metric, trControl =
                  control, 
                method = "rf")
## SVM
fit_svm <- train(target ~ ., data = training_set, metric = metric, trControl =
                   control, 
                 method = "svmRadial")


results <- resamples(list(lda = fit_lda, knn = fit_knn, mlp =  fit_mlp, 
                          rf = fit_rf, svm = fit_svm))


summary(results)
dotplot(results)

summary(lda)