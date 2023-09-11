#Mert Erdinc SWP 04 - 615804

setwd("C:/Users/Administrator/Downloads")

library(mvtnorm)
library(gmnl)      
library(dplyr)
library(corrplot)
library(psych)
library(ggplot2)
library(MASS)
library(data.table)
library(tidyverse)
library(magrittr)
library(ggpubr)
library(tibble)
library(lattice)
library(car)
library(cluster)
library(mclust)
library(dbscan)
library(factoextra)

idList <- read.csv("idListBlueTooth.csv")
idList<-as.data.frame(idList)
VersionStudentId <- read.csv("Version_StudentID.csv")
subset(VersionStudentId,VersionStudentId$StudentId=="615804")

id<-idList$V43
id <- as.data.frame(id)

mxl_betai <- read.csv("mxl_betaibluetooth.csv")
indivData <- read.csv("indivData.csv")
MarketSimulation <- read.csv("marketsimulationbluetooth_swp4.csv")
data.cbc  <- read.csv("cbc_data.csv")
mxl_betai <- merge(mxl_betai,id, by="id")
indivData <- merge(indivData, id, by="id")
data.cbc  <- merge(data.cbc, id, by="id")
data.cbc  <- data.cbc[order(data.cbc[,1],data.cbc[,2],data.cbc[,3]),]


# check the dimensions
nrow(indivData) == 400 # the answer must be "TRUE"
nrow(mxl_betai) == 400 # the answer must be "TRUE"
nrow(data.cbc)/12/4 == 400 # the answer must be "TRUE"
------------------------------------------------------------------------------------------------
  
#  "Checking the "8" incomes, "3" genders and "5" educations to prevent the bias in the results"

table(indivData$Gender == "3") #12 3 genders
table(indivData$Income == "8") #51 8 incomes
table(indivData$Education == "5") #1 5 education #some are overlapping


idgender3 <- indivData$id[indivData$Gender==3]
idincome8 <- indivData$id[indivData$Income==8]
ideduc5 <- indivData$id[indivData$Education==5] 


biased_ids <- c(idgender3,idincome8, ideduc5)
biased_ids_unique <- unique(biased_ids)       #excluding the people that are included twice

mxl_betai_try <- filter(mxl_betai, !(id %in% biased_ids_unique))

(mxl_betai_try$id) == biased_ids_unique  # 1st check: every value is false


length(mxl_betai$id)
length(mxl_betai_try$id)
length(biased_ids_unique) #2nd check that they complement each other


biased_ids_unique %in% mxl_betai_try$id  #last check to make sure. 


mxl_betai<-mxl_betai_try


#Same procedure for the cbc data

data.cbc_try <- filter(data.cbc, !(id %in% biased_ids_unique))
length(data.cbc$id)
length(data.cbc_try$id)

biased_ids_unique %in% data.cbc_try$id  #check that correct ones are excluded

data.cbc<-data.cbc_try


#Same for the indivData

indivData <- indivData[indivData$Gender!= "3" & indivData$Income != "8" & indivData$Education != "5",] 

#drops rows where gender=3, rows where income=8 and education=5



# check the new dimensions
nrow(indivData) == 342 # the answer must be "TRUE"
nrow(mxl_betai) == 342 # the answer must be "TRUE"
nrow(data.cbc)/12/4 == 342 # the answer must be "TRUE"



--------------------------------------------------------------------------------
  
  
  
######################### 
#"IndivData Preparation"#
#########################  

indivData_orig<-indivData



head(indivData$Residence,20)
nrow(indivData[indivData$Residence == "Germany",]) #199 -> 58.19%
nrow(indivData) #342

indivData$Residence <- ifelse(indivData$Residence == "Germany", 1, 0)  #dummy coding residence Germanys
head(indivData$Residence,20)

indivData <- indivData[, -c(27,29,32,34,36)]  #excluding the character values of demographic info

head(indivData)
indivData['brand_awareness'] <- rowMeans(indivData[4:11])
indivData['knowledge'] <- rowMeans(indivData[12:16])
indivData['involvement'] <- rowMeans(indivData[17:21])
head(indivData)

#################################################
#"CBC table construction (interpretation later)"#
#################################################


#PART-WORTH CALCULATION

b1 <- mxl_betai$battery1
b2 <- mxl_betai$battery2
b3 <- mxl_betai$battery3
b4 <- mxl_betai$battery4

nrow(mxl_betai)


pw_battery5 <- -(b1+b2+b3+b4)/5
pw_battery1 <- b1+pw_battery5    
pw_battery2 <- b2+pw_battery5
pw_battery3 <- b3+pw_battery5
pw_battery4 <- b4+pw_battery5

cbc_table_battery_pw <- data.frame(pw_battery1, pw_battery2, pw_battery3, pw_battery4, pw_battery5 )
head(cbc_table_battery_pw)

summary(cbc_table_battery_pw)  #increasing means


cbc_table <- data.frame(pw_battery1, pw_battery2, pw_battery3, pw_battery4, pw_battery5 )



w1 <- mxl_betai$weight1  
w2 <- mxl_betai$weight2
w3 <- mxl_betai$weight3

pw_weight4 <- -(w1+w2+w3)/4
pw_weight1 <- w1+pw_weight4    
pw_weight2 <- w2+pw_weight4
pw_weight3 <- w3+pw_weight4

cbc_table_weight_pw <- data.frame(pw_weight1, pw_weight2, pw_weight3, pw_weight4)
head(cbc_table_weight_pw)

cbc_table$pw_weight1 <- w1+pw_weight4    
cbc_table$pw_weight2 <- w2+pw_weight4
cbc_table$pw_weight3 <- w3+pw_weight4
cbc_table$pw_weight4 <- -(w1+w2+w3)/4

head(cbc_table)

summary(cbc_table_weight_pw)  #decreasing means, in some levels bigger variation



s1 <- mxl_betai$sound1
s2 <- mxl_betai$sound2
s3 <- mxl_betai$sound3

pw_sound4 <- -(s1+s2+s3)/4
pw_sound1 <- s1+pw_sound4    
pw_sound2 <- s2+pw_sound4
pw_sound3 <- s3+pw_sound4

cbc_table_sound_pw <- data.frame(pw_sound1, pw_sound2, pw_sound3, pw_sound4)
head(cbc_table_sound_pw)

summary(cbc_table_sound_pw)  #increasing means

cbc_table$pw_sound1 <- s1+pw_sound4
cbc_table$pw_sound2 <- s2+pw_sound4
cbc_table$pw_sound3 <- s3+pw_sound4
cbc_table$pw_sound4 <- -(s1+s2+s3)/4

head(cbc_table)


#Ranges

range_price<-mxl_betai$price*(-0.8)
cbc_table$range_price <- range_price

mxl_betai$battery5<-0  
mxl_betai$weight4<-0   
mxl_betai$sound4<-0
head(mxl_betai)

max.battery<-apply(mxl_betai[,c("battery1","battery2","battery3","battery4","battery5")],1,max)
min.battery<-apply(mxl_betai[,c("battery1","battery2","battery3","battery4","battery5")],1,min)

range_battery<-max.battery-min.battery

cbc_table$range_battery <- range_battery


max.weight<-apply(mxl_betai[,c("weight1","weight2","weight3","weight4")],1,max)
min.weight<-apply(mxl_betai[,c("weight1","weight2","weight3","weight4")],1,min)
range_weight<-max.weight-min.weight
cbc_table$range_weight<-range_weight

mxl_betai$max.sound<-apply(mxl_betai[,c("sound1","sound2","sound3","sound4")],1,max)
mxl_betai$min.sound<-apply(mxl_betai[,c("sound1","sound2","sound3","sound4")],1,min)
range_sound<-mxl_betai$max.sound-mxl_betai$min.sound
cbc_table$range_sound <- range_sound

ranges <- cbind(range_price, range_battery, range_weight, range_sound)
summary(ranges)


#Importances

sum.range <- (cbc_table$range_price + cbc_table$range_battery + cbc_table$range_weight + cbc_table$range_sound)
imp_price   <- cbc_table$range_price/sum.range
imp_battery <- cbc_table$range_battery/sum.range
imp_weight  <- cbc_table$range_weight/sum.range
imp_sound   <- cbc_table$range_sound/sum.range
cbc_table$imp_price <- cbc_table$range_price/sum.range
cbc_table$imp_battery <- cbc_table$range_battery/sum.range
cbc_table$imp_weight  <- cbc_table$range_weight/sum.range
cbc_table$imp_sound   <- cbc_table$range_sound/sum.range

importances <- data.frame(imp_price, imp_battery, imp_weight, imp_sound)



#Willingness to Pay

wtp_battery1.vs.5 <- (mxl_betai$battery1/(-mxl_betai$price))*100 
wtp_battery2.vs.5 <- (mxl_betai$battery2/(-mxl_betai$price))*100 
wtp_battery3.vs.5 <- (mxl_betai$battery3/(-mxl_betai$price))*100
wtp_battery4.vs.5 <- (mxl_betai$battery4/(-mxl_betai$price))*100
wtp_battery5.vs.5 <- (mxl_betai$battery5/(-mxl_betai$price))*100 #this is 0 ofcourse.



wtp_battery <- cbind(wtp_battery1.vs.5, wtp_battery2.vs.5,
                     wtp_battery3.vs.5, wtp_battery4.vs.5,
                     wtp_battery5.vs.5)

cbc_table$wtp_battery1.vs.5 <- (mxl_betai$battery1/(-mxl_betai$price))*100
cbc_table$wtp_battery2.vs.5 <- (mxl_betai$battery2/(-mxl_betai$price))*100
cbc_table$wtp_battery3.vs.5 <- (mxl_betai$battery3/(-mxl_betai$price))*100
cbc_table$wtp_battery4.vs.5 <- (mxl_betai$battery4/(-mxl_betai$price))*100


wtp_weight1.vs.4 <- (mxl_betai$weight1/(-mxl_betai$price))*100
wtp_weight2.vs.4 <- (mxl_betai$weight2/(-mxl_betai$price))*100
wtp_weight3.vs.4 <- (mxl_betai$weight3/(-mxl_betai$price))*100

wtp_weight <- cbind(wtp_weight1.vs.4, wtp_weight2.vs.4,
                    wtp_weight3.vs.4)

cbc_table$wtp_weight1.vs.4 <- (mxl_betai$weight1/(-mxl_betai$price))*100
cbc_table$wtp_weight2.vs.4 <- (mxl_betai$weight2/(-mxl_betai$price))*100
cbc_table$wtp_weight3.vs.4 <- (mxl_betai$weight3/(-mxl_betai$price))*100     


wtp_sound1.vs.4 <- (mxl_betai$sound1/(-mxl_betai$price))*100
wtp_sound2.vs.4 <- (mxl_betai$sound2/(-mxl_betai$price))*100
wtp_sound3.vs.4 <- (mxl_betai$sound3/(-mxl_betai$price))*100

wtp_sound <- cbind(wtp_sound1.vs.4, wtp_sound2.vs.4, wtp_sound3.vs.4)

cbc_table$wtp_sound1.vs.4 <- (mxl_betai$sound1/(-mxl_betai$price))*100
cbc_table$wtp_sound2.vs.4 <- (mxl_betai$sound2/(-mxl_betai$price))*100
cbc_table$wtp_sound3.vs.4 <- (mxl_betai$sound3/(-mxl_betai$price))*100


data_agg<-cbind(indivData, cbc_table)

summary(data_agg)



#######################################
#"Preliminary MIXED LOGIT examination"#
#######################################


summary(mxl_betai)  #estimates shows the valuation for that level of the product's attribute with
#regard to the left out level



hist(mxl_betai$none, breaks =20,              #all negative, kind of a normal distribution
     xlab ="Estimates", main = "None Estimate Distribution")  
boxplot(mxl_betai$none)


hist(mxl_betai$price, breaks = 20,xlab ="Estimates", main = "Price Estimate Distribution")



mxl_batteries <- mxl_betai[,c(5:8)]
head(mxl_batteries)
boxplot(mxl_batteries, xlab ="  ", ylab ="Estimates from the model",
        main =" Estimates of Battery levels ") 

mxl_weights <- mxl_betai[,c(9:11)]
head(mxl_weights)
boxplot(mxl_weights, xlab ="", ylab ="  ",
        main =" Estimates of Weight levels ")  

mxl_sounds <- mxl_betai[,c(12:14)]
head(mxl_sounds)
boxplot(mxl_sounds, xlab ="", ylab ="",
        main =" Estimates of Sound levels ")  


boxplot_none<-boxplot(mxl_betai$none, horizontal = TRUE)
boxplot_none
mxl_betai$none > -2.349277 #outlier limit




boxplot_price <-boxplot(mxl_betai$price, horizontal = TRUE)
boxplot_price

indivData$id[mxl_betai$price < -19.676]
summary(mxl_betai$price)


mean(indivData$RelImp_price[mxl_betai$price < -19.676 ])  #checking the consistency of the results
mean(indivData$RelImp_price)


mean(importances$imp_price[mxl_betai$price < -19.676 ])
mean(importances$imp_price)
summary(indivData)

#Relimp price mean is 29.42, the mean of sensitive price individuals is 42.02
#The same is seen in imp_price also. 



indivData$id[mxl_betai$weight3 < 0 ]     #rel imp weight and normal imp weight
length(mxl_betai$id[mxl_betai$weight3 < 0 ]) #26 people

indivData$id[mxl_betai$weight2 < 0 ]      #15 is the same with weight 3 
indivData$id[mxl_betai$weight2 < 0 ]

indivData$id[mxl_betai$weight1 < 0 ]      #16 is the same with weight 3, 
                                          #and 18 of the negative weight1 values are same as negative
                                          #weight2 values. So kind of the same people

mean(indivData$RelImp_weight[mxl_betai$weight1 < 0 ])
mean(indivData$RelImp_weight[mxl_betai$weight2 < 0 ])
mean(indivData$RelImp_weight[mxl_betai$weight3 < 0 ])

mean(indivData$RelImp_weight)        #compare the means. An expected result that makes sense 

mean(importances$imp_weight[mxl_betai$weight1 < 0 ])
mean(importances$imp_weight[mxl_betai$weight2 < 0 ])
mean(importances$imp_weight[mxl_betai$weight3 < 0 ])

mean(importances$imp_weight)          #same with the weights





mean(indivData$RelImp_price[mxl_betai$sound1 > 0 ])
mean(indivData$RelImp_price[mxl_betai$sound2 > 0 ])
mean(indivData$RelImp_price[mxl_betai$sound3 > 0 ])
mean(indivData$RelImp_price)


mean(importances$imp_price[mxl_betai$sound1 > 0 ])  #45.54% lower
mean(importances$imp_price[mxl_betai$sound2 > 0 ])  #44.51% lower
mean(importances$imp_price[mxl_betai$sound3 > 0 ])  #29.15% lower
mean(importances$imp_price)




mean(importances$imp_price[mxl_betai$battery1 > 0 ]) #2.03% lower
mean(importances$imp_price[mxl_betai$battery2 > 0 ]) #21.38% lower
mean(importances$imp_price[mxl_betai$battery3 > 0 ]) #18.78% lower
mean(importances$imp_price[mxl_betai$battery4 > 0 ]) #15.41% lower

mean(importances$imp_price)
summary(importances$imp_price)                    #all of the differences in means makes sense











############################
#"CBC Table Interpretation"#
############################



#VISUALIZATION TRYS


boxplot ( cbc_table_battery_pw , xlab =" Part-worth values ", ylab ="",
          main =" Parth-worth values of Battery levels ",
          horizontal = TRUE )


plot(cbc_table_battery_pw) #some correlations are completely different 
                            #but it is hard to interpret from this plot


par ( mfrow =c(3,2) )
with (hist( pw_battery1,
            main =" Part-worth Battery 1",
                 xlab =" Product 1 Sales ", ylab =" Relative frequency ",
                 breaks =30, col=" lightblue ", freq = FALSE))

with (hist ( pw_battery2,
                main =" Part-worth Battery 2",
                  xlab =" Product 1 Sales ", ylab =" Relative frequency ",
                  breaks =30 , col=" lightblue ", freq = FALSE ))

with (hist ( pw_battery3,
             main =" Part-worth Battery 3",
                  xlab =" Product 1 Sales ", ylab =" Relative frequency ",
                  breaks =30 , col=" lightblue ", freq = FALSE ))

with (hist ( pw_battery4,
                  main =" Part-worth Battery 4",
                  xlab =" Product 1 Sales ", ylab =" Relative frequency ",
                  breaks =30 , col=" lightblue ", freq = FALSE ))

with (hist ( pw_battery5,
                  main =" Part-worth Battery 5",
                  xlab =" Product 1 Sales ", ylab =" Relative frequency ",
                  breaks =30 , col=" lightblue ", freq = FALSE ))

par ( mfrow =c(1,1) )
                      #mostly expected but there are some unexpected outliers



corrplot(corr = cor(mxl_betai[, c(3:14)], use = "complete.obs"), method = "ellipse")

     


summary(cbc_table_weight_pw)  #decreasing means, in some levels bigger variation
     

boxplot ( cbc_table_weight_pw , xlab ="  Part-worth values  ", ylab ="",
               main =" Part-worth values of Weight levels",
               horizontal = TRUE )
     

boxplot ( cbc_table_sound_pw , xlab ="  Part-worth values  ", ylab ="",
               main =" Part-worth values of Sound levels",
               horizontal = TRUE )
     

     
cor(ranges,importances)  #shows that naturally ranges and importances correlate positively
     
     
     
     
     
importances <- data.frame(imp_price, imp_battery, imp_weight, imp_sound)

head(importances)

boxplot ( importances , xlab =" Weekly sales ", ylab ="P2",
               main =" cbc_table_importances",
               horizontal = TRUE )
     
boxplot ( ranges , xlab =" Weekly sales ", ylab ="P2",
               main =" cbc_table_ranges",
               horizontal = TRUE )       #These two are the same because they use the same formula.
     
corrplot(cor(importances, use = "complete.obs"), method = "ellipse") #always some trade-off
summary(importances)                  #some are strong some are weaker
     



par ( mfrow =c(2,4) )
     
with(hist(range_price))
with(hist(range_battery))
with(hist(range_weight))
with(hist(range_sound))
with(hist(imp_price))
with(hist(imp_battery))
with(hist(imp_weight))
with(hist(imp_sound))
     
par(mfrow=c(1,1))
     
     
#Willingness to Pay 
     
     
#The interpretation: Shows the average required price reduction for people to 
#prefer products for example with battery 1 rather than battery 6.(When positive that means
#the consumer is willing to pay more for a reduction)
     

boxplot( wtp_battery,    #in line with our expectations and common sense
              main =" Battery WTP ", #assumption of CBC: people don't pay more for the same product.
              xlab =" WTP values ", ylab ="",
              horizontal = TRUE)
                                     #some people like when the battery gets lower

     

mean(cbc_table$imp_battery[wtp_battery1.vs.5 < -158.27]) #115.63% more
mean(cbc_table$imp_battery)
     
mean(indivData$RelImp_battery[wtp_battery1.vs.5 < -158.27]) #35.68% more
mean(indivData$RelImp_battery)
                                          #confirms the expectations
     
     
#Weight

     
boxplot_wtp_weight<-boxplot( wtp_weight,    #the positive values like bluetooths to be heavy
                                  main =" Weight WTP ",xlab =" WTP values ",
                                  horizontal = TRUE)
#also in line with common sense but the difference is not so big and there are some outliers
boxplot_wtp_weight
     

     
#Sound

     
boxplot_wtp_sound<-boxplot( wtp_sound,    
                                 main =" Sound WTP ",xlab =" WTP values ",
                                 horizontal = TRUE)
#in line with common sense with some outliers, big variation in the sound reductionfrom4 to 1


     
head(indivData[,c(26,32,33,34)])
newdf<- cbind(importances,wtp_battery, wtp_weight, wtp_sound)#, indivData[,c(26,32,33,34)]
head(newdf)
colnames(newdf) <- c("Price \n importance", "Battery \n importance", 
                          "Weight \n importance", "Sound \n importance", 
                          "Wtp_bat1", "Wtp_bat2", "Wtp_bat3", "Wtp_bat4",
                          "Wtp_weg1", "Wtp_weg1", "Wtp_weg3", 
                          "Wtp_snd1", "Wtp_snd2", "Wtp_snd3") #"Gender", "Awaren.",  "Knowl.", "Involv.")
     
corrplot.mixed(cor(newdf), lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7) #,number.cex = .7 ,,tl.cex=0.8

head(cbc_table)
wtps<- cbc_table[,c(22:31)]
     
     
#Main idea: if there is significant difference in the importances then there should be in WTPs as well
     
#important
gender_importances <- data.frame( 
female = c(colMeans(importances[indivData$Gender %in% 1,])), 
male = c(colMeans(importances[indivData$Gender %in% 2,])))
gender_importances
     
gender_wtps <- data.frame( 
female = c(colMeans(wtps[indivData$Gender %in% 1,])), 
male = c(colMeans(wtps[indivData$Gender %in% 2,])))
gender_wtps
     

     
#Checking Subsets

     
#Based on Gender
    

gender_importances <- data.frame( 
female = c(colMeans(importances[indivData$Gender %in% 1,])), 
male = c(colMeans(importances[indivData$Gender %in% 2,])))
gender_importances                   
#males give more importance to sound than females do.
 #females more to weight and sound
   #price is indifferent

     
colMeans(importances[indivData$Gender %in% 1,])
colMeans(importances[indivData$Gender %in% 2,])
summary(importances[indivData$Gender %in% 1,])
summary(importances[indivData$Gender %in% 2,])
     
     
     
#Based on Age

hist(indivData$Age)

nrow(importances[indivData$Age %in% 1,]) #1 person younger than 18
nrow(importances[indivData$Age %in% 2,]) #152 people  18-24 
nrow(importances[indivData$Age %in% 3,]) #134 people  25-29
nrow(importances[indivData$Age %in% 4,]) #26 people  30-34
nrow(importances[indivData$Age %in% 5,]) #6 people   35-49
nrow(importances[indivData$Age %in% 6,]) #1 person between 40-44
nrow(importances[indivData$Age %in% 7,]) #4 people between 45-49
nrow(importances[indivData$Age %in% 8,]) #18 people over 50

age_importances <- data.frame( "under 18" = c(importances[indivData$Age %in% 1,]), #doesn't work
                               "between 18-24" = c(importances[indivData$Age %in% 2,]),
                               "between 25-29" = c(importances[indivData$Age %in% 3,]),
                               "between 30-34" = c(importances[indivData$Age %in% 4,]),
                               "between 35-39" = c(importances[indivData$Age %in% 5,]),
                               "between 40-44" = c(importances[indivData$Age %in% 6,]),
                               "between 45-50" = c(importances[indivData$Age %in% 7,]),
                               "over 50" = c(importances[indivData$Age %in% 8,]))

age_importances_means <- data.frame( "under 18" = c(colMeans(importances[indivData$Age %in% 1,])), 
                                          "between 18-24" = c(colMeans(importances[indivData$Age %in% 2,])),
                                          "between 25-29" = c(colMeans(importances[indivData$Age %in% 3,])),
                                          "between 30-34" = c(colMeans(importances[indivData$Age %in% 4,])),
                                          "between 35-39" = c(colMeans(importances[indivData$Age %in% 5,])),
                                          "between 40-44" = c(colMeans(importances[indivData$Age %in% 6,])),
                                          "between 45-50" = c(colMeans(importances[indivData$Age %in% 7,])),
                                          "over 50" = c(colMeans(importances[indivData$Age %in% 8,])))

     
age_importances_means  
#people from 30 to 34 value less to price more to sound
#no diff in between 18-24 and 25-29 age groups
#In over 50 category (since it is relatively higher than other age groups)
#Imp price and sound decreases so they don't value the product
     
#Imp price and sound main drivers of consumers choices age-wise
#also gender wise
     


rel_imps <- indivData[,c(22:25)]
head(rel_imps)
     
     
age_rel_imps <- data.frame( "under 18" = c(colMeans(rel_imps[indivData$Age %in% 1,])), 
                                 "between 18-24" = c(colMeans(rel_imps[indivData$Age %in% 2,])),
                                 "between 25-29" = c(colMeans(rel_imps[indivData$Age %in% 3,])),
                                 "between 30-34" = c(colMeans(rel_imps[indivData$Age %in% 4,])),
                                 "between 35-39" = c(colMeans(rel_imps[indivData$Age %in% 5,])),
                                 "between 40-44" = c(colMeans(rel_imps[indivData$Age %in% 6,])),
                                 "between 45-50" = c(colMeans(rel_imps[indivData$Age %in% 7,])),
                                 "over 50" = c(colMeans(rel_imps[indivData$Age %in% 8,])))
age_rel_imps
hist(indivData$Age)
     
     
head(importances)
head(rel_imps)
newdf<- cbind(importances,rel_imps)
corrplot(cor(newdf))
     


tage_importances_means <- t(age_importances_means)
head(tage_importances_means,15)
imp_price_age_means <- tage_importances_means[,c(1)]
head(imp_price_age_means,20)
summary(imp_price_age_means)
hist(imp_price_age_means,breaks = 20)
     
     
par ( mfrow =c(1,1) )
par(mar=c(5,7,1,1))
boxplot( age_importances_means,    
              main =" Age Importances ",
              horizontal = TRUE, ylim = c(0,1), las=2) 
  
head(indivData)  #No insights from the category age
     

     
     
#Residency
     

head(indivData_orig)
head(importances)
importances$residence<-indivData_orig$Residence
importances[ , "residence"] <- indivData_orig$Residence
head(indivData_orig$Residence)
head(newdf)
     
head(importances)
nrow(dplyr::filter(importances, grepl('Germany', residence)))
nrow(dplyr::filter(importances, !grepl('Germany', residence)))
     
newdf<-dplyr::filter(importances, grepl('Germany', residence))
newdf1<-dplyr::filter(importances, !grepl('Germany', residence))  #indivData$Age %in% 2,
     
residency_importances <- data.frame( "Residence Germany" = c(colMeans(newdf[,1:4])), 
                                          "Residence Elsewhere" = c(colMeans(newdf1[,1:4])))
     
     
#                               "between 18-24" = c(colMeans(rel_imps[indivData$Age %in% 2,])),
     

     
residency_importances   
summary(indivData$Residence)#No difference.


par ( mfrow =c(1,1) )
par(mar=c(5,9,1,1))
boxplot( residency_importances,    
              main =" Residency Importances ",
              horizontal = TRUE, ylim = c(0,1), las=2) 
     
head(newdf)
     
importances$residence <- NULL
head(importances)
     
     
#Occupation
     
occupation_importances <- data.frame( "Employed" = c(colMeans(importances[indivData$Occupation %in% 1,])), 
                                           "Self-employed" = c(colMeans(importances[indivData$Occupation %in% 2,])),
                                           "Student" = c(colMeans(importances[indivData$Occupation %in% 3,])),
                                           "Unemployed" = c(colMeans(importances[indivData$Occupation %in% 4,])),
                                           "Retired" = c(colMeans(importances[indivData$Occupation %in% 5,])))
     
hist(indivData$Occupation)
t(occupation_importances)
     
occupation_wtps <- data.frame( "Employed" = c(colMeans(wtps[indivData$Occupation %in% 1,])), 
                                    "Self-employed" = c(colMeans(wtps[indivData$Occupation %in% 2,])),
                                    "Student" = c(colMeans(wtps[indivData$Occupation %in% 3,])),
                                    "Unemployed" = c(colMeans(wtps[indivData$Occupation %in% 4,])),
                                    "Retired" = c(colMeans(wtps[indivData$Occupation %in% 5,])))
     
t(occupation_wtps)
par(mar=c(5,9,1,1))
boxplot( occupation_importances,    
              main =" Residency Importances ",
              horizontal = TRUE, ylim = c(0,1), las=2)
     
nrow(importances[indivData$Occupation == 1,]) #109 employed 31.87%
nrow(importances[indivData$Occupation == 2,]) #18 self employed
nrow(importances[indivData$Occupation == 3,]) #202 student  59.06%
nrow(importances[indivData$Occupation == 4,]) #12 unemployed
nrow(importances[indivData$Occupation == 5,]) #1 retired
     
     
     
     
#Education
     
education_importances <- data.frame( "Less than Highschool" = c(colMeans(importances[indivData$Education %in% 1,])), 
                                          "Highschool" = c(colMeans(importances[indivData$Education %in% 2,])),
                                          "Undergraduate" = c(colMeans(importances[indivData$Education %in% 3,])),
                                          "Graduate" = c(colMeans(importances[indivData$Education %in% 4,])),
                                          "Other" = c(colMeans(importances[indivData$Education %in% 5,])))
     
education_importances
hist(indivData$Education)
t(education_importances)
education_wtps <- data.frame( "Less than Highschool" = c(colMeans(wtps[indivData$Education %in% 1,])), 
                                   "Highschool" = c(colMeans(wtps[indivData$Education %in% 2,])),
                                   "Undergraduate" = c(colMeans(wtps[indivData$Education %in% 3,])),
                                   "Graduate" = c(colMeans(wtps[indivData$Education %in% 4,])),
                                   "Other" = c(colMeans(wtps[indivData$Education %in% 5,])))
     
t(education_wtps)
nrow(importances[indivData$Education %in% 1,]) #3 less than highshools
nrow(importances[indivData$Education %in% 2,]) #86
nrow(importances[indivData$Education %in% 3,]) #110
nrow(importances[indivData$Education %in% 4,]) #143
nrow(importances[indivData$Education %in% 5,]) #0       
     
     
#Income
income_importances <- data.frame( "Less than 500" = c(colMeans(importances[indivData$Income %in% 1,])), 
                                       "500 to 1000" = c(colMeans(importances[indivData$Income %in% 2,])),
                                       "1000 to 1500" = c(colMeans(importances[indivData$Income %in% 3,])),
                                       "1500 to 2000" = c(colMeans(importances[indivData$Income %in% 4,])),
                                       "2000 to 2500" = c(colMeans(importances[indivData$Income %in% 5,])),
                                       "2500 to 3000" = c(colMeans(importances[indivData$Income %in% 6,])),
                                       "more than 3000" = c(colMeans(importances[indivData$Income %in% 7,])))
     
hist(indivData$Income)
t(income_importances)     #intuitive results
#Interesting: from 500 limit to 1000 limit the change in imp price is higher
#interesting: after income 2500 changes the imp_price variable
     
nrow(importances[indivData$Income %in% 7,]) #27 people, sound is very important
nrow(importances[indivData$Income %in% 6,]) #5 people only
nrow(importances[indivData$Income %in% 5,]) #24 people 
nrow(importances[indivData$Income %in% 4,]) #23 people 
nrow(importances[indivData$Income %in% 3,]) #59 people 
nrow(importances[indivData$Income %in% 2,]) #125 people
nrow(importances[indivData$Income %in% 1,]) #79 people 
     
     
#Own
own_importances <- data.frame( "Own" = c(colMeans(importances[indivData$Own %in% 0,])), 
                                    "Don't Own" = c(colMeans(importances[indivData$Own %in% 1,])))
     
hist(indivData$Own)
t(own_importances) #people who don't own values the sound more 

own_wtps <- data.frame( "Own" = c(colMeans(wtps[indivData$Own %in% 0,])), 
                             "Don't Own" = c(colMeans(wtps[indivData$Own %in% 1,])))
     
t(own_wtps)

     
     
#Intent to Buy
     
intenttobuy_importances <- data.frame( "Yes" = c(colMeans(importances[indivData$IntentToBuy %in% 0,])), 
                                            "No" = c(colMeans(importances[indivData$IntentToBuy %in% 1,])))
     
hist(indivData$IntentToBuy)
t(intenttobuy_importances) #No difference. 
     
     
intenttobuy_wtps <- data.frame( "Yes" = c(colMeans(wtps[indivData$IntentToBuy %in% 0,])), 
                                     "No" = c(colMeans(wtps[indivData$IntentToBuy %in% 1,])))
     
t(intenttobuy_wtps) #No difference.
     
     
     

     
     
     
     
     
#Correlations
     
par ( mfrow =c(1,1) )
data_agg<-cbind(indivData, cbc_table)
summary(data.agg)
length(data.agg)
head(data_agg)
corrplot.mixed(cor(data_agg[,c(2,3,26,27,28,29,30,31,32,33,34,52,53,54,55)]),
                    lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7)
     
#Insight: Males value sound more than females and females value battery and weight more than males.
     
     
     
data_agg<-cbind(indivData, cbc_table)
summary(indivData)
     
summary(data_agg)
length(data_agg)
     
newdf<- cbind(indivData[, 22:25], cbc_table[, 18:21])  
head(newdf)
corrplot(corr = cor(newdf[,c(1:8)], use = "complete.obs"), method = "ellipse") #shows that importance
#confirms and matches with the indivData                          #levels match in both of the studies                 
     
     
head(data_agg)
corrplot.mixed(cor(data_agg[,c(2,3,26,27,28,29,30,31,32,33,34,51,52,53,54)]), #example
                    lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7)
     
corrplot.mixed(cor(data_agg[,c(2:10,51,52,53,54)]), 
                    lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7)
#no correlation between importances and brand awarenesses
#People who know Philips also know Sony.
     
ncol(data_agg)

str(cbc_table)
     
cor(importances)   #shows that there is always trade of between the importances
#with a strong one between price and sound.
     

     
     
     
     
#Checking the subsets manually could be done for the part-worth of the levels as well as 
#WTPs (so, not only importances) but these level of details will be tried to be found with clustering.
     
     
     
     
     
     

#Clustering
     
     
     
MarketSimulation    
MarketSimulation$price<-MarketSimulation$price/100 
     
imps <- data_agg[, c(52:55)] # id  counter x columns are excluded
head(data_agg)
Length(data_agg)
head(imps)
     


seg.summ <- function (data , groups) 
     {aggregate (data , list(groups), function (x) mean(as.numeric (x)))}
     



     
     
#The MDSs start here!
     
     
dist_try <- dist(apply(data_agg, 2, scale))         
dist_try0<- daisy(apply(data_agg, 2, scale),metric="gower") 
     
head()
     
fit <- cmdscale(dist_try, k = 2) #metric MDS scale
fit1 <- isoMDS(dist_try, k = 2) #non-metric MDS scale
fit0 <- cmdscale(dist_try0,k = 2) #metric MDS gower
fit_1 <- isoMDS(dist_try0,k = 2)#non-metric MDS gower
     
head(dist_try,20)
x0 <- fit0[,1]
y0 <- fit0[,2]
x_1<- fit_1$points[,1]
y_1<- fit_1$points[,2]
x1 <- fit1$points[,1]
y1 <- fit1$points[,2]
x <- fit[,1]
y <- fit[,2]
p <- ggplot(as.data.frame(fit),aes(V1, V2)) + #
geom_point(size = 1.75) +                    #the spread of the individuals' similarities.
        theme_minimal() +
       theme(text = element_text(size=10),
plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
p + ggtitle("Metric MDS with Indiv data & Cbc table (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
     
MDS_gower_all<- ggplot(as.data.frame(fit0),aes(V1,V2)) + 
       geom_point(size = 1.75) +                    
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
     
MDS_gower_all + ggtitle("Metric MDS with Indiv data & Cbc table (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
     
nMDS_gower_all<- ggplot(as.data.frame(fit_1),aes(x_1,y_1)) + 
       geom_point(size = 1.75) +                    
       theme_minimal() +                              
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
     
nMDS_gower_all + ggtitle("Non-metric MDS with Indiv data & Cbc table (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
     
     
nMDS_scale_all<- ggplot(as.data.frame(fit1), aes(x1, y1) ) +       #non-metric MDS scale all data
       geom_point(size = 1.75) + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
     
nMDS_scale_all + ggtitle("Non-metric MDS with Indiv data & Cbc table (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
#So, there is no clusters natural clusters in the whole data, as one would expect
#But we will see that there will be none when using imps wtps and pws as well
     
     


     
#Now using the imps
     
summary(data_agg)  
head(data_agg)
head(imps)
     
     
dist_imp_gower <- daisy(apply(imps,2,scale),metric="gower")
tmp_scale <- apply(imps,2,scale)
dist_imp_scale <- dist(tmp_scale, method="euclidean")
     
     
fit2 <- isoMDS(dist_imp_scale, k = 2) #non-metric MDS   
fit3 <- cmdscale(dist_imp_scale, k = 2)
fit4 <- isoMDS(dist_imp_gower, k = 2)
fit5 <- cmdscale(dist_imp_gower, k = 2)
     
x2 <- fit2$points[,1]
y2 <- fit2$points[,2]
x3 <- fit3[,1]
y3 <- fit3[,2]
x4 <- fit4$points[,1]
y4 <- fit4$points[,2]
x5 <- fit5[,1]
y5 <- fit5[,2]
     
     
#Using Importances there is no very distinct clusters   
     
par ( mfrow =c(1,1) )
     
#the terminology used is as scale for the euclidean and gower for gower     

     
MDS_scale_imps <- ggplot(as.data.frame(fit3), aes(x3, y3)) +   #scale, using imps, metric MDS
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_scale_imps + ggtitle("Metric MDS with importances (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
     
     
MDS_gower_imps <- ggplot(as.data.frame(fit5), aes(x5, y5)) +   #gower, using imps, metric MDS
       geom_point(size = 1.75) +                   
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),         #probably two clusters are driven by price and sound
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_gower_imps + ggtitle("Metric MDS with importances (gower)") + theme(plot.title = element_text(hjust = 0.5))
par ( mfrow =c(1,1) )
     
     
nMDS_scale_imps <- ggplot(as.data.frame(fit2), aes(x2, y2)) +   #scale, using imps, non-metric MDS
       geom_point(size = 1.75) + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),                     
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),           
             axis.title.y = element_blank())           
nMDS_scale_imps + ggtitle("Non-metric MDS with importances (scale)") + theme(plot.title = element_text(hjust = 0.5))  
     
     
nMDS_gower_imps <- ggplot(as.data.frame(fit4), aes(x4, y4)) + 
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
nMDS_gower_imps + ggtitle("non-metric MDS with importances (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
     
#So, there is some shape in the distribution of the people but no indication of clear clusters
     
     
     
     
     
#Checking if WTPs form clusters
     
head(cbc_table)
wtps<- cbc_table[,c(22:32)]
head(wtps)                  #Problematic because some WTPs are positive some are negative etc.
     
summary(wtps)     

     
     
dist_wtps_gower <- daisy(apply(wtps,2,scale),metric="gower")
tmp_scale <- apply(wtps,2,scale)
dist_wtps_scale <- dist(tmp_scale, method="euclidean")
     
fit6 <- isoMDS(dist_wtps_scale, k = 2) #non-metric MDS   
fit7 <- cmdscale(dist_wtps_scale, k = 2)
fit8 <- isoMDS(dist_wtps_gower, k = 2)
fit9 <- cmdscale(dist_wtps_gower, k = 2)  

x6 <- fit6$points[,1]
y6 <- fit6$points[,2]
x7 <- fit7[,1]
y7 <- fit7[,2]
x8 <- fit8$points[,1]
y8 <- fit8$points[,2]
x9 <- fit9[,1]
y9 <- fit9[,2]
     
     
     
     
nMDS_scale_wtps <- ggplot(as.data.frame(fit6), aes(x6, y6)) +   
       geom_point(size = 1.75) + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),                     
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),            
             axis.title.y = element_blank())           
     nMDS_scale_wtps +  ggtitle("non-metric MDS with WTPs (scale)") + theme(plot.title = element_text(hjust = 0.5))                                                 # nMDS_scale_imps +  ggtitle("non-Metric importances scale"
     
MDS_scale_wtps <- ggplot(as.data.frame(fit7), aes(x7, y7)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
     MDS_scale_wtps + ggtitle("metric MDS with WTPs (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
nMDS_gower_wtps <- ggplot(as.data.frame(fit8), aes(x8, y8)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
nMDS_gower_wtps + ggtitle("Non-metric MDS with WTPs (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
MDS_gower_wtps <- ggplot(as.data.frame(fit9), aes(x9, y9)) +   
       geom_point(size = 1.75) +                          
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
     MDS_gower_wtps + ggtitle("Metric MDS with WTPs (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
     
#WTPs are biased to use for clustering due to their nature but it can be seen that there are some outliers
     
     

     
     
     
#Checking if Part-worths form clusters
     
head(cbc_table)
pws<- cbc_table[,c(1:13)]
head(pws)                   
     
dist_pws_gower <- daisy(apply(pws,2,scale),metric="gower")
tmp_scale <- apply(pws,2,scale)
dist_pws_scale <- dist(tmp_scale, method="euclidean")

fit10 <- isoMDS(dist_pws_scale, k = 2) #non-metric MDS  
fit11 <- cmdscale(dist_pws_scale, k = 2)
fit12 <- isoMDS(dist_pws_gower, k = 2)
fit13 <- cmdscale(dist_pws_gower, k = 2)   

x10 <- fit10$points[,1]
y10 <- fit10$points[,2]
x11 <- fit11[,1]
y11 <- fit11[,2]
x12 <- fit12$points[,1]
y12 <- fit12$points[,2]
x13 <- fit13[,1]
y13 <- fit13[,2]
     
     

nMDS_scale_pws <- ggplot(as.data.frame(fit10), aes(x10, y10)) +  
       geom_point(size = 1.75) + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),                     
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),         
             axis.title.y = element_blank())           
nMDS_scale_pws +  ggtitle("Non-metric MDS with Part-worths (scale)") + theme(plot.title = element_text(hjust = 0.5))                                              
     
nMDS_gower_pws <- ggplot(as.data.frame(fit12), aes(x12, y12)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
nMDS_gower_pws + ggtitle("Non-metric MDS with Part-worths (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
MDS_scale_pws <- ggplot(as.data.frame(fit11), aes(x11, y11)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
     MDS_scale_pws + ggtitle("Metric MDS with Part-worths (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
     
     
MDS_gower_pws <- ggplot(as.data.frame(fit13), aes(x13, y13)) +  
       geom_point(size = 1.75) +                           
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),              
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_gower_pws + ggtitle("Metric MDS with Part-worths (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
     
#Part-worths also uses estimates directly so they are potential cluster inputs.
#But it is known from prior investigation that price and sound are important drivers of consumers choices
#and since partworths don't have a price column and since price estimates do play a role in consumers choices
#as can be seen from corrplot between price estimates and importances(used because it is a good representative
#of the behavior drivers) they won't be used.
     



     
     
#Trying to combine pws with price and plot to see if it worths to cluster
newdf<- cbind(pws, mxl_betai$price)
head(newdf)
dist_pricepws_gower <- daisy(apply(newdf,2,scale),metric="gower")
tmp_scale <- apply(newdf,2,scale)
dist_pricepws_scale <- dist(tmp_scale, method="euclidean")
     
fit14 <- isoMDS(dist_pricepws_scale, k = 2) #non-metric MDS   
fit15 <- cmdscale(dist_pricepws_scale, k = 2)
fit16 <- isoMDS(dist_pricepws_gower, k = 2)
fit17 <- cmdscale(dist_pricepws_gower, k = 2)
x14 <- fit14$points[,1]
y14 <- fit14$points[,2]
x15 <- fit15[,1]
y15 <- fit15[,2]
x16 <- fit16$points[,1]
y16 <- fit16$points[,2]
x17 <- fit17[,1]
y17 <- fit17[,2]
     
MDS_gower_pws <- ggplot(as.data.frame(fit14), aes(x14, y14)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_gower_pws + ggtitle("Non-metric MDS with Part-worths + price estimates (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
MDS_gower_pws <- ggplot(as.data.frame(fit16), aes(x16, y16)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_gower_pws + ggtitle("Non-metric MDS with Part-worths + price estimates (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
MDS_gower_pws <- ggplot(as.data.frame(fit15), aes(x15, y15)) +  
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_gower_pws + ggtitle("Metric MDS with Part-worths + price estimates (scale)") + theme(plot.title = element_text(hjust = 0.5))
     
MDS_gower_pws <- ggplot(as.data.frame(fit17), aes(x17, y17)) +   
       geom_point(size = 1.75) +                            
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),               
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
MDS_gower_pws + ggtitle("Metric MDS with Part-worths + price estimates (gower)") + theme(plot.title = element_text(hjust = 0.5))
     
     
     
     
     

     
#Clustering PWs
     
     
seg.summ(cbc_table,seg.hc.3)
seg.summ(indivData,seg.hc.3) #Best results from imp clusters
seg.summ(cbc_table,seg.hc.g.3)
seg.summ(indivData,seg.hc.g.3)
seg.summ(cbc_table,seg.hc.4)
seg.summ(indivData,seg.hc.4)
seg.summ(cbc_table,seg.hc.g.4)
seg.summ(indivData,seg.hc.g.4)
     
newdf<- cbind(pws, mxl_betai$price)
head(newdf)
dist_pricepws_gower <- daisy(apply(newdf,2,scale),metric="gower")
tmp_scale <- apply(newdf,2,scale)
dist_pricepws_scale <- dist(tmp_scale, method="euclidean")
     
seg.hc.newdf_scale <- hclust(dist_pricepws_scale, method ="ward.D2") #other methods too
plot(seg.hc.newdf_scale)
plot(rev(seg.hc.newdf_scale$height^2))
     
     
seg.hc.newdf_scale3 <- cutree(seg.hc.newdf_scale, k=3)    
table(seg.hc.newdf_scale3)
     
seg.hc.newdf_gower <- hclust(dist_pricepws_gower, method ="ward.D2") #other methods too
plot(seg.hc.newdf_gower)
plot(rev(seg.hc.newdf_gower$height^2))
     
     
seg.hc.newdf_gower3 <- cutree(seg.hc.newdf_gower, k=3)    
table(seg.hc.newdf_gower3)
     
     
seg.summ(cbc_table,seg.hc.newdf_scale3) #trying scale and gower 3 clusters  
seg.summ(cbc_table,seg.hc.newdf_gower3)  #with part-worhts+price estimates
     

fit_scale_pricepws <-isoMDS(dist_pricepws_scale,k=2)    
x20 <- fit_scale_pricepws$points[,1]
y21 <- fit_scale_pricepws$points[,2] 
phc3_scale_pricepws_nmds <- ggplot(as.data.frame(fit_scale_pricepws) %>% add_column(cluster = factor(seg.hc.newdf_scale3)), aes(x20, y21, color = cluster)) +
       labs(title = "Hierarchical Clustering with 3 Clusters (Scalenewdf ,non-metric mds)") +
       geom_point() + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
phc3_scale_pricepws_nmds  + ggtitle("Hclust with 3 clusters Part-worths+Price estimates Non-metric MDS (scale)") + theme(plot.title = element_text(hjust = 0.5))                   
#with gower the clusters seem more clear.
     
     
     
fit_gower_pricepws<-isoMDS(dist_pricepws_gower,k=2)    
x22 <- fit_gower_pricepws$points[,1]
y23 <- fit_gower_pricepws$points[,2] 
phc3_gower_pricepws_nmds <- ggplot(as.data.frame(fit_gower_pricepws) %>% add_column(cluster = factor(seg.hc.newdf_gower3)), aes(x22, y23, color = cluster)) +
       labs(title = "Hierarchical Clustering with 3 Clusters (Gowernewdf ,non-metric mds)") +
       geom_point() + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
phc3_gower_pricepws_nmds + ggtitle("Hclust with 3 clusters Part-worths+Price estimates Non-metric MDS (gower)") + theme(plot.title = element_text(hjust = 0.5))         
     
     
     
phc3_scale_imps_nmds #with hclust only imps 3 clusters    #done below
phc3_gower_imps_nmds #with hclust only imps 3 clusters
     
phc3_scale_pricepws_nmds #same but with pws and price estimates
phc3_gower_pricepws_nmds   #similar to the one done with imps
#these are not bad (the same comparision needs to be done for 4 clusters too)
     
     
#Now the same with 4 clusters
     
#------same template as above!
seg.summ(cbc_table,seg.hc.3)
seg.summ(indivData,seg.hc.3) #Best results from imp clusters
seg.summ(cbc_table,seg.hc.g.3)
seg.summ(indivData,seg.hc.g.3)
seg.summ(cbc_table,seg.hc.4)
seg.summ(indivData,seg.hc.4)
seg.summ(cbc_table,seg.hc.g.4)
seg.summ(indivData,seg.hc.g.4)
     
newdf<- cbind(pws, mxl_betai$price)
head(newdf)
dist_pricepws_gower <- daisy(apply(newdf,2,scale),metric="gower")
tmp_scale <- apply(newdf,2,scale)
dist_pricepws_scale <- dist(tmp_scale, method="euclidean")
     
seg.hc.newdf_scale <- hclust(dist_pricepws_scale, method ="ward.D2") #other methods too
plot(seg.hc.newdf_scale)
plot(rev(seg.hc.newdf_scale$height^2))
     
     
seg.hc.newdf_scale4 <- cutree(seg.hc.newdf_scale, k=4)    
table(seg.hc.newdf_scale4)
     
seg.hc.newdf_gower <- hclust(dist_pricepws_gower, method ="ward.D2") #other methods too
plot(seg.hc.newdf_gower)
plot(rev(seg.hc.newdf_gower$height^2))
     
seg.hc.newdf_gower4 <- cutree(seg.hc.newdf_gower, k=4)    #table looks good
table(seg.hc.newdf_gower4)
     
     
seg.summ(cbc_table,seg.hc.newdf_scale4)
seg.summ(cbc_table,seg.hc.newdf_gower4)
seg.summ(cbc_table,seg.hc.4) #done later
seg.summ(cbc_table,seg.hc.g.4)              
     
     
     
fit_scale_pricepws <-isoMDS(dist_pricepws_scale,k=2)    
x20 <- fit_scale_pricepws$points[,1]
y21 <- fit_scale_pricepws$points[,2] 
phc4_scale_pricepws_nmds <- ggplot(as.data.frame(fit_scale_pricepws) %>% add_column(cluster = factor(seg.hc.newdf_scale4)), aes(x20, y21, color = cluster)) +
       labs(title = "Hierarchical Clustering with 4 Clusters (Scalenewdf ,non-metric mds)") +
       geom_point() + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
phc4_scale_pricepws_nmds + ggtitle("Hclust with 4 clusters Part-worths+Price estimates Non-metric MDS (scale)") + theme(plot.title = element_text(hjust = 0.5))                        
#with gower the clusters seem more clear.
     
     
fit_gower_pricepws<-isoMDS(dist_pricepws_gower,k=2)    
x22 <- fit_gower_pricepws$points[,1]
y23 <- fit_gower_pricepws$points[,2] 
phc4_gower_pricepws_nmds <- ggplot(as.data.frame(fit_gower_pricepws) %>% add_column(cluster = factor(seg.hc.newdf_gower4)), aes(x22, y23, color = cluster)) +
       labs(title = "Hierarchical Clustering with 3 Clusters (Gowernewdf ,non-metric mds)") +
       geom_point() + 
       theme_minimal() +
       theme(text = element_text(size=10),
             plot.title = element_text(size = 11, hjust = 0.5), 
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank())
phc4_gower_pricepws_nmds + ggtitle("Hclust with 4 clusters Part-worths+Price estimates Non-metric MDS (gower)") + theme(plot.title = element_text(hjust = 0.5))                        
     
phc4_scale_imps_nmds #with hclust only imps 4 clusters
phc4_gower_imps_nmds #with hclust only imps 4 clusters    #this looks good #done below
     
phc4_scale_pricepws_nmds 
phc4_gower_pricepws_nmds                    #so gower4 with imps seems to do better   
     
#----same template!
     
     
     
#How clusters look like with using Part-worths without price estimates
     
     
dist_pws_gower <- daisy(apply(pws,2,scale),metric="gower")
tmp_scale <- apply(pws,2,scale)
dist_pws_scale <- dist(tmp_scale, method="euclidean")
     
seg.hc.scale.pws <- hclust (dist_pws_scale , method ="ward.D2") 
plot(seg.hc.scale.pws)
plot(rev(seg.hc.scale.pws$height^2))

seg.hc.scale.pws3 <- cutree(seg.hc.scale.pws, k=3)    
table(seg.hc.scale.pws3)
seg.summ(cbc_table,seg.hc.scale.pws3)
           
           
seg.hc.gower.pws <- hclust (dist_pws_gower , method ="ward.D2") 
seg.hc.gower.pws3 <- cutree(seg.hc.gower.pws, k=3)
           

           
           
           
           
           
           
#Clustering using importances
           
           
           

# Decision: Model to implement
           
#Hierarchical
           
gower_imps <- daisy(apply(imps,2,scale),metric="gower")

tmp <- apply(imps, 2, scale)
scale_imps <- dist(tmp, method="euclidean")
           
head(scale_imps)
head(gower_imps)  #checking that the results are different
           
           
seg.hc <- hclust(scale_imps, method ="single") #other methods too
plot(seg.hc)
plot(rev(seg.hc$height^2))
table(seg.hc)
           
seg.hc.segment <- cutree(seg.hc, k=6)    #single linkage method not effective
table(seg.hc.segment)

seg.hc <- hclust (gower_imps, method ="single")#as expected not effective with gower metric too.
           
           
           
seg.hc <- hclust (scale_imps , method ="complete") #No, slightly left skewed dendogram
seg.hc <- hclust (scale_imps , method ="average") #No
seg.hc <- hclust (scale_imps , method ="centroid") #No
           
           
           
           
#3 clusters w/ ward
           
seg.hc <- hclust (scale_imps , method ="ward.D2") #scale
plot(seg.hc)
plot(rev(seg.hc$height^2))
seg.hc.3 <- cutree(seg.hc, k=3)  #one cluster price imp, one sound, the other some price and sound also
#but more importantly includes most weights and batteries.
table(seg.hc.3)
seg.summ(cbc_table,seg.hc.3) 
seg.summ(indivData,seg.hc.3)
seg.summ(cbc_table,seg.hc.g.3) #done below.
seg.summ(indivData,seg.hc.g.3)
           
seg.hc.g <- hclust (gower_imps , method ="ward.D2") #gower
plot(seg.hc.g)                    #the dendogram looks more equally divided
plot(rev(seg.hc.g$height^2))
seg.hc.g.3 <- cutree(seg.hc.g, k=3)
table(seg.hc.g.3)
seg.summ(cbc_table,seg.hc.g.3)
seg.summ(indivData,seg.hc.g.3)  #when sound imp increases brand awareness and knowledge slightly 
#increased too.
#Normal scaled dist matrix seems to have performed better w/ 3 clusters.
           

           
           
           
#Trying to plot the hclust with 3 cluster results
fit_scale_imps<-cmdscale(scale_imps,2)    
phc3_scale_imps_mds <- ggplot(as.data.frame(fit_scale_imps) %>% add_column(cluster = factor(seg.hc.3)), aes(V1, V2, color = cluster)) +
             labs(title = "Hierarchical Clustering with 3 Clusters scale mds") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
           abline(h = 0, v = 0, col = "grey")
           
phc3_scale_imps_mds
           
fit_gower_imps<-cmdscale(gower_imps,2)    
phc3_gower_imps_mds <- ggplot(as.data.frame(fit_gower_imps) %>% add_column(cluster = factor(seg.hc.3)), aes(V1, V2, color = cluster)) +
             labs(title = "Hierarchical Clustering with 3 Clusters (Gower ,metric mds)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
phc3_gower_imps_mds                              
           
           
fit_scale_imps_nmds<-isoMDS(scale_imps, k=2)   
x <- fit_scale_imps_nmds$points[,1]
y <- fit_scale_imps_nmds$points[,2] 
phc3_scale_imps_nmds <- ggplot(as.data.frame(fit_scale_imps_nmds) %>% add_column(cluster = factor(seg.hc.3)), aes(x, y, color = cluster)) +
             labs(title = "Hierarchical Clustering with 3 Clusters, scale non-mds") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
           abline(h = 0, v = 0, col = "grey")
           
phc3_scale_imps_nmds
#with gower the clusters seem more clear.
           
           
           
           
fit_gower_imps<-isoMDS(gower_imps,k=2)    #with gower non-metric mds
x <- fit_gower_imps$points[,1]
y <- fit_gower_imps$points[,2] 
phc3_gower_imps_nmds <- ggplot(as.data.frame(fit_gower_imps) %>% add_column(cluster = factor(seg.hc.3)), aes(x, y, color = cluster)) +
          labs(title = "Hierarchical Clustering with 3 Clusters (Gower ,non-metric mds)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
phc3_gower_imps_nmds                        #with gower the clusters seem more clear.
           
phc3_scale_imps_nmds
phc3_gower_imps_nmds
           
           
           
           

           
# Now with 4 segments               Same with 4 segments
           
seg.hc.g.4 <- cutree(seg.hc.g, k=4)
seg.hc.4 <- cutree(seg.hc, k=4) #With gover there are price, sound, price-sound and battery-
seg.summ(cbc_table, seg.hc.4)                    #weight categories, and groups are more equally divided in numbers.
seg.summ(cbc_table, seg.hc.g.4)                  #However without gover each cluster is divided as price,sound,
seg.summ(indivData, seg.hc.4)                                   #battery and weight. Tho not equally divided without gover seems better.
seg.summ(indivData, seg.hc.g.4 )
#Note: part-worths and importances seemed correlated.Not really. 
           
table(seg.hc.g.4)
table(seg.hc.4)
           
newdf<- seg.summ(cbc_table, seg.hc.g.4)
newdf1<-seg.summ(indivData, seg.hc.g.4 )
newdf2<-cbind(newdf,newdf1)
finaldf<-newdf2[,c(19,20,21,22,59,60,64,65,66,67)]
finaldf
#High battery & weight imps have lower brand awareness and knowledge.
#Females seem to put more importance on weight and battery and males to sound. Price is in the middle.
cor(cbc_table[,c(1:21)])
           

           
fit_scale_imps_nmds<-isoMDS(scale_imps, k=2)   
fit_gower_imps_nmds<-isoMDS(gower_imps,k=2)   
x <- fit_gower_imps$points[,1] 
y <- fit_gower_imps$points[,2] 
           

#Trying to plot the hclust with 4 cluster results
fit_scale_imps<-cmdscale(scale_imps,2) 
           
phc4_scale_imps_mds <- ggplot(as.data.frame(fit_scale_imps) %>% add_column(cluster = factor(seg.hc.4)), aes(V1, V2, color = cluster)) +
         labs(title = "Hierarchical Clustering with 4 Clusters scale mds") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
           abline(h = 0, v = 0, col = "grey")
           
phc4_scale_imps_mds                      #bad result
seg.summ(cbc_table,seg.hc.4)
           
           
           
           
           
#trying with data_agg
fit_data_agg_scale_nmds
phc4_scale_imps_mds <- ggplot(as.data.frame(fit_data_agg_gower_nmds) %>% add_column(cluster = factor(seg.hc.g.4)), aes(xg_all, yg_all, color = cluster)) +
             labs(title = "xHierarchical Clustering with 4 Clusters scale mds") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
           abline(h = 0, v = 0, col = "grey")              #also bad result with gower and scaling
phc4_scale_imps_mds
           
           
fit_gower_imps<-cmdscale(gower_imps,2)    #with gower metric mds 4 clusters
phc4_gower_imps_nmds <- ggplot(as.data.frame(fit_gower_imps) %>% add_column(cluster = factor(seg.hc.g.4)), aes(V1, V2, color = cluster)) +
             labs(title = "Hclust with 4 Clusters (Dissimilarity,Gower ,Metric MDS)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
phc4_gower_imps_nmds                              #the result doesn't look bad.
seg.summ(cbc_table,seg.hc.g.4)
           
           
fit_scale_imps_nmds<-cmdscale(scale_imps, k=2)   #scale non-mds hclust imps 4 clusters
x <- fit_scale_imps_nmds[,1]
y <- fit_scale_imps_nmds[,2] 
phc4_scale_imps_nmds <- ggplot(as.data.frame(fit_scale_imps_nmds) %>% add_column(cluster = factor(seg.hc.4)), aes(x, y, color = cluster)) +
             labs(title = "Hclust with 4 Clusters, (Distance, Metric MDS)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),         #bad results
                   axis.title.y = element_blank())
           abline(h = 0, v = 0, col = "grey")

phc4_scale_imps_nmds
           
           
           
           
fit_gower_imps<-isoMDS(gower_imps,k=2)    #with gower metric mds
x <- fit_gower_imps$points[,1]
y <- fit_gower_imps$points[,2] 
phc4_gower_imps_mds <- ggplot(as.data.frame(fit_gower_imps) %>% add_column(cluster = factor(seg.hc.g.4)), aes(x, y, color = cluster)) +
           labs(title = "Hierarchical Clustering with 4 Clusters (Gower ,non-metric mds)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
phc4_gower_imps_mds                        #with gower the clusters seem more clear.
           
#clustering shows that there are no clear clusters and decisions like scale/gower 
#method or other interpretations should be done by the researcher
           
#fitting imps, wtps and pws without clustering shows that there is no clear clusters anyways.
           
           
           
           
#-----------------------------------------------------------------------------------           
corrplot(cor(cbc_table), method = "ellipse")
newdf<-cbind(indivData, importances)
corrplot(cor(newdf), method = "ellipse") 
           
#Insights: males have higher subj knowledge and care more about sound
#while females care more about battery and weight than males.
           
#people with higher sound importance have higher income as expected.
           
#k=3 using importances, first cluster price imp, second price imp sound imp combined
#third cluster only sound imp.
           
#Note: there seems to be a positive correlation between sound imp and involvement.
#------------------------------------------------------------------------------------- 

plot(rev(seg.hc$height^2))
seg.hc.segment <- cutree(seg.hc, k=4)
table(seg.hc.segment)
seg.summ(cbc_table,seg.hc.segment)
seg.summ(cbc_table,seg.hc.g.segment) #done below
           
           

           
#Trying to plot the hclust results
fit_scale<-cmdscale(scale_imps,2)    
phc3_scale <- ggplot(as.data.frame(fit_scale) %>% add_column(cluster = factor(seg.hc.segment)), aes(V1, V2, color = cluster)) +
             labs(title = "Hierarchical Clustering with 5 Clusters (Scale)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
phc3_scale
           
fit_gower<-cmdscale(gower_imps,2)    
phc3_gower <- ggplot(as.data.frame(fit_gower) %>% add_column(cluster = factor(seg.hc.g.segment)), aes(V1, V2, color = cluster)) +
             labs(title = "Hierarchical Clustering with 5 Clusters (Gower)") +
             geom_point() + 
             theme_minimal() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
phc3_gower
           
           
           
           
           
#with 5 segments
seg.hc.g.segment <- cutree(seg.hc.g, k=5)
seg.hc.segment <- cutree(seg.hc, k=5)
           
table(seg.hc.segment)
table(seg.hc.g.segment)
           
seg.summ(indivData,seg.hc.segment) 
seg.summ(cbc_table,seg.hc.segment)  #every imp category is found and there is one extra with middle to high
           #price and sound imps, 5 clusters seem not to be necessary.
           
seg.summ(cbc_table,seg.hc.g.segment)
seg.summ(indivData,seg.hc.g.segment) #two vague clusters, weight imp was not put to a specific cluster
           
           
           
           
           
#K-means
           
           
#3clusters
           

           
ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
set.seed(i+100)         #gower_imps to be done too.
tmp <- kmeans(scale_imps , centers =3) #with 3 clusters, scale_imps, the set seed to 102
ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
summary(ss.all)
ss.all[which.max(ss.all$fit), ]
ss.all[which.min(ss.all$fit), ]
           
set.seed (102) 
           
#3 clusters with gower
ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
set.seed(i+100)         
tmp <- kmeans(gower_imps , centers =3) #with 3 clusters, scale_imps, the set seed to 102
ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
           
set.seed (102) 

seg.k.scaled3 <- kmeans(scale_imps , centers =3)
seg.k.gower3 <- kmeans(gower_imps , centers =3)
table(seg.k.scaled3$cluster)
table(seg.k.gower3$cluster)
seg.summ(cbc_table, seg.k.scaled3$cluster )
seg.summ(indivData, seg.k.scaled3$cluster )    #the clusters are not effective
seg.summ(cbc_table, seg.k.gower3$cluster )
seg.summ(indivData, seg.k.gower3$cluster )     #the clusters are not effective
           
           
           
#4 clusters
ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
set.seed(i+100)
tmp <- kmeans(scale_imps , centers =4) #with 4 clusters and scaled imps, set the seed to 102 also.
ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
summary(ss.all)
str(ss.all)
ss.all[which.max(ss.all$fit), ]
           
ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
set.seed(i+100)
tmp <- kmeans(gower_imps , centers =4) #with 4 clusters and scaled imps, set the seed to 102 also.
ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
ss.all[which.max(ss.all$fit), ]
           
set.seed (102) 
seg.k.scaled4 <- kmeans(scale_imps , centers =4)
set.seed (103) 
           
seg.k.gower4 <- kmeans(gower_imps , centers =4)#with 4 clusters and scaled imps, set the seed to 103 also.
           
table(seg.k.scaled4$cluster)
table(seg.k.gower4$cluster)
           
seg.summ(cbc_table, seg.k.scaled4$cluster )
seg.summ(indivData, seg.k.scaled4$cluster ) #imp battery and imp weight is in the same cluster
#there is also one extra and vague cluster
#hclust() was better
           
seg.summ(cbc_table, seg.k.gower4$cluster )
seg.summ(indivData, seg.k.gower4$cluster )  #clusters mixed
           
           
#5 clusters
set.seed (120) 
           
ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
set.seed(i+100)
tmp <- kmeans(scale_imps , centers =5) #with 5 clusters and scaled imps the set seed to 120
ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
summary(ss.all)                      #close to be useful but not, try it with gower
ss.all[which.max(ss.all$fit), ]
table(seg.k.scaled5$cluster)
           
           
ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
set.seed(i+100)
tmp <- kmeans(gower_imps , centers =5) #with 5 clusters and gower imps, set the seed to 106.
ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
ss.all[which.max(ss.all$fit), ]
set.seed(120)
seg.k.scaled5 <- kmeans(scale_imps , centers =5)
set.seed(106)
seg.k.gower5 <- kmeans(gower_imps , centers =5)

table(seg.k.scaled5$cluster)
table(seg.k.gower5$cluster)
seg.summ(cbc_table, seg.k.scaled5$cluster) #vague and not so clear clusters
seg.summ(indivData, seg.k.scaled5$cluster)
seg.summ(cbc_table, seg.k.gower5$cluster) #same for gower
seg.summ(indivData, seg.k.gower5$cluster)




#seg.df.num$income
par ( mfrow =c(2,3) )
#par(mar=c(5,9,1,1))
with(boxplot(cbc_table[,c(1)]~seg.k.scaled4$cluster ,   #one can examine the part-worths using boxplot easier.
                  xlab ="Income", ylab ="Segment",        #but boxplot numbers would be pws*clusters.So doesn't work.
                        horizontal = TRUE ))  
with(boxplot(cbc_table[,c(2)]~seg.k.scaled4$cluster ,   
                        xlab ="Income", ylab ="Segment",        
                        horizontal = TRUE )) 
with(boxplot(cbc_table[,c(3)]~seg.k.scaled4$cluster ,   
                        xlab ="Income", ylab ="Segment",        
                        horizontal = TRUE )) 
with(boxplot(cbc_table[,c(4)]~seg.k.scaled4$cluster ,   #part worths 2 and 5 have opposite shapes.
                        xlab ="Income", ylab ="Segment",        
                        horizontal = TRUE )) 
with(boxplot(cbc_table[,c(5)]~seg.k.scaled4$cluster ,   
                        xlab ="Income", ylab ="Segment",        
                        horizontal = TRUE ))
par ( mfrow =c(1,1) )
           
boxplot(cbc_table[,c(1)]~seg.hc.segment$group ,   #one can examine the part-worths using boxplot easier.
                   xlab ="Income", ylab ="Segment",        #but boxplot numbers would be pws*clusters.So doesn't work.
                   horizontal = TRUE )
head(cbc_table)
head(cbc_table[,c(1:5)])
head(cbc_table_battery_pw)
cbc_table_weight_pw
cbc_table_sound_pw

library ( cluster )
clusplot(data_agg , seg.k.scaled5$cluster , color =TRUE , shade =TRUE ,
                    labels =4, lines =0, main ="K- means cluster plot ")
           
           
           
#Mclust
           
           
#3 clusters
seg.mc.scaled3 <- Mclust(scale_imps, G=3) # use all defaults
seg.mc.gower3 <- Mclust(gower_imps, G=3) # use all defaults
           
summary (seg.mc.scaled3)
summary (seg.mc.gower3)
           
seg.summ(cbc_table, seg.mc.scaled3$class ) #couldn't even put battery and weight imps to same cluster
seg.summ(indivData, seg.mc.scaled3$class )
seg.summ(cbc_table, seg.mc.gower3$class ) #same for gower clusters
seg.summ(indivData, seg.mc.gower3$class )
           
           
#4 clusters
seg.mc.scaled4 <- Mclust(scale_imps, G=4) # use all defaults
seg.mc.gower4 <- Mclust(gower_imps, G=4) # use all defaults
           
summary (seg.mc.scaled4)
summary (seg.mc.gower4)
           
seg.summ(cbc_table, seg.mc.scaled4$class ) #there are sound and price imps but battery and weight
#was not clustered to a specific cluster
seg.summ(indivData, seg.mc.scaled4$class )
seg.summ(cbc_table, seg.mc.gower4$class ) #only sound is clustered to a cluster
seg.summ(indivData, seg.mc.gower4$class )
           
           
#5 clusters
seg.mc.scaled5 <- Mclust(scale_imps, G=5) # use all defaults
seg.mc.gower5 <- Mclust(gower_imps, G=5) # use all defaults
           
summary (seg.mc.scaled5)
summary (seg.mc.gower5)
           
seg.summ(cbc_table, seg.mc.scaled5$class ) #there are sound and price imps but battery and weight
#was not clustered to a specific cluster
seg.summ(indivData, seg.mc.scaled5$class )
seg.summ(cbc_table, seg.mc.gower5$class )#weight&battery is not clustered properly, there are vague clusters
           
seg.summ(indivData, seg.mc.gower5$class )
           
           

           
           
#DBSCAN
           
library(dbscan)
           
# choose optimal eps
dbscan::kNNdistplot(data_agg, k =  4)
abline(h = 0.5, lty = 2)
           
set.seed(123)
           
# compute DBSCAN using dbscan package
db_all <- dbscan::dbscan(data_agg, 0.5, 3)
print(db)
           
library(fpc)
           
# compute DBSCAN using fpc package
fpc.db_all <- fpc::dbscan(fit, eps = 0.5, MinPts = 3)
           
# make sure the results of both packages are identical
all(fpc.db$cluster == db$cluster)
           
ggplot(as.data.frame(data_agg) %>% add_column(cluster = factor(db_all$cluster)), aes(x, y, color = cluster)) +
labs(title = "DBSCAN with 3 Clusters and Outliers") +
             geom_point() + 
             theme_bw() +
             theme(text = element_text(size=10),
                   plot.title = element_text(size = 11, hjust = 0.5), 
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())
           


           
#end of the clustering   
           
           
--------------------------------------------------------------------------------
             
             

           
#Market Share (Predicted choice) calculations
           

           

head(mxl_betai)
head(mxl_betai_try)
           
head(MarketSimulation)
           


predict.mxl <- function(X, theta) {
eu <- exp(as.matrix(X) %*% t(theta))
p <- t(eu) / colSums(eu)
#  return(p)
return(colMeans(p))
}
tmp<-predict.mxl(MarketSimulation, mxl_betai)
tmp
           
#Calculating the market share using First Choice Method (not realistic)

           
           
MarketSimulation
           
           

head(mxl_betai)
mxl.betai<-mxl_betai[,c(-1,-2,-15:-20)]
head(mxl.betai)
           
#############################################################################
####
#### Predict Market shares for information given in MarketSimulation
####
#############################################################################
           
#Calculate the market shares or predict the probability and check how the shares correlate with 
#other variables in mxl, cbc table and indivdata. Even a regression can be run to see the significances and so on.
           
MarketSimulation_p1_p2_0.7 <- MarketSimulation
MarketSimulation_p1_p2_0.7[c(1,2),c(2)] <-0.7       
head(MarketSimulation_p1_p2_0.7)
           
eu_try <- exp(as.matrix(MarketSimulation_p1_p2_0.7) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_p2_0.7 <- t(eu_try) / colSums(eu_try) #all individuals probability exponents are in the columns
nrow(ind_utilities_p1_p2_0.7)                         #These are rather individual probabilities
head(ind_utilities_p1_p2_0.7)                         #but it is just coded as ind_utilities
shares_p1_p2_0.7<-colMeans(ind_utilities_p1_p2_0.7)
shares_p1_p2_0.7
tmp

MarketSimulation_p1_p2_0.9 <- MarketSimulation
MarketSimulation_p1_p2_0.9[c(1,2),c(2)] <-0.9       
head(MarketSimulation_p1_p2_0.9)
           
eu_try <- exp(as.matrix(MarketSimulation_p1_p2_0.9) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_p2_0.9 <- t(eu_try) / colSums(eu_try) 
nrow(ind_utilities_p1_p2_0.9)
head(ind_utilities_p1_p2_0.9)
shares_p1_p2_0.9 <- colMeans(ind_utilities_p1_p2_0.9)
shares_p1_p2_0.9
shares                     #when the prices remain the same there is no big variation in the shares
           
           
MarketSimulation_p1_p2_1.1 <- MarketSimulation
MarketSimulation_p1_p2_1.1[c(1,2),c(2)] <-1.1       
head(MarketSimulation_p1_p2_1.1)
           
eu_try <- exp(as.matrix(MarketSimulation_p1_p2_1.1) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_p2_1.1 <- t(eu_try) / colSums(eu_try) #all individuals' probability exponents are in the columnls
nrow(ind_utilities_p1_p2_1.1)
head(ind_utilities_p1_p2_1.1)
shares_p1_p2_1.1 <- colMeans(ind_utilities_p1_p2_1.1)
shares_p1_p2_1.1
ind_utilities_p1_p2_1.1
           
           
           
MarketSimulation_p1_p2_1.3 <- MarketSimulation
MarketSimulation_p1_p2_1.3[c(1,2),c(2)] <-1.3      
head(MarketSimulation_p1_p2_1.3)
           
eu_try <- exp(as.matrix(MarketSimulation_p1_p2_1.3) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_p2_1.3 <- t(eu_try) / colSums(eu_try) 
nrow(ind_utilities_p1_p2_1.3)
head(ind_utilities_p1_p2_1.3)
shares_p1_p2_1.3 <- colMeans(ind_utilities_p1_p2_1.3)
shares_p1_p2_1.3
head(ind_utilities_p1_p2_1.3)
           
MarketSimulation_p1_p2_1.5 <- MarketSimulation
MarketSimulation_p1_p2_1.5[c(1,2),c(2)] <-1.5      
head(MarketSimulation_p1_p2_1.5)
           
eu_try <- exp(as.matrix(MarketSimulation_p1_p2_1.5) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_p2_1.5 <- t(eu_try) / colSums(eu_try) 
nrow(ind_utilities_p1_p2_1.5)
head(ind_utilities_p1_p2_1.5)
shares_p1_p2_1.5 <- colMeans(ind_utilities_p1_p2_1.5)
shares_p1_p2_1.5
head(ind_utilities_p1_p2_1.5)
           
shares <- cbind(shares_p1_p2_0.7, shares_p1_p2_0.9, shares_p1_p2_1.1,shares_p1_p2_1.3,shares_p1_p2_1.5)
shares
           
           
           
           
           
           
predict.mxl <- function(X, theta) {
eu <- exp(as.matrix(X) %*% t(theta))
p <- t(eu) / colSums(eu)
#  return(p)
return(colMeans(p))
}
shares_p1_p2_1<-predict.mxl(MarketSimulation, mxl.betai)
shares_p1_p2_1

eu_try <- exp(as.matrix(MarketSimulation) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_p2_1 <- t(eu_try) / colSums(eu_try) 
nrow(ind_utilities_p1_p2_1)
head(ind_utilities_p1_p2_1)
boxplot(ind_utilities_p1_p2_1, horizontal = TRUE)
shares_p1_p2_1 <- colMeans(ind_utilities_p1_p2_1)
shares_p1_p2_1
           
           
newdf<- cbind(mxl.betai, ind_utilities_p1_p2_1)
corrplot.mixed(cor(newdf), #example
lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7)
           
cor_mxl.betai_p1_p2_1 <- cor(mxl.betai, ind_utilities_p1_p2_1)   
corrplot::corrplot(cor_mxl.betai_p1_p2_1)    

           
#In the case where the prices are equal, none doesn't get much shares or correlation
par(mfrow=c(1,3))
           
           

           
           
           
           
head(MarketSimulation)                       #p1 is 1.5 p2 is 1
MarketSimulation_p1_1.5 <- MarketSimulation
MarketSimulation_p1_1.5[1,2] <-1.5
head(MarketSimulation_p1_1.5)
           
           
eu_try <- exp(as.matrix(MarketSimulation_p1_1.5) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_1.5 <- t(eu_try) / colSums(eu_try) 
head(ind_utilities_p1_1.5)
shares_p1_1.5<-colMeans(ind_utilities_p1_1.5)
shares_p1_1.5
           
cor_mxl.betai_p1_1.5 <- cor(mxl.betai, ind_utilities_p1_1.5)
corrplot::corrplot(cor_mxl.betai_p1_1.5)   #the driving force of sound is decreased, 
#other variables significantly decreased mostly price plays a role 
           
           
           
           
           
head(MarketSimulation)                         #p2 is 1.5, p1 is 1
MarketSimulation_p2_1.5 <- MarketSimulation
MarketSimulation_p2_1.5[2,2] <-1.5
head(MarketSimulation_p2_1.5)
           
           
eu_try <- exp(as.matrix(MarketSimulation_p2_1.5) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p2_1.5 <- t(eu_try) / colSums(eu_try) 
head(ind_utilities_p2_1.5)
shares_p2_1.5<-colMeans(ind_utilities_p2_1.5)
shares_p2_1.5
           
cor_mxl.betai_p2_1.5 <- cor(mxl.betai, ind_utilities_p2_1.5)
corrplot::corrplot(cor_mxl.betai_p2_1.5)
           
shares<- data.frame(shares_p1_p2_0.7,shares_p1_p2_0.9,shares_p1_p2_1,shares_p1_p2_1.1, 
shares_p1_p2_1.3,shares_p1_p2_1.5, shares_p1_1.5, shares_p2_1.5 )
shares   
           
# > 0.68/0.31   0.7    none ratio: 0.007
[1] 2.193548
> 0.66/0.30    0.9     none ratio: 0.039
[1] 2.2
> 0.645/0.284   1     none ratio: 0.071
[1] 2.271127
> 0.62/0.27   1.1     none ratio: 0.11
[1] 2.296296
> 0.555/0.239         none ratio: 0.20
[1] 2.322176  1.3
           
#product 1 is still get some shares when its price is 150 euros
#because its sound quality is high which is a major influence together with price
par(mfrow=c(1,1))
           
           
           
           
           
           
#Calculating how much price change is needed for the shares to be equal()
predict.mxl(MarketSimulation_p1_1.5,mxl.betai)
MarketSimulation_p1_1.3 <- MarketSimulation
MarketSimulation_p1_1.3[1,2] <-1.3
head(MarketSimulation_p1_1.3)
predict.mxl(MarketSimulation_p1_1.5,mxl.betai)
predict.mxl(MarketSimulation,mxl.betai)
MarketSimulation_p1_1.1 <- MarketSimulation
MarketSimulation_p1_1.1[1,2] <-1.1
head(MarketSimulation_p1_1.1)
predict.mxl(MarketSimulation_p1_1.1,mxl.betai)
MarketSimulation_p1_1.2 <- MarketSimulation
MarketSimulation_p1_1.2[1,2] <-1.2                #20 is the price difference needed for the shares to be equal
head(MarketSimulation_p1_1.2)
predict.mxl(MarketSimulation_p1_1.2,mxl.betai)
           
           
           
           
-------------------------
#with cbc_table 

           
head(cbc_table)
head(ind_utilities_p1_p2_1)
cor_cbc_table_p1_p2_1 <- cor(cbc_table, ind_utilities_p1_p2_1)
head(shares_p1_p2_1)
           
corrplot::corrplot(cor_cbc_table_p1_p2_1) #method = c("number")
head(shares_p1_p2_1)
head(shares_p1_1.5)
head(shares_p2_1.5)
           


           
#The color change between product1 and product2 regarding weight and sound part-worths makes sense
#The part-worths changing one level different than weight and sounds could be becaue of 
#the trade-off between sound and weight in the products.
           
#price imp is negative with product 1 tho its sound is 4 for example so doesn't make sense.
           
#The weight and battery being better in product 2 outcompansates the sound being 4 in product 1 I think.
           
cor_cbc_table_p1_1.5 <- cor(cbc_table, ind_utilities_p1_1.5)
           
corrplot::corrplot(cor_cbc_table_p1_1.5)
shares 
           
           
           
cor_cbc_table_p2_1.5 <- cor(cbc_table, ind_utilities_p2_1.5)
           
corrplot::corrplot(cor_cbc_table_p2_1.5)
par(mfrow=c(1,1))
#imp price correlates positively always with the none option as expected.
           
#One evidence is because in the last case P1's price importance is still negative although it is cheaper.
           
#For product 2 the price 150 is for battery level 5 is too much.
#For example for the product 1 when the price is 100 one can still ask for a little bit more money for the battery
#When the willingness to pays' correlation is close to 0 then you are close to the max price for both of the product attributes that you can ask for.
           


#With only imps
cor_cbc_table_p1_p2_1_imps <- cor(cbc_table[,c(18:21)], ind_utilities_p1_p2_1)
           
corrplot::corrplot(cor_cbc_table_p1_p2_1_imps) #method = c("number")
shares 
           
cor_cbc_table_p1_1.5_imps <- cor(cbc_table[,c(18:21)], ind_utilities_p1_1.5)
           
corrplot::corrplot(cor_cbc_table_p1_1.5_imps)
           
cor_cbc_table_p2_1.5_imps <- cor(cbc_table[,c(18:21)], ind_utilities_p2_1.5)
           
corrplot::corrplot(cor_cbc_table_p2_1.5_imps)
           
           
corrplot.mixed(cor(indivData_short), #example
                          lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7)
           




#With IndivData
           
head(indivData)
indivData_short <- indivData[,c(-1,-4:-21)]
head(indivData_short)
           
cor_indivdata_p1_p2_1 <- cor(indivData_short, ind_utilities_p1_p2_1)
           
corrplot::corrplot(cor_indivdata_p1_p2_1,method = c("number")) #0.16 ba, 0.22 knowledge correlation
           
cor_indivdata_p1_1.5 <- cor(indivData_short, ind_utilities_p1_1.5)
           
corrplot::corrplot(cor_indivdata_p1_1.5,method = c("number"))
           
cor_indivdata_p2_1.5 <- cor(indivData_short, ind_utilities_p2_1.5)
           
corrplot::corrplot(cor_indivdata_p2_1.5,method = c("number"))
           
#As price of P1 increases rel imp of sound gets more important for the shares of it.
           
#Males buy product 1 more because it has highest sound and females P2 because it is better weight and battery wise.
           
           
#Product 1 with diff prices    (gets too complicated)
           
par(mfrow=c(1,3))
           
           
head(MarketSimulation)
MarketSimulation_p1_0.5 <- MarketSimulation
MarketSimulation_p1_0.5[1,2] <-0.5
head(MarketSimulation_p1_0.5)


eu_try <- exp(as.matrix(MarketSimulation_p1_0.5) %*% t(mxl.betai))
head(eu_try)
ind_utilities_p1_0.5 <- t(eu_try) / colSums(eu_try) 
head(ind_utilities_p1_0.5)
shares_p1_0.5<-colMeans(ind_utilities_p1_0.5)
shares_p1_0.5

par(mfrow=c(1,1))
cor_mxl.betai_p1_0.5 <- cor(cbc_table, ind_utilities_p1_0.5)
corrplot::corrplot(cor_mxl.betai_p1_0.5)






           
           
           
           
           
           
##################################################################################
####
#### Predict Market shares for changing prices given the price of the other product
####
##################################################################################


par(mfrow=c(1,1))
brand <- 2
prices <- seq(0, 2, 0.01)

head(prices)
head(MarketSimulation)
X <- MarketSimulation
share <- matrix(0, length(prices), nrow(X))
X
for (k in seq_along(prices)) {
X[brand, "price"] <- prices[k]
share[k,] <- predict.mxl(X, mxl.betai)
}

matplot(prices, share, type = "l", lty = 1, ylim = c(0, 1))
nn<-nrow(MarketSimulation)
legend("top", rownames(MarketSimulation),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
plot(prices, share[, brand], type = "l", ylim = c(0, 1))
#shows that the break out point is not where the prices meet because of trade offs between attributes and levels
           
#Product 1's price needs to increase more than 100 for the Product 2 to take over.
           
#Product 1 take over before the price of the 2nd product is higher than 100
           
#The percent of people who would buy P1 for 200 euros is higher than P2 for 200 euros
#This can also be seen in the change of the share of the none option
           
#for the 2nd brand the break out point of the shares seems to be near its price, 1.3
           
           
head(share,11)*100
round(apply(share,2,mean)*100,3)


head(MarketSimulation)
MarketSimulation_p2_1.5 <- MarketSimulation
MarketSimulation_p2_1.5[2,2] <-1.5
head(MarketSimulation_p2_1.5)
           
brand <- 2
prices <- seq(0, 2, 0.01)
           
head(prices)
head(MarketSimulation_p2_1.5)       #this case doesn't work.
X <- MarketSimulation_p2_1.5
share <- matrix(0, length(prices), nrow(X))
X
for (k in seq_along(prices)) {
X[brand, "price"] <- prices[k]
share[k,] <- predict.mxl(X, mxl.betai)
}
           
matplot(prices, share, type = "l", lty = 1, ylim = c(0, 1))
legend("top", rownames(MarketSimulation),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
           
plot(prices, share[, brand], type = "l", ylim = c(0, 1))
           
           
#############################################################################
####
#### Predict Profit for changing prices given the price of the other product
####
#############################################################################
           
#Market size will be assumed to be 1 due to its easiness to generalability to the whole market.
           
par(mfrow=c(1,1))
           
head(MarketSimulation_iteration) #done below
           
MarketSimulation_p1_1.3 <- MarketSimulation
MarketSimulation_p1_1.3[1,2] <-1.3
head(MarketSimulation_p1_1.3) 
           
brand <- 1
prices <- seq(0, 2, 0.01)
           
head(prices)
X <- MarketSimulation
profit <- matrix(0, length(prices), nrow(X))
X
for (k in seq_along(prices)) {
X[brand, "price"] <- prices[k]
profit[k,] <- (prices[k]-0.75)*predict.mxl(X, mxl.betai)
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))
plot(prices, profit[, brand], type = "l", ylim = c(0, 1))
profit
head(profit)
max(profit[,brand]) 
max(profit)      #1st iteration max price for P1 is 130 (1.3)
profit       #the profit of the other product always increases because its profit starts from 0
# and the other product's price just increases
head(prices)
max(brand)



           
#Iterative process
brand <- 1
prices <- seq(0, 2, 0.01)

head(MarketSimulation_iteration)
X <- MarketSimulation_iteration
profit <- matrix(0, length(prices), nrow(X))
X
for (k in seq_along(prices)) {
X[brand, "price"] <- prices[k]
profit[k,] <- (prices[k]-0.75)*predict.mxl(X, mxl.betai)
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))
plot(prices, profit[, brand], type = "l", ylim = c(0, 1), main ="Profit Maximizing Price Distribution of Product 1") 
head(profit)
max(profit[,brand])#shows the profit 

max(profit[,brand])*100*342
#max(profit)      #1st iteration, if you set the price to 130 you get max profit which is 0.2062037 from
#an individual
profit       #the profit of the other product always increases because its profit starts from 0
# and the other product's price just increases
           
head(prices)  #second iteration max profit is 0.2469341, best price is 131
max(brand)
           
#third iteration max profit is 0.2503965, best price is 131 again
           
MarketSimulation_iteration <- MarketSimulation #iterative part
head(MarketSimulation_iteration)
MarketSimulation_iteration[2,2] <-1.13
MarketSimulation_iteration[1,2] <-1.31

head(MarketSimulation_iteration) 


brand <- 2
prices <- seq(0, 2, 0.01)
X <- MarketSimulation_iteration
profit <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
X[brand, "price"] <- prices[k]
profit[k,] <- (prices[k]-0.70)*predict.mxl(X, mxl.betai)
}
#matplot(prices, profit, type = "l", lty = 1, ylim = c(0, 1))
plot(prices, profit[, brand], type = "l", ylim = c(0, 1), main ="Profit Maximizing Price Distribution of Product 2")
max(profit[,brand])
max(profit[,brand])*100*342

profit              #after setting the first product's price to 130, we try the second products optimal 
#price
#max profit that can be get from an individual is 0.1607058*100
#best price is 112 in the 1st iteration

#second iteration max profit is 0.1632775, optimal price is 113 euros


head(MarketSimulation_iteration)  #max profit price for P1 is 131 euros
           
#max profit price for P2 is 113 euros
           
           
predict.mxl(MarketSimulation_iteration, mxl.betai)  #the optimal share distribution
#P1 0.4471, P2 0.3794, N 0.1734






#Individual optimal pricing
head(mxl.betai)   #we use the example of the second person.(It is more consistent)
#2 1.115251

lastprice_product1 <- (mxl.betai$none-(mxl.betai$weight3+mxl.betai$battery3))/mxl.betai$price

head(lastprice_product1)

lastutility_product1 <- mxl.betai$weight3+mxl.betai$battery3+mxl.betai$price*1.115251

head(lastutility_product1)
head(lastutility_product1_try) #Now applies to every row

lastprice_product2 <- (mxl.betai$none-(mxl.betai$weight1+mxl.betai$sound2))/mxl.betai$price

head(lastprice_product2)

lastutility_product2 <- mxl.betai$weight1+mxl.betai$sound2+mxl.betai$price*1.106136
           
head(lastutility_product2)
head(lastutility_product2_try) #This applies to every row too.
           
comparision <- cbind(lastprice_product1,lastutility_product1,lastprice_product2,lastutility_product2,mxl.betai$none)
head(comparision)   #not a very big difference between product 1 and 2 for the 2nd person

#3   2.0398191   #For the third person
head(mxl.betai)
           
lastprice_product1 <- (mxl.betai$none-(mxl.betai$weight3+mxl.betai$battery3))/mxl.betai$price
           
head(lastprice_product1)
           
lastutility_product1 <- mxl.betai$weight3+mxl.betai$battery3+mxl.betai$price*2.0398191
           
head(lastutility_product1)
           
lastprice_product2 <- (mxl.betai$none-(mxl.betai$weight1+mxl.betai$sound2))/mxl.betai$price
           
head(lastprice_product2)
           
lastutility_product2 <- mxl.betai$weight1+mxl.betai$sound2+mxl.betai$price*2.0398191
           
head(lastutility_product2)
           
lastprice_btwproducts <- ((mxl.betai$weight3+mxl.betai$battery3)-(mxl.betai$weight1+mxl.betai$sound2))/mxl.betai$price
           
head(lastprice_btwproducts)   #last price the person would be willing to pay before choosing product 1 instead of 2.
hist(lastprice_btwproducts)   #some values are positive and some are negative showing that people choose different products.

comparision2 <- cbind(lastprice_product1,lastutility_product1,lastprice_product2,lastutility_product2,mxl.betai$none)
head(comparision2)
#for product 2 people seem less willing to pay


#THE END#

