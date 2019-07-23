install.packages('fastDummies')
install.packages('corrplot')
install.packages('glmulti')

library(readr)
library(fastDummies)
library(dplyr)
library(corrplot)
library(ggplot2)
library(glmulti)

dataset <- read_delim("data2.csv", ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), propertyCode = col_number(), rooms = col_integer(), thumbnail = col_skip()),  locale = locale(date_names = "es", encoding = "ISO-8859-1"),      trim_ws = TRUE)
#Some data cleaning


#Dummy codificataion
dataset <- cbind(dataset,dummy_cols(dataset$status)) %>% select(-c('.data','.data_NA')) #Status
dataset <- cbind(dataset,dummy_cols(dataset$hasParkingSpace)) %>% 
          select(-c('.data','.data_NA')) %>% 
          rename(d_parking=.data_TRUE) #Parking
dataset <- cbind(dataset,dummy_cols(dataset$hasLift)) %>% 
  select(-c('.data','.data_NA','.data_FALSE')) %>% 
  rename(d_lift=.data_TRUE) #Lift
dataset <- cbind(dataset,dummy_cols(dataset$exterior)) %>% 
  select(-c('.data','.data_FALSE')) %>% 
  rename(d_exterior=.data_TRUE) #Parking



#Filter outliers
sdMax <- 1.5
upper <- mean(dataset$priceByArea)+sdMax*sd(dataset$priceByArea)
lower <- mean(dataset$priceByArea)-sdMax*sd(dataset$priceByArea)
lnData <- dataset %>% filter(priceByArea<upper & priceByArea>lower)
rm('sdMax','upper','lower')

#Big house?
lnData <- lnData %>% mutate(big=1*(size>mean(size)))

#Adjust floor
lnData <- lnData %>% mutate(floor = recode(floor,'en'='0','ss'='0','st'='0','bj'='0')) %>% 
      filter(!is.na(floor)) %>%
      mutate(floor=as.integer(floor))

#Correlation
corMatrix <- lnData %>% select(c(priceByArea,d_exterior,rooms,bathrooms,.data_renew,
                                 .data_newdevelopment,.data_good,d_parking
                               ,d_lift,size,numPhotos,price,big,floor)) %>% cor()
corrplot(corMatrix, method="circle")


#Modeling
linearMod <- lm(priceByArea ~ big+.data_good+rooms+floor+bathrooms+d_lift+d_exterior, data=lnData)  #
summary(linearMod)
dataset %>% group_by(status) %>% tally()


# Basic scatter plot
ggplot(lnData, aes(x=priceByArea, y=rooms)) + geom_point()


#Gmulti optimization
allVariables <- lnData %>% select(c(priceByArea,rooms,bathrooms,.data_renew,
                                    .data_newdevelopment,.data_good,d_parking
                                    ,d_lift,size,numPhotos,price,big,floor,d_exterior))

lmMulti <- glmulti(names(allVariables)[1],names(allVariables)[-1], data=allVariables, method="i",report=TRUE, maxsize=6)

summary(lmMulti[bestmodel])
summary(lm(priceByArea ~ 1 + rooms + bathrooms + d_parking + d_lift + size +     
     numPhotos + price + floor + bathrooms:rooms + .data_renew:rooms + 
    .data_renew:bathrooms + .data_newdevelopment:bathrooms +      
    .data_good:rooms + .data_good:.data_renew + d_parking:rooms +  
    d_lift:bathrooms + d_lift:.data_newdevelopment + size:rooms +    
     size:bathrooms + size:.data_renew + size:.data_newdevelopment +  
     size:d_parking + size:d_lift + price:rooms + price:bathrooms +   
     price:.data_newdevelopment + price:.data_good + price:d_lift +   
    price:size + big:rooms + big:d_parking + big:size + big:price +  
    floor:.data_good + floor:d_lift + floor:price,data=allVariables))