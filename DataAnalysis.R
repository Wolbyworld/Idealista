install.packages('fastDummies')
install.packages('corrplot')
install.packages('glmulti')
install.packages('rJava')

library(readr)
library(fastDummies)
library(dplyr)
library(corrplot)
library(ggplot2)
library(glmulti)
library(rJava)

write.csv2(data,"data2.csv")
dataset <- read_delim("data2.csv", ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), propertyCode = col_number(), rooms = col_integer(), thumbnail = col_skip()),  locale = locale(date_names = "es", encoding = "ISO-8859-1"),      trim_ws = TRUE)
#Some data cleaning


#Dummy codificataion
dataset <- cbind(dataset,dummy_cols(dataset$status)) %>% select(-c('.data','.data_NA')) #Status
dataset <- cbind(dataset,dummy_cols(dataset$hasParkingSpace)) %>% 
          select(-c('.data','.data_NA')) %>% 
          rename(d_parking=.data_TRUE) #Parking
dataset <- cbind(dataset,dummy_cols(dataset$hasLift)) %>% 
  select(-c('.data','.data_FALSE')) %>% 
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


#Gmulti optimization
allVariables <- lnData %>% select(c(priceByArea,rooms,bathrooms,.data_renew,
                                    .data_newdevelopment,.data_good,d_parking
                                    ,d_lift,size,big,floor,d_exterior))

lmMulti <- glmulti(names(allVariables)[1],names(allVariables)[-1], data=allVariables, method="i", report = TRUE, intercept = TRUE , level = 1)

summary(lmMulti)
lmFitted <- lm(priceByArea ~ 1 + rooms + bathrooms + .data_renew + .data_newdevelopment + d_parking + big + floor + d_exterior, data=allVariables )
summary(lmFitted)


#Value our house
rooms <- 1
bathrooms <- 1
.data_newdevelopment <- 0
.data_renew <- 0
d_parking <- 0
big <- 0
floor <- 1
d_exterior <- 1
size <- 68
newdata <- list(rooms=rooms,bathrooms=bathrooms,
     .data_newdevelopment=.data_newdevelopment,.data_renew=.data_renew,d_parking=d_parking,
     big=big,floor=floor,d_exterior=d_exterior) 
predict(lmFitted,newdata = newdata)*size 