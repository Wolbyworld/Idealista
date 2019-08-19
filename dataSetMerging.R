library(dplyr)

diskDB <- as_tibble(read.dcf('DDBBs/sellingDB.dcf'))
internetDB <- data


#Identify new records / ads
new_records <- anti_join(internetDB,diskDB,by=c('propertyCode','propertyCode'))
new_records <- new_records %>% mutate(publishing_date = Sys.Date(), sold_date = 0)

#Identify sold records
sold_records <- anti_join(diskDB,internetDB,by=c('propertyCode','propertyCode'))
sold_records <- sold_records %>% mutate(sold_date=Sys.Date())  

#Puting everything in the database
#a) Removing sold records
tt <- anti_join(diskDB,sold_records,by=c('propertyCode','propertyCode'))
#b) Adding them back with the selling date
tt <- rbind(tt,sold_records)
#c) Adding the new records
tt <- rbind(tt,new_records)

#Update market time
tt<- diskDB %>% mutate(Market_time=sold_date-publishing_date)

#Save a backup of the file
write.dcf(tt,paste('DDBBs/sellingDB',format(Sys.Date(),'%Y%m%d'),'.dcf',sep=''))

#Save the working file
write.dcf(diskDB,'DDBBs/sellingDB.dcf')

#Cleanup
rm(list=setdiff(ls(),'diskDB'))