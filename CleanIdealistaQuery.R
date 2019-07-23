#Requiered packages
#install.packages('httr') #For the request
#install.packages('jsonlite') #to parse json
#install.packages('gtools')

#Load packages
library('httr')
library('jsonlite')
library('tibble')
library('gtools')
library('tidyverse')

#Function definitions

# Authentification, we need to get the auth token
getTokenOAuth2 <- function(url, key) {
  r<- POST(url,body = list(`grant_type`="client_credentials",scope="read")
           , config = add_headers(Authorization = paste("Basic",base64_enc(key), sep = " ")))
  
  token <- fromJSON(content(r,as='text'))$access_token
  return(token)
}
#search
searchIdealista <- function(token,parameters){
  url <- paste('https://api.idealista.com/3.5/',parameters$locale,'/search',sep='')
  r<- POST(url,body=parameters,config=add_headers(Authorization = paste("Bearer",token, sep = " ")))
  responseCode <- status_code(r)
  #browser()
  if (responseCode!=200){
    return (data.frame('responseCode'=responseCode))
  }
  #Parsing results
  totalPages <- content(r,as='parsed')$totalPages
  results <- content(r,as='parsed')$elementList
  summary(totalPages)
  summary(content(r,as='parsed'))
  #Parsing data
  parseResults <- function(results, data) {
    for (element in results){
      element<-append(element,element$parkingSpace)
      element<-append(element,element$detailedType)
      element<-append(element,element$suggestedTexts)
      element$parkingSpace<-NULL
      element$detailedType<-NULL
      element$suggestedTexts<-NULL
      elementDF<-as_tibble(element)
      if(length(data)==0){
        data <- elementDF
      } else {
        data[setdiff(names(elementDF), names(data))] <- NA
        elementDF[setdiff(names(data), names(elementDF))] <- NA
        data<-rbind(data,elementDF)
      }
    }
    return(data.frame('responseCode'=responseCode,'data'=data))
  }
  
  
  data <- tibble()
  data <- parseResults(results,data)
  if (totalPages ==0){
    return (content(r,as='parsed'))
  } else {
  #Requesting the rest of pages
  for (i in 1:totalPages) {
    head(i)
    numPage<-i+1
    lbody = list(locale=locale,operation=operation,
                 propertyType=propertyType,center=center,distance=distance,
                 apiKey=apiKey,maxItems=maxItems,numPage=numPage)
    url <- paste('https://api.idealista.com/3.5/',locale,'/search',sep='')
    r<- POST(url,body=lbody,config=add_headers(Authorization = paste("Bearer",token, sep = " ")))
    results <- content(r,as='parsed')$elementList
    data <- parseResults(results,data)
  }
  
  #Cleaning variables up
#  return(data)
  return(content(r,as='parsed')) }
}


####### Clean Code ######

#API Config
LoginUrl = "https://api.idealista.com/oauth/token"
key <- read_file('API.key')

#Define search parameters
locale <- 'es'
operation <- 'sale' #'rent'
propertyType <- 'homes' # offices, premises, garages, bedrooms
center <- '40.4265735,-3.6804183'
distance <- '100'
maxItems= '30'
numPage='1'
apiKey=key
lbody = list(locale=locale,operation=operation,
             propertyType=propertyType,center=center,distance=distance,
             apiKey=apiKey,maxItems=maxItems,numPage=numPage)

#Let´s go
token <- getTokenOAuth2(LoginUrl,key)
data <- searchIdealista(token,lbody)

rm(list=setdiff(ls(), 'data'))