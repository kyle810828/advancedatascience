library(rvest)
library(xml2)
library(stringr)
library(plyr)
library(httr)
library(dplyr)
library(rebus)
#URL of data scientist form glassdoor page 1 to 30

url <- paste0("https://www.glassdoor.com/Job/data-scientist-jobs-SRCH_KO0,14_IP",c(1:30),".htm")
BasicInfo<-matrix(NA,ncol = 6)

tryCatch({ 
for(i in 1:length(url)){
  
  link<- url[i] %>% read_html() %>% 
    html_nodes("a.jobLink") %>% html_attr("href")
  joblink <- paste0('https://www.glassdoor.com', link)
  joblink <- unique(joblink)
  
tryCatch({  
  for(j in 1:length(joblink)){
    
    #Location
    loc<-joblink[j]%>%read_html() %>% 
      html_nodes(".subtle") %>%  html_text()  %>% str_split(",")
    ##number of char, start with 5
    location <- substr(gsub(" ", "", loc[1], fixed = TRUE), 4, nchar(loc[1])) %>% str_split(", ")
       
    #Load source code
    sourcecode<-suppressWarnings(readLines(joblink[j]))
    
    #title
    title <- sourcecode[str_detect(sourcecode, "\'jobTitle\'")] %>% str_extract("\"(.*)\"") %>% 
      str_replace_all("[[:punct:]]", '')
    title <-title[1]
    
    #company
    company <- sourcecode[str_detect(sourcecode, "\'name\'")] %>% str_extract("\"(.*)\"") %>% 
      str_replace_all("[[:punct:]]", '')
    company <- ifelse(identical(company, character(0)),NA,company)
   
    #industry 
    industry <- sourcecode[str_detect(sourcecode, "\'sector\'")] %>% str_extract("\"(.*)\"") %>% 
      str_replace_all("[[:punct:]]", '')
    industry <- ifelse(identical(industry, character(0)),NA,industry)
    
    #city
    city<-location[[1]][1]
    
    #state
    state<-location[[1]][2]
    
    #state
    website<-joblink[j]
      
    CombineInfo<-cbind(title,company,industry,city,state,website)
    BasicInfo<-rbind(BasicInfo,CombineInfo)
    Sys.sleep(00.1)
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  Sys.sleep(00.1)
}
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


##Handling Error value
goodId<-grepl("^[[:upper:]]+", BasicInfo$city)
Data<-BasicInfo[goodId,]
#write.csv(Data,"glassdoor_datascientist_Info.csv")

