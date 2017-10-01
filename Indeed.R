library(rvest)
library(xml2)
library(stringr)
library(plyr)
library(httr)
library(dplyr)
library(rebus)

###Find job count

page <- html_session(paste0("https://www.indeed.com/jobs?q=data+scientist&l="))
job_count <- unlist(strsplit(page %>%  read_html() %>%
                               html_node("#searchCount") %>%
                               html_text(), split = ' ')) 
job_count <- as.numeric(str_replace_all(job_count[length(job_count)-1],',',''))
jb=ceiling(as.numeric(job_count)/50)
BasicInfo<-matrix(NA,ncol = 5) 

session <- paste("https://www.indeed.com/jobs?q=data+scientist&l=&radius=25&start="
                 ,seq(0,jb)*50,sep = "")


for(i in 1:length(session)){
  
  title<-session[i] %>% read_html() %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "result", " " ))]') %>%
    html_node("a")%>% html_attr("title")
  
  company<-session[i] %>% read_html() %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "result", " " ))]') %>%
    html_node("span a")%>% html_attr("href")
  
  location<-session[i] %>% read_html() %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "result", " " ))]') %>%
    html_node("span.location") %>% html_text
  
  joblink<- session[i] %>% read_html() %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "result", " " ))]') %>% 
    html_node("h2 a")%>% html_attr("href")
  
  #clean data
  link_clean<-joblink[which(is.na(joblink)==FALSE)]
  link_clean<-paste0("http://indeed.com",link_clean)
  title_clean<-title[which(is.na(joblink)==FALSE)]
  company_clean<-company[which(is.na(joblink)==FALSE)]
  location_clean<-location[which(is.na(joblink)==FALSE)] %>% str_split(",")
  city<-c();state<-c()
  for(k in 1:length(location_clean)){
    city[k]<-location_clean[[k]][1]
    state[k]<-location_clean[[k]][2]%>% str_split(" ")
    state[k]<-state[k][[1]][2]
  }
  
  company_clean<-str_replace_all(company_clean, "[\r\n|\n|\t|\r|,|/|<|>|\\.]|cmp", '')
  
  CombineInfo<-cbind(title_clean,company_clean,city,state,link_clean)
  BasicInfo<-rbind(BasicInfo,CombineInfo)
  Sys.sleep(00.1)
}

#Clean data, delete NA and which does not have company informaiton
BasicInfo<-as.data.frame(BasicInfo)
FinaIndeed<-BasicInfo[-which(duplicated(BasicInfo)),]
FinaIndeed[FinaIndeed[,"company_clean"]=="#","company_clean"]<-NA
FinaIndeed$title_clean<-as.character(FinaIndeed$title_clean)
FinaIndeed$company_clean<-as.character(FinaIndeed$company_clean)
FinaIndeed$city<-as.character(FinaIndeed$city)
FinaIndeed$state<-as.character(FinaIndeed$state)
FinaIndeed$link_clean<-as.character(FinaIndeed$link_clean)

write.csv(FinaIndeed,"datascientist_indeed_info.csv")
