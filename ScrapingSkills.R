library(rvest)
library(xml2)
library(stringr)
library(plyr)
library(httr)
library(dplyr)
library(rebus)

GlassdoorInfo<-read.csv("glassdoor_datascientist_Info.csv")
IndeedInfo<-read.csv("datascientist_indeed_info.csv")

ScrSkill<-function(InfoMatrix){
  InfoMatrix<-as.data.frame(InfoMatrix)
  joblink<-as.character(InfoMatrix$website)
  
  ##Function need to clean text
  clean.text <- function(text)
  {
    str_replace_all(text, regex('\r\n|\n|\t|\r|,|/|<|>|\\.'), ' ')
  }
  
  
  #Skill I want to scrape
  keywords <- c('Statistics','Computer science','Math','Machine Learning',
                'Data mining','Statistical Nodeling','Predictive Modeling',
                '\\bR\\b','\\bJava\\b','\\bPython\\b','\\bSQL\\b',
                '\\bSAS\\b','\\bTableau\\b','\\bExcel\\b','\\bC++\\b',
                '\\bHadoop\\b','\\bStata\\b','\\bSpark\\b', '\\bMatlab\\b')
  
  skill_f<-as.data.frame(matrix(NA,ncol = length(keywords)+1))
  colnames(skill_f)<-c("website",sapply(keywords, function(x) gsub("[\\]b", "", x)))

  tryCatch({
    
  pb <- txtProgressBar(min = 1, max = length(joblink), style = 3)
  for(i in 1:length(joblink)){
    
    html <- read_html(joblink[i])
    text <- html_text(html)
    text <- clean.text(text)
    keyskill_logic = unlist(sapply(keywords, function(x) any(grepl(x, text, ignore.case = TRUE,perl = T))))
    names(keyskill_logic) = sapply(keywords, function(x) gsub("[\\]b", "", x))
    skill<-as.data.frame(t(keyskill_logic))
    skill<-cbind(SEQN=joblink[i],skill)
    skill_f<-rbind(skill_f,skill)
    setTxtProgressBar(pb, i)
    
  } 
  close(pb) 
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  SkillSet<-skill_f[2:nrow(skill_f),]
  return(SkillSet)
}

GlassdoorSkill<-ScrSkill(GlassdoorInfo)
IndeedSkill<-ScrSkill(IndeedInfo)
#website unique
nrow(GlassdoorSkill)==length(unique(GlassdoorSkill$website))

GlassdoorInfo<-as.data.frame(GlassdoorInfo)
IndeedInfo<-as.data.frame(IndeedInfo)
Glassdoor<-merge(GlassdoorInfo,GlassdoorSkill,by="website")
Indeed<-merge(IndeedInfo,IndeedSkill,by="website")

#sorting data
Glassdoor<-Glassdoor[,2:25]
Glassdoor_final<-Glassdoor[-which(duplicated(Glassdoor)),]
Indeed<-Indeed[,3:25]
Indeed_final<-Indeed[-which(duplicated(Indeed)),]
write.csv(Glassdoor_final,"Glassdoor.csv")
write.csv(Indeed_final,"Indeed.csv")
