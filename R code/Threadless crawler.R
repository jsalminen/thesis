# Threadless score crawler.
# Given the range of pages, it collects data on design scoring.

# Load libraries
library(XML)
library(RCurl)

# Function to download pages
getPage <- function(url){
  wp <- try(getURLContent(url))
  Sys.sleep(3) # Delay to avoid overloading the website
  if(class(wp) != 'try-error'){
    return(wp)
  }
  else{return('Error!')}
}

# Function to parse pages
parsePage <- function(wp){
  doc <- htmlTreeParse(wp,asText=TRUE)
  return(xmlRoot(doc)) 
}

# Function to find out print status of a design
getPrinted <- function(user){
  url <- "http://www.threadless.com/"
  wp <- getPage(paste(url,user,"/designs/",sep=''))
  prints <- list(NA)
  if(wp == 'Error!'){print(paste('Error getting user',user))}
  else{
    r <- parsePage(wp)
    printed_subs <- NA
    for(i in 1:xmlSize(r[[3]][[7]][[2]][[2]])){
      if(xmlValue(r[[3]][[7]][[2]][[2]][[i]][[1]][[1]])=="Printed designs"){
        printed_subs <- r[[3]][[7]][[2]][[2]][[i]][[2]]
      }
    }
    if(is.na(printed_subs[1])){prints[1] <- 0}
    else{
      for(i in 1:xmlSize(printed_subs)){
        prints[[i]] <- xmlGetAttr(printed_subs[[i]][[1]][[1]], 'href')
      }
    }
  }
  return(prints)
}

# Helper function to find out print status of a design
compare <- function(design,printed){
  designsplit <- strsplit(design, '/')
  printedsplit <- strsplit(printed, '/')
  cleandesign <- designsplit[[1]][length(designsplit[[1]])]
  cleandesign <- tolower(cleandesign)
  cleanprinted <- printedsplit[[1]][length(printedsplit[[1]])]
  cleanprinted <- tolower(cleanprinted)
  cleanprinted <- gsub('_', '-', cleanprinted)
  if(cleandesign == cleanprinted){return(1)}
  else{return(0)}
}

# Function to find out print status of a design
checkPrint <- function(design,user){
  prints <- getPrinted(user)
  if(is.na(prints[[1]])){return(NA)}
  if(prints[[1]]==0){return(0)}
  for(i in 1:length(prints)){
    if(compare(design, prints[[i]])==1){return(1)}
  }
  return(0)
}

# Helper crawler to collect data on each design on a downloaded page
scoreCrawler <- function(design){
  url <- "http://www.threadless.com/"
  wp <- getPage(paste(url, design, sep=''))
  user <- NA
  date <- NA
  scoring <- NA
  avg_score <- NA
  score <- NA
  fives <- NA
  ones <- NA
  printed <- NA
  if(wp == 'Error!'){print(paste('Error crawling design',design))}
  else{
    r <- parsePage(wp)
    user <- try(xmlGetAttr(r[[3]][[7]][[3]][[3]][[4]][[2]][[1]],'href'))
    if(class(user) == 'try-error'){
      user <- try(xmlGetAttr(r[[3]][[7]][[4]][[3]][[4]][[2]][[1]], 'href'))
    }
    
    date <- try(xmlGetAttr(r[[3]][[7]][[2]][[2]][[1]][[4]][[1]], 'data-approved-date'))
    if(class(date) == 'try-error'){
      date <- try(xmlGetAttr(r[[3]][[7]][[3]][[2]][[1]][[4]][[1]], 'data-approved-date'))
    }
    scoring <- try(r[[3]][[7]][[2]][[2]][[1]][[3]])
    if(class(scoring) == 'try-error'){
      scoring <- try(r[[3]][[7]][[3]][[2]][[1]][[3]])
    }
    avg_score <- try(as.numeric(xmlValue(scoring[[1]][[1]][[1]][[1]])))
    if(class(avg_score)=='try-error'){avg_score <- NA}
    score <- try(as.numeric(xmlValue(scoring[[2]][[1]][[1]][[1]])))
    if(class(score)=='try-error'){score <- NA}
    fives <- try(as.numeric(xmlValue(scoring[[2]][[2]][[1]][[1]])))
    if(class(fives)=='try-error'){fives <- NA}
    ones <- try(as.numeric(xmlValue(scoring[[2]][[3]][[1]][[1]])))
    if(class(ones)=='try-error'){ones <- NA}
    printed <- checkPrint(design,user)
  }
  result <- list(approved_date=date,design=design,user=user,avg.score=avg_score, score=score,fives=fives,ones=ones,printed=printed)
  print(paste(design, 'crawled.'))
  return(result)
}

# Actual web crawler function. Downloads web page, crawls the designs and 
# data for each of them and saves the results in csv format.
crawler <- function(startpage=1, stoppage, url="http://www.threadless.com/pick/score/?status=closed&sort=&page="){
  startTime <- proc.time()
  for(i in startpage:stoppage){
    wp <- getPage(url=paste(url,i, sep=''))
    if(wp == 'Error!'){print(paste('Error crawling page',i))}
    else{
      r <- parsePage(wp)
      designs <- r[[3]][[7]][[2]][[4]][[1]]
      design_urls <- list()
      for(j in 1:xmlSize(designs)){
        design_urls[[j]] <- xmlGetAttr(designs[[j]][[1]][[1]], 'href')
      }
      result <- lapply(design_urls,scoreCrawler)
      result <- do.call(rbind.data.frame,result)
      write.table(result,file=paste(i,'.csv', sep=''),sep=',',row.names=FALSE)
      print(paste('Page', i, 'crawled.'))
      print(proc.time()-startTime)
    }
  }
}


