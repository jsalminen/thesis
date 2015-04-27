library(XML)
library(RCurl)

getPage <- function(url){
  wp <- try(getURLContent(url))
  Sys.sleep(3)
  if(class(wp) != 'try-error'){
    return(wp)
  }
  else{return('Error!')}
}

parsePage <- function(wp){
  doc <- htmlTreeParse(wp,asText=TRUE)
  return(xmlRoot(doc)) 
}

getSubmission <- function(url){
  wp <- getPage(url)
  if(wp == 'Error!'){
    print(paste('Error getting submission',url))
    return(NA)
  }
  else{
    r <- parsePage(wp)
    submission <- try(xmlGetAttr(r[[3]][[11]][[2]][[12]][[3]][[6]][[2]][[1]][[1]], 'href'))
    if(class(submission) == 'try-error'){submission <- NA}
  }
  print(paste('Crawled submission',url))
  return(submission)
}