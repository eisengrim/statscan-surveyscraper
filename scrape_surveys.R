# filename: scrape_surveys.R
# author:   kody crowell
# date:     sep 25 2019

# load libs (**install if not found)
library(dplyr)
library(xml2)
library(RCurl)
library(XML)
library(curl)
library(httr)
library(rvest)
library(stringi)
library(stringr)
library(splitstackshape)
library(parallel)

'%!in%' <- function(x,y)!('%in%'(x,y))
# set working directory
wd="~/workspace/datajustice"
setwd(wd)

# master url
master <- "http://www23.statcan.gc.ca/imdb/pIX.pl?Function=getThemeSV&PItem_Id=97413&PCE_Id=326&PCE_Start=01010001&CItem_Id=97413&CCE_Id=326&CCE_Start=01010001&lang=en"

# scrape html and parse for pdf files
page   <- getURL(master)
parsed <- htmlParse(page)
links  <- xpathSApply(parsed, path="//a", xmlGetAttr, "href")
inds   <- grep("*SDDS*", links)
links  <- unlist(links[inds])

survey.stats <- data.frame(matrix(ncol=25, nrow=0))
cols <- c("name", "status", "frequency", "objective", "release", 
          # "description", "reference", "collection", 
          # "subjects", "targets", "instrument design", "sampling", 
          # "sampling (long)","data sources", "data capture", "errors", 
          # "imputation", "estimation", "quality evaluation", "disclosure control", 
          # "revisions", "data accuracy", "summary of changes", 
          "record num", "url")

## extract info
scrape_survey <- function (url){
  # get survey record num
  num <- str_split(url, "SDDS=")[[1]][2]
  link <- paste0("surveys/", num)
  
  # debug print
  print(num)
  
  # curl webpage
  mm <- curl_download(url, link)
  
  # name
  webpage <- read_html(link) %>% 
    html_nodes('h1')  %>%
    html_text()
  
  texts <- read_html(link) %>%
    html_nodes('p')
  
  n <- length(texts)
  i <- 1
  
  # status
  if ((texts %>% .[i] %>% html_text() %>% stri_cmp_eq("Status:"))){
    i <- i + 1
    webpage <- texts %>% 
    .[i] %>% 
    html_text() %>%
    append(webpage, .)
  }
  
  i <- i + 1
  
  # frequency
  if (texts %>% .[i] %>% html_text() %>% stri_cmp_eq("Frequency:")){
    i <- i + 1
    webpage <- texts %>% 
    .[i] %>% html_text() %>%
    append(webpage, .)
  }

  i <- i + 3
  
  # objective
  webpage <- texts %>% .[i] %>% html_text() %>% append(webpage, .)
  
  i <- i + 1
  
  # release date
  webpage <- texts %>% 
    .[i] %>% 
    html_text() %>% 
    str_split("- ") %>% 
    .[[1]] %>% 
    .[2] %>%
    append(webpage, .)
  
  i <- i + 1

  # ## description
  # webpage <- texts %>% .[i] %>% html_text() %>% append(webpage, .)
  # 
  # i <- i + 1
  # 
  # if (i > n){
  #   webpage <- append(webpage, rep(NA, 17))
  #   return(webpage)
  # }
  # 
  # # reference period (?)
  # ref <- texts %>% .[i] %>% html_text() %>% str_split(": ") %>% .[[1]]
  # if (stri_cmp_eq(ref[1], "Reference period")){
  #   webpage <- ref[2] %>% append(webpage, .)
  #   i <- i + 1
  # }
  # else if (!stri_cmp_eq(ref[1], "Reference period")){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # 
  # # collection period (?)
  # colle <- texts %>% .[i] %>% html_text() %>% str_split(": ") %>% .[[1]]
  # if (stri_cmp_eq(colle[1], "Collection period")){
  #   webpage <- ref[2] %>% append(webpage, .)
  #   i <- i + 1
  # }
  # else if (!stri_cmp_eq(colle[1], "Collection period")) {
  #   webpage <- append(webpage, NA)
  # }
  # 
  # headers <- read_html(link) %>%
  #   html_nodes('h4') %>%
  #   html_text()
  # 
  # # subjects
  # if ("Subjects" %in% headers){
  #   webpage <- read_html(link) %>% 
  #     html_nodes('ul') %>% .[6] %>%
  #     html_text() %>%
  #     str_replace_all("\n", "; ") %>%
  #     substr(0, nchar(.)-2) %>%
  #     append(webpage, .)
  # }
  # else if ("Subjects" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # 
  # ## methodology
  # # target population
  # if ("Target population" %in% headers){
  #   webpage <- texts %>% .[i] %>% html_text() %>% append(webpage, .)
  #   i <- i + 1
  # } 
  # else if ("Target population" %!in% headers){
  #   webpage <- append(webpage, NA)
  # } 
  # 
  # # instrument design
  # if ("Instrument design" %in% headers){
  #   webpage <- texts %>% .[i] %>% html_text() %>% append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Instrument design" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # sampling
  # if ("Sampling" %in% headers){
  #   webpage <- texts %>% .[i] %>% html_text() %>% append(webpage, .)
  #   i <- i + 1
  #   webpage <- texts %>% .[i] %>% html_text() %>% append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Sampling" %!in% headers){
  #   webpage <- append(webpage, rep(NA, 2))
  # }
  # 
  # # data sources
  # if ("Data sources" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  #   
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Data sources" %!in% headers){
  #   webpage <- append(webpage, rep(NA, 2))
  # }
  # 
  # # error detection
  # if ("Error detection" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Error detection" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # imputation
  # if ("Imputation" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Imputation" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # estimation
  # if ("Estimation" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Estimation" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # quality evaluation
  # if ("Quality evaluation" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Quality evaluation" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # disclosure control
  # if ("Disclosure control" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Disclosure control" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # revisions
  # if ("Revisions and seasonal adjustment" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Revisions and seasonal adjustment" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }
  # 
  # # data accuracy
  # if ("Data accuracy" %in% headers){
  #   webpage <- texts %>%
  #     .[i] %>% html_text() %>%
  #     append(webpage, .)
  #   i <- i + 1
  # }
  # else if ("Data accuracy" %!in% headers){
  #   webpage <- append(webpage, NA)
  # }

  # summary of changes
  # link.ch <- read_html(link) %>% 
  #   html_nodes('.btn.btn-default') %>% 
  #   .[2] %>% html_attr('href')
  # 
  # webpage <- read_html(link.ch) %>% 
  #   html_nodes('p') %>%
  #   html_text() %>%
  #   append(webpage, .)
    
  # record number and url
  webpage <- append(webpage, num)
  webpage <- append(webpage, url) 
  
  names(webpage) <- cols
  webpage 
}

# run in parallel, check with debugonce()
survey.stats <- lapply(links, scrape_survey)

# # clean up text
# body_text %>%
#   str_replace_all(pattern = "\n", replacement = " ") %>%
#   str_replace_all(pattern = "[\\^]", replacement = " ") %>%
#   str_replace_all(pattern = "\"", replacement = " ") %>%
#   str_replace_all(pattern = "\\s+", replacement = " ") %>%
#   str_trim(side = "both") %>%
#   substr(start = nchar(body_text)-700, stop = nchar(body_text))

# convert to df
survey.stats <- survey.stats %>%
  do.call(rbind, .) %>%
  as.data.frame(., stringsAsFactors=FALSE) 

# write to csv
write.csv(survey.stats, "surveys.csv", row.names=F)
