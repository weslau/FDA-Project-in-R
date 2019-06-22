library(magrittr)
library(jsonlite)
library(data.table)
library(tidyverse)

library(httr)
library(stringr)
library(readr)

create_url <-  function(start_date, end_date, API_key, limit, skip) {
  ##API_key <- 'MA7Lr5hXfkIWZLcdasrZ6Tf0U0zwPAPtZ1ijYVBA'
  ##start_date <- '2015-01-01' ##start date of recall search. Format is YYYY-MM-DD
  ##end_date <- '2015-12-31' ##end date of recall search. Format is YYYY-MM-DD
  daterange <- paste('[',start_date,'+TO+',end_date,']', sep="")
  url <- paste('https://api.fda.gov/device/enforcement.json?api_key=', API_key,
               '&search=report_date:',daterange,'&limit=',limit,'&skip=',skip,sep = "")
  return(url)
}

f <- function() {
  
}

##f()

##still need global variables
LIMIT <- 100

url_test <- create_url(start_date = '2019-01-01', end_date = '2019-12-31', API_key = 'MA7Lr5hXfkIWZLcdasrZ6Tf0U0zwPAPtZ1ijYVBA',
           limit = '1', skip = '0')

recalls_enforce2 <- GET(url_test)
recalls_enforce_data2 <- fromJSON(content(recalls_enforce2,"text"), simplifyVector = F) ##transform the data from GET url into JSON list
total_results <- recalls_enforce_data2$meta$results$total##num of results you want to add to database

database_recalls <- vector("list", 100*ceiling(total_results/100)) ##initialize list that is total_results long, rounded up to the nearest 100

for (i in 1:ceiling(total_results/100)) {
  url_temp <- create_url(start_date = '2019-01-01', end_date = '2019-12-31', API_key = 'MA7Lr5hXfkIWZLcdasrZ6Tf0U0zwPAPtZ1ijYVBA',
                         limit = as.character(LIMIT), skip = as.character((i-1)*100))
  recalls_enforce_temp <- GET(url_temp)
  recalls_enforce_data <- fromJSON(content(recalls_enforce_temp,"text"), simplifyVector = F)
  recalls_enforce_data <- recalls_enforce_data$results
  database_recalls[(100*i-99):(100*i)] <- recalls_enforce_data[1:100]##database_recalls stores your complete list of recalls
}

#find a way to flatten that list so that you can read the "recalling_firm" in each entry
##you need to rbind the long list database_recalls_2019 to a new dataframe
database_flat <- lapply(1:length(database_recalls), function(x) database_recalls[[x]][-c(14)]) %>% rbindlist(fill = T)

database_flat %>% 
  count(recalling_firm)

companies <- database_flat %>% 
  count(recalling_firm) %>% 
  arrange(desc(n))
companies <- companies$recalling_firm
str_subset(string = companies, pattern = "Abbott")

##database_flat['Abbott' %in% recalling_firm]
str_locate_all(string = companies, pattern = "Abbott")
