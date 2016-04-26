# http://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html
# ftp://ftp-cdc.dwd.de/pub/CDC/

rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx8192m")
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)

library(readr)
library(dplyr)
library(ggplot2)

wd <- "~/Documents/github/dwd"
setwd(wd)

## set ftp_proxy in .Renviron instead! 
# library(httr)
# httr::set_config(
#   use_proxy(url=Sys.getenv("proxy.url"), port=3128, 
#             username = Sys.getenv("proxy.username"), 
#             password = Sys.getenv("proxy.password"))
#   )


## function to pull DWD data on German regional average temperatures
f.get.dwd.data <- function() {
  
  # generate empty list to fill with data in the loop
  raw.dta <- vector("list", length = 12)
  
  for(i in seq_len(12)){
    
    # grab all 12 months of data from DWD CDC
    url <- paste0(
      "ftp://ftp-cdc.dwd.de/pub/CDC/",
      "regional_averages_DE/monthly/air_temperature_mean/",
      "regional_averages_tm_", sprintf("%02i", i), ".txt"
    )
    
    # check max length once
    if(i == 1){
      lngth <- length(read_lines(url))
    }
    
    f.get.length <- function(lngth) {
      # if i < current month, return Januar length
      # else, length is one month less than Januar legnth
      if(i < as.integer(format(Sys.Date(), format = "%m"))){
        # -1 for skip, -1 for last line removed
        return(lngth-2)
      } else {
        return(lngth-3)
      }
    }
    
    # read data with readr, since this automatically parses the urls
    txt.dta <- read_lines(url, skip = 1, n_max = f.get.length(lngth))
    
    # add data to list
    # do NOT use readr, since this somehow completely messes up the data
    raw.dta[[i]] <- read.csv2(text = txt.dta, stringsAsFactors = FALSE) %>% 
      # last row is fubar'ed, remove
      select(-X)
  }
  
  # bind all into one data frame
  dta <- bind_rows(raw.dta) %>% 
    arrange(Jahr, Monat)
  
  return(dta)
}

## load data from disk
## if doesn't exist, pull data from DWD
if(file.exists("dwd.Rdata")){
  load("dwd.Rdata")
} else {
  dta <- f.get.dwd.data()
  save(dta, file = "dwd.Rdata")
}
















