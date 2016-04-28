# http://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html
# ftp://ftp-cdc.dwd.de/pub/CDC/

rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx8192m")
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)

library(dplyr)
library(zoo)
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
  
  # load libraries needed in function
  library(readr)
  library(tidyr)
  
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
    raw.dta[[i]] <- read.csv2(text = txt.dta, stringsAsFactors = FALSE, dec = ".") %>% 
      # last row is fubar'ed, remove
      select(-X)
  }
  
  # bind all into one data frame
  dta <- bind_rows(raw.dta) %>% 
    # clean data set
    gather(region, temperature, -Jahr, -Monat) %>% 
    # rename year and month to English
    select(year = Jahr, month = Monat, everything()) %>% 
    arrange(region, year, month)
  
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


## analysis starts here

nrw <- dta %>%
  filter(region == "Nordrhein.Westfalen") %>% 
  filter(month == 3) %>% 
  mutate(
    avg.10.years = rollmeanr(x = temperature,
                             k = 10,
                             fill = NA),
    check = temperature >= avg.10.years
  )

# http://stackoverflow.com/questions/9543402/how-to-obtain-multiple-colours-for-geom-line-conditional-on-a-specific-value
nrw %>% ggplot(aes(x=year, y=temperature))+
  geom_line(aes(group = region, color = temperature > avg.10.years))+
  geom_line(aes(x=year, y=avg.10.years))

















id<-rep(c(1,2,3),each=3)
y<-rnorm(9,2,1)
x<-rep(c(1,2,3),3)
stackOne<-data.frame(cbind(id,y,x))  

threshold <- 2.2 # set colour-transition threshold
yres <- 0.01 # y-resolution (accuracy of colour change location)

d <- stackOne # for code simplification
# new cols for point coordinates of line end
d$y2 <- c(d$y[-1], NA)
d$x2 <- c(d$x[-1], NA) 
d <- d[-findInterval(unique(d$id), d$id), ] # remove last row for each group

# new high-resolution y coordinates between each pair within each group
y.new <- apply(d, 1, function(x) {
  seq(x['y'], x['y2'], yres*sign(x['y2'] - x['y']))
})

d$len <- sapply(y.new, length) # length of each series of points
# new high-resolution x coordinates corresponding with new y-coords
x.new <- apply(d, 1, function(x) {
  seq(x['x'], x['x2'], length.out=x['len'])
})

id <- rep(seq_along(y.new), d$len) # new group id vector

y.new <- unlist(y.new)
x.new <- unlist(x.new)
d.new <- data.frame(id=id, x=x.new, y=y.new)

p <- ggplot(d.new, aes(x=x,y=y)) +
  geom_line(aes(group=d.new$id, color=d.new$y < threshold))+
  geom_point(data=stackOne)+
  scale_color_discrete(sprintf('Below %s', threshold))
p



#new data for testing
tt <- nrw
yres <- 0.01

tt %<>% 
  mutate(
  # rename to use in function later
  x = year,
  y = temperature,
  # generate point coordinates for line end
  x2 = lead(x),
  y2 = lead(y)
  ) %>% 
  # remove last row from group
  filter(!row_number() == n()) %>% 





tt2 <- tt %>% 
  rowwise() %>% 
  mutate(
    y.hr = list(seq(y, y2, yres*sign(y2-y))),
    x.hr = list(seq(x, x2, length.out = length(y.hr)))
  )



new.hr <- data.frame(
  x = unlist(tt2$x.hr), 
  y = unlist(tt2$y.hr)
)

new.hr <- tt2 %>% 
  group_by(region) %>% 
  summarise(
    x = unlist(x.hr), 
    y = unlist(y.hr)
  )





