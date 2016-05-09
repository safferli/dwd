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

nrw %>% ggplot(aes(x=year, y=temperature))+
  geom_line(aes(group = region, color = temperature > avg.10.years))+
  geom_line(aes(x=year, y=avg.10.years))
ggsave(file="temp-mess.png", width = 30, height = 30/((1+sqrt(5))/2), units = "cm")



# http://stackoverflow.com/questions/9543402/how-to-obtain-multiple-colours-for-geom-line-conditional-on-a-specific-value



#new data for testing
tt <- nrw
# resolution scale (0.01 = factor 100)
yres <- 0.01

tt %<>% 
  mutate(
    # rename to use in function later
    x = year,
    y = temperature,
    z = avg.10.years, 
    # generate point coordinates for line end
    x2 = lead(x),
    y2 = lead(y),
    z2 = lead(z)
  ) %>% 
  # remove last row from group
  filter(!row_number() == n()) %>% 
  # remove years in which z is NA
  # (chokes the summarising later)
  filter(!x %in% .$x[is.na(z)]) %>% 
  # get high-resolution versions of the inter-point data
  rowwise() %>% 
  ## old version, use for options below to unnest()/unlist() data: 
  # mutate(
  #   y.hr = list(seq(y, y2, yres*sign(y2-y))),
  #   x.hr = list(seq(x, x2, length.out = length(y.hr))),
  #   z.hr = list(seq(z, z2, length.out = length(y.hr)))
  # )
  do(
    # for each row, generate dataframe with the low-res data,
    # which gets automatically filled up to the high-res data rows
    data.frame(
      year = .$year,
      month = .$month,
      region = .$region,
      temperature = .$temperature,
      x = .$x,
      x2 = .$x2,
      # make a sequence (linear line) of the interpoint data
      # we will use this data length as "basis" for x.hr and z.hr, below
      y.hr = seq(.$y, .$y2, yres*sign(.$y2-.$y)),
      avg.10.years = .$z,
      z2 = .$z2
    ) %>% 
      mutate(
        # make a sequence (linear line) of the remaining interpoint data
        # take the rownumber of data needed from y.hr, above
        x.hr = seq(min(x), min(x2), length.out = nrow(.)),
        z.hr = seq(min(z), min(z2), length.out = nrow(.))
      )
  )

## plot everything! 
ggplot()+
  # high-res version of the temperature data
  geom_line(data = tt, aes(x=x.hr, y=y.hr, group = region, color = y.hr > z.hr))+
  # add rolling average as line
  geom_line(data = tt, aes(x=year, y=avg.10.years, color = "10y rolling avg"))+
  guides(
    colour = guide_legend("temperature is larger than \n10-year rolling avg")
  )+
  # change line colours -- black for the line, red if above, green if below
  scale_color_manual(values=c("#000000", "#A5CF35", "#C44B4B"))+
  labs(title = "March temperatures in North Rhine-Westphalia",
       x = "", y = "temperature (March monthly average)")
# save everything, using golden ratio
ggsave(file="temperature.png", width = 30, height = 30/((1+sqrt(5))/2), units = "cm")




## different ways to unnest the hr data:

## unlist() into new dataframe (cannot do this in dplyr pipe); this is what the original stackoverflow answer did

# new.hr <- data.frame(
#   year = unlist(tt$x.hr), 
#   temperature = unlist(tt$y.hr), 
#   avg.10.year.temperature = unlist(tt$z.hr),
#   region = tt$region[1]
# )

# # Yuki's version with unnest() (can do in dplyr pipe, but cannot do this in group_by())
# tt2 <- tt %>% 
#   #group_by(region) %>% 
#   select(year = x.hr, temperature = y.hr, avg.10.years = z.hr) %>% 
#   unnest()

# # old plot version, with "new.hr" dataframe
# ggplot()+
#   geom_line(data = new.hr, aes(x=year, y=temperature, group = region, color = temperature > avg.10.year.temperature))+
#   geom_line(data = new.hr, aes(x=year, y=avg.10.year.temperature, color = "10y rolling avg"))+
#   #guide_legend("temperature > \n 10-year rolling average")+
#   guides(
#     colour = guide_legend("temperature is larger than \n10-year rolling avg")
#     )+
#   # change line colour -- black for the line, red if above, green if below
#   scale_color_manual(values=c("#000000", "#A5CF35", "#C44B4B"))+
#   labs(title = "March temperatures in North Rhine-Westphalia",
#       x = "", y = "temperature (March monthly average)")
# #ggsave(file="temperature.png", width = 30, height = 30/((1+sqrt(5))/2), units = "cm")

