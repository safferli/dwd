# http://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html
# ftp://ftp-cdc.dwd.de/pub/CDC/

rm(list = ls()); gc(); gc()
# options(java.parameters = "-Xmx8192m")
# options(java.parameters = "-XX:-UseConcMarkSweepGC")
# options(java.parameters = "-XX:-UseGCOverheadLimit")
options(bitmapType='cairo')
options(scipen = 999)

# convert IPs to integers
# http://datadrivensecurity.info/blog/posts/2014/May/speeding-up-ipv4-address-conversion-in-r/
library(Rcpp)
library(inline)
Rcpp::sourceCpp("iputils.cpp")

library(readr)
library(ggplot2)
library(fasttime)
library(dplyr)
library(tidyr)
library(DBI)

wd <- "~/Documents/gitlab/anno6"
setwd(wd)
