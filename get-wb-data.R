library(wbstats)
library(tidyverse)


new_wb_cache <- wbcache()  # list of information regarding countries, indicators, etc.


co2Indicators <- wbsearch("co2.*capita", cache = new_wb_cache)$indicatorID  # CO2, Methane, NO
gni <- c("NY.GNP.PCAP.PP.KD")  # GNI per capita, PPP (constant 2011 international $)
gdp <- c("NY.GDP.PCAP.PP.KD")  # GDP per capita, PPP (constant 2011 international $)
life_expect <- c("SP.DYN.LE00.IN", "SP.DYN.LE60.MA.IN", "SP.DYN.LE60.FE.IN")  # Life expectancy
total_pop <- c("SP.POP.TOTL")
mortality <- c("IN.POV.INF.MORTRATE.UNDR5", "IN.POV.INF.MORTRATE", "SP.DYN.IMRT.IN")

wbsearch("infant.*mortality", cache = new_wb_cache)


myDT <- wb(indicator = c("SP.POP.TOTL",
                         "SP.DYN.LE00.IN",
                         "SP.DYN.TFRT.IN"), 
           mrv = 60)
