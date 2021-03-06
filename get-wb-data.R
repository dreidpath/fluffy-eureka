library(wbstats)
library(tidyverse)

# Read in the world bank Income Group data
income_groupDF <- readxl::read_excel("OGHIST.xls", 
                                     sheet = "Country Analytical History", 
                                     col_names = FALSE, na = "..", skip = 11)
# Rename the variables
names(income_groupDF) <- c("iso3c", "country", paste("yr", 1987:2016, sep=""))

# Select 1990-2015
income_groupDF <- select(income_groupDF, iso3c, country, yr1990:yr2015)
# Remove rows >=219.  These are countries that have disappeared, or notes
income_groupDF <- income_groupDF[1:218, ]




new_wb_cache <- wbcache()  # list of information regarding countries, indicators, etc.

# Set up the character strings of the indicators to be fetched
co2Indicators <- wbsearch("co2.*capita", cache = new_wb_cache)$indicatorID  # CO2, Methane, NO
gni <- c("NY.GNP.PCAP.PP.KD")  # GNI per capita, PPP (constant 2011 international $)
gdp <- c("NY.GDP.PCAP.PP.KD")  # GDP per capita, PPP (constant 2011 international $)
life_expect <- c("SP.DYN.LE00.IN", "SP.DYN.LE60.MA.IN", "SP.DYN.LE60.FE.IN")  # Life expectancy
total_pop <- c("SP.POP.TOTL")
mortality <- c("IN.POV.INF.MORTRATE.UNDR5", "IN.POV.INF.MORTRATE", "SP.DYN.IMRT.IN")

# Fetch indicators and save as a dataframe
dataDF <- wb(indicator = c(co2Indicators, gni, gdp,
                         life_expect,total_pop, 
                         mortality), 
           start = 1990, end = 2016,
           POSIXct = T)

# Select the same indicators but for the World as a whole
worldDF <- wb(country = "WLD", indicator = c(co2Indicators, gni, gdp,
                           life_expect,total_pop, 
                           mortality), 
             start = 1980, end = 2016,
             POSIXct = T)
