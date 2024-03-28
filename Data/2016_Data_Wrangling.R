require(tidycensus)
require(censusapi)
require(dplyr)
require(tidyr)
require(ggplot2)
require(writexl)

county_data_2016 <- read.csv("county_facts_2016.csv")
county_facts_dictionary <- read.csv("county_facts_dictionary.csv")

county_data_2016 <- as.data.frame(county_data_2016)
county_facts_dictionary <- as.data.frame(county_facts_dictionary)

colnames(county_data_2016)[4:54] <- county_facts_dictionary$description


