require(tidycensus)
require(censusapi)
require(dplyr)
require(tidyr)
require(ggplot2)
require(writexl)

#countyFacts <- read.csv(
#  "C:\\Users\\17865\\Downloads\\archive\\county_facts.csv")
#countyFactsCols <- read.csv(
#  "C:\\Users\\17865\\Downloads\\archive\\county_facts_dictionary.csv")
#presResults12and16 <- read.csv(
#  "C:\\Users\\17865\\Downloads\\archive\\US_County_Level_Presidential_Results_12-16.csv"
#)
#votes2016 <- read.csv("C:\\Users\\17865\\Downloads\\archive\\votes.csv")

#census_api_key("a53a105245550a2a1825faf83c3cf51c1d8edf3f", install = TRUE)
Y <- 2020
vars <- load_variables(year = Y, dataset = "acs5/profile", cache = FALSE)

peopleVars = c( #Population ethnicity/race variable names
        "DP05_0001" #Total Population
        ,"DP05_0005P" #Persons under 5 years
        ,"DP05_0019P" #Persons over 18 years 
        ,"DP05_0024P" #Persons over 65 
        ,"DP05_0003P" #Female persons
        ,"DP05_0037P" #White only
        ,"DP05_0038P" #Black or African american only
        ,"DP05_0039P" #Alaskan native or American Indian only
        ,"DP05_0044P" #Asian only
        ,"DP05_0052P" #Native Hawaiian or Pacific Islander
        ,"DP05_0058P" #Two or more races
        ,"DP05_0071P" #Hispanic or Latino
        ,"DP05_0077P" #White alone, not hispanic or latino
        ,"DP02_0080P" #Same house 1 year and over
        ,"DP02_0094P" #Foreign born persons
        ,"DP02_0114P" #Language other than english spoken at home
        #Education and employment variable names
        ,"DP02_0067P" #High school or more attained
        ,"DP02_0068P" #Bachelors or higher
        ,"DP02_0070P" #Veterans
        ,"DP03_0025" #Mean travel time to work
        #Housing variable names
        ,"DP04_0001" #Number of housing units
        ,"DP04_0046P" #Number of owner-occupied units
        ,"DP04_0009P" #Number of two-unit structures
        ,"DP04_0010P" #Number of three or four unit structures
        ,"DP04_0011P" #Number of 5-9 unit structures
        ,"DP04_0012P" #Number of 10-19 unit structures
        ,"DP04_0013P" #Number of 20+ unit structures
        ,"DP04_0089" #Median value of owner-occupied units
        ,"DP02_0001" #Households
        ,"DP02_0016" #Avg household size
        ,"DP03_0088" #Per capita income
        ,"DP03_0062" #Median household income
        ,"DP03_0128P" #Percentage of individuals below the poverty level
        #Employment variable names
        )

censusData <- get_acs(geography = "county",
                      variables  = peopleVars, 
                      year = Y)
finalSet <- pivot_wider(censusData,
                        id_cols = NAME,
                        names_from = variable,
                        values_from = estimate)
dict <- c( "NAME", "Households", "Avg household size",
          "High school or more attained", "Bachelors or higher",
          "Veterans", "Same house 1 year and over",
          "Foreign born persons", "Language other than english spoken at home",
          "Mean travel time to work", "Median household income",
          "Per capita income", "Percentage of individuals below the poverty level",
          "Number of housing units", "Number of two-unit structures",
          "Number of three or four unit structures", "Number of 5-9 unit structures",
          "Number of 10-19 unit structures", "Number of 20+ unit structures",
          "Number of owner-occupied units", "Median value of owner-occupied units",
          "Total Population", "Female persons",
          "Persons under 5 years", "Persons over 18 years ",
          "Persons over 65 ", "White only", "Black or African american only",
          "Alaskan native or American Indian only", "Asian only",
          "Native Hawaiian or Pacific Islander", "Two or more races",
          "Hispanic or Latino", "White alone not hispanic or latino")
colnames(finalSet) <- dict

#______________________________________________________________#
Sys.setenv(CENSUS_KEY="a53a105245550a2a1825faf83c3cf51c1d8edf3f")

#Private non-farm establishment and employment, need percent change
nonFarm <- getCensus(
  name = "cbp",
  vintage = Y,
  vars = c("NAME","ESTAB", "EMP"),
  region = "county:*")
nonFarm <- subset(nonFarm, select = -state)
nonFarm <- subset(nonFarm, select = -county)
finalSet <- merge(finalSet, nonFarm, by="NAME", all = TRUE)

#Number of nonemployer establisments
nonEmp <- getCensus(
  name = "nonemp", 
  vintage = Y, 
  vars = c("NAME", "NESTAB"),
  region = "county:*"
)
nonEmp <- subset(nonEmp, select = -state)
nonEmp <- subset(nonEmp, select = -county)
finalSet <- merge(finalSet, nonEmp, by="NAME", all = TRUE)

#Business statistics
businessVar <- c("NAME", "NAICS2017", "NAICS2017_LABEL", "SEX", "SEX_LABEL",
                 "ETH_GROUP", "ETH_GROUP_LABEL", "RACE_GROUP", "RACE_GROUP_LABEL",
                 "VET_GROUP", "VET_GROUP_LABEL", "FIRMPDEMP", "RCPPDEMP", "EMP",
                 "PAYANN")


firmData <- getCensus(
  name = "abscs",
  vintage = 2017,
  vars = businessVar,
  region = "county:*",
)

counties <- unique(firmData$NAME)
#All firms in each county
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "001") %>% 
  filter(ETH_GROUP == "001") %>% 
  filter(RACE_GROUP == "00") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "All Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#Black owned firms
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "001") %>% 
  filter(ETH_GROUP == "001") %>% 
  filter(RACE_GROUP == "40") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "Black Owned Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#American Indian and Alaskan Native owned firms
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "001") %>% 
  filter(ETH_GROUP == "001") %>% 
  filter(RACE_GROUP == "50") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "American Indian and Alaskan Native Owned Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#Asian owned firms
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "001") %>% 
  filter(ETH_GROUP == "001") %>% 
  filter(RACE_GROUP == "60") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "Asian Owned Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#Native Hawaiian / Pacific Islander owned firms
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "001") %>% 
  filter(ETH_GROUP == "001") %>% 
  filter(RACE_GROUP == "70") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "Native Hawaiian or Pacific Islander Owned Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#Hispanic owned firms
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "001") %>% 
  filter(ETH_GROUP == "020") %>% 
  filter(RACE_GROUP == "00") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "Hispanic Owned Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#Women Owned Firms
firms <- firmData %>%
  filter(NAME %in% counties) %>% 
  filter(NAICS2017 == "00") %>%
  filter(SEX == "002") %>% 
  filter(ETH_GROUP == "001") %>% 
  filter(RACE_GROUP == "00") %>% 
  filter(VET_GROUP == "001") %>% 
  select(NAME, FIRMPDEMP)
colnames(firms) <- c("NAME", "Women Owned Firms")
finalSet <- merge(finalSet, firms, by="NAME", all = TRUE)

#________________________________________________________#
#Economic variables: Only supported for 2017
econVar <- c("NAME", "RCPTOT")

#Wholesale sales

#Retail sales
econData <- getCensus(
  name = "ecnbasic",
  vintage = 2017,
  vars = c("NAME", "RCPTOT"),
  region = "county:*",
  NAICS2017 = "44-45"
)
econData <- subset(econData, select = econVar)
colnames(econData) <- c("NAME", "Retail Sales")
finalSet <- merge(finalSet, econData, by="NAME", all = TRUE)

#Accommodation and Food Services
econData <- getCensus(
  name = "ecnbasic",
  vintage = 2017,
  vars = c("NAME", "RCPTOT"),
  region = "county:*",
  NAICS2017 = "72"
)
econData <- subset(econData, select = econVar)
colnames(econData) <- c("NAME", "Accommodation and Food Sales")
finalSet <- merge(finalSet, econData, by="NAME", all = TRUE)
#________________________________________________________#
#Building permits

#________________________________________________________#
#Geographic
landData <- getCensus(
  name = "pep/population", 
  vintage = 2017, 
  vars = c("GEONAME", "DENSITY"),
  region = "county:*"
)
landData <- subset(landData, select = c("GEONAME", "DENSITY"))
colnames(landData) <- c("NAME", "Population Density")
finalSet <- merge(finalSet, landData, by="NAME", all = TRUE)

write_xlsx(finalSet, "C:\\Users\\17865\\Downloads\\IE490Dataset.xlsx")

