require(tidycensus)
#census_api_key("", install = TRUE)
vars = c( #Population ethnicity/race variable names
        "DP05_0001" #Total Population
        ,"DP05_0004P" #Persons under 5 years
        ,"DP05_0018P" #Persons over 18 years 
        ,"DP05_0021P" #Persons over 65 
        ,"DP05_0024P" #Female persons
        ,"DP05_0032P" #White only
        ,"DP05_0033P" #Black or African american only
        ,"DP05_0034P" #Alaskan native or American Indian only
        ,"DP05_0039P" #Asian only
        ,"DP05_0048P" #Native Hawaiian or Pacific Islander
        ,"DP05_0053P" #Two or more races
        ,"DP05_0066P" #Hispanic or Latino
        ,"DP05_0072P" #White alone, not hispanic or latino
        ,"DP02_0079P" #Same house 1 year and over
        ,"DP02_0092P" #Foreign born persons
        ,"DP02_0112P" #Language other than english spoken at home
        #Education and employment variable names
        ,"DP02_0066P" #High school or more attained
        ,"DP02_0067P" #Bachelors or higher
        ,"DP02_0069P" #Veterans
        ,"DP03_0025" #Mean travel time to work
        #Housing variable names
        ,"DP04_0001" #Number of housing units
        ,"DP04_0045P" #Number of owner-occupied units
        ,"DP04_0009P" #Number of two-unit structures
        ,"DP04_0010P" #Number of three or four unit structures
        ,"DP04_0011P" #Number of 5-9 unit structures
        ,"DP04_0012P" #Number of 10-19 unit structures
        ,"DP04_0013P" #Number of 20+ unit structures
        ,"DP04_0088" #Median value of owner-occupied units
        ,"DP02_0001" #Households
        ,"DP02_0015" #Avg household size
        ,"DP04_0088" #Per capita income
        ,"DP04_0062" #Median household income
        ,"DP03_0128P" #Percentage of individuals below the poverty level
        #Employment variable names
        
        )

censusData <- get_acs(geography = "county",
                      variables  = vars, 
                      year = 2012)
