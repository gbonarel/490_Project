require(readxl)
require(dplyr)

data2016 <- read_xlsx(path = "C:\\Users\\17865\\Downloads\\IE490Dataset(1).xlsx",
                      sheet = 1)
data2016 <- data2016[complete.cases(data2016[,1:38]),]
write_xlsx(data2016, "C:\\Users\\17865\\Downloads\\IE490Dataset.xlsx")
readline(prompt = "Press enter once the 2016 data is updated: ")

data2020 <- read_xlsx(path = "C:\\Users\\17865\\Downloads\\IE490Dataset(1).xlsx",
                      sheet = 2)
data2020 <- data2020[complete.cases(data2020[,1:38]),]
write_xlsx(data2020, "C:\\Users\\17865\\Downloads\\IE490Dataset.xlsx")
readline(prompt = "Press enter once the 2020 data is updated: ")

data2024 <- read_xlsx(path = "C:\\Users\\17865\\Downloads\\IE490Dataset(1).xlsx",
                      sheet = 3)
data2024 <- data2024[complete.cases(data2024[,1:38]),]
write_xlsx(data2024, "C:\\Users\\17865\\Downloads\\IE490Dataset.xlsx")
readline(prompt = "Press enter once the 2024 data is updated: ")

data <- data2024

for(i in 1:nrow(data)){
  county <- as.character(data[i, 1])
  state <- as.character(data[i, 2])
  
  for(j in 38:47){
    if(is.na(data[i, j])){
      row2016 <- data2016[data2016$NAME == county & data2016$STATE == state,j]
      if(nrow(row2016)){
        data[i,j] <- row2016
      }
    }
  }
}
data <- data[complete.cases(data[,1:48]),]
write_xlsx(data, "C:\\Users\\17865\\Downloads\\IE490Dataset.xlsx")
