#Load the dplyr package.
library(dplyr)
#Load the ggplot2 package.
library(ggplot2)
#Load the lubridate package.
library(lubridate)

#Load the dataset "economics".
data("economics")

#Define a function that filters for the highest per capita consumption value in a given year.
transformed_economics <- function(y1){
  transformed_data <- economics %>% 
    #Add a new colunm to display per capita consumption.
    mutate(per_capita_consumption = economics$pce*10^9/(economics$pop*1000)) %>%
    #Filter the row with the highest per capita consumption.
    filter(year(as.Date(date)) == y1) %>% 
    filter(per_capita_consumption == max(per_capita_consumption)) %>%
    #Show the final tidy chart using 'summarise()' function. 
    summarise(Year = y1, 
              Date = date, 
              Max_pcc = per_capita_consumption)
  return(transformed_data)
}

#Preview the first few rows of transformed data.
print(transformed_economics(1980))
