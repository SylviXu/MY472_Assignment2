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
  #Transform 2.
  transformed_data2 <- economics %>%
    #Add a column showing the month-on-month change in the broad unemployment rate.
    mutate(unemployment_change = (economics$unemploy/economics$pop)*100 - lag(economics$unemploy/economics$pop)*100) %>%
    #Filter out all data for the selected year. 
    filter(year(date) == y2) %>%
    #Remove the first month's data as the month-on-month change has no comparator in the first month.
    filter(!is.na(unemployment_change)) %>%
    group_by(date) %>%
    #Show the final tidy chart.
    summarise(unemployment_change)
  result <- list(transformed_data, transformed_data2)
  return(result)
}

#Preview the first few rows of transformed data.
print(transformed_economics(1980))
