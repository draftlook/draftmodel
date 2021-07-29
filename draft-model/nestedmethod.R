
###Get player statistics function
get_player_stats <- function(dataset) {
  library(dplyr)
  library(eeptools)
  library(glue)
  library(tidyverse)
  library(purrr)
  #Loading dataset
  data_original <- read.csv(dataset)
  data <- data_original
  
  #Cleaning data
  
  ##Removing incomplete data
  data <- data[!is.na(as.Date(data$dob)),]
  data <- data %>% mutate(birthyear = as.numeric(substr(data$dob, 1, 4)))
  
  #Calculating age
  data <- data %>% mutate(draft_age = age_calc(as.Date(data$dob), as.Date(ifelse(as.Date(data$dob) > as.Date(paste(data$birthyear, "-09-15", sep="")), paste((as.numeric(data$birthyear)+18), "-09-15", sep=""), paste((as.numeric(data$birthyear)+17), "-09-15", sep=""))), units = "years"))
  
  #Finding first draft eligible year
  data <- data %>% mutate(first_dy = ifelse(as.Date(data$dob) > as.Date(paste(as.numeric(data$birthyear), "-09-15", sep="")), as.numeric(data$birthyear)+19, as.numeric(data$birthyear)+18))
  
  wdata <- data
  
  #Creating nested dataframe
  grouped_data <- wdata[c("playername", "link", "season", "position", "league", "team", "gp", "g", "a", "tp", "ppg", "dob", "height", "weight", "birthyear", "draft_age", "first_dy")]
  grouped_data <- grouped_data[order(grouped_data$season),] %>%
    group_by(playername)%>%
    nest(data = c(season, league, team, gp, g, a, tp, ppg))
  
  #Removing non-CHLers
  number_vector <- integer()
  for(i in 1:nrow(grouped_data)) {
    ifelse(!("ohl" %in% grouped_data$data[[i]]$league || "whl" %in% grouped_data$data[[i]]$league || "qmjhl" %in% grouped_data$data[[i]]$league), number_vector[i] <- i, FALSE)
  }
  number_vector <- number_vector[!is.na(number_vector)]
  grouped_data <- grouped_data[-number_vector,]
  
  #Removing players with less than 5 post-draft seasons
  grouped_data <- grouped_data %>% subset(grouped_data$first_dy < 2017)
  return(grouped_data)
}






             