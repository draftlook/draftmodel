
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
  data$playername <- trimws(data$playername)
  
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

##Getting draft year data function
get_dy_data <- function(dataset) {
  library(dplyr)
  library(eeptools)
  library(glue)
  library(tidyverse)
  library(purrr)
  data_original <- read.csv(dataset)
  data <- data_original
  data <- data[!is.na(as.Date(data$dob)),]
  data <- data %>% mutate(birthyear = as.numeric(substr(data$dob, 1, 4)))
  
  #Calculating age
  data <- data %>% mutate(draft_age = age_calc(as.Date(data$dob), as.Date(ifelse(as.Date(data$dob) > as.Date(paste(data$birthyear, "-09-15", sep="")), paste((as.numeric(data$birthyear)+18), "-09-15", sep=""), paste((as.numeric(data$birthyear)+17), "-09-15", sep=""))), units = "years"))
  
  #Finding first draft eligible year
  data <- data %>% mutate(first_dy = ifelse(as.Date(data$dob) > as.Date(paste(as.numeric(data$birthyear), "-09-15", sep="")), as.numeric(data$birthyear)+19, as.numeric(data$birthyear)+18))
  
  data <- data[c("playername", "link", "season", "position", "league", "team", "gp", "g", "a", "tp", "ppg", "dob", "height", "weight", "birthyear", "draft_age", "first_dy")]
  data <- data[order(data$season),]
  dy_data <- data[paste("20", substr(data$season, 6,7), sep="") == data$first_dy,]
  return(dy_data)
}

##Get comparables function
get_comparables <- function(player, dy_data) {
  dy_data$playername <- trimws(dy_data$playername)
  dy_data <- dy_data[!dy_data$ppg == "-",]
  dy_data <- dy_data[!dy_data$height == "-",]
  position <- dy_data$position[dy_data$playername == player]
  if(position == "D") {
    dy_data <- dy_data[dy_data$position == "D",]
  } else {
    dy_data <- dy_data[!dy_data$position == "D",]
  }
  dy_data <- subset(dy_data, as.numeric(dy_data$gp) > 15)
  dy_data <- dy_data %>% mutate(ppg_zscore = scale(as.numeric(dy_data$ppg))) %>% mutate(age_zscore = scale(as.numeric(dy_data$draft_age))) %>% mutate(height_zscore = scale(as.numeric(dy_data$height)))
  
  #Calculating similarity scores
  dy_data <- dy_data %>% mutate(similarity = sqrt((((dy_data$ppg_zscore[dy_data$playername == player] - dy_data$ppg_zscore)^2)*1) +(((dy_data$age_zscore[dy_data$playername == player] - dy_data$age_zscore)^2)*0.5) +(((dy_data$height_zscore[dy_data$playername == player] - dy_data$height_zscore ))*0.5)))
  dy_data <- dy_data %>% mutate(scaled_sim = (1-(dy_data$similarity - min(dy_data$similarity))/(max(dy_data$similarity)-min(dy_data$similarity)))*100)
  dy_data <- dy_data[order(dy_data$scaled_sim),]
  dy_data <- dy_data[dy_data$first_dy < 2017,]
  dy_data <- dy_data[!(dy_data$playername == player),]
  comparables <- dy_data[c("playername", "scaled_sim")]
  comparables <- comparables[comparables$scaled_sim > 80,]
  comparables <- comparables %>% mutate(compared_player = player)
  return(comparables)
}

##Get comparable paths
get_paths <- function(dataset) {
  temp_df <- as.data.frame(character())
  for(i in 1:nrow(dataset)) {
    dy <- dataset$first_dy[i]
    player <- dataset$playername[i]
    comparable_one <- dataset$data[i][[1]]
    comparable_one <- comparable_one %>% group_by(season) %>% slice(which.max(gp))
    index_vector <- c(
      which(paste("20", substr(comparable_one$season, 6,7),sep="") == dy),
      which(paste("20", substr(comparable_one$season, 6,7),sep="") == dy+1),
      which(paste("20", substr(comparable_one$season, 6,7),sep="") == dy+2),
      which(paste("20", substr(comparable_one$season, 6,7),sep="") == dy+3),
      which(paste("20", substr(comparable_one$season, 6,7),sep="") == dy+4),
      which(paste("20", substr(comparable_one$season, 6,7),sep="") == dy+5))
    comparable_one <- comparable_one[index_vector,]
    path <- comparable_one$league
    comparable_one <- comparable_one %>% mutate(player = player)
    comparable_one <- comparable_one %>% mutate(path = glue_collapse(path, " "))
    temp_df <- rbind(temp_df, comparable_one)
  }
  temp_df <- temp_df %>% ungroup() %>%
    mutate(change = as.numeric(ppg)/as.numeric(lag(ppg)))
  temp_df$change[!temp_df$player == lag(temp_df$player)] <- 1
  temp_df$change[1] <- 1
  temp_df <- temp_df %>%
    group_by(player)
  
  format_comparables <- temp_df %>%
    group_by(player) %>%
    nest(data = c(season, league, team, gp, g, a, tp, ppg, change))
  format_comparables$path <- format_comparables$path %>% str_replace_all("ohl", "chl")
  format_comparables$path <- format_comparables$path %>% str_replace_all("whl", "chl")
  format_comparables$path <- format_comparables$path %>% str_replace_all("qmjhl", "chl")
  
  
  double_format_comparables <- format_comparables %>%
    group_by(path) %>%
    nest(data = c(player, data))
  return(double_format_comparables)
}






             