#Loading packages
library(dplyr)
library(eeptools)
library(glue)

#Loading dataset
data_original <- read.csv("fulldata.csv")
data <- data_original

#Cleaning data

##Removing incomplete data
data <- data[!is.na(as.Date(data$dob)),]
data <- data %>% mutate(birthyear = as.numeric(substr(data$dob, 1, 4)))

#Calculating age
data <- data %>% mutate(draft_age = age_calc(as.Date(data$dob), as.Date(ifelse(as.Date(data$dob) > as.Date(paste(data$birthyear, "-09-15", sep="")), paste((as.numeric(data$birthyear)+18), "-09-15", sep=""), paste((as.numeric(data$birthyear)+17), "-09-15", sep=""))), units = "years"))

#Finding first draft eligible year
data <- data %>% mutate(first_dy = ifelse(as.Date(data$dob) > as.Date(paste(as.numeric(data$birthyear), "-09-15", sep="")), as.numeric(data$birthyear)+19, as.numeric(data$birthyear)+18))




data_test <- data
data_test <- data_test[c("playername", "link", "season", "position", "league", "team", "gp", "g", "a", "tp", "ppg", "dob", "height", "weight", "birthyear", "draft_age", "first_dy")]
forwards <- data_test[!data_test$position == "D",]
defenders <- data_test[data_test$position == "D",]

#Building forward list
forwards_list <- list()
for(i in as.numeric(rownames(forwards[match(unique(forwards$link), forwards$link),]))) {
  forwards_list[[length(forwards_list) + 1]] <- forwards[forwards$link == forwards$link[i],]
}

#Renaming list elements to player links to remove non-CHLers
names_vector = character()
for(i in 1:length(forwards_list)) {
  names_vector[i] <- forwards_list[[i]][['link']][1]
}
names(d) <- names_vector

#Removing non-CHLers
vector_list <- character()
for(i in 1:length(forwards_list)) {
  if(!("ohl" %in% forwards_list[[i]][['league']] || "whl" %in% forwards_list[[i]][['league']] || "qmjhl" %in% forwards_list[[i]][['league']])) {
    vector_list[i] <- forwards_list[[i]][['link']][1]
  } 
}
vector_list <- vector_list[!is.na(vector_list)]
for(i in vector_list) {
  forwards_list[[i]] <- NULL
}

#Renaming list elements to player names
names_vector = character()
for(i in 1:length(forwards_list)) {
  names_vector[i] <- forwards_list[[i]][['playername']][1]
}
names(forwards_list) <- names_vector

#Building defenders list
defenders_list <- list()
for(i in as.numeric(rownames(defenders[match(unique(defenders$link), defenders$link),]))) {
  defenders_list[[length(defenders_list) + 1]] <- defenders[defenders$link == defenders$link[i],]
}

#Renaming list elements to player links to remove non-CHLers
names_vector = character()
for(i in 1:length(defenders_list)) {
  names_vector[i] <- defenders_list[[i]][['link']][1]
}
names(defenders_list) <- names_vector

#Removing non-CHLers
vector_list <- character()
for(i in 1:length(defenders_list)) {
  if(!("ohl" %in% defenders_list[[i]][['league']] || "whl" %in% defenders_list[[i]][['league']] || "qmjhl" %in% defenders_list[[i]][['league']])) {
    vector_list[i] <- defenders_list[[i]][['link']][1]
  } 
}
vector_list <- vector_list[!is.na(vector_list)]
for(i in vector_list) {
  defenders_list[[i]] <- NULL
}

#Renaming list elements to player names
names_vector = character()
for(i in 1:length(defenders_list)) {
  names_vector[i] <- defenders_list[[i]][['playername']][1]
}
names(defenders_list) <- names_vector

