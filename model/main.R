source("functions.R")

if(!exists("loaded_data")) {loaded_data <- get_dy_data("fulldata.csv")}
comparables <- get_comparables("Alexis LafreniÃ¨re", loaded_data)
comparable_names <- comparables$playername
if(!exists("comparable_careers")) {comparable_careers <- get_player_stats("fulldata.csv")}
comparable_careers <- comparable_careers[comparable_careers$playername %in% comparable_names,]
comparable_paths <- get_paths(comparable_careers)


dataset <- comparable_careers

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
                    
                    







  
  
