source("functions.R")

##Getting data (run once)
if(!exists("player_stats")) {player_stats <- get_player_stats("fulldata.csv")}
if(!exists("dy_data")) {dy_data <- get_dy_data("fulldata.csv")}

player <- "Alexis LafreniÃ¨re"
loaded_data <- dy_data
comparables <- get_comparables(player, loaded_data)
comparable_names <- comparables$playername
comparable_careers <- player_stats
comparable_careers <- comparable_careers[comparable_careers$playername %in% comparable_names,]
comparable_paths <- get_paths(comparable_careers)
comparable_paths <- get_curves(comparable_paths)

ppg <- as.numeric(loaded_data$ppg[loaded_data$playername == player])
projection <- comparable_paths %>% ungroup()
projection <- projection %>% mutate(player = loaded_data$playername[loaded_data$playername == player])
projection <- projection %>% mutate(dy = ppg)
projection <- projection %>% mutate(plus_one = projection$dy * projection$one)
projection <- projection %>% mutate(plus_two = projection$plus_one * projection$two)
projection <- projection %>% mutate(plus_three = projection$plus_two * projection$three)
projection <- projection %>% mutate(plus_four = projection$plus_three * projection$four)
projection <- projection %>% mutate(plus_five = projection$plus_four * projection$five)



