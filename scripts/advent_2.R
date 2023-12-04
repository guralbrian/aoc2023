data <- c('Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green',
          'Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue',
          'Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red',
          'Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red',
          'Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green')

#### Part 1
data <- readLines("input.txt")

# Parse out the data into games, rounds and pulls
n.list <-  lapply(str_split(data, ": "), "[[", 2) %>% 
        str_split(., "; ") %>%
        lapply(function(x){
        lapply(x, function(y){str_split(y, ", ")})
        })

# Initialize df
df <- data.frame(game = rep(NA, 10^5),
                 round = NA,
                 pull = NA)

# Pull out nested data into long df
i <- 1
for(game in 1:length(n.list)){
  print(game)
  for(round in 1:length(n.list[[game]])){
    print(round)
    for(pull in 1:length(n.list[[game]][[round]][[1]])){
      print(pull)
      df[i,"game"]  <- game
      df[i,"round"] <- round
      df[i,"pull"]  <- n.list[[game]][[round]][[1]][[pull]]
      i <- i + 1
    }
  }
}

df <- na.omit(df)

# Split up draws into color and counts
df$count <-  lapply(df$pull, function(x){str_split(x, " ")[[1]][[1]]})
df$color <-  lapply(df$pull, function(x){str_split(x, " ")[[1]][[2]]})

df <- df |> select(-pull)

df.wide <- df |> pivot_wider(id_cols= c("game", "round"), 
                        values_from = "count", 
                        names_from = "color")

df.final <- df.wide %>%
  group_by(game, round) %>%
  mutate(
    # Replace NULL with NA and convert to numeric
    blue = as.numeric(unlist(map(blue, ~ if(is.null(.x)) 0 else .x))),
    red = as.numeric(unlist(map(red, ~ if(is.null(.x)) 0 else .x))),
    green = as.numeric(unlist(map(green, ~ if(is.null(.x)) 0 else .x))),
    # Correct the case_when usage
    possible = case_when(
      red <= 12 & green <= 13 & blue <= 14 ~ "yes",
      TRUE ~ "no"))

# Filter for games that have "no" in any round
games_with_no <- df.final %>%
  filter(possible == "no") %>%
  group_by(game) |> 
  slice_head(n=1) |> # Get distinct games that have "no"
  pull(game) |>  # Extract the game numbers as a vector
  sum()

sum_of_games <- df.final %>%
  group_by(game) |> 
  slice_head(n=1) |> # Get distinct games that have "no"
  pull(game) |>  # Extract the game numbers as a vector
  sum()


# Output the sum
print(sum_of_games - games_with_no)

#### Part 2

df.max <- df.final |> 
  group_by(game) |> 
  mutate(m.blue  = max(blue),
         m.green = max(green),
         m.red   = max(red)) |> 
  group_by(game) |> 
  slice_head(n = 1) |> 
 mutate(power = m.blue * m.red * m.green) |> 
 pull(power)

sum(df.max)





