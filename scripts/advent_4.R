data <- readLines("advent/input_4.txt")

# Parse the data
data.split <- str_split(data, "[:]") %>% 
  lapply(. , "[[", 2) %>%  
  str_split(., "[|]")

# function to split string into list
splitCards <- function(position){
    lapply(data.split, '[[', position) %>% 
    lapply(., function(cards){str_trim(cards)}) %>%
    lapply(., function(cards){str_split(cards, "  | ")})
}
cards <- splitCards(2)
winners <- splitCards(1)

# Make loop to compare each game
# Tally # of matches
# 2^matches
points <- c()
for(games in 1:length(cards)){
  matches <- cards[[games]][[1]][which(cards[[games]][[1]] %in% winners[[games]][[1]])] |> length()
  if(matches != 0) {
  points[games] <-  2^(matches-1)
  }else(
    points[games] <- 0
  )
}

sum(points)

#### Part 2
# Make list of wins per card
matches <- c()
for(games in 1:length(cards)){
  matches[games] <- cards[[games]][[1]][which(cards[[games]][[1]] %in% winners[[games]][[1]])] |> 
    length() 
}

# Initialize vector of total cards 
cards <- rep(1,length(matches))

# for each card, add the count of itself to the next n cards, where n is the number of wins on the initial card
for(games in 1:length(cards)){
  # Add cards[games] to cards[(games+1):(games+matches[games])]
  if(matches[games] != 0) {
  cards[(games+1):(games+matches[games])] <- cards[(games+1):(games+matches[games])] + cards[games]
  }
} 

sum(cards)


