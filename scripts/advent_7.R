# Advent 7
library(tidyverse)
library(data.table)

data <- c(
'32T3K 765',
'T55J5 684',
'KK677 28',
'KTJJT 220',
'QQQJA 483')

data <- readLines("data/input_7.txt")

# Add bets
hand <- lapply(strsplit(data, " "), "[[", 1)
# Parse data
df <- data.frame(do.call("rbind", strsplit(as.character(hand), "",
                                     fixed = TRUE)))
# Make df to convert cards to value tier
cards <- c('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
value.df <- data.frame(cards = cards, value = rev(seq(1, length(cards), 1)))

# Replace characters in df, using value.df as a dictionary
df <- apply(df, 2, function(column) {
  # Use 'match' to find the positions of 'df' elements in 'value.df'
  matched_positions <- match(column, value.df$cards)
  # Replace the positions with the corresponding values from 'value.df'
  value.df$value[matched_positions]
}) |> as.data.frame()

# Get duplicates
df <- lapply(apply(df, 1, table),function(x){table(x) %>% as.list()}) %>% 
  rbindlist(fill = T) %>% 
  cbind(df, .)

# Add bets
df$bet <- sapply(strsplit(data, " "), "[[", 2)

# Add hand # 
df$hand <- seq(1,nrow(df), 1)

df[is.na(df)] <- 0
df.ordered <- df |>
  arrange(`5`, `4`, `3`, `2`, `1`, X1, X2, X3, X4, X5, decreasing = TRUE)
  #arrange(`3`, `2`, `1`, X1, X2, X3, X4, X5, decreasing = TRUE)
#Set order
df.ordered$rank <- seq(1,nrow(df.ordered), 1)

answer1 <- df.ordered %>% 
  mutate(score = as.numeric(bet)*rank) %>% 
  pull(score) %>% 
  sum()
answer1
251106089


# Part 2
# Make Jokers raise the value of the max in 1:5
df <- data.frame(do.call("rbind", strsplit(as.character(hand), "",
                                           fixed = TRUE)))
df.jokers <- df[apply(df, 1, function(x) any(x == 10)), ]

# Find most common numbers
# if there are two twos, reference the higher
df.hands <- df.jokers[,c(1:5)] 
# Make temp df with jokers converted to best cards
for(i in 1:nrow(df.hands)){
target.value <- table(unlist(df.hands[i,])) %>% subset(. == max(.)) %>% subset(names(.) == max(names(.))) %>% names()
df.hands[i,which(df.hands[i,] == 10)] <- as.numeric(target.value)
}

# Get duplicates and add back id and bet info
df.2 <- lapply(apply(df.hands, 1, table),function(x){table(x) %>% as.list()}) %>% 
  rbindlist(fill = T) %>% 
  cbind(df.jokers[,c(1:5)], .) 
df.2[df.2 == 10] <- 0
df.2[is.na(df.2)] <- 0
df.2 <- df.2 %>% 
  cbind(df.jokers[,c("bet", "hand")]) 

# Merge them back together
df.2 <- full_join(df[which(!df$hand %in% df.2$hand),],df.2, by = c(colnames(df)[which(colnames(df) %in% colnames(df.2))]))

df.2[is.na(df.2)] <- 0
# Turn jokers into 0s 

df.ordered.2  <- df.2 |>
  arrange(desc(`5`), desc(`4`), desc(`3`), desc(`2`),
          desc(`1`), desc(X1),  desc(X2),
          desc(X3),  desc(X4),  desc(X5))
  #arrange(`5`, `4`, `3`, `2`, `1`, X1, X2, X3, X4, X5, decreasing = TRUE)
  
#Set order
df.ordered.2$rank <- rev(seq(1,nrow(df.ordered.2), 1))

answer2 <- df.ordered.2 %>% 
  mutate(score = as.numeric(bet)*rank) %>% 
  pull(score) %>% 
  sum()
answer2
