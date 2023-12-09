library(stringr)
library(numbers)
data <- c('RL',
'AAA = (BBB, CCC)',
'BBB = (DDD, EEE)',
'CCC = (ZZZ, GGG)',
'DDD = (DDD, DDD)',
'EEE = (EEE, EEE)',
'GGG = (GGG, GGG)',
'ZZZ = (ZZZ, ZZZ)')

data <- c('LLR',
          'AAA = (BBB, BBB)',
          'BBB = (AAA, ZZZ)',
          'ZZZ = (ZZZ, ZZZ)')

data <- readLines("data/input_8.txt") 
data <- data[which(data != "")]
pattern <- data[[1]]
nodes <- data[-1]
# Use regex to detect word character sequences
nodes.extracted <- str_extract_all(nodes, "\\w+")
# Turn left/right into usable indexes 
pattern.number <- str_replace_all(pattern, c("R" = "3", "L" = "2")) %>% 
  str_split(.,"") 
  
pattern.number <- pattern.number[[1]] %>% as.numeric()

# Make nodes names the first value
names(nodes.extracted) <- lapply(str_extract_all(nodes, "\\w+"), "[[", 1)
steps <- 0
pattern.iteration <- 1
current_node <- nodes.extracted[["AAA"]]
next_node = 111
while(next_node != "ZZZ"){
  steps <- steps + 1
  print(steps)
  # Track which step we're on
  index <- (steps - 1) %% length(pattern.number) + 1
  print(index)
  # Access the element
  current_element <- pattern.number[index]
  print(current_element)
  # apply steps to current itteration -> find next 
  next_node <- current_node[[current_element]]
  print(next_node)
  current_node <- nodes.extracted[[next_node]]
  
}


#### Part 2
# Make a list of every node that starts with "A"
# Run the loop until it finds a node that ends with "Z"
# Record how long it took
# Repeat for all nodes that start with "A"
# Find the least common multiple of those numbers

data <- c('LR',
'11A = (11B, XXX)',
'11B = (XXX, 11Z)',
'11Z = (11B, XXX)',
'22A = (22B, XXX)',
'22B = (22C, 22C)',
'22C = (22Z, 22Z)',
'22Z = (22B, 22B)',
'XXX = (XXX, XXX)')

data <- readLines("data/input_8.txt") 
data <- data[which(data != "")]
pattern <- data[[1]]
nodes <- data[-1]
# Use regex to detect word character sequences
nodes.extracted <- str_extract_all(nodes, "\\w+")
# Turn left/right into usable indexes 
pattern.number <- str_replace_all(pattern, c("R" = "3", "L" = "2")) %>% 
  str_split(.,"") 

pattern.number <- pattern.number[[1]] %>% as.numeric()

# Make nodes names the first value
names(nodes.extracted) <- lapply(str_extract_all(nodes, "\\w+"), "[[", 1)
# Function to encapsulate the while loop logic
process_node <- function(start_node) {
  steps <- 0
  pattern_iteration <- 1
  current_node <- nodes.extracted[[start_node]]
  next_node <- "111" # Initial value
  
  while(!(next_node %in% nodes_ending_with_Z) && !is.null(current_node)) {
    steps <- steps + 1
    index <- (steps - 1) %% length(pattern.number) + 1
    current_element <- pattern.number[index]
    next_node <- current_node[[current_element]] %>% unlist %>% .[1]
    current_node <- nodes.extracted[[next_node]]
  }
  
  return(steps)
}

# Create a list of nodes ending with 'A'
nodes_ending_with_A <- names(nodes.extracted)[grepl("A$", names(nodes.extracted))]
nodes_ending_with_Z <- names(nodes.extracted)[grepl("Z$", names(nodes.extracted))]

# Apply the function to each node in the list
steps_record <- lapply(nodes_ending_with_A, process_node)

# Convert the list to a named vector
steps_record <- unlist(steps_record)
answer2 <- numbers::mLCM(steps_record)
