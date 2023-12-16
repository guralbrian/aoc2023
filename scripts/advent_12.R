data <- c('???.### 1,1,3',
'.??..??...?##. 1,1,3',
'?#?#?#?#?#?#?#? 1,3,1,6',
'????.#...#... 4,1,1',
'????.######..#####. 1,6,5',
'?###???????? 3,2,1')

data <- readLines("data/input_12.txt")
data <- data[1:100]

# Extract the pattern part
patterns <- str_extract(data, "^[^0-9]+") |>
  str_replace(pattern = " ", replacement = "")

# Extract the numbers part and convert to list
numbers <- lapply(str_extract(data, "[0-9,]+"), function(x) as.numeric(unlist(strsplit(x, ","))))

# Preprocess string operations
split_patterns <- lapply(patterns, str_split, "") %>% lapply(`[[`, 1)
locations_list <- lapply(split_patterns, function(x) which(x == "?"))
n_springs_list <- lapply(split_patterns, function(x) sum(x == "#"))

# Function to process each row
process_row <- function(spring_row) {
  #spring_row <- 1
  locations <- locations_list[[spring_row]]
  n_springs <- n_springs_list[[spring_row]]
  m <- sum(numbers[[spring_row]]) - n_springs
  
  if (m == 0) {
    return(1)
  } else {
    all_combinations <- combn(locations, m, simplify = FALSE)
    matches <- lapply(seq_along(all_combinations), function(option){
      test_pattern <- split_patterns[[spring_row]]
      test_pattern[all_combinations[[option]]] <- "#"
      # Find contiguous ranges of '#'
      contiguous_ranges <- rle(test_pattern)$lengths[rle(test_pattern)$values == "#"]
      all(contiguous_ranges == numbers[[spring_row]])}
    )
    
    count <- sum(unlist(matches))
  }
  return(count)
}


start <- Sys.time()
answer_1 <- lapply(seq_along(split_patterns), process_row)
Sys.time() - start
answer_1 %>% unlist() %>%  sum()

#7541

#### Part 2
# Multiply all of the inputs by 5, wooo

data <- c('???.### 1,1,3',
          '.??..??...?##. 1,1,3',
          '?#?#?#?#?#?#?#? 1,3,1,6',
          '????.#...#... 4,1,1',
          '????.######..#####. 1,6,5',
          '?###???????? 3,2,1')

#data <- readLines("data/input_12.txt")
#data <- data[1:100]
# Extract the pattern part
patterns <- str_extract(data, "^[^0-9]+") |>
  str_replace(pattern = " ", replacement = "")
patterns <- lapply(patterns, function(x){
  repped <- rep(x, times = 5) 
  paste(repped, collapse = "")
} 
) %>% unlist()


numbers <- lapply(str_extract(data, "[0-9,]+"), function(x){
  repped <- rep(x, times = 5) 
  paste(repped, collapse = ",")
} 
) %>% unlist()
# Extract the numbers part and convert to list
numbers <- lapply(numbers, function(x) as.numeric(unlist(strsplit(x, ","))))

#### Dynamic Programming
# Function to get combinations of a certain length

possible.arrangements <- rep(0, length(numbers)) # Initialize vector for answer

start <- Sys.time()

for(spring_row in seq_along(patterns)){
  #spring_row <- 1
  #print(paste(spring_row, "out of", length(patterns)))
  locations <- str_locate_all(patterns[[spring_row]], "[?]")[[1]][,1] # Get all ? locations at this string
  n_springs <- str_locate_all(patterns[[spring_row]], "[#]")[[1]][,1] %>% length()
  m <- sum(numbers[[spring_row]]) - n_springs # Find how many #'s we need to add
  if(m == 0){ # If the input already has as many #s as needed
    possible.arrangements[[spring_row]] <- possible.arrangements[[spring_row]] + 1
  } else{
    all_combinations <- combn(locations, m, simplify = FALSE)
    # Try each combination, test if it matches the rule
    for(option in seq_along(all_combinations)){
      #print(paste(spring_row, option, length(all_combinations)))
      #n.digits <- 1
      #option <- 1
      test.pattern <- patterns[[spring_row]] %>% str_split(., "") # Split the pattern into a list
      current.combinations <- all_combinations[option] %>% unlist() # get the test locations
      test.pattern[[1]][current.combinations] <- "#" # Replace the test locations
      test.pattern <- paste(test.pattern[[1]], collapse = "") # Join back to a string
      locations <- str_locate_all(test.pattern, "[#]+")[[1]] # Find the ranges in the test pattern
      ranges <- locations[,"end"] - locations[,"start"] + 1 
      if(all(ranges == numbers[[spring_row]])){ # check if ranges match what we want in the rule numbers
        possible.arrangements[[spring_row]] <- possible.arrangements[[spring_row]] + 1
      }
    }
  }
}

sum(possible.arrangements)


Sys.time() - start
