
# Practice data
data <- c("0 3 6 9 12 15",
"1 3 6 10 15 21",
"10 13 16 21 30 45")

data <- readLines("data/input_9.txt")
# Parse it to numbers
data.num <- lapply(strsplit(data, " "), as.numeric)

# Initialize summed number list
summed.last <- rep(0, length(data.num))
# Find delta of numbers # 
# Get the last number for each list and add 
# it to its corresponding running sum
last.numbers <-  lapply(seq(data.num), function(x){
  data.num[[x]][length(data.num[[x]])]})

while(any(unlist(data.num) != 0)) {
# extract last number and add it to group         
data.num <- lapply(data.num, function(x){diff(x)})
  #c(diff(x), diff(x)[length(diff(x))] + x[length(x)])})
summed.last <- lapply(seq(data.num), function(x){
  data.num[[x]][length(data.num[[x]])] + summed.last[[x]]})
}

sum(unlist(summed.last)) + sum(unlist(last.numbers))
  
## Part 2

# Parse it to numbers
data.num <- lapply(strsplit(data, " "), as.numeric)

# just reverse the numbers!
data.rev <- lapply(data.num, rev)
# Initialize summed number list
summed.last <- rep(0, length(data.rev))

# Find delta of numbers # 
last.numbers <-  lapply(seq(data.rev), function(x){
  data.rev[[x]][length(data.rev[[x]])]})

while(any(unlist(data.rev) != 0)) {
  # extract last number and add it to group         
  data.rev <- lapply(data.rev, function(x){diff(x)})
  #c(diff(x), diff(x)[length(diff(x))] + x[length(x)])})
  summed.last <- lapply(seq(data.rev), function(x){
    data.rev[[x]][length(data.rev[[x]])] + summed.last[[x]]})
}

sum(unlist(summed.last)) + sum(unlist(last.numbers))



