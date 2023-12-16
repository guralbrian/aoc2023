# Load data
data <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
data <- readLines("data/input_15.txt")
# Split each comma seperated value 
data.lines <- str_split(data, ",") |> unlist()
# Split each sublist 
data.lines <- lapply(data.lines, function(x){
  str_split(x, "") |> unlist() })

#Initialize score list
all.scores <- c()
for(item in seq_along(data.lines)) {
  cur.data <- data.lines[[item]]
  # Convert it all to ASCII
  cur.score <- 0
  for(i in seq_along(cur.data)){
    print(i)
    # For each value, run the computation
    cur.score <- 17*(cur.score + utf8ToInt(cur.data[[i]])) %% 256
    cur.score <- cur.score  %% 256
  }
  # Record the score for each item
  all.scores[[item]] <- cur.score
}
# Sum the scores to get part 1 answer
all.scores |> unlist() |> sum()
