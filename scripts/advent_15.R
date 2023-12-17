# Load data
data <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
data <- readLines("data/input_15.txt")
# Split each comma seperated value 
data <- str_split(data, ",") |> unlist()
# Split each sublist 
data.lines <- lapply(data, function(x){
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


#### Part 2
# The letters are the label for the lens
# The ASCII value of the label is the target box
# The sign tells us to remove or add a lens
# The number tells us which focal length
# Get scores for just labels, old scores don't matter
all.scores <- c()
letters <- str_extract_all(data, "[aA-zZ]+") 
letters <- str_split(letters, "")

for(item in seq_along(letters)) {
  cur.data <- letters[[item]]
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

# Parse it all into a dataframe
lens.df <- data.frame(raw = data,
                      score = unlist(all.scores))
# Get scores, letters, focal lengths, and signs
lens.df <- lens.df |> 
  mutate(
    focal = str_extract_all(raw, "[0-9]+"),
    letters = str_extract_all(raw, "[aA-zZ]+"),
    sign = case_when(
      focal %in% c(1:9) ~ "=",
      .default = "-"
    )
  )

boxes <- rep(NA, 256)
names(boxes) <- seq(0,255,1)
# Start working through rules:
#   1. Figure out which box with 'score'
#   2. Figure out if a lens with the name 'letters' exists there
#   3. Remove it if sign == '-'
#   4. Replace/add if sign == "=", to end of lenses
for(rule in 1:nrow(lens.df)) {
  box <- as.character(lens.df[rule, "score"])
  letter <- lens.df[rule, "letters"][[1]]
  sign <- lens.df[rule, "sign"]
  focal <- as.numeric(lens.df[rule, "focal"][[1]])

if(all(is.na(boxes[[box]]))) {
    boxes[[box]] <- list("dummy")
  }
  if(sign == "-") {
    # Remove the labeled lens if it exists
    if (any(!is.na(boxes[[box]]))) {
      boxes[[box]] <- boxes[[box]][names(boxes[[box]]) != letter]
    }
  } else {
  # Replace or add the lens
  boxes[[box]][[letter]] <- list(focal)
  }
}

clean.boxes <- boxes[!is.na(boxes)]

total_focusing_power <- 0

for(box_number in names(clean.boxes)) {
  # Filter out the "dummy" entry
  lenses <- clean.boxes[[box_number]][names(clean.boxes[[box_number]]) != ""]
  
  for(lens_number in seq_along(lenses)) {
    lens_name <- names(lenses)[lens_number]
    focal_length <- unlist(lenses[lens_name])
    
    # Calculate focusing power of this lens
    lens_focusing_power <- (as.numeric(box_number) + 1) * lens_number * as.numeric(focal_length)
    total_focusing_power <- total_focusing_power + as.numeric(lens_focusing_power)
  }
}

print(total_focusing_power)

