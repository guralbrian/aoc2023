# Day 13

data <- c("#.##..##.",
"..#.##.#.",
"##......#",
"##......#",
"..#.##.#.",
"..##..##.",
"#.#.##.#.",
"",
"#...##..#",
"#....#..#",
"..##..###",
"#####.##.",
"#####.##.",
"..##..###",
"#....#..#")

data <- readLines("data/input_13.txt")
# Split data into groups
split_indices <- which(data == "")
groups <- split(data, cumsum(data == ""))

# Remove the empty strings
groups <- lapply(groups, function(x) x[x != ""])

# Convert each group into a long string
long_strings <- lapply(groups, function(group) {
  paste(group, collapse = "")
})

mats <- lapply(seq_along(long_strings),function(x){
  split_strings <- str_split(long_strings[[x]], "")
  df <- matrix(split_strings[[1]], ncol = nchar(groups[[x]][1]), byrow = T) |>
    as.data.frame()
  colnames(df) <- seq(1,length(df),1)
  df
} )

# flip the data at each possible middle point
for(i in 1:(length(mats[[1]])-1)){
 # i <- 1
range <- 0
mir.test <- identical(mats[[1]][,i-range], mats[[1]][,i+1+range])

while(mir.test){
  range <- range + 1
  print(range)
 mir.test <- identical(mats[[1]][,i-range], mats[[1]][,i+1+range])
}
}
for(mat in mats){
middles <- c()
for(i in 1:(length(mat) - 1)) {
  range <- 0
  safe_identical <- function() {
    tryCatch(
      identical(mat[, i - range], mat[, i + 1 + range]),
      error = function(e) FALSE
    )
  }
  
  mir.test <- safe_identical()
  
  while(mir.test) {
    range <- range + 1
    print(range)
    mir.test <- safe_identical()
  }
  middles[[i]] <- range
}
middles
}


findMirrors <- function(position, v.mult = 100, h.mult = 1){
  position <- 97
  # Find repeated columns, record stretches of repeats
  dup.cols <- duplicated(t(matrices[[position]])) | duplicated(t(matrices[[position]]), fromLast = T)
  ranges <- rle(as.vector(dup.cols)) 
  if(any(dup.cols)){
  ranges.h <- data.frame(lengths = ranges$lengths, values = ranges$values, range = c(1:length(ranges$values)))
  longest.h <- ranges.h |>
    subset(values == T)|>
    arrange(desc(lengths)) |>
    head(n=1) |>
    pull(range)
  h.length <- ranges.h |>
    subset(range == longest.h) |>
    pull(lengths)
  if(h.length %% 2 == 1){
    # pull out columns in this range
    start <- ranges.h |> subset(range < longest.h) |> pull(lengths) |> sum()
    testing.range <- c((start+1):(start + h.length))
    testing.mat <- matrices[[position]][,testing.range]
    dup.cols <- duplicated(t(testing.mat)) | duplicated(t(testing.mat), fromLast = T)
    if(dup.cols[[1]]){
      table(dup.cols)
      # If the first value isn't duplicated, subtract 
      ranges.h[which(ranges.h$range == longest.h-1),"lengths"] <- ranges.h[which(ranges.h$range == longest.h-1),"lengths"] + 1
      ranges.h[which(ranges.h$range == longest.h),"lengths"] <- ranges.h[which(ranges.h$range == longest.h),"lengths"] - 1
    }
    if(dup.cols[[length(dup.cols)]]){
      # If the first value isn't duplicated, subtract 
      ranges.h[which(ranges.h$range == longest.h-1),"lengths"] <- ranges.h[which(ranges.h$range == longest.h-1),"lengths"] + 1
      ranges.h[which(ranges.h$range == longest.h),"lengths"] <- ranges.h[which(ranges.h$range == longest.h),"lengths"] - 1
    }
  }
  }else{
    h.length <- 0
  }
  
  dup.cols <- duplicated(matrices[[position]]) | duplicated(matrices[[position]], fromLast = T)
  if(any(dup.cols)){
    ranges <- rle(as.vector(dup.cols)) 
    ranges.v <- data.frame(lengths = ranges$lengths, values = ranges$values, range = c(1:length(ranges$values)))
    longest.v <- ranges.v |>
      subset(values == T)|>
      arrange(desc(lengths)) |>
      head(n=1) |>
      pull(range)
    v.length <- ranges.v |>
      subset(range == longest.v) |>
      pull(lengths)
    if(h.length %% 2 == 1){
      # pull out columns in this range
      start <- ranges.h |> subset(range < longest.h) |> pull(lengths) |> sum()
      testing.range <- c((start+1):(start + h.length))
      testing.mat <- matrices[[position]][,testing.range]
      dup.cols <- duplicated(t(testing.mat)) | duplicated(t(testing.mat), fromLast = T)
      if(dup.cols[[1]]){
        table(dup.cols)
        # If the first value isn't duplicated, subtract 
        ranges.h[which(ranges.h$range == longest.h-1),"lengths"] <- ranges.h[which(ranges.h$range == longest.h-1),"lengths"] + 1
        ranges.h[which(ranges.h$range == longest.h),"lengths"] <- ranges.h[which(ranges.h$range == longest.h),"lengths"] - 1
      }
      if(dup.cols[[length(dup.cols)]]){
        # If the first value isn't duplicated, subtract 
        ranges.h[which(ranges.h$range == longest.h-1),"lengths"] <- ranges.h[which(ranges.h$range == longest.h-1),"lengths"] + 1
        ranges.h[which(ranges.h$range == longest.h),"lengths"] <- ranges.h[which(ranges.h$range == longest.h),"lengths"] - 1
      }
    }
  }else{
    v.length <- 0
  }

  # Decide if row or column repeats are longer
  # Record score of longest
  if(h.length > v.length){
    # find where the mirror starts
    n.count <- ranges.h |>
      subset(range < longest.h) |>
      pull(lengths) |>
      sum() + h.length/2
    
    #return(n.count * h.mult)
  }else{
    n.count <- ranges.v |>
      subset(range < longest.v) |>
      pull(lengths) |>
      sum() + v.length/2

    #return(n.count * v.mult)
  }
}

# Check for which rows/columns are duplicated
answer1 <- lapply(seq_along(matrices), findMirrors) #|> unlist() |> sum()

findMirrors(100)
