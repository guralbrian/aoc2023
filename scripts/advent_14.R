data <- c('O....#....',
'O.OO#....#',
'.....##...',
'OO.#O....O',
'.O.....O#.',
'O.#..O.#.#',
'..O..#O..O',
'.......O..',
'#....###..',
'#OO..#....')

data <- readLines("data/input_14.txt")

# Format as data.frame
length <- nchar(data[[1]])
long_string <- paste(data, collapse = "")
split_string <- str_split(long_string, "")
mat <- matrix(split_string[[1]], ncol = length, byrow = T) |>
    as.data.frame()
colnames(mat) <- seq(1,length(mat),1)


#### Part 1
table <- mat
table$row <- c(1:nrow(table))
table.long <- table |>
  pivot_longer(-row) |>
  mutate(
    anchors = case_when(
      value == "#" ~ 1,
      value != "#" ~ 0
    ))
table.reordered <- table.long |> 
    group_by(name) |>
    mutate(
      segments = cumsum(anchors),
      value.num = case_when(
        value == "#" ~ 2,
        value == "O" ~ 1,
        value == "." ~ 0,
      )) |> 
  group_by(name, segments) |>
  arrange(desc(value.num), .by_group = T) |>
  ungroup() |>
  select(name, value, row) 
table.reordered$row <- rep(c(1:nrow(table)), times = ncol(table)-1)
# Assuming table.reordered is your data frame
table.reordered <- table.reordered %>%
  mutate(name = factor(name, levels = unique(name)))

# Pivot the data wider
# Pivot the data wider
wider_df <- table.reordered %>%
  group_by(row) %>%
  pivot_wider(
    names_from = name,
    values_from = value,
    values_fill = list(value = NA)  # Fill missing values with NA
  ) %>%
  ungroup() 
wider_df <- wider_df[,as.character(c(1:(ncol(wider_df)-1), "row"))]

row.scores <- rowSums(sapply(wider_df[,-ncol(wider_df)], grepl, pattern = "O"))

names(row.scores) <- c(1:length(row.scores)) |> rev()

(as.numeric(names(row.scores)) * row.scores) |> sum()


wider_df <- wider_df |>
  select(-row)
#### Part 2
  # Rotate the df, dont change the code
  # .O#.   ..O.
  # O..O   ...O
  # ..O# ->.O.#  
  # ....   .#O.
# Keep running cycles until you find one that existed before
table <- mat
permutations <- c()

for(j in 1:10^5){
  print(j)
  i <- 0
  while(i < 4){ # Tilt 4 times
  table$row <- c(1:nrow(table))
  table.long <- table |>
  pivot_longer(-row) |>
  mutate(
    anchors = case_when(
      value == "#" ~ 1,
      value != "#" ~ 0
    ))
  table.reordered <- table.long |> 
    group_by(name) |>
    mutate(
      segments = cumsum(anchors),
      value.num = case_when(
        value == "#" ~ 2,
        value == "O" ~ 1,
        value == "." ~ 0,
      )) |> 
   group_by(name, segments) |>
   arrange(desc(value.num), .by_group = T) |>
    ungroup() |>
    select(name, value, row) 
  table.reordered$row <- rep(c(1:nrow(table)), times = ncol(table)-1)
  # Assuming table.reordered is your data frame
  table.reordered <- table.reordered %>%
   mutate(name = factor(name, levels = unique(name)))
  # Pivot the data wider
  wider_df <- table.reordered %>%
    group_by(row) %>%
    pivot_wider(
     names_from = name,
     values_from = value,
      values_fill = list(value = NA)  # Fill missing values with NA
   ) %>%
    ungroup() |>
    select(-row)
  table <- wider_df[,as.character(c(1:ncol(wider_df)))] |> as.data.frame()
  table <- table |> t() |> as.data.frame() |> rev()
  colnames(table) <- c(1:length(table))
  table <- table
  i <- i + 1
  } 
  # After each cycle, test to see if the result has been found before
  if(lapply(seq_along(permutations), function(x){
    identical(permutations[[x]],table)
  }) |> unlist() |> any()){
  permutations[[j]] <- table
  break # Stop if it has
  }
  # Keep running if it hasn't
  permutations[[j]] <- table
}

# Find where the layout has occurred before
df.pattern <- lapply(seq_along(permutations), function(x){
  identical(permutations[[x]],table)
}) |> unlist()

# Find how long it takes for the puzzle to repeat
cycle.length <- which(df.pattern)[[2]] - which(df.pattern)[[1]]
target.val <- 1000000000
# find iterations to reach cycle
cycle.start <- which(df.pattern)[[1]]
cycle.index <- (target.val-cycle.start) %% cycle.length

target.layout <- permutations[[cycle.index + cycle.start]]

# Get the weight again
row.scores <- rowSums(sapply(target.layout, grepl, pattern = "O"))

names(row.scores) <- c(1:length(row.scores)) |> rev()

(as.numeric(names(row.scores)) * row.scores) |> sum()

