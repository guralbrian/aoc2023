library(stringr)

text <- readLines("input_3.txt")

#### Part 1

# find length of each string
length <- nchar(text[[1]]) 

# turn into list
text <- text |> paste(collapse = "")

# Locate all special characters
symbols <- str_locate_all(text, "[^A-Za-z0-9.]")[[1]][,1] |> c()

# Add regions to indicate adjacency 
symbols <- c(symbols, symbols - 1,symbols + 1)
symbols <- c(symbols, symbols - length, symbols + length)

# Get locations of all numbers
numbers <- str_locate_all(text, "\\d+") |>  as.data.frame() |> 
  mutate(number = str_extract_all(text, "\\d+")[[1]]) 

# add ID for next step
numbers$id <- seq(1,nrow(numbers),1) 

# Filter to overlapping locations and sum
numbers |> 
  pivot_longer(!c(id, number)) |> 
  dplyr::filter(value %in% symbols) |> 
  group_by(id) |> 
  slice_head(n = 1) |> 
  pull(number) |> 
  as.numeric() |> 
  sum()

#### Part 2

# Locate all gears
gears <- str_locate_all(text, "[*]")[[1]][,1] |> c()
names(gears) <- gears

# Add regions to indicate adjacency 
gears <- c(gears, gears - 1,gears + 1)
gears <- c(gears, gears - length, gears + length)

# Get locations of all numbers
numbers <- str_locate_all(text, "\\d+") |>  as.data.frame() |> 
  mutate(number = str_extract_all(text, "\\d+")[[1]])  

# add ID for next step
numbers$id <- seq(1,nrow(numbers),1) 

# Subset to numbers adjacent to a gear
numbers <- numbers |> 
  pivot_longer(!c(id, number)) |> 
  dplyr::filter(value %in% gears) 

# Add names of gears to corresponding locations in number df
# Pray that no more that two numbers contact any given gear
numbers$gear <- NA
for(position in 1:nrow(numbers)){
  gear_names <- names(gears)[which(gears == numbers$value[[position]])]
  if(length(gear_names) == 1){
    numbers$gear[[position]] <- gear_names
  } else(print("Uh oh"))
}

# Subset to gears that have only two numbers contacting, find product, sum all
numbers |> 
  group_by(gear, id) |> 
  slice_head(n = 1) |> 
  group_by(gear) |> 
  filter(n() > 1) |> 
  summarize(gear_value = prod(as.numeric(number))) |> 
  pull(gear_value) |> 
  sum()
         