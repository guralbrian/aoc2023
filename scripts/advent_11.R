data <- c('...#......',
'.......#..',
'#.........',
'..........',
'......#...',
'.#........',
'.........#',
'..........',
'.......#..',
'#...#.....')

data <- readLines("data/input_11.txt")

# Store all of the locations as coordinates
data.string <- paste(data, collapse = "")

coords <- str_locate_all(data.string, "#")[[1]] |>
  as.data.frame() |>
  mutate(x = start %% nchar(data[1]),
         x = case_when(x == 0  ~ nchar(data[1]), 
                       x != nchar(data[1]) ~ x),
         y = (start - x)/nchar(data[1]) + 1)  |>
  select(x, y)

# Find x and y gaps
empty_x <- !(seq(1, max(coords$x), 1) %in% coords$x)
empty_x <- cumsum(empty_x) * 10
names(empty_x) <- seq(1,length(empty_x), 1)

coords <- coords |>
  mutate(x.adj = case_when(
    x %in% names(empty_x) ~ x + empty_x[x]
  ))

# Repeat for y
empty_y <- !(seq(1, max(coords$y), 1) %in% coords$y)
empty_y <- cumsum(empty_y) * 10
names(empty_y) <- seq(1,length(empty_y), 1)

coords <- coords |>
  mutate(y.adj = case_when(
    y %in% names(empty_y) ~ y + empty_y[y]
  ))

# Get all unique x combinations
distance.x <- abs(combn(coords$x.adj, 2)[1,] - 
                  combn(coords$x.adj, 2)[2,])
# and for y
distance.y <- abs(combn(coords$y.adj, 2)[1,] - 
                    combn(coords$y.adj, 2)[2,])

sum(distance.x) + sum(distance.y)


#### Part 2

# add 10^6 for gaps

# Find x and y gaps
empty_x <- !(seq(1, max(coords$x), 1) %in% coords$x)
empty_x[empty_x == T] <- 10^6 - 1
empty_x <- cumsum(empty_x) 
names(empty_x) <- seq(1,length(empty_x), 1)

coords <- coords |>
  mutate(x.adj = case_when(
    x %in% names(empty_x) ~ x + empty_x[x]
  ))

# Repeat for y
empty_y <- !(seq(1, max(coords$y), 1) %in% coords$y)
empty_y[empty_y == T] <- 10^6 - 1
empty_y <- cumsum(empty_y) 
names(empty_y) <- seq(1,length(empty_y), 1)

coords <- coords |>
  mutate(y.adj = case_when(
    y %in% names(empty_y) ~ y + empty_y[y]
  ))

# Get all unique x combinations
distance.x <- abs(combn(coords$x.adj, 2)[1,] - 
                    combn(coords$x.adj, 2)[2,])
# and for y
distance.y <- abs(combn(coords$y.adj, 2)[1,] - 
                    combn(coords$y.adj, 2)[2,])

ans2 <- sum(distance.x) + sum(distance.y)
as.character(ans2)

