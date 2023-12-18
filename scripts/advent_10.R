data <- c('.....',
'.S-7.',
'.|.|.',
'.L-J.',
'.....')

data <- c('..F7.',
'.FJ|.',
'SJ.L7',
'|F--J',
'LJ...')
data <- c('.F----7F7F7F7F-7....',
          '.|F--7||||||||FJ....',
          '.||.FJ||||||||L7....',
          'FJL7L7LJLJ||LJ.L-7..',
          'L--J.L7...LJS7F-7L7.',
          '....F-J..F7FJ|L7L7L7',
          '....L7.F7||L7|.L7L7|',
          '.....|FJLJ|FJ|F7|.LJ',
          '....FJL-7.||.||||...',
          '....L---J.LJ.LJLJ...')

data <- readLines("data/input_10.txt")
length <- nchar(data[[1]])

data.long <- paste(data, collapse = "")

start <- str_locate(data.long, "S")[1,1][[1]]

data.split <- str_split(data.long, "")[[1]]

# Hardcode the rules
north.allowed <- c("|", "F", "7", "S")
south.allowed <- c("|", "J", "L", "S")
west.allowed  <- c("-", "F", "L", "S")
east.allowed  <- c("-", "7", "J", "S")

# Find adjacent locations
cur.pos <- start - length
last.pos <- start
steps <- 1
loop.pos <- c(last.pos)
while(data.split[[cur.pos]] != "S"){
# Get possible moves
dir.vect <- c(
 north = cur.pos - length,
 south = cur.pos + length,
 west  = cur.pos - 1,
 east  = cur.pos + 1
)

north.pos <- south.pos <- west.pos <- east.pos <- F

# Hardcode options for current.possition based on it's character
if(data.split[[cur.pos]] == "|"){
  path.poss <- c(T,T,F,F)
}
if(data.split[[cur.pos]] == "-"){
  path.poss <- c(F,F,T,T)
}
if(data.split[[cur.pos]] == "J"){
  path.poss <- c(T,F,T,F)
}
if(data.split[[cur.pos]] == "L"){
  path.poss <- c(T,F,F,T)
}
if(data.split[[cur.pos]] == "7"){
  path.poss <- c(F,T,T,F)
}
if(data.split[[cur.pos]] == "F"){
  path.poss <- c(F,T,F,T)
}
# Limit to viable moves

try(north.pos <- data.split[[dir.vect[["north"]]]] %in% north.allowed)
try(south.pos <- data.split[[dir.vect[["south"]]]] %in% south.allowed)
try(west.pos  <- data.split[[dir.vect[["west"]]]]  %in% west.allowed)
try(east.pos  <- data.split[[dir.vect[["east"]]]]  %in% east.allowed)

poss.vect <- c(north.pos, south.pos, west.pos, east.pos)

# Just get it to run the simple loop
next.pos <- dir.vect[poss.vect & path.poss]
next.pos <- next.pos[!(next.pos %in% last.pos)]

if(length(next.pos) == 1){
  loop.pos <- c(loop.pos, cur.pos)
  steps <- steps + 1
  last.pos <- cur.pos
  cur.pos <- next.pos[[1]]
}
}

distance <- steps/2
distance

### Part 2

# Convert linear positions to (x, y) coordinates
coords <- lapply(loop.pos, function(pos) c(x = ((pos - 1) %% length) + 1, y = (pos - 1) %/% length + 1))
coords <- do.call(rbind, coords)

# Apply the Shoelace formula
n <- nrow(coords)
area <- 0.5 * abs(sum(coords[1:(n-1), 1] * coords[2:n, 2] - coords[2:n, 1] * coords[1:(n-1), 2]) + 
                    (coords[n, 1] * coords[1, 2] - coords[1, 1] * coords[n, 2]))

# Calculate the number of boundary points (B)
B <- n

# Apply Pick's theorem to find the number of interior points (I)
# Area = I + B/2 - 1
I <- area - B / 2 + 1

I