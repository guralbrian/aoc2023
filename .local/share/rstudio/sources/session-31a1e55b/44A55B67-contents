# Advent 6

time <- as.numeric(c('55','99','97','93'))
dist <- as.numeric(c('401','1485','2274','1405'))

i <- rep(0,length(time))

for(race in 1:length(time)){
  time_hold <- 0
  while(time_hold < time[race]){
    test <- time_hold*(time[race] - time_hold)
    if(test >  dist[race]){
      i[race] <- i[race] + 1
    }
    time_hold <- time_hold + 1
  }
}
prod(i)

# Part 2
time.2 <- paste0(time, collapse = "") |> as.numeric()
dist.2 <- paste0(dist,collapse = "") |> as.numeric()

possibility <- 0
time_hold <- 0
  while(time_hold < time.2){
    test <- time_hold*(time.2 - time_hold)
    if(test >  dist.2){
      possibility <- possibility + 1
    }
    time_hold <- time_hold + 1
  }
possibility
