# Advent day 1

#### Part 1

df <- readLines("input.txt")
df <- data.frame(values = df) |> 
  mutate(code = paste0(str_extract(values, "\\d"),
                       str_extract(values,"\\d(?!.*\\d)")))

ans1 <- df$code |> as.numeric() |> sum()
                       
#### Part 2

df <- readLines("input.txt") |> str_replace_all(c(
  "1" = "one",  "2" = "two",  "3" = "three",
  "4" = "four", "5" = "five", "6" = "six",
  "7" = "seven","8" = "eight","9" = "nine"))

location_df <- data.frame(code = df,
                          first.best = 10^6,
                          last.best = 0,
                          value = NA,
                          first.word = NA,
                          last.word = NA)

# Look for the first number in each with str_locate
words <- c("one","two","three","four","five","six","seven","eight","nine")

# Iterate through each word in the list
for(i in words) {
  # Find the start and end position of the first occurrence of word 'i' in each string
  location_df[,c("start", "end")] <- as.data.frame(str_locate(location_df$code, i)) 
  
  # Reverse the strings in 'code' and the word 'i' for finding the last occurrence
  reversed_code <- sapply(location_df$code, function(x) stringi::stri_reverse(x))
  reversed_word <- stringi::stri_reverse(i)
  # Find the position of the first occurrence in the reversed string
  last_occurrence <- as.data.frame(str_locate(reversed_code, reversed_word))
  
  # Adjust the positions to reflect the original string
  last_occurrence$start <- nchar(location_df$code) - last_occurrence$end + 1
  last_occurrence$end <- nchar(location_df$code) - last_occurrence$start + nchar(i)
  
  # Loop through each row of location_df
  for(j in 1:nrow(location_df)) {
    # Update the earliest occurrence if the current one is earlier
    if(!is.na(location_df$start[j]) && location_df$start[j] < location_df$first.best[j]) {
      location_df$first.best[j] <- location_df$start[j]
      location_df$first.word[j] <- i
    }
    # Update the latest occurrence if the current one is later
    if(!is.na(last_occurrence$start[j]) && last_occurrence$start[j] > location_df$last.best[j]) {
      location_df$last.best[j] <- last_occurrence$start[j]
      location_df$last.word[j] <- i
    }
  }
}


# Join the words and make numeric
location_df <- location_df |> 
  mutate(joined_words = paste0(first.word, last.word),
         value =  as.numeric(str_replace_all(joined_words, c(
           "one"   = "1",
           "two"   = "2",
           "three" = "3",
           "four"  = "4",
           "five"  = "5",
           "six"   = "6",
           "seven" = "7",
           "eight" = "8",
           "nine"  = "9"))))

# Sum up values
ans2 <- location_df$value |> as.numeric() |> sum()
ans2

