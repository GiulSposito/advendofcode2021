---
title: "Day 4: Giant Squid"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). 

### Test Case

For example:

```
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
```

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):

```
22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
```
After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:

```
22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
```

Finally, 24 is drawn:

```
22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
 
```

At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).


```r
library(tidyverse)

# read raw data
input_raw <- readLines("test.txt")

# extract draws sequence
draws <- strsplit(input_raw[1],",")[[1]] |> 
  as.integer()

# lets see
draws
```

```
##  [1]  7  4  9  5 11 17 23  2  0 14 21 24 10 16 13  6 15 25 12 22 18 20  8 19  3
## [26] 26  1
```

```r
# prepare board data
boards_raw <- input_raw[c(-1,-2)] %>% 
  str_c(collapse = " ") %>% 
  str_split(" +") %>% 
  .[[1]] %>% 
  as.integer()

# build boards
boards <- 1:(length(boards_raw)/25) %>% 
  map(function(.x,.dt){
    .dt[(.x*25-24):(.x*25)] %>% 
      matrix(nrow=5, ncol=5, byrow = T) %>% 
      return()
  }, .dt=boards_raw)

# lets see
boards
```

```
## [[1]]
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   22   13   17   11    0
## [2,]    8    2   23    4   24
## [3,]   21    9   14   16    7
## [4,]    6   10    3   18    5
## [5,]    1   12   20   15   19
## 
## [[2]]
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    3   15    0    2   22
## [2,]    9   18   13   17    5
## [3,]   19    8    7   25   23
## [4,]   20   11   10   24    4
## [5,]   14   21   16   12    6
## 
## [[3]]
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   14   21   17   24    4
## [2,]   10   16   15    9   19
## [3,]   18    8   23   26   20
## [4,]   22   11   13    6    5
## [5,]    2    0   12    3    7
```

```r
# I'll generate all sequences to be marked from each board (all lines and all rows)
# for each sequence I'll test against the draw sequence
# get which position in the draw sequence we get the match in the last
# so a board is marked in the sequence which least (min) last (max) draw position
winnerBoard <- boards %>% 
  # for each board
  map_df(function(.b){
    tibble(
      # generate a tibble with the board and the unmarked possible sequence
      boardNumbers = list(.b),
      boardComb    = list(rbind(.b, t(.b))) # all rows and all columns
    )
  }, .id="boardId") %>% 
  # unnest the tibblel according with each possible sequence to be marked
  mutate( seqComb = map(boardComb, ~split(.x,1:nrow(.x))) ) %>% 
  unnest( seqComb ) %>% 
  # for each sequence in all boards
  mutate( markedSeq = map(seqComb, function(.bcSeq, .draws){
    .bcSeq %>% 
        # check the draw positions necessary do mark the sequence
        map(function(.num,.dv){
          .num == .dv
        }, .dv=.draws) %>% 
        reduce(`|`) %>% 
        which() 
  }, .draws=draws)) %>% 
  mutate( 
    # count marked numbers in the sequence (should be always 5)
    winners = map_int(markedSeq, length), 
    # last draw position necessary to mark all numbers in the sequence
    winnerRound = map_int(markedSeq, max) ) %>% 
  # which board has the lower last draw number
  filter(winners==5, winnerRound==min(winnerRound)) 

# Winner Board Number
winnerBoard$boardId
```

```
## [1] "3"
```

```r
# Winner Sequence
winnerBoard$seqComb[[1]]
```

```
## [1] 14 21 17 24  4
```

The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.


```r
# the sum of all unmarked numbers on that board
boardNumbers <- c(winnerBoard$boardNumbers[[1]]) 
markedNumbers <- draws[1:winnerBoard$winnerRound]
sumUnmarked <- sum(boardNumbers[ !boardNumbers %in% markedNumbers ])
sumUnmarked
```

```
## [1] 188
```

```r
# the number that was just called when the board won
justCalled <- draws[winnerBoard$winnerRound]
justCalled
```

```
## [1] 24
```

```r
#response
justCalled * sumUnmarked
```

```
## [1] 4512
```

### Puzze Answer

To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?


```r
# read raw data
input_raw <- readLines("input.txt")

# extract draws sequence
draws <- strsplit(input_raw[1],",")[[1]] |> 
  as.integer()

# prepare board data
boards_raw <- input_raw[c(-1,-2)] %>% 
  str_c(collapse = " ") %>% 
  str_split(" +") %>% 
  .[[1]] %>% 
  as.integer()

# build boards
boards <- 1:(length(boards_raw)/25) %>% 
  map(function(.x,.dt){
    .dt[(.x*25-24):(.x*25)] %>% 
      matrix(nrow=5, ncol=5, byrow = T) %>% 
      return()
  }, .dt=boards_raw)

# find winner board
winnerBoard <- boards %>% 
  map_df(function(.b){
    tibble(
      boardNumbers = list(.b),
      boardComb    = list(rbind(.b, t(.b)))
    )
  }, .id="boardId") %>% 
  mutate( seqComb = map(boardComb, ~split(.x,1:nrow(.x))) ) %>% 
  unnest( seqComb ) %>% 
  mutate( markedSeq = map(seqComb, function(.bcSeq, .draws){
    .bcSeq %>% 
        map(function(.num,.dv){
          .num == .dv
        }, .dv=.draws) %>% 
        reduce(`|`) %>% 
        which()
  }, .draws=draws)) %>% 
  mutate( winners = map_int(markedSeq, length),
          winnerRound = map_int(markedSeq, max) ) %>% 
  filter(winners==5, winnerRound==min(winnerRound))

# the sum of all unmarked numbers on that board
boardNumbers <- c(winnerBoard$boardNumbers[[1]]) 
markedNumbers <- draws[1:winnerBoard$winnerRound]
sumUnmarked <- sum(boardNumbers[ !boardNumbers %in% markedNumbers ])
sumUnmarked
```

```
## [1] 668
```

```r
# the number that was just called when the board won
justCalled <- draws[winnerBoard$winnerRound]
justCalled
```

```
## [1] 66
```

```r
#response
justCalled * sumUnmarked
```

```
## [1] 44088
```


## Part Two

On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?


```r
# read raw data
input_raw <- readLines("input.txt")

# extract draws sequence
draws <- strsplit(input_raw[1],",")[[1]] |> 
  as.integer()

# prepare board data
boards_raw <- input_raw[c(-1,-2)] %>% 
  str_c(collapse = " ") %>% 
  str_split(" +") %>% 
  .[[1]] %>% 
  as.integer()

# build boards
boards <- 1:(length(boards_raw)/25) %>% 
  map(function(.x,.dt){
    .dt[(.x*25-24):(.x*25)] %>% 
      matrix(nrow=5, ncol=5, byrow = T) %>% 
      return()
  }, .dt=boards_raw)

# find winner board
lastWinBoard <- boards %>% 
  map_df(function(.b){
    tibble(
      boardNumbers = list(.b),
      boardComb    = list(rbind(.b, t(.b)))
    )
  }, .id="boardId") %>% 
  mutate( seqComb = map(boardComb, ~split(.x,1:nrow(.x))) ) %>% 
  unnest( seqComb ) %>% 
  mutate( markedSeq = map(seqComb, function(.bcSeq, .draws){
    .bcSeq %>% 
        map(function(.num,.dv){
          .num == .dv
        }, .dv=.draws) %>% 
        reduce(`|`) %>% 
        which()
  }, .draws=draws)) %>% 
  mutate( winners = map_int(markedSeq, length),
          winnerRound = map_int(markedSeq, max),
          winnerDrawPosition = ) %>% 
  # in this case we want to check which board has the highest (max)
  # lower (min) draw position
  group_by(boardId) %>% 
  filter(winners==5, winnerRound==min(winnerRound)) %>% 
  ungroup() %>% 
  filter(winnerRound==max(winnerRound))

# the sum of all unmarked numbers on that board
boardNumbers <- c(lastWinBoard$boardNumbers[[1]]) 
markedNumbers <- draws[1:lastWinBoard$winnerRound]
sumUnmarked <- sum(boardNumbers[ !boardNumbers %in% markedNumbers ])
sumUnmarked
```

```
## [1] 263
```

```r
# the number that was just called when the board won
justCalled <- draws[lastWinBoard$winnerRound]
justCalled
```

```
## [1] 90
```

```r
#response
justCalled * sumUnmarked
```

```
## [1] 23670
```

