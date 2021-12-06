---
title: "Day 6: Lanternfish"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

## Part 01

The sea floor is getting steeper. Maybe the sleigh keys got carried this way?

A massive school of glowing lanternfish swims past. They must spawn quickly to reach such large numbers - maybe exponentially quickly? You should model their growth rate to be sure.

Although you know nothing about this specific species of lanternfish, you make some guesses about their attributes. Surely, each lanternfish creates a new lanternfish once every 7 days.

However, this process isn't necessarily synchronized between every lanternfish - one lanternfish might have 2 days left until it creates another lanternfish, while another might have 4. So, you can model each fish as a single number that represents the number of days until it creates a new lanternfish.

Furthermore, you reason, a new lanternfish would surely need slightly longer before it's capable of producing more lanternfish: two more days for its first cycle.

So, suppose you have a lanternfish with an internal timer value of 3:

* After one day, its internal timer would become 2.
* After another day, its internal timer would become 1.
* After another day, its internal timer would become 0.
* After another day, its internal timer would reset to 6, and it would create a new lanternfish with an internal timer of 8.
* After another day, the first lanternfish would have an internal timer of 5, and the second lanternfish would have an internal timer of 7.

A lanternfish that creates a new fish resets its timer to 6, not 7 (because 0 is included as a valid timer value). The new lanternfish starts with an internal timer of 8 and does not start counting down until the next day.

Realizing what you're trying to do, the submarine automatically produces a list of the ages of several hundred nearby lanternfish (your puzzle input). For example, suppose you were given the following list:

```
3,4,3,1,2
```

This list means that the first fish has an internal timer of 3, the second fish has an internal timer of 4, and so on until the fifth fish, which has an internal timer of 2. Simulating these fish over several days would proceed as follows:

```
Initial state: 3,4,3,1,2
After  1 day:  2,3,2,0,1
After  2 days: 1,2,1,6,0,8
After  3 days: 0,1,0,5,6,7,8
After  4 days: 6,0,6,4,5,6,7,8,8
After  5 days: 5,6,5,3,4,5,6,7,7,8
After  6 days: 4,5,4,2,3,4,5,6,6,7
After  7 days: 3,4,3,1,2,3,4,5,5,6
After  8 days: 2,3,2,0,1,2,3,4,4,5
After  9 days: 1,2,1,6,0,1,2,3,3,4,8
After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8
```

Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each other number decreases by 1 if it was present at the start of the day.

### Test Case

In this example, after 18 days, there are a total of 26 fish. After 80 days, there would be a total of **5934**.


```r
library(tidyverse)

input_raw <- c(3,4,3,1,2)

# keep the fish state
fishes <- tibble(
  id = 1:length(input_raw),
  age = input_raw
)

# repeat for 18 days
for(i in 1:80) {
  # decrease timers
  fishes <- fishes %>% 
    mutate( age = age -1 )
  
  # is there any timer negative?
  # if so: new fishes <- count the negatives
  new_borns <- fishes %>% 
    filter(age<0) %>% 
    nrow()
  
  # set the negatives to 6
  fishes <- fishes %>% 
    mutate(age = if_else(age<0,6,age))
  
  # create new fishes with timer 8
  if (new_borns>0){
    fishes <- fishes %>% 
      bind_rows(
        tibble(
          id=(1:new_borns)+max(fishes$id),
          age=8
        ))
  }
  
}
  
# how many fishes
nrow(fishes)
```

```
## [1] 5934
```

### Puzzle Answer

Find a way to simulate lanternfish. How many lanternfish would there be after 80 days?


```r
# raw data
input_raw <- readLines("./input.txt") 

# convert it into int vector
input <- input_raw %>% 
  str_split(",") %>% 
  unlist() %>%
  as.integer()

# keep the fish state
fishes <- tibble(
  id = 1:length(input),
  age = input
)

# repeat for 18 days
for(i in 1:80) {
  # decrease timers
  fishes <- fishes %>% 
    mutate( age = age -1 )
  
  # is there any timer negative?
  # if so: new fishes <- count the negatives
  new_borns <- fishes %>% 
    filter(age<0) %>% 
    nrow()
  
  # set the negatives to 6
  fishes <- fishes %>% 
    mutate(age = if_else(age<0,6,age))
  
  # create new fishes with timer 8
  if (new_borns>0){
    fishes <- fishes %>% 
      bind_rows(
        tibble(
          id=(1:new_borns)+max(fishes$id),
          age=8
        ))
  }
  
}
  
# how many fishes
nrow(fishes)
```

```
## [1] 343441
```

## Part 02

Suppose the lanternfish live forever and have unlimited food and space. Would they take over the entire ocean?

### Test Case

After 256 days in the example above, there would be a total of 26984457539 lanternfish!


```r
# to work with big numbers
library(gmp)

# its necessary to do another aproach
# for this one we will be more wise tracking not each fish individually
# but tracking the amount of fishes in each age
simulateLanternFishPop <- function(iniAges, days){

  # generate initial populations
  iniPop <- tibble(ages = iniAges) %>% 
    count(ages, name="fishes")
  
  # population tracker
  pop <- tibble(ages=-1:8) %>% 
    left_join(iniPop, by="ages") %>% 
    replace_na(list(fishes=0)) %>% 
    mutate(fishes = map(fishes, as.bigz))
  
  # for each day
  for(i in 1:days) {
    # decrease ages (lead)
    pop <- pop %>% 
      mutate(fishes = lead(fishes))
    
    # update population counts
    # new borns: negative ages at position 8
    pop[pop$ages==8,]$fishes <- pop[pop$ages==-1,]$fishes
    
    # update negative timers: negatives + ages at position 6
    pop[pop$ages==6,]$fishes <- list(add.bigz(pop[pop$ages==-1,]$fishes[[1]], pop[pop$ages==6,]$fishes[[1]]))
    
    # reset negative age fishes
    pop[pop$ages==-1,]$fishes <- list(as.bigz(0))
    
  }
  
  # count all fishes in all ages
  pop$fishes %>% 
    reduce(add.bigz) %>% 
    return()
}

# lets test
iniAges <- c(3,4,3,1,2)
simulateLanternFishPop(iniAges, 256)
```

```
## Big Integer ('bigz') :
## [1] 26984457539
```

### Puzzle Answer

How many lanternfish would there be after 256 days?


```r
# raw data
input_raw <- readLines("./input.txt") 

# convert it into int vector
input <- input_raw %>% 
  str_split(",") %>% 
  unlist() %>%
  as.integer()

simulateLanternFishPop(input, 256)
```

```
## Big Integer ('bigz') :
## [1] 1569108373832
```

