---
title: "Day 10: Syntax Scoring"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

## Part One

You ask the submarine to determine the best route out of the deep-sea cave, but it only replies:

```
Syntax error in navigation subsystem on line: all of them
```

All of them?! The damage is worse than you thought. You bring up a copy of the navigation subsystem (your puzzle input).

The navigation subsystem syntax is made of several lines containing chunks. There are one or more chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start. Every chunk must open and close with one of four legal pairs of matching characters:

* If a chunk opens with (, it must close with ).
* If a chunk opens with [, it must close with ].
* If a chunk opens with {, it must close with }.
* If a chunk opens with <, it must close with >.

So, `()` is a legal chunk that contains no other chunks, as is `[]`. More complex but valid chunks include `([])`, `{()()()}`, `<([{}])>`, `[<>({}){}[([])<>]]`, and even `(((((((((())))))))))`.

Some lines are incomplete, but others are corrupted. Find and discard the corrupted lines first.

A corrupted line is one where a chunk closes with the wrong character - that is, where the characters it opens and closes with do not form one of the four legal pairs listed above.

Examples of corrupted chunks include `(]`, `{()()()>`, `(((()))}`, and `<([]){()}[{}])`. Such a chunk can appear anywhere within a line, and its presence causes the whole line to be considered corrupted.

For example, consider the following navigation subsystem:

```
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
```

Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now. The remaining five lines are corrupted:

* `{([(<{}[<>[]}>{[]{[(<()>` - Expected ], but found } instead.
* `[[<[([]))<([[{}[[()]]]` - Expected ], but found ) instead.
* `[{[{({}]{}}([{[{{{}}([]` - Expected ), but found ] instead.
* `[<(<(<(<{}))><([]([]()` - Expected >, but found ) instead.
* `<{([([[(<>()){}]>(<<{{` - Expected ], but found > instead.

Stop at the first incorrect closing character on each corrupted line.

Did you know that syntax checkers actually have contests to see who can get the high score for syntax errors in a file? It's true! To calculate the syntax error score for a line, take the first illegal character on the line and look it up in the following table:

* `)`: 3 points.
* `]`: 57 points.
* `}`: 1197 points.
* `>`: 25137 points.

### Test Case

In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal ] was found once (57 points), an illegal } was found once (1197 points), and an illegal > was found once (25137 points). So, the total syntax error score for this file is 6+57+1197+25137 = 26397 points!


```r
library(tidyverse)

# function that process the puzzle answer
getIllegalsFirstChars <- function(input){
  # for each line
  input |>
    map(function(codeline){
      
      # opening chars and respective closing chars
      openChunk <- c("{","(","[","<")
      closeChunk <- c("}",")","]",">")
      
      # heap to track open chunks
      heap <- vector("character",0)
  
      # illegal closing track
      illegal <- vector("character",0)
      
      # transform a line in a char vector
      vcode <- codeline |>
        strsplit("") |>
        unlist() 
      
      # for each char
      for(i in 1:length(vcode)){
  
        char <- vcode[i]
        
        # if we are opening a chunk we put it in the stack tracing
        if(char %in% openChunk)
          heap[length(heap)+1] <- char
      
        # if we are closing we need to check if we are closing with correct char
        if(char %in% closeChunk){
          # last open chunk char
          lastOpen <- heap[length(heap)]
          # is we should expect?
          if (which(lastOpen==openChunk) == which(char==closeChunk)) {
            # yes: legal -> remove the chunk from the stack
            heap<-heap[-length(heap)]
          } else {
            # no: illegal -> we store the illegal char
            illegal[length(illegal)+1] <- char
          }
        }
      }
      
      # for this exercise we only interesting in the first illegal char
      return(head(illegal,1))
      
    }) |>
    flatten_chr()
}

# get test
input_test <- c("[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]")

# extract illegals firsts characters
illegals <- getIllegalsFirstChars(input_test)

illegals
```

```
## [1] "}" ")" "]" ")" ">"
```

```r
# compute points function
calcPoints <- function(illegals_chars){
  # illegal char points table
  points <- tibble(
    char = c(")","]","}",">"),
    points = c(3,57,1197,25137)
  )
  
  # que compute the final score
  illegals_chars %>% 
    enframe(value="char") %>% 
    inner_join(points, by="char") %>% 
    summarise( answer = sum(points) ) %>% 
    return()
}

calcPoints(illegals)
```

```
## # A tibble: 1 × 1
##   answer
##    <dbl>
## 1  26397
```

### Puzzle Answer

Find the first illegal character in each corrupted line of the navigation subsystem. What is the total syntax error score for those errors?


```r
# input data
readLines("./input.txt") %>% 
  getIllegalsFirstChars() %>% 
  calcPoints()
```

```
## # A tibble: 1 × 1
##   answer
##    <dbl>
## 1 318081
```

## Part Two

Now, discard the corrupted lines. The remaining lines are incomplete.

Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out the sequence of closing characters that complete all open chunks in the line.

You can only use closing characters (`), ], }, or >`), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.

In the example above, there are five incomplete lines:

* `[({(<(())[]>[[{[]{<()<>>` - Complete by adding `}}]])})]`.
* `[(()[<>])]({[<{<<[]>>(` - Complete by adding `)}>]})`.
* `(((({<>}<{<{<>}{[]{[]{}` - Complete by adding `}}>}>))))`.
* `{<[[]]>}<{[{[{[]{()[[[]` - Complete by adding `]]}}]}]}>`.
* `<{([{{}}[<[[[<>{}]]]>[]]` - Complete by adding `])}>`.

Did you know that autocomplete tools also have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of 0. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:

* `)`: 1 point.
* `]`: 2 points.
* `}`: 3 points.
* `>`: 4 points.

So, the last completion string above - `])}>` - would be scored as follows:

* Start with a total score of 0.
* Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
* Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
* Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
* Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.

The five lines' completion strings have total scores as follows:

* `}}]])})]` - 288957 total points.
* `)}>]})` - 5566 total points.
* `}}>}>))))` - 1480781 total points.
* `]]}}]}]}>` - 995444 total points.
* `])}>` - 294 total points.

Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then taking the middle score. (There will always be an odd number of scores to consider.) In this example, the middle score is 288957 because there are the same number of scores smaller and larger than it.


```r
# we can handle this modifying the original findIllegalChars function to return
# if there is an illegal sequence in a give line
# and also return the chunk stack that will be the opened
# from that we run the stack backwards to autocomplete the closing chars

# function that process the puzzle answer
processNavigationCode <- function(input){
  # for each line
  input |>
    map_df(function(codeline){
      
      # opening chars and respective closing chars
      openChunk <- c("{","(","[","<")
      closeChunk <- c("}",")","]",">")
      
      # heap to track open chunks
      heap <- vector("character",0)
  
      # illegal closing track
      illegal <- vector("character",0)
      
      # transform a line in a char vector
      vcode <- codeline |>
        strsplit("") |>
        unlist() 
      
      # for each char
      for(i in 1:length(vcode)){
  
        char <- vcode[i]
        
        # if we are opening a chunk we put it in the stack tracing
        if(char %in% openChunk)
          heap[length(heap)+1] <- char
      
        # if we are closing we need to check if we are closing with correct char
        if(char %in% closeChunk){
          # last open chunk char
          lastOpen <- heap[length(heap)]
          # is we should expect?
          if (which(lastOpen==openChunk) == which(char==closeChunk)) {
            # yes: legal -> remove the chunk from the stack
            heap<-heap[-length(heap)]
          } else {
            # no: illegal -> we store the illegal char
            illegal[length(illegal)+1] <- char
          }
        }
      }
      
      # for this exercise we only interesting in the first illegal char
      tibble(
        inputline  = codeline,
        is_valid   = length(illegal)==0,
        open_stack = list(heap)
      ) %>% 
        return()
    }) %>% 
    return()
}

# process the navigation lines 
processedCodes <- input_test %>% 
  processNavigationCode()

# lets see what we have
head(processedCodes)
```

```
## # A tibble: 6 × 3
##   inputline                is_valid open_stack
##   <chr>                    <lgl>    <list>    
## 1 [({(<(())[]>[[{[]{<()<>> TRUE     <chr [8]> 
## 2 [(()[<>])]({[<{<<[]>>(   TRUE     <chr [6]> 
## 3 {([(<{}[<>[]}>{[]{[(<()> FALSE    <chr [10]>
## 4 (((({<>}<{<{<>}{[]{[]{}  TRUE     <chr [9]> 
## 5 [[<[([]))<([[{}[[()]]]   FALSE    <chr [7]> 
## 6 [{[{({}]{}}([{[{{{}}([]  FALSE    <chr [11]>
```

```r
processedCodes[1,]$open_stack[[1]]
```

```
## [1] "[" "(" "{" "(" "[" "[" "{" "{"
```

```r
# lets build autocomplete sequence
autoCompleteCodes <- function(incompleteCodes){

  # illegal char points table
  missingChars <- tibble(
    openChunk = c("(","[","{","<"),
    autoCmpChar = c(")","]","}",">"),
    addPoints = c(1,2,3,4)
  )

  incompleteCodes %>% 
    mutate( id=1:nrow(.)) %>% 
    mutate( auto_char = map(open_stack,function(.stack, .missChars){
      .stack %>% 
        enframe(name="position", value="openChunk") %>% 
        inner_join(.missChars, by="openChunk") %>% 
        arrange(desc(position)) %>% 
        return()
    }, .missChars=missingChars)) %>% 
    mutate( autoComplete = map_chr(auto_char, function(.x){
      paste0(.x$autoCmpChar, collapse = "")
    })) %>% 
    select(id, inputline, autoComplete, everything()) %>%  
    return()
}

# autocomplete valid codes
autoCompleted <- processedCodes %>% 
  filter(is_valid) %>% 
  autoCompleteCodes()

autoCompleted
```

```
## # A tibble: 5 × 6
##      id inputline                autoComplete is_valid open_stack auto_char     
##   <int> <chr>                    <chr>        <lgl>    <list>     <list>        
## 1     1 [({(<(())[]>[[{[]{<()<>> }}]])})]     TRUE     <chr [8]>  <tibble [8 × …
## 2     2 [(()[<>])]({[<{<<[]>>(   )}>]})       TRUE     <chr [6]>  <tibble [6 × …
## 3     3 (((({<>}<{<{<>}{[]{[]{}  }}>}>))))    TRUE     <chr [9]>  <tibble [9 × …
## 4     4 {<[[]]>}<{[{[{[]{()[[[]  ]]}}]}]}>    TRUE     <chr [9]>  <tibble [9 × …
## 5     5 <{([{{}}[<[[[<>{}]]]>[]] ])}>         TRUE     <chr [4]>  <tibble [4 × …
```

```r
computePoints <- function(autocompletedCodes) {
  autocompletedCodes %>% 
    mutate( points = map_dbl(auto_char, function(.x){
      acc <- 0
      for(pts in .x$addPoints) acc <- acc*5+pts
      return(acc)
    })) %>% 
    return()
}

# compute autocomplete points
allPoints <- computePoints(autoCompleted)  
allPoints
```

```
## # A tibble: 5 × 7
##      id inputline          autoComplete is_valid open_stack auto_char     points
##   <int> <chr>              <chr>        <lgl>    <list>     <list>         <dbl>
## 1     1 [({(<(())[]>[[{[]… }}]])})]     TRUE     <chr [8]>  <tibble [8 ×… 2.89e5
## 2     2 [(()[<>])]({[<{<<… )}>]})       TRUE     <chr [6]>  <tibble [6 ×… 5.57e3
## 3     3 (((({<>}<{<{<>}{[… }}>}>))))    TRUE     <chr [9]>  <tibble [9 ×… 1.48e6
## 4     4 {<[[]]>}<{[{[{[]{… ]]}}]}]}>    TRUE     <chr [9]>  <tibble [9 ×… 9.95e5
## 5     5 <{([{{}}[<[[[<>{}… ])}>         TRUE     <chr [4]>  <tibble [4 ×… 2.94e2
```

```r
# answer
median(allPoints$points)
```

```
## [1] 288957
```

### Puzzle Answer

Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?


```r
# input data
readLines("./input.txt") %>% 
  processNavigationCode() %>% 
  filter(is_valid) %>% 
  autoCompleteCodes() %>% 
  computePoints() %>% 
  pull(points) %>% 
  median()
```

```
## [1] 4361305341
```

