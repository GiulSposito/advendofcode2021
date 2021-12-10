---
title: "Day 10: Syntax Scoring"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

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

# test case
input <- c("[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]")

# for each line
illegals <- input |>
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

# illegal char points table
points <- tibble(
  char = c(")","]","}",">"),
  points = c(3,57,1197,25137)
)

# que compute the final score
illegals %>% 
  enframe(value="char") %>% 
  inner_join(points, by="char") %>% 
  summarise( answer = sum(points) )
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
input <- readLines("./input.txt")

# for each line
illegals <- input |>
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

# illegal char points table
points <- tibble(
  char = c(")","]","}",">"),
  points = c(3,57,1197,25137)
)

# que compute the final score
illegals %>% 
  enframe(value="char") %>% 
  inner_join(points, by="char") %>% 
  summarise( answer = sum(points) )
```

```
## # A tibble: 1 × 1
##   answer
##    <dbl>
## 1 318081
```

