---
title: "13: Transparent Origami "
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

## Part 01

You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

>> Congratulations on your purchase! To activate this infrared thermal imaging camera system, please enter the code found on page 1 of the manual.

Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

```
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
```

The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0.


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
## ✓ readr   2.1.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
readPuzzleInput <- function(filename){
  input <- readLines(filename)
  dotmap_raw <- input[1:(which(input=="")-1)]
  folds_raw <- input[(which(input=="")+1):length(input)]
  return(list(dotmap_raw=dotmap_raw, folds_raw=folds_raw))
}

createTranspPaper <- function(.dotmap_raw){
  dotmap <- .dotmap_raw %>% 
    enframe() %>% 
    separate(value, into=c("x","y"), convert = T) %>% 
    select(-name) %>% 
    mutate(x=x+1, y=y+1)
  
  m <- matrix(F, nrow = max(dotmap$y), ncol = max(dotmap$x))

  for(i in 1:nrow(dotmap))
    m[dotmap$y[i], dotmap$x[i]] <- T
  
  return(m)
  
}

translateInstructions <- function(.folds_raw){
  .folds_raw %>% 
    str_remove_all("fold along ") %>% 
    enframe(name = "step",value="instruction") %>% 
    separate(instruction, into = c("direction","position"), convert = T) %>% 
    mutate(position = position + 1) %>% # R is base 1
    return()
}
```

The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

```
...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
```


```r
input <- readPuzzleInput("./test_case.txt")

m <- createTranspPaper(input$dotmap_raw)
folds <- translateInstructions(input$folds_raw)
m
```

```
##        [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11]
##  [1,] FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE
##  [2,] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
##  [3,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [4,]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [5,] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE
##  [6,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [7,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [8,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [9,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [10,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [11,] FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE
## [12,] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
## [13,] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
## [14,]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [15,]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```


Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):


```
...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
```

Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

```
#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
```


```r
# manual step 1
fold <- folds[folds$step==1,]
fold
```

```
## # A tibble: 1 × 3
##    step direction position
##   <int> <chr>        <dbl>
## 1     1 y                8
```

```r
s1 <- m[1:fold$position-1,]
s2 <- m[dim(m)[1]:(fold$position+1),]

f1 <- s1|s2
f1
```

```
##       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11]
## [1,]  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE
## [2,]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
## [3,] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
## [4,]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
## [5,] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
## [6,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [7,] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```

Now, only 17 dots are visible.


```r
sum(f1)
```

```
## [1] 17
```


Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

```
#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....
```

Because this is a vertical line, fold left:

```
#####
#...#
#...#
#...#
#####
.....
.....
```


```r
fold <- folds[folds$step==2,]
fold
```

```
## # A tibble: 1 × 3
##    step direction position
##   <int> <chr>        <dbl>
## 1     2 x                6
```

```r
s1 <- f1[,1:fold$position-1]
s2 <- f1[,dim(f1)[2]:(fold$position+1)]

f2 <- s1|s2
f2
```

```
##       [,1]  [,2]  [,3]  [,4]  [,5]
## [1,]  TRUE  TRUE  TRUE  TRUE  TRUE
## [2,]  TRUE FALSE FALSE FALSE  TRUE
## [3,]  TRUE FALSE FALSE FALSE  TRUE
## [4,]  TRUE FALSE FALSE FALSE  TRUE
## [5,]  TRUE  TRUE  TRUE  TRUE  TRUE
## [6,] FALSE FALSE FALSE FALSE FALSE
## [7,] FALSE FALSE FALSE FALSE FALSE
```


The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.


```r
sum(f1)
```

```
## [1] 17
```

How many dots are visible *after completing just the first fold instruction* on your transparent paper?


```r
input <- readPuzzleInput("./input.txt")

m <- createTranspPaper(input$dotmap_raw)
folds <- translateInstructions(input$folds_raw)

foldIt <- function(paper, fold){
  # direction
  if(fold$direction=="y"){
    # fold in direction Y (lines)
    fold1 <- paper[1:(fold$position-1),]
    fold2 <- paper[dim(paper)[1]:(fold$position+1),]

  } else {
    # fold in direction X (columns)
    fold1 <- paper[,1:(fold$position-1)]
    fold2 <- paper[,dim(paper)[2]:(fold$position+1)]
  }
  
  offsets <- (dim(fold1)-dim(fold2))+1
  resp <- fold1
  resp[offsets[1]:dim(resp)[1],offsets[2]:dim(resp)[2]] <- 
    fold1[offsets[1]:dim(resp)[1],offsets[2]:dim(resp)[2]] |
    fold2
    
  return(resp)
}

input <- readPuzzleInput("./input.txt")
m <- createTranspPaper(input$dotmap_raw)
folds <- translateInstructions(input$folds_raw)

f1 <- foldIt(m, folds[folds$step==1,])
sum(f1) 
```

```
## [1] 737
```

### Part Two

Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.


```r
# run all fold instructions
for(i in 1:nrow(folds)) 
  m <- foldIt(m, folds[folds$step==i,])

m 
```

```
##       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11] [,12]
## [1,]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
## [2,] FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
## [3,] FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
## [4,] FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
## [5,]  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
## [6,]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE
##      [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
## [1,]  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE
## [2,] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE
## [3,] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE
## [4,] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
## [5,] FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE
## [6,]  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
##      [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36]
## [1,] FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
## [2,] FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
## [3,] FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
## [4,] FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
## [5,] FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
## [6,] FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
##      [,37] [,38] [,39] [,40]
## [1,]  TRUE  TRUE FALSE FALSE
## [2,] FALSE FALSE  TRUE FALSE
## [3,] FALSE FALSE  TRUE FALSE
## [4,]  TRUE  TRUE FALSE FALSE
## [5,] FALSE FALSE FALSE FALSE
## [6,] FALSE FALSE FALSE FALSE
```

What code do you use to activate the infrared thermal imaging camera system?


```r
tibble(
  y = rep(nrow(m):1,ncol(m)),
  x = rep(1:ncol(m), each=nrow(m)),
  val = as.vector(m)
) %>% 
  ggplot(aes(x=x, y=y, fill=val)) +
  geom_tile() +
  scale_fill_manual(values=c("grey","black"))+
  theme_void() +
  theme(aspect.ratio = 1/8, legend.position = "none")
```

![](AdventOfCode_Day13_files/figure-html/unnamed-chunk-9-1.png)<!-- -->




