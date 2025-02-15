---
title: "Day 5: Hydrothermal Venture"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

## Part 01

You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

```
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
```

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

* An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
* An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

```
.......1..
..1....1..
..1....1..
.112111211
..........
.......1..
..........
..........
..........
222111....
```

In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

### Test Case


```r
library(tidyverse)

# read the files separing start positions from end positions
input <- read_delim(".//test.txt", delim = " -> ",
                    col_names = c("start","end"), col_types = "cc") %>% 
  # from start and end separate each x,y coordinates
  separate(start, into = c("x1","y1"), convert = T) %>% 
  separate(end, into = c("x2","y2"), convert = T)

# lets see
input
```

```
## # A tibble: 10 × 4
##       x1    y1    x2    y2
##    <int> <int> <int> <int>
##  1     0     9     5     9
##  2     8     0     0     8
##  3     9     4     3     4
##  4     2     2     2     1
##  5     7     0     7     4
##  6     6     4     2     0
##  7     0     9     2     9
##  8     3     4     1     4
##  9     0     0     8     8
## 10     5     5     8     2
```

```r
# process lines
vent_lines <- input %>% 
  # only fixed x or y coords
  filter(x1==x2 | y1==y2 ) %>%
  # calc the displacement 
  mutate( delta.x = map2(x1,x2, seq),
          delta.y = map2(y1,y2, seq)) %>%
  # group displacement in form (x,y)
  mutate( positions = map2(delta.x, delta.y, function(.x,.y){
    tibble(x=.x, y=.y) %>% 
      mutate( positions =  str_c(x,y, sep = ",") ) %>% 
      .$positions
  }))

# lets see
vent_lines %>% 
  glimpse()
```

```
## Rows: 6
## Columns: 7
## $ x1        <int> 0, 9, 2, 7, 0, 3
## $ y1        <int> 9, 4, 2, 0, 9, 4
## $ x2        <int> 5, 3, 2, 7, 2, 1
## $ y2        <int> 9, 4, 1, 4, 9, 4
## $ delta.x   <list> <0, 1, 2, 3, 4, 5>, <9, 8, 7, 6, 5, 4, 3>, 2, 7, <0, 1, 2>, …
## $ delta.y   <list> 9, 4, <2, 1>, <0, 1, 2, 3, 4>, 9, 4
## $ positions <list> <"0,9", "1,9", "2,9", "3,9", "4,9", "5,9">, <"9,4", "8,4", "…
```

```r
# just count common coordinates
vent_lines %>% 
  select(positions) %>% 
  unnest(positions) %>% 
  count(positions, sort=T) %>% 
  # how much has more than one vent line in commmon
  filter(n>=2) %>% 
  nrow()
```

```
## [1] 5
```

### Puzzle Answer

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?


```r
# read the files separating start positions from end positions
read_delim(".//input.txt", delim = " -> ", 
           col_names = c("start","end"), col_types = "cc") %>% 
  # from start and end separate each x,y coordinates
  separate(start, into = c("x1","y1"), convert = T) %>% 
  separate(end, into = c("x2","y2"), convert = T) %>%  
  # only fixed x or y coords
  filter(x1==x2 | y1==y2 ) %>%
  # calc the displacement 
  mutate( delta.x = map2(x1,x2, seq),
          delta.y = map2(y1,y2, seq)) %>%
  # group displacement in form (x,y)
  mutate( positions = map2(delta.x, delta.y, function(.x,.y){
    tibble(x=.x, y=.y) %>% 
      mutate( positions =  str_c(x,y, sep = ",") ) %>% 
      .$positions
  })) %>% 
  # just count common coordinates
  select(positions) %>% 
  unnest(positions) %>% 
  count(positions, sort=T) %>% 
  # how much has more than one vent line in commmon
  filter(n>=2) %>% 
  nrow()
```

```
## [1] 7269
```


## Part Two

Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

Considering all lines from the above example would now produce the following diagram:

```
1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....
```

You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

### Test Case


```r
# read the files separing start positions from end positions
vent_map <- read_delim(".//test.txt", delim = " -> ", 
           col_names = c("start","end"), col_types = "cc") %>% 
  # from start and end separate each x,y coordinates
  separate(start, into = c("x1","y1"), convert = T) %>% 
  separate(end, into = c("x2","y2"), convert = T) %>%  
  filter(
      # horizontal lines and rows
      (x1==x2 | y1==y2) |
      # -45o degree lines
      ((x2-x1) == (y2-y1)) |
      # + 45o degree lines
      ((x2-x1) == -(y2-y1))) %>%
  # calc the displacement 
  mutate( delta.x = map2(x1,x2, seq),
          delta.y = map2(y1,y2, seq)) %>%
  # group displacement in form (x,y)
  mutate( positions = map2(delta.x, delta.y, function(.x,.y){
    tibble(x=.x, y=.y) }))

# lets visualize
vent_map %>% 
  select(positions) %>% 
  unnest(positions) %>% 
  add_count(x,y) %>% 
  ggplot(aes(x,-y)) +
  geom_tile(aes(fill=n)) +
  geom_label(aes(label=n)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_light() +
  theme(legend.position = "none")
```

![](AdventOfCode_Day05_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



```r
# counting
vent_map %>% 
  mutate(positions = map(positions, function(.p){
    .p %>% 
      mutate( positions =  str_c(x,y, sep = ",") ) %>% 
      .$positions
  })) %>% 
  # just count common coordinates
  select(positions) %>% 
  unnest(positions) %>% 
  count(positions, sort=T) %>% 
  # how much has more than one vent line in commmon
  filter(n>=2) %>% 
  nrow()
```

```
## [1] 12
```

### Puzzle Answer

Consider all of the lines. At how many points do at least two lines overlap?


```r
# read the files separating start positions from end positions
read_delim(".//input.txt", delim = " -> ", 
           col_names = c("start","end"), col_types = "cc") %>% 
  # from start and end separate each x,y coordinates
  separate(start, into = c("x1","y1"), convert = T) %>% 
  separate(end, into = c("x2","y2"), convert = T) %>%  
  filter(
      # horizontal lines and rows
      (x1==x2 | y1==y2) |
      # -45o degree lines
      ((x2-x1) == (y2-y1)) |
      # + 45o degree lines
      ((x2-x1) == -(y2-y1))) %>%
  # calc the displacement 
  mutate( delta.x = map2(x1,x2, seq),
          delta.y = map2(y1,y2, seq)) %>%
  # group displacement in form (x,y)
  mutate( positions = map2(delta.x, delta.y, function(.x,.y){
    tibble(x=.x, y=.y) %>% 
      mutate( positions =  str_c(x,y, sep = ",") ) %>% 
      .$positions
  })) %>% 
  # just count common coordinates
  select(positions) %>% 
  unnest(positions) %>% 
  count(positions, sort=T) %>% 
  # how much has more than one vent line in commmon
  filter(n>=2) %>% 
  nrow()
```

```
## [1] 21140
```

## References

* [https://limnu.com/sketch-easy-90-degree-rotate-vectors/](https://limnu.com/sketch-easy-90-degree-rotate-vectors/)
