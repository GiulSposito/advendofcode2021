---
title: "Day 8: Seven Segment Search"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

## Part 01

You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.

As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

Each digit of a seven-segment display is rendered by turning on or off any of seven segments named **a** through **g**:

```
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
 
```

So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.

The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments **randomly**. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)

So, you might know that only signal wires b and g are turned on, but that doesn't mean **segments** b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.

For each display, you watch the changing signals for a while, make a note of **all ten unique signal patterns** you see, and then write down a single four **digit output value** (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.

For example, here is what you might see in a single entry in your notes:

```
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```

(The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)

Each entry consists of ten **unique signal patterns**, a | delimiter, and finally the **four digit output value**. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.

Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.

For now, **focus on the easy digits**. Consider this larger example:

```
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
```

Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting **only digits in the output values** (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).

### Test Case


```r
# searching for codes 1,4,7 and 8
# with segment number of 1:2, 4:4, 7:3, 8:7

library(tidyverse)

# read as a tibble of (pattern,output)
input_raw <- read_delim("./test.txt", delim = " | ", col_names = c("pattern","output"))
head(input_raw)
```

```
## # A tibble: 6 × 2
##   pattern                                                    output             
##   <chr>                                                      <chr>              
## 1 be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb fdgacbe cefdb cefb…
## 2 edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec fcgedb cgb dgebacf…
## 3 fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef cg cg fdcagb cbg   
## 4 fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega efabcd cedba gadfe…
## 5 aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga gecf egdcabf bgf b…
## 6 fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf gebdcfa ecba ca fa…
```

```r
# separate the output signals and count the number of segments
n_segs <- input_raw %>% 
  separate_rows(output, sep=" ") %>% 
  mutate(segs_on=map_int(output, nchar))
head(n_segs)
```

```
## # A tibble: 6 × 3
##   pattern                                                    output  segs_on
##   <chr>                                                      <chr>     <int>
## 1 be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb fdgacbe       7
## 2 be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb cefdb         5
## 3 be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb cefbgd        6
## 4 be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb gcbe          4
## 5 edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec fcgedb        6
## 6 edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec cgb           3
```

```r
# count segments of lenght 2, 4, 3 and 7 
n_segs %>% 
  filter(segs_on %in% c(2,4,3,7)) %>% 
  nrow()
```

```
## [1] 26
```

### Puzzle Answer 

In **the output values, how many times do digits 1, 4, 7, or 8 appear?**


```r
# read as a tibble of (pattern,output)
read_delim("./input.txt", delim = " | ", col_names = c("pattern","output")) %>% 
  # separate the output signals and count the number of segments
  separate_rows(output, sep=" ") %>% 
  mutate(segs_on=map_int(output, nchar)) %>% 
  # count segments of lenght 2, 4, 3 and 7 
  filter(segs_on %in% c(2,4,3,7)) %>% 
  nrow()
```

```
## [1] 387
```

## Part 2

Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

```
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```
After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

```
 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
```

So, the unique signal patterns would correspond to the following digits:

* acedgfb: 8
* cdfbe: 5
* gcdfa: 2
* fbcad: 3
* dab: 7
* cefabd: 9
* cdfgeb: 6
* eafb: 4
* cagedb: 0
* ab: 1

Then, the four digits of the output value can be decoded:

* cdfeb: 5
* fcadb: 3
* cdfeb: 5
* cdbaf: 3

Therefore, the output value for this entry is **5353**.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

* fdgacbe cefdb cefbgd gcbe: 8394
* fcgedb cgb dgebacf gc: 9781
* cg cg fdcagb cbg: 1197
* efabcd cedba gadfec cb: 9361
* gecf egdcabf bgf bfgea: 4873
* gebdcfa ecba ca fadegcb: 8418
* cefg dcbef fcge gbcadfe: 4548
* ed bcgafe cdgba cbgef: 1625
* gbdfcae bgc cg cgb: 8717
* fgae cfgab fg bagce: 4315

Adding all of the output values in this larger example produces **61229**.

### Test Case


```r
# input data
input <- read_delim("./test.txt", delim = " | ", col_names = c("pattern","output")) %>% 
  mutate(id = 1:nrow(.) ) %>% 
  select(id, everything())

# normal digits segment sequence mapping
digits <- tibble(
    digit = c(0:9),
    segs = c("abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg")
  )

decode_segments <- function(.pat, .out, .dig){

  # transform the string inputs in vectors
  input_patterns <- .pat %>% 
    str_split(pattern=" ") %>% unlist()
  
  output_values <- .out %>% 
    str_split(pattern=" ") %>% unlist()

  # frequency analysis
  seg_freq <- str_c(input_patterns, collapse="") %>% 
    str_split("") %>% 
    unlist() %>% 
    enframe() %>% 
    count(value, sort = T)
  
  # the segment that appers 4 times is 'e'
  seg_e <- filter(seg_freq, n==4)$value

  # the segment that appers 6 times is 'b'
  seg_b <- filter(seg_freq, n==6)$value
  
  # the segment that appers 9 times is 'f' 
  seg_f <- filter(seg_freq, n==9)$value
  
  # length analysis
  seg_len <- input_patterns %>% 
    enframe() %>% 
    mutate(length = map_int(value,nchar)) %>% 
    rename(pattern=value)
  
  # pattern with length 2 (#1), removing 'f' to find segment 'c'
  seg_c <- filter(seg_len, length==2)$pattern %>% str_remove_all(seg_f)
  
  # pattern with length 3 (#7), removing 'c' and 'f' to find segment 'a'
  seg_a <- filter(seg_len, length==3)$pattern %>% str_remove_all(paste(seg_c, seg_f, sep="|"))
  
  # pattern with length 4 (#4), removing 'b', 'c' and 'f' to find segment 'd'
  seg_d <- filter(seg_len, length==4)$pattern %>% str_remove_all(paste(seg_b, seg_c, seg_f, sep="|"))
  
  # pattern with length 7 (#8), removing all known segments to find segment 'g'
  seg_g <- filter(seg_len, length==7)$pattern %>% str_remove_all(paste(seg_a, seg_b, seg_c, seg_d, seg_e, seg_f, sep="|"))
  
  # creating a from/to segment mapping
  seg_map <- tibble(
    to = letters[1:7],
    from = c(seg_a, seg_b, seg_c, seg_d, seg_e, seg_f, seg_g)
  )
  
  # translating the segments in the output values
  output_values %>% 
    enframe() %>% 
    mutate(name=1:nrow(.)) %>% 
    # for each sequence split the segments
    mutate( segs = map(value,function(.x){
      .x %>% 
        str_split("") %>% 
        unlist() %>% 
        enframe() %>% 
        select(seg=value) %>% 
        return()
    })) %>% 
    unnest(segs) %>% 
    # apply segment decoding 
    inner_join(seg_map, by=c("seg"="from")) %>% 
    arrange(name,to) %>% 
    group_by(name) %>% 
    # bring together the segments decoded to a digit sequence
    mutate(decoded = str_c(to, collapse = "")) %>% 
    ungroup() %>% 
    select(name, value, decoded) %>% 
    distinct() %>% 
    # applying the normal pattern decoding to find the digit
    inner_join(.dig, by=c("decoded"="segs")) %>% 
    pull(digit) %>% 
    # bring together the number shown in the display
    str_c(collapse = "") %>% 
    as.integer() %>% 
    return()  
}

# process input
input_translated <- input %>% 
  mutate( display_number = map2_int(pattern, output, decode_segments, .dig = digits))

# let's
head(input_translated)
```

```
## # A tibble: 6 × 4
##      id pattern                                 output            display_number
##   <int> <chr>                                   <chr>                      <int>
## 1     1 be cfbegad cbdgef fgaecd cgeb fdcge ag… fdgacbe cefdb ce…           8394
## 2     2 edbfga begcd cbg gc gcadebf fbgde acbg… fcgedb cgb dgeba…           9781
## 3     3 fgaebd cg bdaec gdafb agbcfd gdcbef bg… cg cg fdcagb cbg            1197
## 4     4 fbegcd cbd adcefb dageb afcb bc aefdc … efabcd cedba gad…           9361
## 5     5 aecbfdg fbg gf bafeg dbefa fcge gcbea … gecf egdcabf bgf…           4873
## 6     6 fgeab ca afcebg bdacfeg cfaedg gcfdb b… gebdcfa ecba ca …           8418
```

```r
# final number
sum(input_translated$display_number)
```

```
## [1] 61229
```


### Puzzle Answer

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?


```r
# input data
read_delim("./input.txt", delim = " | ", col_names = c("pattern","output")) %>% 
  mutate(id = 1:nrow(.) ) %>% 
  select(id, everything()) %>% 
  mutate( display_number = map2_int(pattern, output, decode_segments, .dig = digits)) %>% 
  summarise( answer = sum(display_number) )
```

```
## # A tibble: 1 × 1
##   answer
##    <int>
## 1 986034
```

