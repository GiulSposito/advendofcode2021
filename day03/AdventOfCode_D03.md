---
title: "Day 2: Binay Diagnostic"
output:
  html_document:
    keep_md: yes
---

## Part 01

The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report just in case.

The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly, can tell you many useful things about the conditions of the submarine. The first parameter to check is the power consumption.

You need to use the binary numbers in the diagnostic report to generate two new binary numbers (called the gamma rate and the epsilon rate). The power consumption can then be found by multiplying the gamma rate by the epsilon rate.

Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of all numbers in the diagnostic report. For example, given the following diagnostic report:

```
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
```

Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.

The most common second bit of the numbers in the diagnostic report is 0, so the second bit of the gamma rate is 0.

The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and so the final three bits of the gamma rate are 110.

So, the gamma rate is the binary number 10110, or 22 in decimal.

The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.

### Test Case


```r
# test data (from the example)
test <- c("00100","11110","10110","10111","10101","01111",
          "00111","11100","10000","11001","00010","01010")

# converting the string data into a matrix of integer
binData <- do.call(rbind, lapply(strsplit(test, split="", fixed=T), as.numeric))

binData
```

```
##       [,1] [,2] [,3] [,4] [,5]
##  [1,]    0    0    1    0    0
##  [2,]    1    1    1    1    0
##  [3,]    1    0    1    1    0
##  [4,]    1    0    1    1    1
##  [5,]    1    0    1    0    1
##  [6,]    0    1    1    1    1
##  [7,]    0    0    1    1    1
##  [8,]    1    1    1    0    0
##  [9,]    1    0    0    0    0
## [10,]    1    1    0    0    1
## [11,]    0    0    0    1    0
## [12,]    0    1    0    1    0
```

```r
# for each column 
gammaVector <- apply(binData, 2, function(x){
    # tables the 0s and 1s, sort it desc, get the name of first element (most frequent)
    as.integer(names(sort(table(x),decreasing=T)[1]))
  })

# epson is the binnary inverted
epsonVector <- as.integer(!gammaVector)

# binary values
gammaVector
```

```
## [1] 1 0 1 1 0
```

```r
epsonVector
```

```
## [1] 0 1 0 0 1
```

```r
# convert to integers
gammaNum <- strtoi(paste(gammaVector, collapse = ""), base=2)
epsonNum <- strtoi(paste(epsonVector, collapse = ""), base=2)

# the values
gammaNum
```

```
## [1] 22
```

```r
epsonNum
```

```
## [1] 9
```

```r
# the response
gammaNum * epsonNum
```

```
## [1] 198
```

### Puzzle Answer

Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply them together. What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)


```r
# using fixed width file from readr package to read the binary file representation
binData <- readr::read_fwf("input.txt", col_positions = readr::fwf_widths(rep(1,12)) , col_types = "i") |> 
  as.matrix()

head(binData)
```

```
##      X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12
## [1,]  0  0  0  0  1  1  1  1  0   0   1   0
## [2,]  0  1  0  0  0  0  1  0  0   1   0   0
## [3,]  0  1  0  0  1  1  1  1  1   1   1   1
## [4,]  0  0  0  1  0  1  0  0  1   1   1   1
## [5,]  0  1  0  1  0  0  0  1  1   1   1   1
## [6,]  1  1  1  1  0  0  1  1  1   0   1   1
```

```r
# for each column 
gammaVector <- apply(binData, 2, function(x){
    # tables the 0s and 1s, sort it desc, get the name of first element (most frequent)
    as.integer(names(sort(table(x),decreasing=T)[1]))
  })

# epson is the binnary inverted
epsonVector <- as.integer(!gammaVector)

# binary values
gammaVector
```

```
##  X1  X2  X3  X4  X5  X6  X7  X8  X9 X10 X11 X12 
##   1   1   0   0   0   0   1   1   1   1   1   1
```

```r
epsonVector
```

```
##  [1] 0 0 1 1 1 1 0 0 0 0 0 0
```

```r
# convert to integers
gammaNum <- strtoi(paste(gammaVector, collapse = ""), base=2)
epsonNum <- strtoi(paste(epsonVector, collapse = ""), base=2)

# the values
gammaNum
```

```
## [1] 3135
```

```r
epsonNum
```

```
## [1] 960
```

```r
# the response
gammaNum * epsonNum
```

```
## [1] 3009600
```

## Part Two

Next, you should verify the **life support rating**, which can be determined by multiplying the **oxygen generator rating** by the **CO2 scrubber rating**.

Both the oxygen generator rating and the CO2 scrubber rating are values that can be found in your diagnostic report - finding them is the tricky part. Both values are located using a similar process that involves filtering out values until only one remains. Before searching for either rating value, start with the full list of binary numbers from your diagnostic report and **consider just the first bit** of those numbers. Then:

* Keep only numbers selected by the **bit criteria** for the type of rating value for which you are searching. Discard numbers which do not match the bit criteria.
* If you only have one number left, stop; this is the rating value for which you are searching.
* Otherwise, repeat the process, considering the next bit to the right.

The **bit criteria** depends on which type of rating value you want to find:

* To find **oxygen generator rating**, determine the **most common** value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. **If 0 and 1 are equally common, keep values with a 1 in the position being considered**.
* To find **CO2 scrubber rating**, determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. **If 0 and 1 are equally common, keep values with a 0 in the position being considered**.

### Test Case

For example, to determine **the oxygen generator** rating value using the same example diagnostic report from above:

* Start with all 12 numbers and consider only the first bit of each number. There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with a 1 in the first position: 11110, 10110, 10111, 10101, 11100, 10000, and 11001.
* Then, consider the second bit of the 7 remaining numbers: there are more 0 bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in the second position: 10110, 10111, 10101, and 10000.
* In the third position, three of the four numbers have a 1, so keep those three: 10110, 10111, and 10101.
* In the fourth position, two of the three numbers have a 1, so keep those two: 10110 and 10111.
* In the fifth position, there are an equal number of 0 bits and 1 bits (one each). So, to find the **oxygen generator** rating, keep the number with a 1 in that position: 10111.
* As there is only one number left, stop; the **oxygen generator rating** is 10111, or 23 in decimal.


```r
# test data (from the example)
test <- c("00100","11110","10110","10111","10101","01111",
          "00111","11100","10000","11001","00010","01010")

# converting the string data into a matrix of integer
binData <- do.call(rbind, lapply(strsplit(test, split="", fixed=T), as.numeric))

# generate the bit criteria
getBitCriteria <- function(binMatrix){
  # for each column, round+mean (short cut to most frequent)
  apply(binMatrix, 2, function(.x){ floor(0.5+mean(.x)) })
}

# first position
mostFreqBit <- getBitCriteria(binData)
data <- binData[(binData[,1] == mostFreqBit[1]),]
data
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    1    1    1    0
## [2,]    1    0    1    1    0
## [3,]    1    0    1    1    1
## [4,]    1    0    1    0    1
## [5,]    1    1    1    0    0
## [6,]    1    0    0    0    0
## [7,]    1    1    0    0    1
```

```r
# second position
mostFreqBit <- getBitCriteria(data)
data <- data[(data[,2] == mostFreqBit[2]),]
data
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    1    1    0
## [2,]    1    0    1    1    1
## [3,]    1    0    1    0    1
## [4,]    1    0    0    0    0
```

```r
# third position
mostFreqBit <- getBitCriteria(data)
data <- data[(data[,3] == mostFreqBit[3]),]
data
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    1    1    0
## [2,]    1    0    1    1    1
## [3,]    1    0    1    0    1
```

```r
# forth position
mostFreqBit <- getBitCriteria(data)
data <- data[(data[,4] == mostFreqBit[4]),]
data
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    1    1    0
## [2,]    1    0    1    1    1
```

```r
# fifth position
mostFreqBit <- getBitCriteria(data)
oxigenRateBin <- data[(data[,5] == mostFreqBit[5]),]

# show values
oxigenRateBin
```

```
## [1] 1 0 1 1 1
```

```r
strtoi(paste(oxigenRateBin, collapse = ""), base=2)
```

```
## [1] 23
```


Then, to determine the **CO2 scrubber rating** value from the same example above:

* Start again with all 12 numbers and consider only the first bit of each number. There are fewer 0 bits (5) than 1 bits (7), so keep only the 5 numbers with a 0 in the first position: 00100, 01111, 00111, 00010, and 01010.
* Then, consider the second bit of the 5 remaining numbers: there are fewer 1 bits (2) than 0 bits (3), so keep only the 2 numbers with a 1 in the second position: 01111 and 01010.
* In the third position, there are an equal number of 0 bits and 1 bits (one each). So, to find the CO2 scrubber rating, keep the number with a 0 in that position: 01010.
* As there is only one number left, stop; the **CO2 scrubber rating** is 01010, or 10 in decimal.


```r
# start from all numbers at first position
data <- binData
pos  <- 1

# while we don't have just one number
while( is.matrix(data)&& pos <= ncol(data) ){
  # recalc bit criteria for the remaining candidates for CO2
  bitCriteria <- !getBitCriteria(data)  
  # filter new candidates
  data <- data[(data[,pos])==bitCriteria[pos],]
  # next index
  pos <- pos+1
}

# the CO2 Scrubber Rating
co2ScrubRateBin <- data

# show values
co2ScrubRateBin
```

```
## [1] 0 1 0 1 0
```

```r
strtoi(paste(co2ScrubRateBin, collapse = ""), base=2)
```

```
## [1] 10
```


Finally, to find the life support rating, multiply the **oxygen generator rating** (23) by the **CO2 scrubber rating** (10) to get 230.


```r
bin2dec <- function(.v) strtoi(paste(.v, collapse = ""), base=2)

bin2dec(oxigenRateBin) * bin2dec(co2ScrubRateBin)
```

```
## [1] 230
```

### Puzzle Answer

Use the binary numbers in your diagnostic report to calculate the oxygen generator rating and CO2 scrubber rating, then multiply them together. **What is the life support rating of the submarine?** (Be sure to represent your answer in decimal, not binary.)


```r
# using fixed width file from readr package to read the binary file representation
binData <- readr::read_fwf("input.txt", col_positions = readr::fwf_widths(rep(1,12)) , col_types = "i") |> 
  as.matrix()

### find Oxygen Rate, starting from all number as position on in the bit criteria
data <- binData
pos  <- 1

while( is.matrix(data)&& pos <= ncol(data) ){
  bitCriteria <- getBitCriteria(data)  
  data <- data[(data[,pos])==bitCriteria[pos],]
  pos <- pos+1
}

oxigenRateBin <- data

# what we find?
oxigenRateBin
```

```
##  X1  X2  X3  X4  X5  X6  X7  X8  X9 X10 X11 X12 
##   1   1   1   1   0   1   1   0   0   0   1   1
```

```r
bin2dec(oxigenRateBin)
```

```
## [1] 3939
```

```r
### find CO2 Scrubber Rate, starting from all number as position on in the bit criteria
data <- binData
pos  <- 1

while( is.matrix(data)&& pos <= ncol(data) ){
  bitCriteria <- !getBitCriteria(data)  
  data <- data[(data[,pos])==bitCriteria[pos],]
  pos <- pos+1
}

co2ScrubRateBin <- data

# what we find?
co2ScrubRateBin
```

```
##  X1  X2  X3  X4  X5  X6  X7  X8  X9 X10 X11 X12 
##   0   1   1   0   1   1   1   0   0   0   1   0
```

```r
bin2dec(co2ScrubRateBin)
```

```
## [1] 1762
```

```r
# final answer
bin2dec(oxigenRateBin) * bin2dec(co2ScrubRateBin)
```

```
## [1] 6940518
```

