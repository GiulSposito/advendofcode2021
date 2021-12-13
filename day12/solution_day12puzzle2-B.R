library(tidyverse)
library(furrr)

createCaveEdges <- function(.lines){
  .lines %>% 
    enframe() %>% 
    select(-name) %>% 
    separate(value, into = c("from","to"), sep="-") %>% 
    bind_rows(select(.,to=from, from=to)) %>% 
    return()
}

createCaveNodes <- function(.edges){
  .edges %>% 
    select(from) %>% 
    distinct()  %>%
    rename( name=from ) %>% 
    mutate( type = factor(case_when(
      name == "start" ~ "start",
      name == "end" ~ "end",
      substr(name,1,1) %in% letters ~ "small",
      T ~ "big"
    ))) %>% 
    mutate( visits = 0L ) %>% 
    return()
} 

# .nodes <- nodes
# .edges <- edges
# .currNode <- "start"
# .path <- vector("character", 0)

# recursive function to find paths from a point
findPaths2VisitsFrom <- function(.net, .currNode, .path){
  
  # count current node
  net <- .net %>% 
    mutate( visits = if_else(name==.currNode, visits+1L, visits))
  
  allowTwoVisits <- all((net %>% 
                           filter(type=="small") %>% 
                           pull(visits)) < 2)
  
  # type of current node
  currNodeType <- net %>% 
    filter(name==.currNode) %>% 
    pull(type)
  
  .path[length(.path)+1] <- .currNode
  
  # if current type is end, so its over
  if (currNodeType == "end") return(.path)
  
  paths <- net %>% 
    filter( from == .currNode ) %>% 
    filter( allowTwoVisits | !(type=="small" & visits>0),
            type!="start") %>% 
    pull(name) %>% 
    unique()
  
  # there is not new path
  if (length(paths)==0) return(.path)
  
  # go for next steps
  paths %>% 
    future_map(~findPaths2VisitsFrom(.net=net, .currNode = .x, .path=.path)) %>% 
    return()
}

caveInput <- readLines("./day12/input.txt")
caveEdges <- createCaveEdges(caveInput)
caveNodes <- createCaveNodes(caveEdges)
caveNet <- caveEdges %>% 
  inner_join(caveNodes, by=c("to"="name")) %>% 
  rename(name=to)

plan(multicore, workers=8)
findPaths2VisitsFrom(caveNet, "start", vector("character", 0)) %>% 
  rlist::list.flatten() %>% 
  keep(~("end" %in% .x)) %>% 
  length() %>% 
  print()



