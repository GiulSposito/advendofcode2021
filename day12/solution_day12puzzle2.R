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
findPaths2VisitsFrom <- function(.nodes, .edges, .currNode, .path){
  
  # count current node
  nodes <- .nodes %>% 
    mutate( visits = if_else(name==.currNode, visits+1L, visits))
  
  allowTwoVisits <- all((nodes %>% 
                           filter(type=="small") %>% 
                           as_tibble() %>% 
                           pull(visits)) < 2)
  
  # type of current node
  currNodeType <- nodes %>% 
    filter(name==.currNode) %>% 
    pull(type)
  
  .path[length(.path)+1] <- .currNode
  
  # if current type is end, so its over
  if (currNodeType == "end") return(.path)
  
  paths <- .edges %>% 
    filter( from == .currNode ) %>% 
    inner_join(nodes, by=c("to"="name")) %>% 
    filter( allowTwoVisits | !(type=="small" & visits>0),
            type!="start") %>% 
    pull(to)
  
  # there is not new path
  if (length(paths)==0) return(.path)
  
  # go for next steps
  paths %>% 
    future_map(~findPaths2VisitsFrom(.nodes = nodes, .edges = .edges,
                                     .currNode = .x, .path=.path)) %>% 
    return()
}

caveInput <- readLines("./day12/test03.txt")
caveEdges <- createCaveEdges(caveInput)
caveNodes <- createCaveNodes(caveEdges)

plan(multisession, workers=8)
findPaths2VisitsFrom(caveNodes, caveEdges, "start", vector("character", 0)) %>% 
  rlist::list.flatten() %>% 
  keep(~("end" %in% .x)) %>% 
  length()



