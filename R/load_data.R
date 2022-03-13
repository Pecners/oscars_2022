library(tidyverse)

crew <- read_rds("data/title.crew.rda")
principals <- read_rds("data/title.principals.rda")
names <- read_rds("data/name.basics.rda")
ratings <- read_rds("data/title.ratings.rda")
title_basics <- read_rds("data/title.basics.rda")

movies <- title_basics %>%
  filter(titleType == "movie")

principals <- principals %>%
  filter(tconst %in% movies$tconst)

