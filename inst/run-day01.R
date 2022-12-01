library(adventofcode22)
x <- readLines("./inst/input01.txt")

p1 <- f01a_find_max_calories(x)
p2 <- f01b_find_top_3_calories_sum(x)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)
