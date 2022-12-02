library(adventofcode22)
x <- readLines("./inst/input02.txt")

p1 <- f02a_find_total_points(x)
p2 <- f02b_find_total_points(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
