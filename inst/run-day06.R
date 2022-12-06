library(adventofcode22)
x <- readLines("./inst/input06.txt")

p1 <- f06_find_end_of_first_marker(x, width = 4)
p2 <- f06_find_end_of_first_marker(x, width = 14)

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
