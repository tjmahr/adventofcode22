library(adventofcode22)
x <- readLines("./inst/input03.txt")

p1 <- f03a_sum_priorities(x)
p2 <- f03b_sum_priorities(x)

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)
