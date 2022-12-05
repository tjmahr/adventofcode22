library(adventofcode22)
x <- readLines("./inst/input05.txt")

p1 <- f05_move_crates_and_read_top(x)
p2 <- f05_move_crates_and_read_top(x, is_part_2 = TRUE)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)
