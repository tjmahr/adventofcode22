test_that("day 05", {
  example_data_05() |>
    f05_move_crates_and_read_top() |>
    expect_equal("CMZ")

  example_data_05() |>
    f05_move_crates_and_read_top(is_part_2 = TRUE) |>
    expect_equal("MCD")
})
