test_that("day 01", {
  example_data_01() |>
    f01a_find_max_calories() |>
    expect_equal(24000)

  example_data_01() |>
    f01b_find_top_3_calories_sum() |>
    expect_equal(45000)
})
