test_that("day 03", {
  example_data_03() |>
    f03a_sum_priorities() |>
    expect_equal(157)

  example_data_03() |>
    f03b_sum_priorities() |>
    expect_equal(70)
})
