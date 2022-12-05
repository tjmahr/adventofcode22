test_that("day 04", {
  example_data_04() |>
    f04a_count_contained_ranges() |>
    expect_equal(2)

  example_data_04() |>
    f04b_count_overlaps() |>
    expect_equal(4)
})
