test_that("day 02", {
  x <- example_data_02()

  f02a_find_total_points(x) |> expect_equal(15)
  f02b_find_total_points(x) |> expect_equal(12)
})
