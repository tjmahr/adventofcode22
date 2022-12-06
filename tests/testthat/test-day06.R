test_that("day 06", {
  example_data_06() |>
    f06_find_end_of_first_marker() |>
    expect_equal(7)

  example_data_06(2) |>
    f06_find_end_of_first_marker() |>
    expect_equal(5)

  example_data_06(3) |>
    f06_find_end_of_first_marker() |>
    expect_equal(6)

  example_data_06(4) |>
    f06_find_end_of_first_marker() |>
    expect_equal(10)

  example_data_06(5) |>
    f06_find_end_of_first_marker() |>
    expect_equal(11)

  example_data_06() |>
    f06_find_end_of_first_marker(14) |>
    expect_equal(19)

  example_data_06(2) |>
    f06_find_end_of_first_marker(14) |>
    expect_equal(23)

  example_data_06(3) |>
    f06_find_end_of_first_marker(14) |>
    expect_equal(23)

  example_data_06(4) |>
    f06_find_end_of_first_marker(14) |>
    expect_equal(29)

  example_data_06(5) |>
    f06_find_end_of_first_marker(14) |>
    expect_equal(26)
})
