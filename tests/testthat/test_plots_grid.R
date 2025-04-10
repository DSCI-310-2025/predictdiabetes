# expected cases ---------------------------------------------------------------
test_that("plots_grid combines bar plots correctly", {
  # Create a dummy ggplot object for bar plots
  bar_plots <- list(ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg)) + ggplot2::geom_bar())

  # Call the function with bar plot list
  result <- plots_grid(bar_plots, num_cols = 2)

  # Check if the result is a ggplot object (the return type from patchwork is ggplot)
  expect_s3_class(result, "gg")
})

# edge cases -------------------------------------------------------------------

test_that("plots_grid handles empty plot lists", {
  bar_plots <- list()

  # Call the function with an empty plot list
  result <- plots_grid(bar_plots, num_cols = 2)

  # Check that the result is still a ggplot object, but effectively empty
  expect_s3_class(result, "gg")
  expect_equal(length(result$layers), 0)
})


test_that("plots_grid handles large num_cols gracefully", {
  bar_plots <- list(ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg)) + ggplot2::geom_bar())

  # Call the function with a large number of columns
  result <- plots_grid(bar_plots, num_cols = 100)

  # Check that the result is a ggplot object
  expect_s3_class(result, "gg")
})

# error cases ------------------------------------------------------------------

test_that("plots_grid handles invalid inputs", {
  bar_plots <- NULL

  # Expect an error due to invalid bar_plots input
  expect_error(plots_grid(bar_plots), "'bar_plots' must be a list of ggplot objects.")
})
