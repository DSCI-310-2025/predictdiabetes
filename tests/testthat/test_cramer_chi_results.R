set.seed(100)
df <- tibble::tibble(
  Category1 = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
  Category2 = sample(c("X", "Y"), 100, replace = TRUE),
  Category3 = sample(c(1, 2, 4.5), 100, replace = TRUE),
  Category4 = sample(c(1L, 2L, 3L), 100, replace = TRUE),
  Target = sample(c("Yes", "No"), 100, replace = TRUE)
)

empty_df <- tibble::tibble(Category1 = character(), Target = character())

# expected cases ---------------------------------------------------------------

test_that("cramer_chi_results returns expected structure", {
  categorical_vars <- c("Category1")
  result <- cramer_chi_results(df, categorical_vars, "Target")

  # Check if result is a tibble
  testthat::expect_s3_class(result, "tbl_df")

  # Check if all necessary columns are present
  testthat::expect_true(all(c("Variable", "Statistic", "DF", "p_value", "Expected_Min", "Expected_Max", "CramersV") %in% colnames(result)))

  # Check if the number of rows matches the number of categorical variables
  testthat::expect_equal(nrow(result), length(categorical_vars))
})


test_that("cramer_chi_results calculates valid p-values", {
  categorical_vars <- c("Category1")
  result <- cramer_chi_results(df, categorical_vars, "Target")

  # Check if p-values are between 0 and 1
  testthat::expect_true(all(result$p_value >= 0 & result$p_value <= 1))
})


test_that("cramer_chi_results calculates valid Cramer's V", {
  categorical_vars <- c("Category1")
  result <- cramer_chi_results(df, categorical_vars, "Target")

  # Check if Cramer's V is between 0 and 1
  testthat::expect_true(all(result$CramersV >= 0 & result$CramersV <= 1))
})


test_that("cramer_chi_results calculates valid Chi-square statistics", {
  categorical_vars <- c("Category1", "Category2")
  result <- cramer_chi_results(df, categorical_vars, "Target")

  # Check if Chi-square statistics are positive
  testthat::expect_true(all(result$Statistic > 0))
})


test_that("cramer_chi_results calculates valid degrees of freedom", {
  categorical_vars <- c("Category1", "Category2")
  result <- cramer_chi_results(df, categorical_vars, "Target")

  # Check if degrees of freedom are positive
  testthat::expect_true(all(result$DF > 0))
})

# edge cases ---------------------------------------------------------------------

test_that("cramer_chi_results handles empty input gracefully", {
  empty_df <- tibble::tibble(Category1 = character(), Target = character())
  testthat::expect_error(cramer_chi_results(empty_df, "Category1", "Target"), "Insufficient data: the dataframe is empty.")
})




# error cases ------------------------------------------------------------------

test_that("cramer_chi_results handles dataframe with no rows correctly", {
  empty_df <- tibble::tibble(Category1 = character(), Target = character())
  testthat::expect_error(cramer_chi_results(empty_df, c("Category1"), "Target"),
                         "Insufficient data: the dataframe is empty.")
})


test_that("function stops if categorical_vars is not a character vector", {
  non_character_categorical_vars <- 123  # Not a character vector

  expect_error(cramer_chi_results(df, non_character_categorical_vars, "mpg"),
               "categorical_vars must be a character vector of column names.")
})

test_that("function stops if categorical_vars are not in the dataframe", {
  invalid_categorical_vars <- c("cyl", "nonexistent_col")  # 'nonexistent_col' doesn't exist in df

  expect_error(cramer_chi_results(df, invalid_categorical_vars, "mpg"),
               "The following variables are not present in the dataframe: cyl, nonexistent_col")
})

