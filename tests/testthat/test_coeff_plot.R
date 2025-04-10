# Creating a minimal LASSO workflow for testing
make_lasso_workflow <- function() {
  data("mtcars")
  set.seed(123)
  
  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors())
  
  lasso_spec <- parsnip::linear_reg(penalty = 0.1, mixture = 1) %>%
    parsnip::set_engine("glmnet")
  
  wf <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(lasso_spec) %>%
    workflows::fit(data = mtcars)
  
  return(suppressWarnings(wf))
}

# Expected cases
test_that("coeff_plot returns a ggplot object", {
  wf <- make_lasso_workflow()
  plot_output <- coeff_plot(wf)
  
  expect_s3_class(plot_output, "gg")
})

# Edge case
test_that("coeff_plot handles models with zero or few nonzero coefficients", {
  data("mtcars")
  set.seed(123)
  
  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_normalize(recipes::all_predictors())
  
  lasso_spec <- parsnip::linear_reg(penalty = 10, mixture = 1) %>%
    parsnip::set_engine("glmnet")
  
  wf_zero <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(lasso_spec) %>%
    workflows::fit(data = mtcars)
  
  plot_output_zero <- coeff_plot(wf_zero)
  
  expect_s3_class(plot_output_zero, "gg")
  expect_gt(nrow(plot_output_zero$data), 0)
})

# Error cases
test_that("coeff_plot throws error for NULL input", {
  expect_error(coeff_plot(NULL), "The input must be a trained workflow object.")
})

test_that("coeff_plot throws error for non-workflow input", {
  expect_error(coeff_plot("not_a_workflow"), "The input must be a trained workflow object.")
})

test_that("coeff_plot throws error for non-glmnet model in workflow", {
  data("mtcars")
  
  rec <- recipes::recipe(mpg ~ ., data = mtcars)
  
  lm_spec <- parsnip::linear_reg() %>%
    parsnip::set_engine("lm")
  
  bad_wf <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(lm_spec) %>%
    workflows::fit(data = mtcars)
  
  expect_error(coeff_plot(bad_wf), "The workflow must contain a fitted LASSO model")
})
