#' Plot LASSO Classification Model Coefficients
#'
#' This function extracts the coefficients from a fitted LASSO classification
#' workflow object, arranges them by importance, and creates a bar plot to
#' visualize the coefficients.
#'
#' @param model A workflow object containing a trained LASSO model.
#'
#' @return A `ggplot` object with the bar plot of LASSO model coefficients.
#
#' @export
#'
#' @examples
#' # Assuming you have a trained workflow named 'lasso_model'
#' \dontrun{
#' coeff_plot(lasso_model)
#' }
#'
coeff_plot <- function(model) {

  # Check if input is a workflow
  if (!inherits(model, "workflow")) {
    stop("The input must be a trained workflow object.")
  }

  # Check if input workflow contains a fitted glmnet (LASSO) model
  fitted_model <- workflows::extract_fit_parsnip(model)

  if (!inherits(fitted_model$fit, "glmnet")) {
    stop("The workflow must contain a fitted LASSO model (class 'glmnet').")
  }

  # Extract coefficients using workflows and broom
  lasso_coefs <- model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy()

  lasso_coefs_summarized <- lasso_coefs %>%
    dplyr::arrange(desc(estimate))

  cf_plot <- ggplot2::ggplot(lasso_coefs_summarized, ggplot2::aes(x = reorder(term, estimate), y = estimate)) +
    ggplot2::geom_bar(stat = "identity", fill = "#1f77b4") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Feature", y = "Coefficient") +
    ggplot2::theme_minimal()

  return(cf_plot)
}
