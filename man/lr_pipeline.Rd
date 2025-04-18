% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lr_pipeline.R
\name{lr_pipeline}
\alias{lr_pipeline}
\title{Logistic Regression Binary Classification Pipeline}
\usage{
lr_pipeline(
  data,
  target_col,
  vfolds = 5,
  grid_size = 10,
  tuning_metric,
  output_path
)
}
\arguments{
\item{data}{A data frame or data frame extension (e.g. a tibble).}

\item{target_col}{A string specifying the variable of interest.}

\item{vfolds}{A number specifying the amount of folds used in k-fold cross-validation (Default = 5).}

\item{grid_size}{A number specifying penalty values to test during model tuning (Default = 10).}

\item{tuning_metric}{A string specifying the metric used to select for the most optimal model (e.g. "recall").}

\item{output_path}{String path location to save the model as an RDS object.}
}
\value{
An RDS file containing the workflow object
}
\description{
Trains and fits a logistic regression model and cross-validates for optimal
hyperparameter values.
}
\examples{
\dontrun{
  lr_pipeline(mtcars, "am", vfolds = 5, grid_size = 10, tuning_metric = "recall",
  output_path = "lasso_tuned_wflow.RDS")
}

}
