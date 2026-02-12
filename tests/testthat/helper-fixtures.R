# Shared test fixtures loaded automatically by testthat before tests run.

# Classification fixture using train_covid / test_covid
build_classification_fixture <- function() {
  data_all <- add_data_type(data_train = train_covid, data_test = test_covid)
  data_all <- prepare_features(data_all, target_lab = "Outcome", task = "classification")

  train_result <- train_tree(
    data_train = train_covid,
    target_lab = "Outcome",
    model = "rpart"
  )
  fit <- train_result$fit
  var_imp <- train_result$var_imp

  tree_res <- compute_tree(
    fit,
    model = "rpart", show = "test",
    data = data_all, target_lab = "Outcome",
    task = "classification"
  )

  sorted_dat <- sorted_mat(
    tree_res,
    target_lab = "Outcome", show = "test"
  )

  list(
    data_all   = data_all,
    fit        = fit,
    var_imp    = var_imp,
    tree_res   = tree_res,
    sorted_dat = sorted_dat
  )
}
