# --- train_rf ------------------------------------------------------------------

test_that("train_rf returns correct structure", {
  rf <- train_rf(train_covid, target_lab = "Outcome", ntree = 10)

  expect_type(rf, "list")
  expect_named(rf, c("forest", "var_imp", "ntree"))
  expect_s3_class(rf$forest, "cforest")
  expect_equal(rf$ntree, 10)
})

test_that("train_rf var_imp sums to 1", {
  rf <- train_rf(train_covid, target_lab = "Outcome", ntree = 10)

  expect_equal(sum(rf$var_imp), 1)
  expect_true(all(names(rf$var_imp) %in% c("LDH", "hs_CRP", "Lymphocyte")))
})

test_that("train_rf accepts mtry parameter", {
  expect_no_error(
    train_rf(train_covid, target_lab = "Outcome", ntree = 10, mtry = 2)
  )
})
