mock_get_docs <- function(expr, no_internet = FALSE, fys = NULL, doc_ls = NULL) {
  with_mocked_bindings(
    {
      expr
    },
    no_internet = function() no_internet,
    get_doc_ls = function() doc_ls %||% c(
      gaa = "General Appropriations Act",
      nep = "National Expenditure Program"
    ),
    get_fys = function() fys %||% list(gaa = 2020:2026, nep = 2020:2026),
    download_docs = function() NULL
  )
}

test_that("errors without internet", {
  mock_get_docs({
    expect_error(get_docs(), "internet connection not detected")
  }, no_internet = TRUE)
})

test_that("invalid inputs are rejected", {
  mock_get_docs({
    expect_error(get_docs(1), "`type` must be character")
    expect_error(get_docs(character()), "`type` must not be empty")
    expect_error(get_docs(c("gae", "nea")), "invalid budget document")
    expect_error(get_gaa("a"), "`year` must be numeric")
    expect_error(get_nep(list()), "`year` must not be empty")
    expect_error(get_nep(list(2020, 2021)), "`year` length cannot exceed `type` length")
    expect_error(get_docs(year = 1), "`year` unsupported")
  })
})

test_that("duplicate warning is emitted", {
  mock_get_docs({
    expect_warning(
      get_docs(type = c("gaa", "gaa")),
      "`type` duplicates found"
    )
  })
})

test_that("recycling warning is emitted", {
  mock_get_docs({
    expect_warning(
      get_docs(year = rep(list(2020), 2)),
      "not a multiple"
    )
  }, doc_ls = c(
    gaa = "General Appropriations Act",
    nep = "National Expenditure Program",
    fake = "Fake Document Type"
  ))
})
