mock_get_docs <- function(code, con = TRUE, ...) {
  with_mocked_bindings(
    code = code,
    has_connection = function() con,
    ...,
    get_fys = function() {
      doc_fys <- list(
        gaa = c(2020:2026),
        nep = c(2020:2026)
      )
      return(doc_fys)
    },
    download_docs = function() NULL
  )
}

test_that("errors without internet", {
  mock_get_docs(
    con = FALSE,
    code = expect_error(get_docs(), "connection not detected")
  )
})

test_that(
  "invalid inputs are rejected",
  mock_get_docs({
    expect_error(get_docs(1), "must be character")
    expect_error(get_docs(character()), "length, invalid")
    expect_error(get_docs(c("gaa", "gaa", "nep")), "length, invalid")
    expect_error(get_docs(c("gae", "nea")), "invalid budget document")
    expect_error(get_gaa("a"), "must be numeric")
    expect_error(get_gaa(list(c(1, "a"))), "must be numeric")
    expect_error(get_nep(list()), "length, invalid")
    expect_error(
      get_nep(rep(list(2020), length(get_doc_ls()))),
      "length, invalid"
    )
    expect_error(get_docs(year = 1), "`year` unsupported")
  })
)

test_that(
  "duplicate warning is emitted",
  mock_get_docs({
    expect_warning(
      get_docs(year = list(gaa = 2020, gaa = 2021)),
      "`type` duplicates found"
    )
  })
)

test_that("recycling warning is emitted", {
  mock_get_docs(
    code = expect_warning(
      get_docs(year = rep(list(2020), 2)),
      "not a multiple"
    ),
    get_doc_ls = function() {
      doc_types <- c(
        gaa = "General Appropriations Act",
        nep = "National Expenditure Program",
        fake = "Fake Document Type"
      )
      return(doc_types)
    }
  )
})
