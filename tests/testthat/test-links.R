breakfast_row <- function(extra = list()) {
  program <- jsonlite::read_json(example_program())
  jsonlite::toJSON(c(list(name = "Breakfast Schedule", program_json = program), extra),
                   auto_unbox = TRUE, null = "null", digits = NA)
}

# A fake site: records the URLs asked for, answers the public API routes.
fake_site <- function(env) {
  function(url) {
    env$asked <- c(env$asked, url)
    ok <- grepl("/api/public/programs/11111111-1111-1111-1111-111111111111$", url) ||
      grepl("/api/public/shares/0123456789abcdef$", url)
    body <- if (ok) breakfast_row() else '{"error": "Program not found or not public"}'
    list(status_code = if (ok) 200L else 404L, content = charToRaw(as.character(body)))
  }
}

test_that("a public program page link reads through the public API on the same site", {
  env <- new.env()
  local_mocked_bindings(http_get = fake_site(env))
  link <- "https://kitchen.rhylthyme.com/p/11111111-1111-1111-1111-111111111111"
  p <- read_rhylthyme(link)
  expect_equal(env$asked, "https://kitchen.rhylthyme.com/api/public/programs/11111111-1111-1111-1111-111111111111")
  expect_equal(p$steps, read_rhylthyme(example_program())$steps)
  expect_equal(p$source, link)
})

test_that("share links read through the share API, with or without a path", {
  env <- new.env()
  local_mocked_bindings(http_get = fake_site(env))
  read_rhylthyme("https://www.rhylthyme.com/?share=0123456789abcdef")
  read_rhylthyme("https://rhylthyme.com?share=0123456789ABCDEF&view=timeline")
  read_rhylthyme("https://appistry.com/p/11111111-1111-1111-1111-111111111111?share=0123456789abcdef")
  expect_equal(env$asked, c(
    "https://www.rhylthyme.com/api/public/shares/0123456789abcdef",
    "https://rhylthyme.com/api/public/shares/0123456789abcdef",
    "https://appistry.com/api/public/shares/0123456789abcdef"
  ))
})

test_that("a private or missing program says so", {
  env <- new.env()
  local_mocked_bindings(http_get = fake_site(env))
  expect_error(read_rhylthyme("https://www.rhylthyme.com/p/22222222-2222-2222-2222-222222222222"),
               "not public, or does not exist: https://www.rhylthyme.com/p/2222")
  expect_error(read_rhylthyme("https://www.rhylthyme.com/?share=ffffffffffffffff"), "not public, or does not exist")
})

test_that("other URLs, on rhylthyme.com or elsewhere, are fetched as they are", {
  expect_null(rhylthyme_link_api("https://www.rhylthyme.com/static/examples/tea.json"))
  expect_null(rhylthyme_link_api("https://example.org/p/11111111-1111-1111-1111-111111111111"))
  expect_null(rhylthyme_link_api("https://notrhylthyme.com/?share=0123456789abcdef"))
  expect_equal(rhylthyme_link_api("http://localhost.rhylthyme.com:5000/p/11111111-1111-1111-1111-111111111111"),
               "http://localhost.rhylthyme.com:5000/api/public/programs/11111111-1111-1111-1111-111111111111")
})
