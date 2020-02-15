context("outOfPackage")

test_that("outOfPackage - coverage", {
  expect_true('error' %in% class(guardExecution(abort('volontary aborption'), FALSE)))

  expect_error(ensureFilenameExtension('x', character(0)))
  expect_equal(ensureFilenameExtension('x', 'R'), 'x.R')
  expect_equal(ensureFilenameExtension('x', ''), 'x.')
  expect_equal(ensureFilenameExtension('a', NA_character_), 'a')

  expect_true(is.na(capitalize()))
  expect_true(is.na(capitalize(NA)))
  expect_equal(capitalize(character(0)), list()) # weird
  expect_equal(capitalize(45L), 45L)
  expect_equal(capitalize(''), '')
  expect_equal(capitalize('f'), 'F')
  expect_true(is.character(lubridate::date()))

  expect_equal(normalizeFilename('a-<-'), "a--_ee97e9bb28882d92fa97001097fd97ba")
})
