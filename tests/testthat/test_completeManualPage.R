context("completeManualPage")

ic <- InputContext(NULL, 'beautify')

po <- ProcessingContext(extraneous_l = list(
  zorg = 'extraneous section required'
))
p <- produceManualPage(ic, po)

pc <- ProcessingContext(postProcessing_l = list(
  details = function(content_s) 'some more details',
  concept = function(content_s) 'yet another concept'
))
r1 <- completeManualPage(p$context$filename, pc)
r2 <- completeManualPage(p$context$filename, pc, FALSE)

test_that("completeManualPage", {
  expect_true(r1)
  expect_true(r2)
  expect_false(completeManualPage(p$context$filename, ProcessingContext()))
  expect_output(completeManualPage(p$context$filename, pc,
                                   verbosity_b_1 = TRUE)) # coverage
})


p <- produceManualPage(ic, po)

px <- ProcessingContext(postProcessing_l = list(
  details = function(content_s) 'some other details bla bli blo'
))

test_that("completeManualPage - coverage", {
  expect_true(completeManualPage(p$context$filename, px,
                               add_b_1 = FALSE,
                               verbosity_b_1 = TRUE)) # coverage
})
