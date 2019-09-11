context("createManualPage")

createManualPage <- function(fun_f_1) {
  fn <- deparse(substitute(fun_f_1))
  #cat('function is', fn, '\n')
  o <- generateWrapperObject(fun_f_1, fn)
  doc <- generateDocumentationContent(tempdir(), 'method', fn,
                                      o, 'lambda',
                                      overwrite_b_1 = TRUE)
  list(wrapper = o, doc = doc)
}


ow <- list(
  createManualPage(append),
  createManualPage(cos)
)

# Still far from perfect. Manual rework is about
#    a. removing WrapperObject from name and alias
#    b. aligning usage - not an S3 method
#    c. adding return information
#    d. adding examples

test_that("createManualPage", {
  sapply(seq_len(length(ow)), function(k) {
    expect_true(file.exists(ow[[!!k]]$doc$filename))
  })
})
