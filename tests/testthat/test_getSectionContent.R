context("getSectionContent")

f <- findFilesInPackage("markup.Rd", 'wyz.code.rdoc')
g <- findFilesInPackage("empty.Rd", 'wyz.code.rdoc')

test_that("getSectionContent", {
  expect_error(getSectionContent('/tmp/not-a-file.Rd'))
  expect_length(getSectionContent(f[1]), 1L)
  expect_equal(getSectionContent(g[1]), list())
})
