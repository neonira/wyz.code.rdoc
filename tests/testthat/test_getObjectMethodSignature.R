context("getObjectMethodSignature")

source(file.path(system.file(package = "wyz.code.offensiveProgramming"),
                 'code-samples', 'classes', 'sample-classes.R'))


obj <- list( MyEnv(),
             Bu_S3(),
             new('Person_RC', name = 'neonira'),
             new('Person_S4', name = 'neonira'),
             Wyx(1:7),
             Accumulator_R6$new(),
             Zarg(),
             Zirg(),
             Zorg(),
             Zurg(),
             PureR()
)

# R weirdness
# p <- new('Person_RC', name = 'neonira')
# print(p[['setName']])
# print(p$setName)
# print(p[['setName']])

mn <- c('f', 'alpha', 'setName', 'show', 'f', 'add', rep('f', 5))

robj <- lapply(seq_len(length(obj)), function(k) {
  tryCatch(getObjectMethodSignature(obj[[k]], mn[k]),
           error = function(err) err)
})

test_that("getObjectMethodSignature", {
  lapply(seq_len(length(obj)), function(k) {
    expect_true(is.character(robj[[!!k]]))
  })
})


test_that("getObjectMethodSignature - direct calls", {
  expect_true(is.character(getObjectMethodSignature(new('Person_RC', name = 'neonira'), 'setName')))

  expect_error(getObjectMethodSignature(1L))
  expect_error(getObjectMethodSignature(obj[[1]], 'unexistentFunction'))
  expect_error(getObjectMethodSignature(Unknown()))
})