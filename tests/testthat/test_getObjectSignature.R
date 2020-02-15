context("getObjectSignature")

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

#print(obj)

robj <- lapply(obj, function(e) tryCatch(getObjectSignature(e),
                                         error = function(err) err))

#print(obj)

test_that("getObjectSignature - coverage", {
  expect_true(is.na(getObjectSignature(new.env())))
  expect_error(getObjectSignature(MyBadEnv()))

  sapply(seq_len(length(robj)), function(k) {
    ock <- wyz.code.offensiveProgramming::getObjectClassNames(obj[[k]])$classname
    b <- grepl(ock, robj[[k]][1])
    #cat('\n', k, ock, 'robj', robj[[k]][1], 'result', b, '\n')
    expect_true(b)
  })
})
