context("generateDocumentationContent")

target_folder <- tempdir()

source_package <- 'wyz.code.offensiveProgramming'

source_files <- c(
  'code-samples/both-defs/good/full/AdditionTCFIG1.R',
  'code-samples/no-defs/Addition.R',
  'code-samples/frt-defs/good/partial/AdditionFIPartial.R',
  'code-samples/tcd-defs/good/partial/AdditionTCPartial.R'
)

sapply(source_files, function(e) {
  source(system.file(e, package = source_package))
})

object <- AdditionTCFIG1()
package_name <- 'wyz.code.rdoc'

refs <- list(
  list(url = 'https://cran.r-project.org/doc/manuals/R-exts.html',
       label = 'Writing R extensions',
       comment = 'to know more about R documentation requirements'),
  list(url = 'https://www.burns-stat.com/pages/Tutor/R_inferno.pdf',
       label = 'The R Inferno',
       comment = 'to discover some well-known R weirdness')
)

extra_method <- list(
  keyword = c('classes', 'environment', 'utilities', 'misc'),
  concept = c('evaluation mode', 'standard evaluation',
              'function return type evaluation', 'parameter check evaluation'),
  references = c(sentensize(paste('see',
                                  generateContent('wyz.code.offensiveProgramming', 'code'),
                                  'package documentation')),
                 '',
                 sentensize(paste('You may read',
                                  generateContent('https://neonira.github.io/offensiveProgrammingBook/',
                                                  'href', 'Offensive Programming Book'),
                                  'to get introduction and expert advices on offensive programming')),
                 '',
                 generateReference(refs[[1]])
  ),
  seealso = c(sentensize(paste('see',
                               generateContent(generateSpecialLink('wyz.code.offensiveProgramming',
                                                                   'runTransientFunction'),
                                               'code'),
                               'to call interactively an offensive programming function, whether instrumented or not.')),
              '',
              sentensize(paste('see',
                               generateContent(generateSpecialLink('wyz.code.offensiveProgramming',
                                                                   'runTestCase'),
                                               'code'),
                               'to reuse on-demand instrumented offensive programming function tests'))
  )
)

# explicit invocation for method
em <- generateDocumentationContent(target_folder, 'method', 'addMultiDouble',
                                   object, package_name,
                                   extra_method, overwrite_b_1 = TRUE)

# explicit invocation for class
extra_class <- extra_method
extra_class$references <- extra_method$references[[5]]
extra_class$seealso <- NULL
ec <- generateDocumentationContent(target_folder, 'class', 'AdditionTCFIG1',
                                   object, package_name,
                                   extra_class, overwrite_b_1 = TRUE)


# generic invocation for method
gm1 <- generateDocumentationContent(target_folder, 'method', NA,
                                    Addition(), package_name,
                                    extra_method, overwrite_b_1 = TRUE)


# generic invocation for method
gm2 <- generateDocumentationContent(target_folder, 'method', NA,
                                    AdditionTCPartial(), package_name,
                                    extra_method, overwrite_b_1 = TRUE)


# generic invocation for method
gm3 <- generateDocumentationContent(target_folder, 'method', NA,
                                    AdditionFIPartial(), package_name,
                                    extra_method, overwrite_b_1 = TRUE)

# generic invocation for method
gm4 <- generateDocumentationContent(target_folder, 'method', NA,
                                    object, package_name,
                                    extra_method, overwrite_b_1 = TRUE)

# specific invocation for package
extra_package <- extra_method
extra_package$seealso <- NULL
extra_package$content <- c('AdditionTCFIG1', 'AdditionTCFIP', 'Addition')
ep <- generateDocumentationContent(target_folder, 'package', package_name,
                                   object, package_name, extra_package, overwrite_b_1 = TRUE)


exp <- extra_package
exp$content <- NULL

test_that("generateDocumentationContent", {
  expect_true(file.exists(em$filename))
  expect_true(file.exists(ec$filename))
  expect_true(file.exists(ep$filename))

  sapply(seq_len(length(gm1)), function(k) expect_true(file.exists(gm1[[k]]$filename)))
  sapply(seq_len(length(gm2)), function(k) expect_true(file.exists(gm2[[k]]$filename)))
  sapply(seq_len(length(gm3)), function(k) expect_true(file.exists(gm3[[k]]$filename)))
  sapply(seq_len(length(gm4)), function(k) expect_true(file.exists(gm4[[k]]$filename)))
})

test_that("generateDocumentationContent - coverage", {
  # not existing target folder
  expect_error(generateDocumentationContent(file.path(target_folder, 'xxx'), 'method', 'x',
                                            AdditionTCPartial(), package_name,
                                            extra_method, overwrite_b_1 = TRUE))

  # not existing kind
  expect_error(generateDocumentationContent(target_folder, 'methods', 'x',
                                            AdditionTCPartial(), package_name,
                                            extra_method, overwrite_b_1 = TRUE))

  # no content for package - see exp variable above
  expect_error(generateDocumentationContent(target_folder, 'package', package_name,
                                            object, package_name, exp, overwrite_b_1 = TRUE))
})


# commented as it fails with unknown origin - unable to find given function???
sumValues <- function(x_i, y_i) sum(x_i, y_i, na.rm = TRUE)

gmf <- generateDocumentationContent(target_folder, 'method', 'sumValues',
                                    NULL, package_name,
                                    extra_method, overwrite_b_1 = TRUE)

test_that("generateDocumentationContent - standalone function", {
  expect_error(generateDocumentationContent(target_folder, 'method', NA,
                                            NULL, package_name,
                                            extra_method, overwrite_b_1 = TRUE))
  # unexisting function
  expect_error(generateDocumentationContent(target_folder, 'method', 'zorg',
                                            NULL, package_name,
                                            extra_method, overwrite_b_1 = TRUE))
})

#
