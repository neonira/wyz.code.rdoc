library("data.table")
library("wyz.code.offensiveProgramming")
library("wyz.code.rdoc")

source_package_name <- target_package_name <- 'wyz.code.rdoc'

pc <- ProcessingContext(postProcessing_l = list(
    title = function(content_s) { 'a patched title'},
    concept = function(content_s) { 'zorg'},
    details = function(content_s) { 'details' }
  )
)


file.copy('inst/man-generated/append.Rd', 'inst/man-generated/append-1.Rd')
rv_add <- wyz.code.rdoc:::completeManualPageBis('inst/man-generated/append-1.Rd', pc)

file.copy('inst/man-generated/append.Rd', 'inst/man-generated/append-2.Rd')
rv_patch <- wyz.code.rdoc:::completeManualPageBis('inst/man-generated/append-2.Rd', pc, FALSE)

