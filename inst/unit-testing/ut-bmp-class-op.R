library("data.table")
library("wyz.code.offensiveProgramming")
library("wyz.code.rdoc")

gc <- GenerationContext('inst/man-generated', overwrite_b_1 = TRUE,
                        verbosity_b_1 = FALSE, useMarkers_b_1 = FALSE)

target_package_name <- 'wyz.code.rdoc'

# beautifier <- beautify()
#
# pc <- ProcessingContext(
#   extraneous_l = list(
#     seealso = sentensize(paste('class', beautifier$codelink('ProcessingContext'),
#                                'and class',  beautifier$codelink('InputContext'))),
#     keyword = 'documentation',
#     concept = 'documentation generation'
#   )
# )

source(findFilesInPackage('AdditionTCFIP.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(AdditionTCFIP(), packageName_s_1 = target_package_name)
r <- produceManualPage(ic, gene = gc)
interpretResults(r)

source(findFilesInPackage('Addition_TCFI_Partial_R6.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(Addition_TCFI_Partial_R6$new(), packageName_s_1 = target_package_name)
r <- produceManualPage(ic, gene = gc)
interpretResults(r)

source(findFilesInPackage('Addition_TCFI_Partial_S3.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(Addition_TCFI_Partial_S3(), packageName_s_1 = target_package_name)
r <- produceManualPage(ic, gene = gc)
interpretResults(r)

source(findFilesInPackage('Addition_TCFI_Partial_S4.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(new('Addition_TCFI_Partial_S4'), packageName_s_1 = target_package_name)
r <- produceManualPage(ic, gene = gc)
interpretResults(r)

source(findFilesInPackage('Addition_TCFI_Partial_RC.R', 'wyz.code.offensiveProgramming')[1])
ic <- InputContext(new('Addition_TCFI_Partial_RC'), packageName_s_1 = target_package_name)
r <- produceManualPage(ic, gene = gc)
interpretResults(r)


