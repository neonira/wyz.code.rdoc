extractObjectOPInformation <- function(object_o_1,
                                       methodName_s_1 = NA_character_) {

  vos <- verifyObjectNames(object_o_1)
  class_name <- getObjectClassNames(object_o_1)$classname
  no_method <- is.na(methodName_s_1)
  beautifier <- beautify()
  testnums <- NA_integer_
  function_name <- NULL #nse

  symbol <- documentationSymbols()$black_right_triangle

  getSemanticNaming <- function() {
    c(
      if (no_method) {
        sentensize(paste('class name compliance is', beautifier$code(vos$class_name_compliance)))
      } else {
        w <- which(names(vos$function_name_compliance) == methodName_s_1)
        if (length(w) == 0) abort('unable to find function', methodName_s_1, 'from object of class', class_name)
        generateParagraph2NL(c(
          sentensize(
            paste('function name compliance is', beautifier$code(vos$function_name_compliance[w]))
          ),
          sentensize(
            paste('function parameter-names compliance is',
                  beautifier$code(vos$parameter_name_compliance[function_name == methodName_s_1]$name_compliance_check)
            )
          ),
          sentensize(
            paste('function parameter-names semantic naming compliance is',
                  beautifier$code(vos$parameter_name_compliance[function_name == methodName_s_1]$semantic_naming_check)
            )
          )
        )
        )
      }
    )
  }

  getFunctionReturnTypes <- function() {
    frt <- retrieveFunctionReturnTypes(object_o_1)
    if (no_method) {
      c(
        sentensize(
          paste('class owns',
                if (vos$owns_function_return_type_information) {
                  ifelse(vos$is_function_fully_instrumented, 'full', 'partial')
                } else {
                  'no'
                },
                'function return type instrumentation')
        ),
        if (data.table::is.data.table(frt)) {
          dk <- copy(frt)
          data.table::set(dk, NULL, 'function_name', beautifier$bold(dk$function_name))
          data.table::setnames(dk, colnames(dk), beautifier$code(colnames(dk)))
          generateTable(dk, 'rr')
        } else NULL
      )
    } else {
      paste(symbol,
            sentensize(
              paste('function return type is',
                    if (data.table::is.data.table(frt)) {
                      beautifier$bold(
                        beautifier$code(frt[function_name == methodName_s_1]$return_value)
                      )
                    } else {
                      'not instrumented'
                    }
              )
            )
      )
    }
  }

  getTestCaseDefinitions <- function() {
    rtc <- retrieveTestCaseDefinitions(object_o_1)
    if (is.data.table(rtc)) testnums <<- seq_len(nrow(rtc))
    function_name <- NULL # nse

    if (no_method) {
      c(
        sentensize(
          paste('class owns',
                if (vos$owns_test_case_definitions) {
                  ifelse(vos$is_test_case_fully_instrumented, 'full', 'partial')
                } else {
                  'no'
                },
                'test case definitions')
        ),
        if (data.table::is.data.table(rtc)) {
          dk <- rtc[, list(recorded_tests = .N), by = c('function_name')]
          data.table::set(dk, NULL, 'function_name', beautifier$bold(dk$function_name))
          data.table::setnames(dk, colnames(dk), beautifier$code(colnames(dk)))
          generateTable(dk, 'rr')
        } else NULL
      )
    } else {
      paste(symbol,
            sentensize(
              paste('Test case definition',
                    if (data.table::is.data.table(rtc)) {
                      paste('brings', beautifier$bold(
                        beautifier$code(nrow(rtc[function_name == methodName_s_1]))
                      ), 'cases'
                      )
                    } else {
                      'is not instrumented'
                    }
              )
            )
      )
    }
  }

  getExamples <- function() {
    vn <- tolower(
      paste(unlist(regmatches(class_name, gregexpr('[A-Z]+', class_name))),
            collapse = '')
    )
    ob <- paste(vn, '<-', getObjectSignature(object_o_1))

    if (is.na(testnums[1])) return(ob)
    l <- length(testnums)
    if (l == 0) return(ob)

    # en <- environmentName(environment(tryCatch(get(class_name, mode = 'function'),
    #                                            error = function(e) 'XXX_PKG' )))
    n <- sample(testnums, 1)
    p <- c( 'library("data.table")',
            'library("wyz.code.offensiveProgramming")',
            '',
            # paste0('source(findFilesInPackage("', class_name, '", "',
            #        en, '")[1])'),
            # '',
            ob,
            paste('tcnum <-', n, paste0('# [1,', max(testnums), ']')),
            '',
            '# standard_R_evaluation mode',
            'se <- EvaluationMode(defineEvaluationMode()[1])',
            paste0('ser <- runTestCase(', vn, ', tcnum , se)'),
            '',
            '# type_checking_enforcement mode',
            'tc <- EvaluationMode(defineEvaluationMode()[3])',
            paste0('tcr <- runTestCase(', vn, ', tcnum, tc)')
    )
    beautifier$preformatted(generateParagraph(p))
  }

  l <- list(
    'offensive programming - semantic naming' = getSemanticNaming(),
    'offensive programming - function return types' = getFunctionReturnTypes(),
    'offensive programming - test case definitions' = getTestCaseDefinitions(),
    'offensive programming - examples' = getExamples()
  )

  Filter(Negate(is.null), l)
}
