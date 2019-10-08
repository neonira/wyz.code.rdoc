generateDocumentationContent <- function(targetFolder_s_1,
                                         kind_s_1,
                                         name_s_1,
                                         object_o_1,
                                         packageName_s_1,
                                         extraneous_l = list(),
                                         typeFactory_o_1 = FunctionParameterTypeFactory(),
                                         overwrite_b_1 = FALSE) {

  generateStandaloneMethodSignature <- function(methodName_s_1, argumentNames_s) {
    args <- if (length(argumentNames_s) == 0) '' else paste(argumentNames_s, collapse = ', ')
    paste0(methodName_s_1, '(', args, ')')
  }

  if (!dir.exists(targetFolder_s_1))
    abort('targetFolder_s_1 must be an existing directory.')

  ak <- c('package', 'class', 'method')
  if (!kind_s_1 %in% ak)
    abort('kind_s_1 must be chosen amongst', strBracket(paste(ak, collapse = ',')))

  no_name <- is.na(name_s_1)
  no_obj <-  !is.object(object_o_1)
  if (no_name && no_obj)
    abort('provide name_s_1 or/and object_o_1 - cannot work when both are missing')

  if (no_obj) {
    FunctionWrapperEnv <- function() {
      self <- environment()
      class(self) <- append('FunctionWrapperEnv', class(self))
      self
    }
    object_o_1 <- FunctionWrapperEnv()
    f <- tryCatch(get(name_s_1, mode = 'function', envir = parent.frame()), error = function(e) e)
    if (!is.function(f)) abort('unable to find function', name_s_1)
    object_o_1[[name_s_1]] <- f
  }

  if (no_name) {
    gem <- sapply(wyz.code.offensiveProgramming::getObjectFunctionNames(object_o_1),
                  function(e) {
      generateDocumentationContent(targetFolder_s_1, 'method', e, object_o_1,
                                   packageName_s_1, extraneous_l, typeFactory_o_1,
                                   overwrite_b_1)
    }, simplify = FALSE)
    return(gem)
  }

  cn <- setdiff(class(object_o_1), c('environment', 'R6'))[1]
  nam <- ifelse(kind_s_1 == ak[1], paste0(packageName_s_1, '-', ak[1]),
                ifelse(kind_s_1 == ak[2], cn,
                       ifelse(no_obj, name_s_1, paste0(name_s_1, '.', cn))))
  fn <- file.path(targetFolder_s_1, nam)

  rdk <- rdocKeywords(TRUE)
  key_pieces <- union(union(rdk$documentingFunctions, rdk$indices), rdk$other)

  funs <- lapply(key_pieces, function(e) {
    function(content_s) generateSection(e, content_s)
  })
  names(funs) <- key_pieces

  dt <- data.table(
    section = c('name', 'alias', 'docType', 'title', 'description', 'usage',
                'arguments',
                'details', 'value',
                'references', 'author', 'note', 'seealso',
                'examples',
                'keyword', 'concept'),
    package = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                FALSE,
                FALSE, FALSE,
                FALSE, TRUE, FALSE, FALSE,
                FALSE,
                FALSE, FALSE),
    class = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
              TRUE,
              FALSE, TRUE,
              FALSE, TRUE, FALSE, FALSE,
              FALSE,
              FALSE, FALSE),
    method = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
               TRUE,
               TRUE, TRUE,
               FALSE, TRUE, FALSE, FALSE,
               TRUE,
               FALSE, FALSE)
  )

  gencode <- function(content_s_1) generateContent(content_s_1, 'code')
  anunlist <- function(list_l) unlist(list_l, use.names = FALSE)
  asparagraph <- function(v_s) paste(v_s, collapse = '\n')

  genarg <- function(argname_s_1) {
    generateContent(argname_s_1, # do not use code here for argname as it
                    # brings R cmd issues on S3 classes
                    'item',
                    typeFactory_o_1$getTypeDescription(FunctionParameterName(argname_s_1))
    )
  }

  genvalue <- function(frt_s_1) {
    if (kind_s_1 == 'class') return(sentensize('an object of class', cn))
    if (length(frt_s_1) == 0) return(sentensize("function return type not instrumented"))
    typeFactory_o_1$getTypeDescription(FunctionParameterName(frt_s_1))
  }

  gendetails <- function(nums_i) {
    l <- length(nums_i)
    if (l == 0) return(sentensize('no test provided for this function'))
    s <- sentensize('see examples below for immediate reuse')
    if (l == 1) return(paste(sentensize(paste('Offensive programming instrumentation provides',
                                              'a single test for this function.')),
                             sentensize(paste('it has identification number', nums_i[1])),
                             s))
    paste(sentensize(paste("Offensive programming instrumentation provides", l,
                           'tests for this function.')),
          sentensize(paste('their identification number ranges from', nums_i[1],
                           'to', nums_i[l])),
          s)
  }

  genexample <- function(nums_i, objectSignature_s_1) {
    ob <- paste('ob <-', objectSignature_s_1)
    if (kind_s_1 == 'class') return(generateContent(ob, 'dontrun', inline_b_1 = FALSE))

    l <- length(nums_i)
    if (l == 0) return(sentensize('# no test provided for this function implies no example'))
    n <- sample(nums_i, 1)
    li <- 'library("wyz.code.offensiveProgramming")'
    se <- 'se <- EvaluationMode(defineEvaluationMode()[1])'
    tc <- 'tc <- EvaluationMode(defineEvaluationMode()[3])'
    ser <- paste0('ser <- runTestCase(ob, ', n, ', se)')
    tcr <- paste0('tcr <- runTestCase(ob, ', n, ', tc)')
    cm1 <- paste('# standard_R_evaluation mode, here based on sampled test case number', n)
    cm2 <- paste('# type_checking_enforcement mode, here based on sampled test case number', n)
    generateContent(asparagraph(c(li, ob, '', cm1, se, ser, '', cm2, tc, tcr)), 'dontrun', inline_b_1 = FALSE)
  }

  loop <- function(v_, function_f, ...) {
    sapply(v_, function(e) {
      function_f(e, ...)
    }, simplify = FALSE)
  }

  genpkgdetails <- function() {
    v <- loop(extraneous_l$content, function(e) {
      x <- generateContent(generateContent(e, 'link'), 'code')
      generateContent(x, 'item', '')
    })
    paste(
      c('', 'Most important package entries are',
        generateContent(paste(c('', v, ''), collapse = '\n'), 'itemize'),
        ''),
      collapse = '\n'
    )
  }

  dx <- dt[, c('section', kind_s_1), with = FALSE]
  pkgn <- paste('package', gencode(packageName_s_1))
  desc <- ifelse(kind_s_1 == ak[1], pkgn,
                 ifelse(kind_s_1 == ak[2],
                        paste('class', gencode(cn), 'from', pkgn),
                        paste('use method', gencode(name_s_1), 'to', generateLabel(name_s_1))))

  frt <- copy(retrieveFunctionReturnTypes(object_o_1))
  rtc <- copy(retrieveTestCaseDefinitions(object_o_1))
  function_name <- NULL # data.table NSE issue with Rcmd check

  os <- if (!no_obj) getObjectSignature(object_o_1) else '()'
  pieces <- list(
    name = funs$name(nam),
    alias = funs$alias(nam),
    author = funs$author(c(generateContent(packageName_s_1, 'packageAuthor'),
                           '',
                           paste('Maintainer:', generateContent(packageName_s_1, 'packageMaintainer'))))

  )

  if (kind_s_1 == 'package') {
    if (!'content' %in% names(extraneous_l)) abort('need a content name in extraneous_l for package')
    pieces$alias <- c(pieces$alias, funs$alias(packageName_s_1))
    pieces$docType <- generateContent('package', 'docType')
    pieces$title <-  funs$title(generateContent(packageName_s_1, 'packageTitle'))
    pieces$description <- funs$description(generateContent(packageName_s_1, 'packageDescription'))
    pieces$details <- generateContent(genpkgdetails(), 'details')
  } else {
    pieces$title <-  funs$title(generateLabel(name_s_1))
    pieces$description <- funs$description(sentensize(desc))
    pieces$value <- if (is.data.table(frt)) {
      funs$value(genvalue(frt[function_name == name_s_1]$return_value))
    } else funs$value('No function return type instrumentation.')
    if (is.data.table(rtc)) {
      rtc[, `:=`(k = .I)]
      testnums <- rtc[function_name == name_s_1]$k
    } else testnums <- NA
    if (kind_s_1 == 'method') {
      ofa <- wyz.code.offensiveProgramming::getObjectFunctionArgumentNames(object_o_1)
      pd <- if (length(ofa[[name_s_1]]) != 0) {
        asparagraph(anunlist(loop(ofa[[name_s_1]], genarg)))
      } else NA
      pu <- if (!no_obj) generateS3MethodSignature(name_s_1, cn, ofa[[name_s_1]]) else {
        generateStandaloneMethodSignature(name_s_1, ofa[[name_s_1]])
      }
      pieces$usage <- funs$usage(pu)
      if (!is.na(pd)) pieces$arguments <- funs$arguments(pd)
      if (!is.na(testnums[1])) {
        pieces$details <- funs$details(gendetails(testnums))
        pieces$examples <- if (!no_obj) funs$examples(genexample(testnums, os)) else {
          funs$examples('# no tests instrumentation')
        }
      } else {
        pieces$examples <- funs$examples('# no tests instrumentation')
      }
    }

    if (kind_s_1 == 'class') {
      fa <- retrieveFunctionArguments(name_s_1)
      pd <- if (!is.null(fa)) asparagraph(anunlist(loop(fa, genarg))) else NA
      pieces$usage <- funs$usage(os)
      if (!is.na(pd)) pieces$arguments <- funs$arguments(pd)
      ex <- if (!is.na(testnums[1])) genexample(testnums, os) else '# no tests instrumentation'
      pieces$examples <- funs$examples(ex)
    }
  }

  extrap <- sapply(seq_len(length(extraneous_l)), function(k) {
    nm <- names(extraneous_l)[k]
    if (nm %in% c('keyword', 'concept')) { # iteration required on keywords
      return(anunlist(loop(extraneous_l[[k]], generateContent, nm)))
    }
    if (nm != 'content')  {
      ex <- loop(extraneous_l[[k]], sentensize) # iteration required on content
      funs[[nm]](asparagraph(anunlist(ex)))
    }
  }, simplify = FALSE)
  names(extrap) <- names(extraneous_l)

  # allow override by extraneous_l
  mp <- intersect(dx$section, setdiff(names(pieces), names(extraneous_l)))
  me <- intersect(dx$section, names(extrap))
  wh <- c(pieces[mp], extrap[me])
  w <- intersect(dx$section, names(wh)) # to get it in order

  rv <- generateDocumentationFile(fn, paste(unlist(wh[w]), collapse = '\n'),
                                  overwrite_b_1 = overwrite_b_1, verbose_b_1 = FALSE)

  #list(dx = dx, dt = dt, pieces = pieces, extra = extrap, filename = rv)
  invisible(rv)
}
