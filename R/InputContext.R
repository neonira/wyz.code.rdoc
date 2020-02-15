InputContext <- function(object_o_1,
                         methodName_s_1 = NA_character_,
                         packageName_s_1 = NA_character_,
                         dataFilename_s_1 = NA_character_) {
  self <- environment()
  class(self) <- append('InputContext', class(self))

  kinds <- c('package', 'class', 'method', 'function', 'data')

  kind <- if (is.na(dataFilename_s_1)) {
    if (is.null(object_o_1)) {
      ifelse(is.na(methodName_s_1), 1, 4)
    } else {
      ifelse(is.na(methodName_s_1), 2, 3)
    }
  } else 5

  data_name <- if (kind == 5) removeFilenameExtension(basename(dataFilename_s_1)) else NULL

  file_name <- NA_character_

  use_markers <- FALSE

  hack_description <- {
    value <- options('rdoc_hack')$rdoc_hack
    if (is.null(value)) FALSE else isTRUE(value)
  }

  setUseMarkers <- function(value_b_1) use_markers <<- value_b_1

  number_replacements <- 0L

  markerGenerator <- function() {
    function() {
      number_replacements <<- number_replacements + 1
      sprintf('XXX_%03d', number_replacements)
    }
  }

  generateConditionalMarkerGenerator <- function() {
    mg <- markerGenerator()
    function(force_b_1 = FALSE) { if (use_markers || force_b_1) mg() else NULL }
  }

  generateConditionalMarker <- generateConditionalMarkerGenerator()

  getKind <- function() kinds[kind]

  instrumentationLevel <- if (kind != 5) identifyOPInstrumentationLevel(object_o_1, methodName_s_1)

  class_kind <- if (is.null(object_o_1)) NA else getObjectClassKind(object_o_1)

  class_name <- if (is.null(object_o_1)) 'no-class' else getObjectClassNames(object_o_1)$classname

  beautifier <- beautify()

  retrieveStrategy <- function() {

    presence <- buildIdentityList(c("Won't", 'Could', 'Should', 'Must'))
    generation <- buildIdentityList(c('systematic', 'conditionned', 'none'))
    patch <- buildIdentityList(c('replace', 'add', 'none'))

    buildResult <- function(strategy_l) {
      dt <- data.table::rbindlist(strategy_l)
      data.table::setnames(dt, colnames(dt), c('section_name', 'section_presence', 'section_generation', 'patchable'))
      list(case = kinds[kind], strategy = dt)
    }

    if (kind == 5) {
      data_strategy <- list(
        list('name'          , presence$MUST  , generation$SYSTEMATIC  , patch$NONE   ),
        list('docType'       , presence$MUST  , generation$SYSTEMATIC  , patch$NONE   ),
        list('alias'         , presence$MUST  , generation$SYSTEMATIC  , patch$ADD    ),
        list('title'         , presence$MUST  , generation$SYSTEMATIC  , patch$REPLACE),
        list('description'   , presence$MUST  , generation$SYSTEMATIC  , patch$REPLACE),
        list('usage'         , presence$MUST  , generation$SYSTEMATIC  , patch$NONE   ),
        list('arguments'     , presence$COULD , generation$CONDITIONNED, patch$REPLACE),
        list('details'       , presence$SHOULD, generation$CONDITIONNED, patch$REPLACE),
        list('value'         , presence$COULD , generation$SYSTEMATIC  , patch$REPLACE),
        list('format'        , presence$MUST  , generation$NONE        , patch$ADD    ),
        list('source'        , presence$MUST  , generation$NONE        , patch$ADD    ),
        list('custom_section', presence$COULD , generation$NONE        , patch$ADD    ),
        list('references'    , presence$COULD , generation$NONE        , patch$ADD    ),
        list('author'        , presence$COULD , generation$CONDITIONNED, patch$REPLACE),
        list('note'          , presence$COULD , generation$NONE        , patch$ADD    ),
        list('seealso'       , presence$COULD , generation$NONE        , patch$ADD    ),
        list('examples'      , presence$COULD , generation$SYSTEMATIC , patch$REPLACE),
        list('keyword'       , presence$MUST  , generation$SYSTEMATIC  , patch$ADD    ),
        list('concept'       , presence$COULD , generation$NONE        , patch$ADD    ),
        list('encoding'      , presence$SHOULD, generation$CONDITIONNED, patch$REPLACE),
        list('synopsis'      , presence$COULD , generation$NONE        , patch$ADD    ),
        list('Rdversion'     , presence$COULD , generation$NONE        , patch$ADD    ),
        list('RdOpts'        , presence$COULD , generation$NONE        , patch$ADD    ),
        list('Sexpr'         , presence$COULD , generation$NONE        , patch$ADD    )
      )
      return(buildResult(data_strategy))
    }

    method_strategy <- list(
      list('name'          , presence$MUST  , generation$SYSTEMATIC  , patch$NONE   ),
      list('alias'         , presence$MUST  , generation$SYSTEMATIC  , patch$ADD    ),
      list('title'         , presence$MUST  , generation$SYSTEMATIC  , patch$REPLACE),
      list('description'   , presence$MUST  , generation$SYSTEMATIC  , patch$REPLACE),
      list('usage'         , presence$MUST  , generation$SYSTEMATIC  , patch$NONE   ),
      list('arguments'     , presence$COULD , generation$CONDITIONNED, patch$REPLACE),
      list('details'       , presence$SHOULD, generation$CONDITIONNED, patch$REPLACE),
      list('value'         , presence$MUST  , generation$SYSTEMATIC  , patch$REPLACE),
      list('custom_section', presence$COULD , generation$NONE        , patch$ADD    ),
      list('references'    , presence$COULD , generation$NONE        , patch$ADD    ),
      list('author'        , presence$COULD , generation$CONDITIONNED, patch$REPLACE),
      list('note'          , presence$COULD , generation$NONE        , patch$ADD    ),
      list('seealso'       , presence$COULD , generation$NONE        , patch$ADD    ),
      list('examples'      , presence$MUST  , generation$SYSTEMATIC  , patch$REPLACE),
      list('keyword'       , presence$COULD , generation$NONE        , patch$ADD    ),
      list('concept'       , presence$COULD , generation$NONE        , patch$ADD    ),
      list('encoding'      , presence$SHOULD, generation$CONDITIONNED, patch$REPLACE),
      list('synopsis'      , presence$COULD , generation$NONE        , patch$ADD    ),
      list('Rdversion'     , presence$COULD , generation$NONE        , patch$ADD    ),
      list('RdOpts'        , presence$COULD , generation$NONE        , patch$ADD    ),
      list('Sexpr'         , presence$COULD , generation$NONE        , patch$ADD    )
    )

    if (kind %in% c(3, 4)) return(buildResult(method_strategy))

    class_strategy <- append(method_strategy,
                             list(list('docType', presence$MUST, generation$SYSTEMATIC,
                                       patch$NONE)),
                             1L)
    if (kind == 2) return(buildResult(class_strategy))

    package_strategy <- class_strategy

    findIndex <- function(seekFor_s_1) {
      nex <- unlist(lapply(seq_len(length(package_strategy)), function(k) {
        if (package_strategy[[k]][[1]] == seekFor_s_1) k else 0
      }))
      nex[nex != 0][1]
    }

    package_strategy[[findIndex('arguments')]] <- list('arguments', presence$COULD,
                                                       generation$NONE, patch$ADD)
    package_strategy[[findIndex('value')]] <- list('value', presence$COULD,
                                                   generation$NONE, patch$ADD)
    package_strategy[[findIndex('examples')]] <- list('examples', presence$COULD,
                                                      generation$NONE, patch$ADD)

    buildResult(package_strategy)
  }

  buildMethodName <- function() {
    switch(class_kind,
           'S3' = paste0(methodName_s_1, '.', class_name),
           'S4' = paste0(class_name, '@', methodName_s_1),
           paste0(class_name, '$', methodName_s_1)
    )
  }

  getName <- function() {
    switch(kind,
           packageName_s_1,
           class_name,
           methodName_s_1,
           methodName_s_1,
           data_name
    )
  }

  produceName <- function() {
    switch(kind,
           paste0(packageName_s_1, '-package'),
           paste0(class_name, '-class'),
           paste0(class_name, '-', methodName_s_1),
           methodName_s_1,
           data_name
    )
  }

  getFilename <- function() {
    if (is.na(file_name))
      file_name <<- normalizeFilename(produceName())

    file_name
  }

  produceDocType <- function() {
    switch(kind, 'package', 'class', NULL, NULL, 'data')
  }

  produceAlias <- function() {
    switch(kind,
           c(paste0(packageName_s_1, '-package'), packageName_s_1),
           c(paste0(class_name, '-class'), class_name),
           c(buildMethodName(), methodName_s_1),
           methodName_s_1,
           data_name
    )
  }

  produceTitle <- function() {
    switch(kind,
           generateMarkup(packageName_s_1, 'packageTitle', inline_b_1 = TRUE),
           paste('Class', class_name),
           paste0('Method ', buildMethodName()),
           paste('Function', methodName_s_1),
           paste('Data set', data_name)
    )
  }

  produceDescription <- function() {
    switch(kind,
           generateMarkup(packageName_s_1, 'packageDescription', inline_b_1 = TRUE),
           sentensize(paste(class_kind, 'class', class_name)),
           if (hack_description) {
             sentensize(paste('use this method to', generateLabel(methodName_s_1)))
           } else {
             if (!instrumentationLevel$offensive_programming) generateConditionalMarker() else {
               sentensize(paste('use this method to', generateLabel(methodName_s_1)))
             }
           },
           if (hack_description || instrumentationLevel$semantic_naming) {
             sentensize(paste('use this function to', generateLabel(methodName_s_1)))
           } else generateConditionalMarker()
    )
  }

  produceUsage <- function() {
    switch(kind,
           paste0('library(', packageName_s_1, ')'),
           getObjectSignature(object_o_1),
           getObjectMethodSignature(object_o_1, methodName_s_1),
           getFunctionSignature(methodName_s_1),
           data_name
    )
  }

  produceArguments <- function() {

    transformEmptyness <- function(x_s) {
      if (is.null(x_s) || length(x_s) == 0) return(NULL)
      x_s
    }

    args <- switch(kind,
                   NULL,
                   {
                     fn <- getObjectConstructor(object_o_1)
                     if (!is.function(fn$function_f)) NULL else retrieveFunctionArgumentNames(fn$function_f)
                     #fn <- tryCatch(get(class_name, mode = 'function'), error = NULL)
                     #if (!is.function(fn)) NULL else retrieveFunctionArgumentNames(fn)
                     # NULL
                   },
                   transformEmptyness(getObjectFunctionArgumentNames(object_o_1)[[methodName_s_1]]),
                   transformEmptyness(retrieveFunctionArgumentNames(methodName_s_1)),
                   NULL
    )

    if (is.null(args)) return(NULL)

    sapply(args, function(e) {
      fpn <- FunctionParameterName(e)
      generateMarkup(e ,'item',
                     if (fpn$isSemanticName())
                       retrieveFactory()$getTypeDescription(fpn) else
                         generateConditionalMarker(TRUE),
                     inline_b_1 = FALSE
      )
    }, USE.NAMES = FALSE)
  }

  produceValue <- function() {
    switch(kind,
           NULL,
           sentensize(paste('an object instance of class', beautifier$code(class_name))),
           # ifelse(instrumentationLevel$offensive_programming,
           #        'OP instrumentation for method ... TBD XXX',
           #        generateConditionalMarker()),
           # ifelse(instrumentationLevel$offensive_programming,
           #        'OP instrumentation for function... TBD XXX',
           #        generateConditionalMarker()),
           generateConditionalMarker(),
           generateConditionalMarker(),
           NULL
    )
  }

  produceDetails <- function() {
    switch(kind,
           c('The DESCRIPTION file',
             generateMarkup(packageName_s_1, 'packageDESCRIPTION', inline_b_1 = FALSE),
             generateMarkup(packageName_s_1, 'packageIndices', inline_b_1 = FALSE)
           ),
           generateConditionalMarker(),
           generateConditionalMarker(),
           generateConditionalMarker()
    )
  }

  produceAuthor <- function() {
    p <- if (!is.na(packageName_s_1)) {
      c(generateMarkup(packageName_s_1, 'packageAuthor'),
        '',
        paste('Maintainer:', generateMarkup(packageName_s_1, 'packageMaintainer'))
      )
    } else generateConditionalMarker()
  }

  produceKeyword <- function() {
    switch(kind, 'package', 'class', 'method', 'function', 'datasets')
  }

  produceCustom_section <- function() {

    produceSubsections <- function(data_l) {
      unlist(lapply(seq_len(length(data_l)), function(k) {
        generateMarkup(names(data_l)[k], 'subsection', data_l[[k]], inline_b_1 = FALSE)
      }))
    }

    if (kind %in% c(2, 3)) {
      op <- extractObjectOPInformation(object_o_1,
                                       ifelse(kind == 2, NA_character_, methodName_s_1))
      l <- switch(getObjectClassKind(object_o_1),
                  'R6' = extractR6ObjectInformation(object_o_1),
                  'S4' = extractS4ObjectInformation(object_o_1),
                  'S3' = extractS3ObjectInformation(object_o_1),
                  'RC' = extractRCObjectInformation(object_o_1),
                  'environment' = extractEnvObjectInformation(object_o_1),
                  NULL
      )
      return(c(produceSubsections(l), produceSubsections(op)))
    }

    NULL
  }

  produceFormat <- function() {
    if (kind == 5) {
      d <- dim(object_o_1)
      dd <- paste0('dimension', ifelse(length(d) < 2, '', 's'))
      sentensize(paste(class_name, 'of', dd, paste0(d, collapse = 'x')))
    } else {
      NULL
    }
  }

  produceSource <- function() {
    if (kind == 5) {
      generateConditionalMarker()
    } else  {
      NULL
    }
  }

  produceReferences <- produceNote <- produceSeealso <- produceExamples <-
    produceConcept <- produceRdversion <- produceSynopsis <- produceSexpr <-
    produceRdOpts <- function() generateConditionalMarker()

  produceEncoding <- function() 'UTF-8'

  self
}
