ManualPageBuilder <- function(inputContext_o_1,
                              processingContext_o_1 = ProcessingContext(),
                              generationContext_o_1 = GenerationContext()) {

  self <- environment()
  class(self) <- append('ManualPageBuilder', class(self))

  strategy <- inputContext_o_1$retrieveStrategy()

  colorizer <- Colorizer()

  getStrategy <- function() strategy

  assembleManualPage <- function(pieces_l) {
    pieces <- pieces_l

    extraneous <- processingContext_o_1$extraneous_l
    post_processing <- processingContext_o_1$postProcessing_l

    special_sections <- c('name', 'alias', 'usage')
    standard_sections <- setdiff(strategy$strategy$section_name, 'custom_section')
    modifiable_sections <- setdiff(standard_sections, special_sections)
    multi_sections <- c('keyword', 'concept')
    patched <- modified <- vector('character', 128L)
    pi <- mi <- 1L

    # manage standard sections creations and replacements - MONO
    ss <- intersect(names(extraneous),
                    setdiff(modifiable_sections, multi_sections))
    sapply(ss, function(e) {
      if (generationContext_o_1$verbosity_b_1) cat('standard section mono', e, '\n')
      pieces[[e]] <<- extraneous[[e]]
      modified[mi] <<- e
      mi <<- mi + 1L
    })

    # manage standard sections creations and replacements - MULTI
    ms <- intersect(names(extraneous), multi_sections)
    sapply(ms, function(e) {
      if (generationContext_o_1$verbosity_b_1) cat('standard section multi', e, '\n')
      pieces[[e]] <<- extraneous[[e]]
      modified[mi] <<- e
      mi <<- mi + 1L
    })

    # manage custom sections creations
    cs <- setdiff(names(extraneous), modifiable_sections)
    sapply(cs, function(e) {
      if (generationContext_o_1$verbosity_b_1) cat('custom section', e, '\n')
      pieces[[e]] <<- extraneous[[e]]
      modified[mi] <<- e
      mi <<- mi + 1L
    })

    # patch content
    patchable <- setdiff(names(pieces), special_sections)
    senv <- {
      value <- options('rdoc_classification')$rdoc_classification # must hold the package name
      if (is.null(value)) '' else if (is.character(value)) value else ''
    }
    if (senv != '') {
      ns <- tryCatch(getNamespace(senv), error = function(e) {NA}) #
      if (is.environment(ns)) {
        value_fun <- options('rdoc_function')$rdoc_function # must hold the inventory function name e.g. opRdocInformation
        value_fun <- if (is.null(value_fun)) '' else if (is.character(value_fun)) value else ''
        if (value_fun %in% getNamespaceExports(ns)) {
          m <- 'classification'
          if (!m %in% patchable) {
            km <- inputContext_o_1$getName()
            if (generationContext_o_1$verbosity_b_1) cat(m, km, '\n')
            pieces[[m]] <- extractClassificationInformation(km, senv)
            modified[mi] <- m
            mi <- mi + 1L
          }
        }
      }
    }

    patchable <- setdiff(names(pieces), special_sections)
    sapply(patchable, function(e) {
      if (exists(e, post_processing)) {
        if (generationContext_o_1$verbosity_b_1) cat('patch', e, '\n')
        pieces[[e]] <<- post_processing[[e]](pieces[[e]])
        patched[pi] <<- e
        pi <<- pi + 1L
      }
    })

    nm <- names(pieces)
    sl <- lapply(seq_len(length(pieces)), function(k) {
      if (is.null(pieces[[k]])) NULL else generateSection(nm[k], pieces[[k]])
    })

    list(pieces = Filter(Negate(is.null), sl),
         modifications = if (mi > 1L) modified[1:(mi - 1L)] else NA_character_,
         patches = if (pi > 1L) patched[1:(pi - 1L)] else NA_character_)
  }

  documentContent <- function() {
    sections <- setdiff(getStandardSectionNames(), strategy$section_name)
    modified <- vector('logical', length(sections))
    pieces <- lapply(seq_len(length(sections)), function(k) {
      function_name <- paste0('produce', capitalize(sections[[k]]))
      fn <- inputContext_o_1[[function_name]]
      b <- is.function(fn)
      #cat(sections[[k]], 'function', function_name, 'does exist\n')
      rv <- if (b) fn() else NULL
      modified[k] <<- !is.null(rv)
      rv
    })
    names(pieces) <- sections
    w <- which(names(pieces) == "custom_section")
    if (length(w) > 0) names(pieces)[w] <- 'Information'
    list(pieces = pieces, modifications = sections[modified])
  }

  interpretResults <- function(result_l) {
    simplePluralize <- function(x_s, flag_b_1, suffix_s_1 = ':') {
      paste0(x_s, ifelse(flag_b_1, 's', ''), suffix_s_1)
    }

    if (!is.list(result_l)) abort('result_l must be a list')
    if (length(setdiff(c('logic', 'context'), names(result_l))) > 0)
      abort('result_l does not seem to be of right format')
    if (length(setdiff(c('filename', 'overwritten'), names(result_l$context))) > 0)
      abort('result_l does not seem to be of right format')


    generated <- extra_generated <- section_presence <- patched <- NULL # nse

    cat('filename is', result_l$context$filename,
        strBracket(ifelse(result_l$context$overwritten, 'OVERWRITTEN', 'UNTOUCHED')),
        '\n')

    gen <- result_l$logic[generated == TRUE | extra_generated == TRUE]$section_name
    lgen <- length(gen)
    if (lgen > 0) cat('generated', lgen, simplePluralize('section', lgen > 1),
                      paste(colorizer$info(gen), collapse = ', '), '\n')

    pat <- result_l$logic[patched == TRUE]$section_name
    lpat <- length(pat)
    if (lpat > 0) cat('patched', lpat, simplePluralize('section', lpat > 1),
                      paste(colorizer$info(pat), collapse = ', '), '\n')

    wrong <- result_l$logic[section_presence == 'MUST' &
                              !(generated == TRUE | extra_generated == TRUE | patched == TRUE)]$section_name
    lwrong <- length(wrong)
    if (lwrong > 0) cat('missing', lwrong, simplePluralize('section', lwrong > 1),
                        paste(colorizer$error(wrong), collapse = ', '), '\n')

    advice <- result_l$logic[section_presence == 'SHOULD' &
                               !(generated == TRUE | extra_generated == TRUE | patched == TRUE)]$section_name
    ladvice <- length(advice)
    if (ladvice > 0) cat('probably missing', ladvice, simplePluralize('section', ladvice > 1),
                         paste(colorizer$other(advice), collapse = ', '), '\n')

    if (inputContext_o_1$number_replacements > 0L)
      cat('replacements to manage:', colorizer$error(inputContext_o_1$number_replacements), '\n')

  }

  buildManualPage <- function() {
    if (generationContext_o_1$verbosity_b_1)
      cat("Creating manual page for", inputContext_o_1$getKind(),
          inputContext_o_1$getFilename(),'\n')
    inputContext_o_1$setUseMarkers(generationContext_o_1$useMarkers_b_1)
    pieces <- documentContent()
    content <- assembleManualPage(pieces$pieces)
    fb <- produceDocumentationFile(inputContext_o_1$getFilename(),
                                   paste(unlist(content$pieces), collapse = '\n'),
                                   generationContext_o_1)

    section_name <- section_presence <- extra_generated <- generated <- patched <- NULL # nse
    generation <- copy(strategy$strategy)
    generation[, `:=`(section_generation = NULL, patchable = NULL)]
    data.table::set(generation, NULL, 'generated', FALSE)
    data.table::set(generation, NULL, 'extra_generated', FALSE)
    data.table::set(generation, NULL, 'patched', FALSE)
    generation[section_name %in% pieces$modifications, `:=`(generated = TRUE)]
    generation[section_name %in% content$modifications, `:=`(extra_generated = TRUE)]
    generation[section_name %in% content$patches, `:=`(patched = TRUE)]
    generation[, `:=`(missing = (section_presence == 'MUST' & generated == FALSE)) ]

    invisible(list(logic = generation, context = fb))
  }

  self
}
