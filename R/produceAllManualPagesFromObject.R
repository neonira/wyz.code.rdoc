produceAllManualPagesFromObject <-
  function(object_o_1,
           processingContext_o_1 = ProcessingContext(),
           generationContext_o_1 = GenerationContext(),
           packageName_s_1 = NA_character_) {
    ic <- InputContext(object_o_1, packageName_s_1 = packageName_s_1)
    if (ic$kind != 2)
      abort('object_o_1 does not seem to be a class object')

    cl <- produceManualPage(ic,
                            processingContext_o_1,
                            generationContext_o_1)

    l <- lapply(getObjectFunctionNames(object_o_1), function(fn) {
      ic <- InputContext(object_o_1, fn, packageName_s_1 = packageName_s_1)
      rv <- produceManualPage(ic, processingContext_o_1, generationContext_o_1)
      if (generationContext_o_1$verbosity_b_1)
        cat('produced file', rv$context$filename , '\n')
      rv
    })

    list(class = cl, methods = l)
  }
