opRdocInformation <- function() {

  stratum <- buildIdentityList(c('core', paste0('layer_', 1:3)))
  phasing <- buildIdentityList(c('design', 'build', 'test', 'run', 'maintain', 'evolve', 'transversal'))
  intent <- buildIdentityList(c('parts_building', 'parts_assembly', 'quality_control', 'statistics', 'feedback',
                                'content_generation', 'utilities'))
  category <- buildIdentityList(c('function', 'class', 'data'))
  nature <- buildIdentityList(c('exported', 'internal'))

  buildList <- function(name_s_1, category_s_1, nature_s_1,
                        stratum_s_1, phasing_s_1, intent_s_1) {
    list(name = name_s_1, category = category_s_1,
         nature = nature_s_1, stratum = stratum_s_1, phasing = phasing_s_1,
         intent = intent_s_1
    )
  }

  dt <- data.table::rbindlist(list(
    buildList("beautify", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_2, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("Colorizer", category$CLASS, nature$INTERNAL,
              stratum$LAYER_1, phasing$TRANSVERSAL, intent$FEEDBACK),
    buildList("computeDocumentationStatistics", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$STATISTICS),
    buildList("convertExamples", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$PARTS_ASSEMBLY),
    buildList("extractClassificationInformation", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("extractEnvObjectInformation", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("extractObjectOPInformation", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("extractR6ObjectInformation",  category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("extractRCObjectInformation",  category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("extractS3ObjectInformation", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("extractS4ObjectInformation",category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("generateMarkup",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateEnc",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateEnumeration",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateLabel", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateParagraph",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateParagraphCR",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateParagraph2NL",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateReference", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateS3MethodSignature", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateSection",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateOptionLink",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateOptionSexpr",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("generateTable",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("GenerationContext", category$CLASS, nature$EXPORTED,
              stratum$CORE, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("getFunctionSignature", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("getObjectConstructor",category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("getObjectMethodSignature", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("getObjectSignature", category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("getSectionContent",category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("identifyReplacementVariables", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$FEEDBACK),
    buildList("InputContext", category$CLASS, nature$EXPORTED,
              stratum$CORE, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("manageSingleStrings",category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("generatePublicFieldParagraph",category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("generatePublicMethodParagraph",category$FUNCTION, nature$INTERNAL,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("ManualPageBuilder", category$CLASS, nature$EXPORTED,
              stratum$CORE, phasing$BUILD, intent$PARTS_ASSEMBLY),
    buildList("opRdocInforamtion",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$FEEDBACK),
    buildList("ProcessingContext",category$CLASS, nature$EXPORTED,
              stratum$CORE, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("produceDocumentationFile", category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("producePackageLink",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("rdocKeywords",category$FUNCTION, nature$EXPORTED,
              stratum$CORE, phasing$RUN, intent$FEEDBACK),
    buildList("sentensize",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    buildList("auditDocumentationFiles",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$QUALITY_CONTROL),
    buildList("completeManualPage",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("escapeContent",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("getStandardSectionNames",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$BUILD, intent$CONTENT_GENERATION),
    buildList("produceAllManualPagesFromObject",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$QUALITY_CONTROL),
    buildList("produceManualPage",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$QUALITY_CONTROL),
    buildList("interpretResults",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_3, phasing$RUN, intent$QUALITY_CONTROL),
    buildList("verifyDocumentationFile",category$FUNCTION, nature$EXPORTED,
              stratum$LAYER_1, phasing$RUN, intent$QUALITY_CONTROL),
    buildList( "dummy", category$DATA, nature$EXPORTED,
               stratum$LAYER_3, phasing$BUILD, intent$UTILITIES),
    buildList( "family", category$DATA, nature$EXPORTED,
               stratum$LAYER_3, phasing$BUILD, intent$UTILITIES)
  ))

  name <- NULL # nse
  dt[order(name)]
}