rdocKeywords <- function(asList_b_1 = FALSE) {
  # Taken from "Writing R Extensions book" and "Parsing_Rd_files"
  kw <- list(
    # apparently a mix of what (object kind) and how (presentation format)
    markingTest = c('acronym', 'bold', 'cite', 'code', 'command', 'dfn', 'dQuote',
                    'email', 'emph', 'env', 'file', 'href', 'kbd', 'option', 'pkg',
                    'preformatted', 'samp', 'sQuote', 'special', 'strong',
                    'url', 'var', 'verb'),
    math = c('deqn', 'eqn'),
    insertions = c('dots', 'enc', 'ldots', 'R'),
    indices = c('concept', 'keyword'),
    sectionning = c('cr', 'section', 'tab', 'subsection'),
    listAndTables = c('describe', 'enumerate', 'item', 'itemize', 'tabular'),
    figures = c('figures'),
    crossReferences = c('link', 'linkS4class'),
    documentingFunctions = c('alias', 'docType', 'arguments', 'author',
                             'description', 'details',
                             'dontrun', 'dontshow', 'donttest', 'testonly',
                             'examples', 'method', 'name',
                             'note', 'references', 'seealso', 'S3method',
                             'S4method', 'title', 'usage',
                             'Rdversion', 'synopsis', 'encoding',
                             'value'),
    conditionalText = c('if', 'ifelse', 'out'),
    dynamiquePages = c('RdOpts', 'Sexpr'),
    userDefinedMacros = c('CRANpkg', 'doi', 'packageAuthor',
                          'packageDescription', 'packageDESCRIPTION',
                          'packageIndices', 'packageMaintainer', 'packageTitle',
                          'newcommand', 'renewcommand', 'sspace'),
    documentingDataSets = c('format', 'source')
  )

  # some usage knowledge
  # code ==> e.g. \code{\link{help}}
  # section ==> to declare new section as Warning with \section{Warning}{.....}
  # itemize ==> does not seem to be necessary under value and arguments, but required elsewhere
  # item ==> to emphasize on items \item{comp1}{Description of 'comp1'} - might need space in between
  # concept ==> use instead of keyword if no keyword matches (see. RshowDow('KEYWORDS') to get a list of keywords)
  # keywords requires no tilde in R documentation files
  # href ==> e.g. \href{https://www.google.com}{google search}

  if (asList_b_1) return(kw)
  sort(unlist(kw, use.names = FALSE))
}
