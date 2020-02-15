traceenv <- function(localisator_s_1, n_ui_1, environement_e_1) {
  cat(n_ui_1, '. localisator=', localisator_s_1,
      ' name=', environmentName(environement_e_1), '\n', sep = '')
}

f <- function() {
  n <- 1
  p <- e <- environment()
  traceenv('function env', n, e)

  repeat {
    n <- n + 1
    p <- parent.env(p)
    traceenv('parent env', n, p)
    if (environmentName(p) == 'R_EmptyEnv') break
  }
}

cat('search length', length(search()), '\n')
print(search())
f()
