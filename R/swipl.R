
pl_eval <- function(body, query="true", nsol=10 , verbose=F, timeout=10, data, ...) {

  if (missing(data))
    data <- list(...)

  l_file <- tempfile("filepl", ".", ".pl" )
  on.exit(unlink(l_file), add = T)

  l_src <- paste(
    body,
    "\n",
    paste0("main :-
           term_variables( (", query, "), ListVars),
           findnsols(",nsol,",ListVars , (", query, "), ListRes),
           maplist(writeln, ListRes),
           halt.",
           "\n:- call_with_time_limit(", timeout,", main)."),
    collapse = "", sep = "\n"
  )

  l_src <- whisker::whisker.render(l_src, data=data)
  cat(l_src, file = l_file)

  if ( verbose == TRUE )
    cat(l_src, "\n")

  l_cmd <- paste("/usr/local/bin/swipl ",
                  #"-g main " ,
                  "-f ", l_file, sep = " " )

  l_cmd_ret <- system(l_cmd, intern = T, wait = T )

  l_cmd_ret <- l_cmd_ret[l_cmd_ret != ""]

  l_r_data <- lapply(l_cmd_ret, function(x) {
    x <- gsub("]-1", "]", x)
    if (x=="")
      l_ret <- NULL
    else if (x=="true")
      l_ret <- TRUE
    else if (x=="false")
      l_ret <- FALSE
    else if ( startsWith(x, "[") ) {
      x <-
        gsub(perl = T,
        pattern = '([\\[\\ ]*)\'?([^\', \\]]*)\'?([\', \\]]*)',
        replacement = '\\1"\\2"\\3',
        x )
      x <-
        gsub(perl = T,
             pattern = '"([0-9.-]*)"',
             replacement = '\\1',
             x )

      l_ret <- try(rjson::fromJSON( x, simplify = T,  ) )

      if (inherits(l_ret, "try-error"))
        l_ret <- x

    } else {
      l_ret <- x
      warning("Unable to parse: ", x)
    }

    return(l_ret)

  }  )

  l_r <- gregexpr("[A-Z][a-zA-Z0-9_]*",query)
  l_variables <- unique(unlist(regmatches(query,  l_r)))

  l_sizes <- sapply(l_r_data, function(x) sapply(x, length) )
  if ( min(l_sizes) == max(l_sizes) ) {

    l_table <- lapply(seq_along(l_variables), function(i)  {
      sapply(l_r_data, function(x) unlist(x[[i]], use.names = T))
    })

    names(l_table) <- l_variables

    l_r_data <-  as.data.frame(l_table)

  } else {
    for ( i in seq_along(l_r_data)) {
      names(l_r_data[[i]]) <- l_variables
      ?unlist()
    }
  }

  return(l_r_data)
}


knit_prolog_engine <- function (options) {

  if (is.null(options$maxnsols) )
    options$maxnsols <- 1

  if (is.null(options$silent) )
    options$silent <- F

  if (is.null(options$timeout) )
    options$timeout <- 10


  l_query <- gsub("^ *\\?- *(.*[^\\. ])[\\. ]*$", "\\1",
                  grep(pattern = "^ *\\?- *(.*)$",
                       options$code, value = T))
  l_body <- paste(
    grep(pattern = "^ *\\?- *(.*)$", options$code, value = T, invert = T),
    collapse  = "\n")

  if (options$eval) {
    out_list <- lapply(l_query, function(x) pl_eval(l_body, query = x, nsol = options$maxnsols,
                                                    timeout = options$timeout,
                                                    verbose = (!is.null(options$verbose) && options$verbose ),
                                                    data = .GlobalEnv))
  }  else
    out_list <- list()

  cat("res: ", options$results, "\n")
  class(options$code)<- "prolog"
  if (options$silent == TRUE)
    options$results = "hide"
  else if (options$results == "hide") {}
  else
    options$results = "asis"

  #options$render = NULL

  # assign resutls in prolog_output
  assign(envir = .GlobalEnv, x = "prolog_output", value = out_list)

  # If chunk is named, assigne results to a variable named after the chunk
  if ( !grepl("^unnamed-chunk", options$label) )
    assign(envir = .GlobalEnv, x = options$label, value = out_list)

  # Assigne first result variables
  if (length(out_list) > 0 && length(out_list[[1]]) > 0)
    for (n in names(out_list[[1]][[1]]))
      assign(envir = .GlobalEnv, x = n, value = out_list[[1]][[1]][[n]])

  l_out <- vector(mode="list", length = length(out_list))
  for ( i in seq_along(out_list) ) {

    if (inherits(out_list[[i]], "data.frame")) {
      l_out_kable <- knitr::kable(out_list[[i]])
      for ( j in seq_along(l_out_kable) )
        l_out_kable[j] <- paste0("  ", l_out_kable[j])

      l_out_kable[length(l_out_kable)+1] <- ""
      l_out[[i]] <- l_out_kable

    } else {
      l_out[[i]] <- paste0(rjson::toJSON(out_list[[i]]), "\n")
    }

  }

  l_out <- unlist(l_out)
  attr(l_out, "format") <- "markdown"
  class(l_out) <- "knitr_kable"

  if (options$results == "hide")
    l_out <- ""

  knitr::engine_output(options, options$code, l_out)
}

knitr::knit_engines$set(prologr=knit_prolog_engine)
