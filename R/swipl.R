fDecodeStdErr <- function(txtList, l_file) {
  l_line <- as.numeric(gsub(".*.pl:(\\d+):.*$", '\\1', txtList[[1]]))
  l_col  <- gsub(".*.pl:\\d+:(\\d*).*$", '\\1', txtList[[1]])

  l_msg <- c("\n",paste(
    system(paste0("cat -n ", l_file, " | head -n ",l_line," | tail -n 4"), intern = T),
    collapse = "\n"))

  if ( nchar(l_col) > 0 )
    l_msg <- c(l_msg, "\t", paste( rep(" ", as.numeric(l_col)), sep = ''), "^\n")
  if ( length(txtList) >= 2)
    l_msg <- c(l_msg, txtList[[2]], "\n")

  warning(l_msg, call. = F)
}

fCleanStdOut <- function(txtList, verbose=F) {
  if (verbose == TRUE) {
    cat(paste(txtList[grepl(pattern = "^[^\\[]", txtList)], sep = "\n", collapse = "\n"))
    cat("\n")
  }
  txtList[grepl(pattern = "^\\[", txtList)]
}


#' pl_eval
#'
#' @param body p1
#' @param query p1
#' @param nsol p1
#' @param verbose v
#' @param timeout p1
#' @param data p1
#' @param ... p1
#'
#' @return p1
#' @export
#'
pl_eval <- function(body, query="true", nsol=10 , verbose=F, timeout=10, data, ...) {
  if (missing(data))
    data <- list(...)

  l_file <- tempfile("filepl", ".", ".pl" )
  on.exit(unlink(l_file), add = T)

  l_src <- paste(
    body,
    "\n",
    paste0("main_query :-
           term_variables( (", query, "), ListVars),
           findnsols(",nsol,",ListVars , (", query, "), ListRes),
           maplist(writeln, ListRes).",
           "\nmain :- call_with_time_limit(", timeout,", main_query)."),
    collapse = "", sep = "\n"
  )

  l_src <- whisker::whisker.render(l_src, data=data)
  cat(l_src, file = l_file)

  if ( verbose == TRUE )
    cat(l_src, "\n")

  l_cmd <- paste("/usr/local/bin/swipl -q ",
                  " -f ", l_file,
                  " -g main -t halt 2>&1 " ,
                   sep = " " )

  l_cmd_ret <- system(l_cmd, intern = T, wait = T )

  l_cmd_ret <- l_cmd_ret[l_cmd_ret != ""]

  if ( verbose == TRUE ) {
    cat("prolog output:\n")
    cat(paste(l_cmd_ret, collapse = "\n"), "\n")
  }

  if ( grepl("^Warning:", l_cmd_ret[1]) ) {
    fDecodeStdErr(l_cmd_ret, l_file)
  }

  if ( grepl("^ERROR:", l_cmd_ret[1]) ) {
    l_r_data <- FALSE

    fDecodeStdErr(l_cmd_ret, l_file)

  } else {

    l_cmd_ret <- fCleanStdOut(l_cmd_ret, verbose = verbose)
    l_r <- gregexpr("\\<_\\>|[A-Z][a-zA-Z0-9_]*",query)
    l_variables <- unlist(regmatches(query,  l_r))
    for ( v in which(l_variables == '_') ) { l_variables[[v]] <- paste0("HIDDEN34342_", v) }
    l_variables <- unique(l_variables)

    if (length(l_cmd_ret) > 0) {

      l_r_data <- lapply(l_cmd_ret, function(x) {
        x <- gsub("]-1", "]", x)
        if (x=="[]") {
          l_ret <- TRUE
        } else if ( startsWith(x, "[") ) {
          x1 <- gsub("(\\<_G?[0-9]+)", "`\\1`", x)
          x2 <- gsub("\\[", "c(", x1)
          x3 <- gsub("\\]", ")",  x2)
          l_err <- try({
            l_l <- parse(text = x3)
            l_ret <- unlist(lapply(l_l[[1]][2:length(l_l[[1]])],
                                   function(x) if ( is.language(x) )
                                     paste(capture.output(print(x)), collapse = "")
                                   else
                                     paste(capture.output(cat(x)), collapse = "")
            ) )
          }, silent = T)

          if (inherits(l_err, "try-error")) {
            l_ret <- character(length(l_variables))
            l_ret[[1]] <- x1
          }

        } else {
          l_ret <- character(length(l_variables))
          l_ret[[1]] <- paste0("#msg: ", x)
        }

        return(l_ret)

      }  )

    } else {
      l_r_data <- list()
    }

    if ( length(l_r_data) == 0) {
      l_r_data <- list(lapply(l_variables, function(x) c(NA) ) )
    }

    l_sizes <- unlist(lapply(l_r_data, length))
    # print(l_sizes)
    if ( min(l_sizes) == max(l_sizes) ) {

      if ( length(l_variables) == 0 ) {

        l_r_data <-  l_r_data[[1]]

      } else {

        l_table <- lapply(seq_along(l_variables), function(i)  {
          sapply(l_r_data, function(x) unlist(x[[i]], use.names = T))
        })

        names(l_table) <- l_variables

        l_r_data <-  as.data.frame(l_table)
        l_r_data <- l_r_data[,grep("^HIDDEN34342_", l_variables, invert = T)]

      }

    } else {
      l_r_data <- paste("res=",l_r_data)
    }
  }

  return(l_r_data)
}


#' knit_prolog_engine
#'
#' @param options p1
#'
#' @return p1
#' @export
#'
knit_prolog_engine <- function (options) {

  if (is.null(options$maxnsols) )
    options$maxnsols <- 10

  if (is.null(options$silent) )
    options$silent <- F

  if (is.null(options$timeout) )
    options$timeout <- 10

  # Push multiple lines on the same line
  for ( l in which(endsWith(options$code, "\\")) ) { 
    options$code[[l+1]] <- paste0(gsub("\\$", "", options$code[[l]]), " ", options$code[[l+1]]) 
    options$code[[l]] <- ""  
  }

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
    out_list <- lapply(l_query, function(x) "")

  if (options$silent == TRUE)
    options$results = "hide"
  else if (options$results == "hide") {}
  else
    options$results = "asis"

  #options$render = NULL

  # assign resutls in prolog_output
   if ( length(out_list) > 0 ) {
       l_out <- vector(mode="list", length = length(out_list))
 
      names(out_list) <- as.character(paste0("result_", seq_along(out_list)))
      assign(envir = .GlobalEnv, x = "pl", value = out_list)
      assign(envir = .GlobalEnv, x = "prolog_output", value = out_list)
    
      # If chunk is named, assigne results to a variable named after the chunk
      if ( !grepl("^unnamed-chunk", options$label) )
        assign(envir = .GlobalEnv, x = options$label, value = out_list[[length(out_list)]])
    
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
  } else {
      l_out <- NULL
  }
  
  options$highlights <- TRUE
  options$engine <- "prolog"

  class(options$code) <- "prolog"

  if (options$results == "hide")
    l_out <- NULL

  knitr::engine_output(options, options$code, l_out)
}

