fDecodeStdErr <- function(txtList, l_file) {
  l_content <- readLines(l_file, warn = F)

  suppressWarnings(
    l_line        <- min(as.numeric(gsub(".*.pl:(\\d+):.*$", '\\1', txtList[[1]])),  length(l_content) - 7)
  )
  if ( ! is.na(l_line) ) {
    l_line_top    <- max(l_line - 2, 1)
    l_line_bottom <- min(l_line + 2, length(l_content) - 6 )

    l_col     <- gsub(".*.pl:\\d+:(\\d*).*$", '\\1', txtList[[1]])
    l_warning <- gsub(".*.pl:\\d+:\\d*(.*)$", '\\1', txtList[[1]])

    if ( nchar(l_col) > 0 )
      l_msg_pos <- paste(
        unlist(list(rep(" ", as.numeric(l_col)-1),
                  "^ ", l_warning)),
        collapse = "")
    else
      l_msg_pos <- "^^^^^^^^^^^^^^^^"

    l_msg <- c("\n",paste(
      paste(l_content[l_line_top:(l_line)], collapse = "\n"),
      l_msg_pos,
      paste(l_content[(l_line+1):l_line_bottom], collapse = "\n"),
      collapse = "\n", sep = "\n"))

  } else {
    l_msg <- ""
  }

  l_msg <- paste0(
    "Prolog details:",
    paste(l_msg, collapse = "\n", sep = "\n"),
    '\n',
    paste(txtList[!grepl(pattern = "^\\[", txtList)], collapse = "\n", sep = "\n")
    )
  warning(l_msg, call. = F)
}

fCleanStdOut <- function(txtList) {
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
#' @param mode query, profile, duration (default query)
#' @param ... p1
#'
#' @return p1
#' @export
#'
pl_eval <- function(body, query="true",
                    nsol=10 , verbose=F,
                    timeout=10, mode = "query",
                    more_options = "",
                    data, ...) {
  opt <- options("swipl_binary")

  l_mode_map <- c(
    query='main_print_tl',
    profile='main_with_profile_tl',
    duration='main_with_duration_tl'
  )

  # If unknown mode, try to use it as predicate
  if (! mode %in% names(l_mode_map) )
    l_mode_map[[mode]] <- mode

  if (is.null(opt$swipl_binary)) {
    opt$swipl_binary <- "swipl"
    options(opt)
  }

  l_swipl_bin_path <- opt$swipl_binary

  if (missing(data))
    data <- list(...)

  l_file <- tempfile("filepl", ".", ".pl" )
  on.exit(unlink(l_file), add = T)

  l_src <- paste(
    body,
    "\n",
    paste0(
    "writeqln(X) :- writeq(X), nl.\n\n",
    "main_query(ListRes) :-
           term_variables( (", query, "), ListVars),
           findnsols(",nsol,",ListVars , (", query, "), ListRes).\n\n",
    "main_print :-
           main_query(ListRes),
           maplist(writeqln, ListRes).\n\n",
    "main_with_duration    :-
           statistics(walltime, []),
           main_query(_),
           statistics(walltime, [_,ExecutionTime]),
           nl,write('# Execution took: '), write(ExecutionTime), write(' ms.'), nl.\n\n",
    "main_with_profile :-
           profile(main_query(ListRes), [top(20), cummulative(true)]),
           maplist(writeqln, ListRes).\n\n",
    "main_print_tl :-
           call_with_time_limit(", timeout,", main_print).\n\n",
    "main_with_duration_tl :-
           call_with_time_limit(", timeout,", main_with_duration).\n\n",
    "main_with_profile_tl :-
           call_with_time_limit(", timeout,", main_with_profile).\n\n",
    "main :- main_print_tl.\n\n"),
    collapse = "", sep = "\n"
  )

  l_src <- whisker::whisker.render(l_src, data = data, strict = FALSE)
  cat(l_src, file = l_file)

  if ( verbose == TRUE )
    cat(sep = "",
        "---------------\n",
        "SOURCE FILE:\n",
        "---------------\n",
        l_src,"\n")

  if ( .Platform$OS.type == "unix" )
    l_display_var = "DISPLAY= "
  else
    l_display_var = ""

  # Unset DISPLAY variable to make sure no
  l_cmd <- paste(l_display_var, l_swipl_bin_path, "  --nopce -q ",
                  " -f ", l_file,
                  " -g ", l_mode_map[[mode]], more_options ," -t halt 2>&1 " ,
                   sep = "" )

  suppressWarnings(
    l_cmd_ret <- system(l_cmd, intern = T, wait = T )
  )

  l_cmd_ret <- l_cmd_ret[l_cmd_ret != ""]

  if ( verbose == TRUE ) {
    cat(sep = "",
      "---------------\n",
      "PROLOG OUTPUT:\n",
      "---------------\n")
    cat(paste(l_cmd_ret, collapse = "\n"), "\n")
    cat(
      "---------------\n")
  }

  if ( grepl("^Warning:", l_cmd_ret[1]) ) {
    fDecodeStdErr(l_cmd_ret, l_file)
  }

  if ( grepl("^ERROR:", l_cmd_ret[1]) ) {
    l_r_data <- FALSE

    fDecodeStdErr(l_cmd_ret, l_file)

  } else {
    l_r_data <- list()

    if (mode == "profile") {
      warning("Profiling:\n",
        paste(grep("^[^[]", l_cmd_ret, value = T), collapse = "\n"),
        call. = F
      )
    }

    if (mode == "duration") {
      l_value <- paste(
        grep("^# Execution took: \\d+ ms.$", l_cmd_ret, value = T)
        , collapse = "\n")

      l_r_data <- as.numeric(
        gsub("^# Execution took: (\\d+) ms.$", "\\1", l_value))
    }

    if (mode == "query" || mode == "profile") {

      l_cmd_ret <- fCleanStdOut(l_cmd_ret)
      l_r <- gregexpr("\\<_\\>|\\<[A-Z][a-zA-Z0-9_]*\\>",query)
      l_variables <- unlist(regmatches(query,  l_r))
      for (v in which(l_variables == '_') ) { l_variables[[v]] <- paste0("HIDDEN34342_", v) }
      l_variables <- unique(l_variables)

      if (length(l_cmd_ret) > 0) {
        safe_eval_env <- new.env(parent = emptyenv())
        safe_eval_env$`+` <- `+`
        safe_eval_env$`-` <- `-`
        safe_eval_env$`/` <- `/`
        safe_eval_env$`*` <- `*`
        safe_eval_env$`c` <- `c`

        l_r_data <- lapply(l_cmd_ret, function(x) {
          x <- gsub("]-1", "]", x)
          if (x == "[]") {
            l_ret <- TRUE
          } else if ( startsWith(x, "[") ) {
            x1 <- gsub("(\\<_G?[0-9]+)", "`\\1`", x)
            x2 <- gsub("\\[", "c(", x1)
            x3 <- gsub("\\]", ")", x2)
            #x4 <- gsub("([\\(\\), ]*)([^\\(\\), ][^\\(\\),]+)([\\), ])", "\\1'\\2'\\3", x3)
            #x5 <- gsub(",([a-zA-Z]),", ",'\\1',", x4)

            l_err <- try({
              l_l <- parse(text = x3)
              l_ret <- unlist(lapply(l_l[[1]][2:length(l_l[[1]])],
                                     function(x) {

                                       if ( is.numeric(x) ) {
                                         ret <- as.numeric(x)
                                       } else if ( is.character(x) ) {
                                         ret <- x
                                       } else if ( is.symbol(x) ) {
                                         ret <- as.character(x)
                                       } else if ( is.language(x) ) {
                                         ret <- paste(capture.output(print(x)), collapse = "")
                                         try(silent = T, {
                                            ret <- paste0(unlist(eval(x, envir = safe_eval_env)), collapse = ",")
                                          })
                                       } else {
                                         ret <- paste(capture.output(print(x)), collapse = "")
                                       }

                                       ret
                                     }
              ) )
            }, silent = T)

            if (inherits(l_err, "try-error")) {
              l_ret <- character(length(l_variables))
              l_ret[[1]] <- substr(x3, 3, nchar(x3) - 1)
            }

          } else {
            l_ret <- character(length(l_variables))
            l_ret[[1]] <- paste0("#msg: ", x)
          }

          if ( length(l_ret) == 0 )
            l_ret <- FALSE

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

          l_r_data <-  as.data.frame(l_table, stringsAsFactors = F)

          l_silent_vars <-
            grepl("._$",           l_variables) |
            grepl("^HIDDEN34342_", l_variables)

          l_r_data <- l_r_data[which(!l_silent_vars)]

        }

      } else {
        l_r_data <- paste("res=",l_r_data)
      }
    }
  }

  # in case we have no output at all
  if ( length(l_r_data) == 0 )
    l_r_data <- list(FALSE)

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

  if (is.null(options$verbose) )
    options$verbose <- FALSE

  if (is.null(options$maxnsols) )
    options$maxnsols <- 10

  if (is.null(options$silent) )
    options$silent <- F

  if (is.null(options$timeout) )
    options$timeout <- 10

  if (is.null(options$mode) )
    options$mode <- "query"

  if (is.null(options$more_options) )
    options$more_options <- ""

  # Push multiple lines on the same line
  l_code <- options$code
  for (l in which(endsWith(l_code, "\\")) ) {
	    l_code[[l+1]] <- paste0(gsub("[ \t]*\\\\$", "", l_code[[l]]), " ", gsub("^[ \t]*","",l_code[[l+1]]))
    l_code[[l]] <- ""
    }

  # remove comments
  # This does not work if you mix " and '
  for (i in seq_len(length(l_code)) ) {

    l_split <- unlist(strsplit(l_code[[i]], split = "%"))

    l_p1 <- 0
    l_p2 <- 0
    for ( l_piece in seq_along(l_split) ) {
      l_p1 <- l_p1 + stringr::str_count(l_split[[l_piece]], '"')
      l_p2 <- l_p2 + stringr::str_count(l_split[[l_piece]], "'")

      if ( l_p1 %% 2 == 0 && l_p2 %% 2 == 0 ) {
        l_code[[i]] <- paste0(l_split[1:l_piece], collapse = "%")
        break
      }

    }

  }

  # push multiple lines query into single line
  for (i in seq_len(length(l_code)-1) ) {
    if ( startsWith(l_code[[i]], "?-") &&
         ! grepl(pattern = "\\.\\s*$",x =  l_code[[i]]) &&
         ! startsWith(l_code[[i+1]], "?-") &&
         ! grepl(pattern = "^[\t ]*$", l_code[[i+1]])
    ) {
      l_code[[i+1]] <- paste0(gsub("[ \t]*\\\\$", "", l_code[[i]]), " ", gsub("^[ \t]*","",l_code[[i+1]]))
      l_code[[i]] <- ""
    }
  }

  l_query <- gsub("^ *\\?- *(.*[^\\. ])[\\. ]*$", "\\1",
		  grep(pattern = "^ *\\?- *(.*)$",
		       l_code, value = T))

  l_body <- paste(grep(pattern = "^ *\\?- *(.*)$",
		       l_code,
		       value = T, invert = T),
		  collapse = "\n")

  l_eval_env <- as.list(.GlobalEnv)
  l_eval_env$.swiplr_chunks <- .GlobalEnv$.swiplr_chunks

  if (options$eval) {
    out_list <- lapply(l_query, function(x) pl_eval(l_body, query = x, nsol = options$maxnsols,
                                                    timeout = options$timeout, mode = options$mode,
                                                    verbose = (!is.null(options$verbose) && options$verbose ),
                                                    more_options = options$more_options,
                                                    data = l_eval_env))
  }  else
    out_list <- lapply(l_query, function(x) "")

  if (options$silent == TRUE || options$eval == FALSE)
    options$results = "hide"
  if (options$results == "hide") {}
  else
    options$results = "asis"

  # assign resutls in prolog_output
   if ( length(out_list) > 0 && options$eval == TRUE ) {
       l_out <- vector(mode = "list", length = length(out_list))

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
          l_out[[i]] <- out_list[[i]]
        }

        l_out[[i]] <- paste0("Query: ", l_query[[i]], "\n\n",
                             paste(l_out[[i]], collapse = "\n") )

      }

      l_out <- unlist(l_out)
      attr(l_out, "format") <- "markdown"
      class(l_out) <- "knitr_kable"
  } else {
      l_out <- NULL
  }

  # Save named notebook chunks
  if ( is.null(.GlobalEnv$.swiplr_chunks) ) .GlobalEnv$.swiplr_chunks <- list()

  if ( !grepl(pattern = "^unnamed-chunk-.*", x = options$label) ) {

    # save current chunk content
    l_computed_body <- whisker::whisker.render(l_body, data = l_eval_env, strict = FALSE)
    .GlobalEnv$.swiplr_chunks[[options$label]] <- paste(l_computed_body, sep = "\n")

  }


  options$highlights <- TRUE
  options$engine <- "prolog"

  class(options$code) <- "prolog"

  if (options$results == "hide")
    l_out <- NULL

  knitr::engine_output(options, options$code, l_out)
}


#' Title
#'
#' @param data_in data.frame to convert in list of named rows
#'
#' @return a list of list with data
#' @export
#'
#' @examples
#' by_row_iris <- r_to_pro(iris)
#'
r_to_pro <- function(data_in) {
  lapply(seq(nrow(data_in)), function(i) {
    l_out <- as.list(data_in[i,])
    names(l_out) <- gsub("\\.", "_", names(l_out))
    return(l_out)
  })
}

#' run a prolog query an plot the result as graph
#'
#' @param body prolog chunk. use " {{{ .swiplr_chunks.<label> }}}" to use a notebook chunk
#' @param query query to convert to graph. Must contain FROM, TO, and LINK
#' @param timeout maximum query duration
#' @param maxnsols maximum number of solutions (default 100)
#'
#' @example
#' plot_query("foo(node, target, use).", "foo(FROM, TO, LINK)")
#'
#' @export
plot_query <- function(body, query, maxnsols=100, timeout = 10 ) {

  l_eval_env <- as.list(.GlobalEnv)
  l_eval_env$.swiplr_chunks <- .GlobalEnv$.swiplr_chunks

  l_data <- pl_eval(body,
                    query = query,
                    nsol = maxnsols,
                    timeout = timeout,
                    mode = "query",
                    data = l_eval_env)

  if ( !all(c("FROM", "TO", "LINK") %in% names(l_data)))
    error("FROM, TO and LINK shall be in the query variables")

  l_node_from <- unique(l_data$FROM)
  l_node_to <- unique(l_data$TO)
  l_node_names <- unique(c(l_node_from, l_node_to))

  l_nodes <- data.frame( id = l_node_names,
                         label = l_node_names,
                         group = as.character(l_node_names %in% l_data$FROM), stringsAsFactors = F)

  l_links <- data.frame( from = l_data$FROM,
                         to = l_data$TO,
                         label = l_data$LINK, stringsAsFactors = F)
  l_links$arrows <- "to"


  visNetwork::visNetwork(l_nodes, l_links, width = "100%")

}



#' run a prolog query an plot the result as a wide table
#'
#' @param body prolog chunk. use "{{{ .swiplr_chunks$<label> }}}" to use a notebook chunk
#' @param query query to convert to graph. Must contain FROM, TO, and LINK
#' @param timeout maximum query duration
#' @param maxnsols maximum number of solutions (default 100)
#'
#' @return, data.frame
#' @export
#'
#' @examples
#'
#'
table_query <- function(body, query, maxnsols=100, timeout = 10 ) {

  l_eval_env <- as.list(.GlobalEnv)
  l_eval_env$.swiplr_chunks <- .GlobalEnv$.swiplr_chunks

  l_data <- pl_eval(body,
                    query = query,
                    nsol = maxnsols,
                    timeout = timeout,
                    mode = "query",
                    data = l_eval_env)

  if ( !all(c("ENTITY", "COLUMN_HEADER", "CELL") %in% names(l_data)))
    error("ENTITY, COLUMN_HEADER and CELL shall be in the query variables")

  l_table <- tidyr::pivot_wider(l_data,
                                id_cols = "ENTITY",
                                names_from = "COLUMN_HEADER",
                                values_from = "CELL",
                                values_fill = list(CELL=""))

  l_table
}
