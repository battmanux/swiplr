
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

f_str_sub <- function(txt, start, stop) {
  if (stop < 0)
    stop <- nchar(txt) + stop + 1
  if (start < 0)
    start <- nchar(txt) + start + 1
  substring(txt, start, stop)
}

f_str_count <- function(txt, match) {
  length(strsplit(paste0(txt, "\r"), match, fixed = TRUE)[[1]])-1
}

fParse <- function(txt = "['coeur de boeuf',cuisson,'à point']") {
  txt <- gsub(pattern = "(^ +)|( +$)", replacement = "", txt)

  if ( grepl(x = txt, "^\\d+(?:\\.\\d+)$") == TRUE )
    return(as.numeric(txt))

  l_nchar <- nchar(txt)

  if ( l_nchar <= 1 )
    return(txt)

  if ( l_nchar > 1 &&
       f_str_sub(txt, 1, 1) == "'" &&
       f_str_sub(txt, -1, -1) == "'")
    return(f_str_sub(txt, 2, -2))

  if ( l_nchar > 1 &&
       f_str_sub(txt, 1, 1) == "\"" &&
       f_str_sub(txt, -1, -1) == "\"")
    return(f_str_sub(txt, 2, -2))

  if ( l_nchar > 1 &&
       f_str_sub(txt, 1, 1) == "[" &&
       f_str_sub(txt, -1, -1) == "]")
    return(fParseList(txt))

  return(txt)

}

fParseList <- function(txt) {

  if ( !( nchar(txt) > 1 &&
          f_str_sub(txt, 1, 1) == "[" &&
          f_str_sub(txt, -1, -1) == "]") ) {

    return(fParse(txt))

  } else {

    txt <- f_str_sub(txt, 2, -2)

  }

  l <- strsplit(txt, ",")[[1]]
  par_count      <- numeric(length(l))
  brackers_count <- numeric(length(l))
  quote_count    <- numeric(length(l))
  dquote_count    <- numeric(length(l))

  out <- list()
  cur <- list()

  for ( i in seq_along(l) ) {
    t <- l[[i]]

    if ( i > 1 ) {
      prev_c <- par_count[i-1]
      prev_b <- brackers_count[i-1]
      prev_q <- quote_count[i-1]
      prev_dq <- dquote_count[i-1]
    } else {
      prev_c <- 0
      prev_b <- 0
      prev_q <- 0
      prev_dq <- 0
    }

    l_quote_count <- f_str_count(t, "'")
    l_dquote_count <- f_str_count(t, "\"")

    quote_count[[i]]    <- prev_q + l_quote_count
    dquote_count[[i]]    <- prev_dq + l_dquote_count

    # increase symbol count only outside quotes
    if ( l_quote_count == 0 && l_dquote_count == 0) {

      if ( quote_count[[i]] %% 2 == 0 &&
           dquote_count[[i]] %% 2 == 0 ) {
        t_end   <- t
        t_start <- t
      } else {
        t_end   <- ''
        t_start <- ''
      }
    } else {

      l_start1 <- 1
      l_end1 <- nchar(t)

      if ( l_quote_count > 0 ) {
        if ( quote_count[[i]] %% 2  == 0 ) {
          l_end1 <- stringi::stri_locate_last_fixed(t, pattern = "'")[[1]]
        }

        if ( i > 1 && ( quote_count[[i-1]] %% 2  == 0 ) ) {
          l_start1 <- stringi::stri_locate_first_fixed(t, pattern = "'")[[1]]
        }

      }

      l_start2 <- 1
      l_end2 <- nchar(t)

      if ( l_dquote_count > 0 ) {

        if ( dquote_count[[i]]  %% 2  == 0  ) {
          l_end2 <- stringi::stri_locate_last_fixed(t, pattern = "\"")[[1]]
        }

        if ( i > 1 && ( dquote_count[[i-1]] %% 2  == 0 ) ) {
          l_start2 <- stringi::stri_locate_first_fixed(t, pattern = "\"")[[1]]
        }
      }

      t_start <- substr(t, 1, max(l_start1, l_start2))
      t_end <- substr(t, max(l_end1, l_end2), nchar(t))
    }

    par_count[[i]]      <- prev_c + f_str_count(t_start, "(") - f_str_count(t_end, ")")
    brackers_count[[i]] <- prev_b + f_str_count(t_start, "[") - f_str_count(t_end, "]")

    cur[[length(cur)+1]] <- t

    if (
        ( par_count[[i]]         == 0 &&
          quote_count[[i]] %% 2  == 0 &&
          dquote_count[[i]] %% 2  == 0 &&
          brackers_count[[i]]    == 0 ) ||
        (
          i == length(l)
        )) {
      out[[length(out)+1]] <- fParse( paste(cur, collapse = "," ) )
      cur <- list()
    }
  }

  return(out)

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

  # Unset DISPLAY variable to make sure no X ERROR
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

      l_r <- gregexpr("[\\(\\,]\ *(_|[A-Z][a-zA-Z0-9_]*)\\b", query)
      l_variables <- unlist(regmatches(query,  l_r))
      l_variables <- unique(l_variables)

      if (length(l_variables) == 0 ) {
        if (length(l_cmd_ret) == 0)
          return(FALSE)
        else
          return(TRUE)
      } else {

        # We have variables, so show a table
        if (length(l_cmd_ret) > 0) {
          l_r_data <- lapply(l_cmd_ret, fParse )
        } else {
          l_r_data <- list(lapply(l_variables, function(x) c(NA) ) )
        }

        l_sizes <- unlist(lapply(l_r_data, length))

        if ( min(l_sizes) == max(l_sizes) && min(l_sizes) == length(l_variables) ) {

          l_table <- do.call(cbind,
                             lapply(
                               seq_along(l_variables),
                               function(col_id) {

                                 l_col_data <- lapply(l_r_data, function(x) x[[col_id]])
                                 l_ret <- data.frame(col=I(l_col_data))
                                 names(l_ret) <- l_variables[[col_id]]

                                 return(l_ret)

                               } )
          )

          # l_r_data <-  as.data.frame(l_table, stringsAsFactors = F)
          # names(l_r_data) <- l_variables
          l_r_data <- l_table

          l_silent_vars <- grepl("_$", l_variables)
          l_r_data <- l_r_data[which(!l_silent_vars)]

        } else {
          l_r_data <- paste("res=",l_r_data)
        }
      }

    }
  }

  return(l_r_data)
}

#' Title
#'
#' @param l_swipl_bin_path
#' @param l_args
#'
#' @return
#' @export
#'
#' @examples
NewProlog <- function(l_swipl_bin_path="swipl", l_args = c("-q","--nopce")) {
  processx::process$new(command = l_swipl_bin_path,
                        args = l_args,
                        stdin = "|", stdout = "|", stderr = "|")

}

#' Title
#'
#' @param l_swipl_bin_path
#' @param l_args
#'
#' @return
#' @export
#'
#' @examples
Query <- function(cnx, q, timeout=1) {
  o<- ""
  last <- ""
  err <- ""
  cnx$read_error()
  cnx$write_input(paste0(q,"\n") )
  l_end <- proc.time()[["elapsed"]]+timeout
  l_cmd_ret <- capture.output(
    while ( err == "" &&  ! endsWith(last, ".") &&  proc.time()[["elapsed"]] < l_end ) {
      err <- paste0(err, cnx$read_error())
      o<-cnx$read_output()
      if (trimws(o) != "") {
        last <- trimws(o)
        cat(o)
      }
    }
  )
  l_ret <- list(out=l_cmd_ret, err=err, q=q )
  #str(l_ret)
  return(l_ret)
}

#' pl_eval_fast
#'
#' @param body p1
#' @param query p1
#' @param nsol p1
#' @param verbose v
#' @param timeout p1
#' @param data p1
#' @param cnx persistant connection created with NewProlog
#' @param mode query, profile, duration (default query)
#' @param ... p1
#'
#' @return p1
#' @export
#'
pl_eval_fast <- function(body, query="true",
                    nsol=10 , verbose=F,
                    timeout=10, mode = "query",
                    more_options = "",
                    data, cnx, ...) {

  #tic("entire function")

  #tic("Loading files")

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

  l_src_base <- paste(
    ":- use_module(library(apply)).",
    ":- use_module(library(time)).",
    ":- use_module(library(lists)).",
    "\n",
    "\n",
"
writeqln(X) :- writeq(X), nl.

main_query(Query, ListRes, LIMIT) :-
           term_variables( (Query), ListVars),
           findnsols(LIMIT,ListVars , (Query), ListRes).

main_print(Query, LIMIT) :-
           write('=START=\\n'),
           main_query(Query, ListRes, LIMIT),
           maplist(writeqln, ListRes),
           write('=STOP==\\n').

main_with_duration(Query, LIMIT)    :-
           statistics(walltime, []),
           main_query(Query, _, LIMIT),
           statistics(walltime, [_,ExecutionTime]),
           nl,write('# Execution took: '), write(ExecutionTime), write(' ms.'), nl.

main_with_profile(Query, LIMIT) :-
           profile(main_query(Query, ListRes, LIMIT), [top(20), cummulative(true)]),
           maplist(writeqln, ListRes).

main_print_tl(Query, LIMIT, TIMEOUT) :-
           call_with_time_limit(TIMEOUT, main_print(Query, LIMIT)).

main_with_duration_tl(Query, LIMIT, TIMEOUT) :-
           call_with_time_limit(TIMEOUT, main_with_duration(Query, LIMIT)).

main_with_profile_tl(Query, LIMIT, TIMEOUT) :-
           call_with_time_limit(TIMEOUT, main_with_profile(Query, LIMIT)).

main(Query) :- main_print_tl(Query, 10, 10).
",
    collapse = "", sep = "\n"
  )

  l_src <- whisker::whisker.render(body, data = data, strict = FALSE)
  cat(l_src_base, file = "base.pl")

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

  # Unset DISPLAY variable to make sure no X ERROR
  l_args <- c("--nopce", "-q")

  if (missing(cnx)) {
    cnx <- NewProlog()
    on.exit(cnx$kill(), add = T)
  }

  l_file <- basename(l_file)
  Query(cnx, "consult('base').")
  Query(cnx, paste0("consult('",substr(l_file, 0,nchar(l_file)-3 ) ,"')."))

  #toc()
  #tic("Query")
  l_ret <- Query(cnx, paste0("main_print_tl((",query,"), ",nsol,", ",timeout,")."), timeout=timeout)
  l_cmd_ret <- l_ret$out
  err <- l_ret$err

  Query(cnx, paste0("unload_file('",substr(l_file, 0,nchar(l_file)-3 ) ,"')."))
  Query(cnx, "unload_file('base').")

  #toc()
  #tic("decoding")

  l_cmd_ret <- strsplit(l_cmd_ret, "\n")
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

  if ( grepl("^Warning:", err) ) {
    fDecodeStdErr(err, l_file)
  }

  if ( grepl("^ERROR:", err) ) {
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

      l_r <- gregexpr("\\b(_|[A-Z][a-zA-Z0-9_]*)\\b", query)
      l_variables <- unlist(regmatches(query,  l_r))
      l_variables <- unique(l_variables)

      if (length(l_variables) == 0 ) {
        if (length(l_cmd_ret) == 0)
          return(FALSE)
        else
          return(TRUE)
      } else {

        # We have variables, so show a table
        if (length(l_cmd_ret) > 0) {
          l_r_data <- lapply(l_cmd_ret, fParse )
        } else {
          l_r_data <- list(lapply(l_variables, function(x) c(NA) ) )
        }

        l_sizes <- unlist(lapply(l_r_data, length))

        if ( min(l_sizes) == max(l_sizes) && min(l_sizes) == length(l_variables) ) {

          l_table <- do.call(cbind,
                             lapply(
                               seq_along(l_variables),
                               function(col_id) {

                                 l_col_data <- lapply(l_r_data, function(x) x[[col_id]])
                                 l_ret <- data.frame(col=I(l_col_data))
                                 names(l_ret) <- l_variables[[col_id]]

                                 return(l_ret)

                               } )
          )

          # l_r_data <-  as.data.frame(l_table, stringsAsFactors = F)
          # names(l_r_data) <- l_variables
          l_r_data <- l_table

          l_silent_vars <- grepl("_$", l_variables)
          l_r_data <- l_r_data[which(!l_silent_vars)]

        } else {
          l_r_data <- paste("res=",l_r_data)
        }
      }

    }
  }
  #toc()
  #toc()
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
