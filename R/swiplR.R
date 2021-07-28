pl_eval <- function(...) {
  swiplR()$query(...)
}

swiplR <- function(l_swipl_bin_path="swipl", l_args = c("-q","--nopce")) {

  self <- environment()

  self$NewProlog <- function(l_swipl_bin_path, l_args = c("-q","--nopce")) {
    processx::process$new(command = l_swipl_bin_path,
                          args = l_args,
                          stdin = "|", stdout = "|", stderr = "|")
  }

  self$cnx <- self$NewProlog(l_swipl_bin_path, l_args)

  self$restart <- function() {
    self$cnx$kill()
    self$cnx <- self$NewProlog(l_swipl_bin_path, l_args)
  }

  self$send <- function(msg, timeout=10) {
    o<- ""
    out<-""
    last <- ""
    err_full <- ""
    err <- ""

    # make sure buffer is empty
    self$cnx$read_output()
    self$cnx$read_error()

    if ( ! endsWith(x = msg, ".") )
      l_nl <- ".\n"
    else
      l_nl <- "\n"

    self$cnx$write_input(paste0(msg,l_nl) )
    l_end <- proc.time()[["elapsed"]]+timeout

    # This is just to handle very fast errors
    while ( err == "" &&  o == "" && proc.time()[["elapsed"]] < l_end ) {
      o<-self$cnx$read_output()
      err <- self$cnx$read_error()
      if (nchar(err) > 0 ) {
        err_full <- paste0(err_full, err)
      }
    }

    if (nchar(err_full) > 0 || proc.time()[["elapsed"]] >= l_end) {

      l_ret <- list(out=o, err=err_full, q=msg )

    } else {

      last <- trimws(o)
      out <- paste0(out, o)
      while ( err == "" &&  ! endsWith(last, ".") && proc.time()[["elapsed"]] < l_end ) {

        o<-self$cnx$read_output()
        last <- trimws(o)
        if (last != "") {
          l_end <- proc.time()[["elapsed"]]+timeout
          out <- paste0(out, o)
        } else {
          # We've got empty line inside results, check for errors and wait in background

          err <- self$cnx$read_error()
          if (nchar(err) > 0 ) {
            err_full <- paste0(err_full, err)
          } else {
            Sys.sleep(0.001)
          }
        }

        while ( nchar(err) > 0 ) {
          err <- self$cnx$read_error()
          err_full <- paste0(err_full, err)
        }

        }


      l_ret <- list(out=out, err=err, q=msg )

    }

    return(l_ret)
  }

  self$query <- function(body, query="true",
                         nsol=10 , verbose=F,
                         timeout=10, mode = "query",
                         more_options = "",
                         data, ...) {

    # Make sure there is no '.' at the end of query
    query <- gsub(pattern = "\\.[\\t ]*$", "", query)

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

    cat(self$wrapper(), file = l_file)
    if ( !missing(body) ) {
      l_src <- whisker::whisker.render(body, data = data, strict = FALSE)
      cat(l_src, file = l_file, append = T)
    }

    if ( verbose == TRUE )
      cat(sep = "",
          "---------------\n",
          "SOURCE FILE:\n",
          "---------------\n",
          self$wrapper(),"\n",
          l_src,"\n")

    # Unset DISPLAY variable to make sure no X ERROR
    if ( .Platform$OS.type == "unix" )
      l_display_var = "DISPLAY= "
    else
      l_display_var = ""

    if ( ! is.null(self$cnx$get_exit_status()) )
      self$restart()

    l_file <- basename(l_file)
    l_load_ret <- self$send(paste0("consult('",substr(l_file, 0,nchar(l_file)-3 ) ,"')."))
    if ( nchar(l_load_ret$err) > 0 ) {

      fDecodeStdErr(l_load_ret$err, l_file)
      self$send(paste0("unload_file('",substr(l_file, 0,nchar(l_file)-3 ) ,"')."))

      return(NULL)
    }

    l_ret <- self$send(paste0(l_mode_map[[mode]], "((",query,"), ",nsol,", ",timeout,")."))

    self$send(paste0("unload_file('",substr(l_file, 0,nchar(l_file)-3 ) ,"')."))

    l_cmd_ret <- unlist(strsplit(l_ret$out, "\n"))
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

    if ( l_ret$err != "" && grepl("^ERROR:", l_ret$err) ) {
      l_r_data <- FALSE

      fDecodeStdErr(l_ret$err, l_file)

    } else {
      if ( l_ret$err != "" && grepl("^WARNING:", l_ret$err) ) {
        fDecodeStdErr(l_ret$err, l_file)
      }

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

        l_var_txt <- gsub(pattern = "'[^']*'", replacement = "", query)
        l_r <- gregexpr("\\b(_|[A-Z][a-zA-Z0-9À-ÿ_]*)\\b", l_var_txt)
        l_variables <- unlist(regmatches(l_var_txt,  l_r))
        l_unnamed_pos <- which(l_variables == "_")
        l_variables[l_unnamed_pos] <- paste("_", l_unnamed_pos, "_", sep = "")
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

    if (inherits(l_r_data, "data.frame")) {
      for ( c in names(l_r_data)) {

        if ( all( unlist(lapply(l_r_data[[c]], is.list)) == FALSE) ) {
          l_r_data[[c]] <- unlist(l_r_data[[c]])
        } else {
          class(l_r_data[[c]]) <- "list"
        }
      }
    }

    return(l_r_data)
  }


  self$wrapper <- function() {return(
    "
:- set_prolog_flag(debug_on_error, false).
:- use_module(library(apply)).
:- use_module(library(time)).
:- use_module(library(lists)).

writeqln(X) :- writeq(X), nl.

main_query(Query, ListRes, LIMIT) :-
           term_variables( (Query), ListVars),
           findnsols(LIMIT,ListVars , (Query), ListRes).

main_print(Query, LIMIT) :-
           main_query(Query, ListRes, LIMIT),
           maplist(writeqln, ListRes).

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
")}

  return(self)
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

fParse <- function(txt = "[]") {
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

fDecodeStdErr <- function(txtList, l_file) {
  txtList <- gsub("\n\t", "\t", txtList)
  txtList <- strsplit(txtList, split = "\n")[[1]]
  l_content <- readLines(l_file, warn = F)

  suppressWarnings(
    l_line        <- min(as.numeric(gsub(".*.pl:(\\d+):.*$", '\\1', txtList[[1]])),  length(l_content))
  )
  if ( ! is.na(l_line) ) {
    l_line_top    <- l_line
    l_line_bottom <- max(l_line + 2, length(l_content) )

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

as.pl_data.data.frame <- function(l_data) {
  l_out <- list()
  l_name <- tolower(gsub("^(\\w+).*$", "\\1", deparse(substitute(l_data))))
  l_out[[l_name]] <- unname(apply(l_data, 1, as.list))
  return(l_out)
}
