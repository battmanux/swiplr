#' knit_prolog_engine
#'
#' @param options knitr option list
#'
#' @return knitr result
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



#' Run a prolog query over a prolog code an plot the result as a graph
#'
#' @param body prolog chunk. use " {{{ .swiplr_chunks.<label> }}}" to use a notebook chunk
#' @param query query to convert to graph. Must contain FROM, TO, and LINK
#' @param timeout maximum query duration (default 10s)
#' @param maxnsols maximum number of solutions (default 100)
#'
#' @example
#' plot_query("foo(node, target, use).", "foo(FROM, TO, LINK)")
#'
#' @export
plot_query <- function(body, query, maxnsols=100, timeout = 10, cnx = swiplR() ) {

  l_eval_env <- as.list(.GlobalEnv)
  l_eval_env$.swiplr_chunks <- .GlobalEnv$.swiplr_chunks

  l_data <- cnx$query(body,
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
