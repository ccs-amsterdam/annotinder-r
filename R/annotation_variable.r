#' Create an annotation variable
#'
#' Creates an annotation variable that can be passed as an argument to
#' \code{\link{create_codebook}}.
#'
#' @param name The name/label of the variable. The name/label of the
#'   question. Recommended to keep short. Cannot contain a "."
#' @param instruction  A brief (think 1 or 2 sentences) instruction to the
#'   coder.
#' @param codes The codes that the coder can choose from. Can be a
#'   character vector, named character vector or data.frame. An unnamed
#'   character vector creates simple codes. A named character vector uses the
#'   labels as colors, either as HEX or a name recognized by browsers (see
#'   \url{https://www.w3schools.com/colors/colors_names.asp}). A data.frame must
#'   have a code column, and can use certain special columns (see details). For
#'   most control, codes can be a list of 'code' objects created with
#'   \code{\link{code}}.
#' @param relations If specified, this becomes a 'relation' type annotation.
#'   relations can be created with \code{\link{relation}}. You can also specify
#'   multiple relations by providing a list of relations.
#' @param only_edit If TRUE, coders can only edit imported annotations. You
#'   can import annotations in \code{\link{create_job}} with the annotations
#'   argument
#' @param only_imported If TRUE, codes can only use codes that were used at
#'   least once in a unit in the imported annotations.
#' @param multiple If TRUE, coder can select multiple codes for a selected
#'   piece of text before closing the popup. Note that they can always select
#'   multiple codes by opening the popup multiple times. The setting exists for
#'   cases where multiple codes for a selection are common (e.g. topics in a
#'   paragraph).
#'
#' @details Using a data.frame for the codes argument gives more flexibility.
#' This data.frame should have a "code" column, and can in addition have a
#' "color" and "parent" column The color should be a color name, either as HEX
#' or a name recognized by browsers (see
#' \url{https://www.w3schools.com/colors/colors_names.asp}) The parent column is
#' only relevant if you have many codes and use selection="dropdown". The
#' dropdown menu will then show the codes with parent names, and parent names
#' are included in the search string. A parent can be the name of another code,
#' and parents can have parents, thus creating trees (just make sure not to
#' create cycles). Use case would for example be an ontology with actor ->
#' government -> president, and issue -> economy -> taxes.
#'
#' @return A variable object, to be used within the
#'   \code{\link{create_codebook}} function
#' @export
#'
#' @examples
#' # simple variable with simple codes
#' codes <- c("Economy", "War", "Health")
#' annotation_variable("topic", "Assign topics to words", codes = codes)
#'
#' # quick hand for setting colors
#' codes <- c(Economy = "blue", War = "red", Health = "green")
#' annotation_variable("topic", "Assign topics to words", codes = codes)
#'
#' # get codes from a data.frame
#' codes_df <- data.frame(
#'   code = c("negative", "neutral", "positive"),
#'   color = c("red", "grey", "green")
#' )
#' annotation_variable("sentiment", "Assign sentiment to words", codes_df)
#'
#' # codes data.frame with parents
#' codes_df <- data.frame(
#'   parent = c("", "actor", "government", "", "media", "newspaper"),
#'   code = c("actor", "government", "president", "media", "newspaper", "NYT")
#' )
#' codes_df
#'
#' annotation_variable(
#'   "actors",
#'   "Label actors. Use the most specific label available",
#'   codes_df
#' )
annotation_variable <- function(name, instruction, codes = NULL, relations = NULL, only_edit = F, only_imported = F, multiple = F) {
  if (grepl("\\.", name)) stop('Variable name is not allowed to contain a "." symbol')

  selection <- "buttons"

  a <- as.list(environment())
  l <- list(codes = codes)
  for (key in names(a)) {
    if (is.null(a[[key]])) next
    if (key == "codes") next
    if (key == "relations") next
    if (key == "selection") {
      if (selection == "buttons") {
        l[["searchBox"]] <- jsonlite::unbox(FALSE)
        l[["buttonMode"]] <- jsonlite::unbox("all")
      }
      if (selection == "dropdown") {
        l[["searchBox"]] <- jsonlite::unbox(TRUE)
        l[["buttonMode"]] <- jsonlite::unbox("recent")
      }
      next
    }
    l[[key]] <- jsonlite::unbox(a[[key]])
  }

  l$type <- "span"
  if (!is.null(relations)) {
    if (methods::is(relations, "codeRelation")) relations <- list(relations)
    l$relations <- relations
    l$type <- "relation"
  }


  l$editMode <- jsonlite::unbox(only_edit)
  l$onlyImported <- jsonlite::unbox(only_imported)


  if (methods::is(l$codes, "character")) {
    if (!is.null(names(l$codes))) {
      l$codes <- data.frame(code = l$codes, color = names(l$codes))
    } else {
      l$codes <- data.frame(code = l$codes)
    }
  }
  if (methods::is(l$codes, "list")) {
    l$codes <- bind_codes(codes)
  }
  if (!methods::is(l$codes, "data.frame")) {
    stop("The codes argument has to be a character vector, data.frame, or created with the codes() function")
  }
  if (is.null(l$codes$code)) {
    stop('The data.frame passed to the codes argument needs to have a column named "code"')
  }
  structure(l, class = c("codebookVariable", "list"))
}


#' Create relation codes. Used inside of annotation_variable
#'
#' @param codes A character vector with the code values. These need
#'   to correspond to codes specified in the 'codes' argument of
#'   annotation_variable
#' @param from_variable The name of a variable in the codebook. This
#'   specifies from which variable the relation starts at. (if you need multiple
#'   from variables, note that you can create multiple relations)
#' @param to_variable Like from_variable, but for specifying where the
#'   relation goes to
#' @param from_values Optionally, specify specific code values within the
#'   from variable. If not specified, all code values in the from_variable can
#'   be used
#' @param to_values Like from_values
#'
#' @return A codeRelation code object
#' @export
#'
#' @examples
#' label_var <- annotation_variable("Labels", "Span annotations", codes = c("Person", "Issue"))
#' relation_var <- annotation_variable("Relations", "Relation annotations",
#'   code = c(green = "Positive stance", grey = "Neutral stance", red = "Negative stance"),
#'   relations = relation(c("Positive stance", "Neutral stance", "Negative stance"),
#'     from_variable = "Labels", from_values = "Person",
#'     to_variable = "labels", to_values = "Issue"
#'   )
#' )
relation <- function(codes, from_variable, to_variable, from_values = NULL, to_values = NULL) {
  l <- list(
    codes = as.list(codes),
    from = list(variable = from_variable),
    to = list(variable = to_variable)
  )
  if (!is.null(from_values)) l$from$values <- as.list(from_values)
  if (!is.null(to_values)) l$to$values <- as.list(to_values)
  structure(l, class = c("codeRelation", class(l)))
}

#' S3 print method for codebookVariable objects
#'
#' @param x an codebookVariable object, created with \link{annotation_variable}
#' @param ... not used
#'
#' @method print codebookVariable
#' @export
print.codebookVariable <- function(x, ...) {
  for (name in names(x)) {
    if (name == "codes") next
    if (name == "relations") next
    if (x[[name]] == F) next
    label <- if (name == "name") "variable name" else name
    cat(sprintf("%s:\t%s\n", label, x[[name]]))
  }
  cat("\ncodes:\n")
  print(x$codes)
  cat("\nrelations:\n")
  print(x$relations)
}

#' S3 print method for codeRelation objects
#'
#' @param x a codeRelation object, created with \link{relation}
#' @param ... not used
#'
#' @method print codeRelation
#' @export
print.codeRelation <- function(x, ...) {
  str <- paste0("Relations: ", paste(x$codes, collapse = " - "))
  str <- paste0(str, "\n   From: ", x$from$variable)
  if ("values" %in% names(x$from)) str <- paste0(str, " (", paste(x$from$values, collapse = ", "), ")")
  str <- paste0(str, "\n   To:   ", x$to$variable)
  if ("values" %in% names(x$to)) str <- paste0(str, " (", paste(x$to$values, collapse = ", "), ")")
  cat(str)
}

#' S3 summary method for codebookVariable objects
#'
#' @param object an codebookVariable object, created with \link{annotation_variable}
#' @param ... not used
#'
#' @method summary codebookVariable
#' @export
summary.codebookVariable <- function(object, ...) {
  for (name in names(object)) {
    if (name == "codes") next
    if (object[[name]] == F) next
    label <- if (name == "name") "variable name" else name
    cat(sprintf("%s:\t%s\n", label, object[[name]]))
  }
}
