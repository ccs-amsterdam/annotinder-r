

#' Create unit settings
#'
#' @param d       A data.frame
#' @param id      A character string with the name of a column with unique ids.
#' @param text    A character vector with column names to use as text fields. Can also be the output of \code{\link{text_fields}},
#'                which allows more customization options such as styling (bold, fontsize) per field, and distinguishing the coding unit and context unit.
#' @param meta    A character vector with column names to show as a table of meta data above the units.
#' @param variables A character vector with column names to use as unit variables
#'
#'
#' @return A list of units, to be used inside \code{\link{create_codebook}}
#' @export
#'
#' @examples
#' d = data.frame(id = c(1,2),
#'                title = c('title 1', 'title 2'),
#'                text=c('text 1','text 2'),
#'                pre=c('<','<'), post=c('>','>'),
#'                date=c('2010','2020'),
#'                topic=c('a','b'))
#'
#' ## simple units
#' create_units(d, id='id', text=c('title','text'), meta='date', variables='topic')
#'
#' ## using text_fields() for more options
#' create_units(d, 'id', text_fields(
#'    text_field('title'),
#'    text_field('text', context_before='pre', context_after='post')
#' ))
create_units <- function(d, id, text, meta=NULL, variables=NULL) {
  ## check if all the stuff is there
  if (!id %in% colnames(d)) stop(sprintf('"%s" is not a column name in d', id))
  if (anyDuplicated(d[[id]])) stop(sprintf('The id column (%s) needs to have unique values', id))
  if (methods::is(text, 'textFields')) {
    for (textfield in text) {
      for (textpart in c('coding_unit','context_before','context_after')) {
        if (!is.null(textfield[[textpart]]) && !textfield[[textpart]] %in% colnames(d)) stop(sprintf('"%s" is not a column name in d', textfield[[textpart]]))
      }
    }
  } else {
    for (textcol in text) {
      if (!textcol %in% colnames(d)) stop(sprintf('"%s" is not a column name in d', textcol))
    }
  }
  for (metacol in meta) {
    if (!metacol %in% colnames(d)) stop(sprintf('"%s" is not a column name in d', metacol))
  }
  for (variable in variables) {
    if (!variable %in% colnames(d)) stop(sprintf('"%s" is not a column name in d', variable))
  }


  ## create_units just bundles the arguments. The actual transformation to the required json
  ## happens when calling create_job
  cub = list(df = dplyr::as_tibble(d), id=id, text=text, meta=meta, variables=variables)
  structure(cub, class = c('createUnitsBundle', 'list'))
}

prepare_units <- function(createUnitsBundle, annotations) {
  d = createUnitsBundle$df

  if (is.null(annotations) || nrow(annotations) == 0) {
    ann_list = NULL
  } else {
    ann_list = split(annotations, annotations$id)
    ann_list = ann_list[match(d$id, names(ann_list))]
  }

  units = vector('list', nrow(d))
  for (i in 1:nrow(d)) {
    rowdict = as.list(d[i,])
    id = rowdict[[createUnitsBundle$id]]
    text_fields = create_text_fields(rowdict, createUnitsBundle$text)
    meta_fields = create_meta_fields(rowdict, createUnitsBundle$meta)
    variables = create_variables(rowdict, createUnitsBundle$variables)
    importedAnnotations = create_imported_annotations(ann_list[[i]])

    ## to do: add "gold". Then when creating coding job check if gold answers match with questions
    units[[i]] = list(unit = list(unit_id=jsonlite::unbox(id),
                                  text_fields=text_fields,
                                  meta_fields=meta_fields,
                                  variables=variables))

    if (!is.null(importedAnnotations)) units[[i]]$unit$importedAnnotations = importedAnnotations

  }

  structure(units, class=c('codingjobUnits', 'list'))
}


create_text_fields <- function(rowdict, text_cols) {
  lapply(seq_along(text_cols), function(i) {
    ## if text_cols is a simple character vector
    if (methods::is(text_cols, 'character')) {
      field = text_cols[i]
      value = rowdict[[text_cols[i]]]


      return(list(name=jsonlite::unbox(field), value=jsonlite::unbox(value)))
    }

    ## if text_cols was created with text_fields()
    if (methods::is(text_cols, 'textFields')) {
      tf = text_cols[[i]]

      if (is.null(tf$sep)) tf$sep=c('', '')
      if (length(tf$sep) == 1) tf$sep = c(tf$sep,tf$sep)


      text = if (is.null(tf$coding_unit)) '' else rowdict[[tf$coding_unit]]
      text_field = list(name=tf$field, value=text, bold=tf$bold, italic=tf$italic, size=tf$size, justify=tf$justify, paragraphs=tf$paragraphs)
      if (!is.null(tf$label)) text_field$label = tf$label
      if (!is.null(tf$context_before)) text_field$context_before = paste0(rowdict[[tf$context_before]], tf$sep[1])
      if (!is.null(tf$context_after)) text_field$context_after = paste0(tf$sep[2], rowdict[[tf$context_after]])
      return(lapply(text_field, jsonlite::unbox))
    }
  })
}


create_meta_fields <- function(rowdict, meta_cols) {
  lapply(seq_along(meta_cols), function(i) {
    if (methods::is(meta_cols, 'character')) return(list(name=meta_cols[i], value=rowdict[[meta_cols[i]]]))
    NULL
  })
}

create_variables <- function(rowdict, variable_cols) {
  l = list()
  for (vc in variable_cols) {
    l[[vc]] = jsonlite::unbox(rowdict[[vc]])
  }
  l
}

create_imported_annotations <- function(ann) {
  if (is.null(ann)) return(NULL)
  ann = ann[,c('field','variable','value','offset','length')]
  ann = apply(ann, 1, function(x) {
    l = as.list(x)
    l$offset = as.numeric(l$offset)
    l$length = as.numeric(l$length)
    l = lapply(l, jsonlite::unbox)
  })
  names(ann) = NULL
  ann
}




#' Helper function for text_field customization in \code{\link{create_units}}
#'
#' @param ... unnamed arguments, each one a text_field. Can either be a character like "title" or "text",
#'            or a more detailed text_field created with the \code{\link{text_field}} function.
#'
#' @return A textFields object, that can be passed to the "text" argument in \code{\link{create_units}}
#' @export
#'
#' @examples
#' d = data.frame(id=c(1,2), title = c('title 1', 'title 2'), text=c('text 1','text 2'),
#' pre=c('<','<'), post=c('>','>'), date=c('2010','2020'), topic=c('a','b'))
#'
#' ## simple units
#' create_units(d, id='id', text=c('title','text'), meta='date', variable='topic')
#'
#' ## using text_fields() for more options
#' create_units(d, 'id',
#'              text_fields(text_field('title'),
#'                          text_field('text', context_before='pre', context_after='post'))
#' )
text_fields <- function(...) {
  l = list(...)
  l = lapply(l, function(x) {
    if (methods::is(x, 'character')) x = text_field(x)
    x
  })
  names = sapply(l, function(x) x$field)
  if (anyDuplicated(names)) stop('Text fields need to use unique columns for setting the coding_unit')
  structure(l, class=c('textFields', 'list'))
}


#' Create a detailed text_field for \code{\link{create_units}}
#'
#' This function can be used inside the \code{\link{create_units}} function for more
#' customization options for text fields.
#'
#' @param coding_unit The name of a "character" column in the "data" argument in \code{\link{create_units}} that contains coding unit text.
#'                    Note that this can also be empty if the current text field is only context (before or after the coding unit).
#'                    For example, the data.frame could be a keyword in context listing with the columns "pre", "keyword" and "post" (see for instance quanteda's kwic function).
#'                   These could then be set to the "context_before", "coding_unit" and "context_after" arguments, respectively.
#' @param context_before The text can have a context before and after the coding unit. This context will be shown to coders in grey,
#'                   and they cannot annotate it (in annotate mode). The context_before argument can be the name of a "character" column. If specified,
#'                   the value of this column will be included in the current text_field as context. NOTE that if a text_field has a context_before, all text_fields before it
#'                   will automatically also be considered as context. (i.e. context can only occur before of after the coding unit, not within it)
#' @param context_after See context_before.
#' @param sep        A character string to separate the coding_unit from the context_before and context_after. Can also be a vector of length 2 for
#'                   different separators before and after.
#' @param label      A character value to label the text field. Coders will then see this label where this field starts.
#' @param bold       If TRUE, make the text field bold
#' @param italic     If TRUE, make the text field bold
#' @param size       A scaling value for the text size. default is 1. A value of 2 would be twice the size. A value of 0.5 half the size.
#' @param justify    If TRUE (default) justify the text
#' @param paragraphs If TRUE (default) show line breaks
#' @param offset     If the text is a part of a bigger text, you can include the offset for the character position where it starts. This can
#'                   be relevant for connecting annotations at specific character positions between the full text and this text_field.
#'
#' @return Nothing. Should only be used inside of the \code{\link{create_units}} function
#' @export
#'
#' @examples
text_field <- function(coding_unit=NULL, context_before=NULL, context_after=NULL, sep=' ', label=NULL, bold=F, italic=F, size=1, justify=T, paragraphs=T, offset=0) {
  if (is.null(coding_unit)) {
    if (is.null(context_before) && is.null(context_after)) stop('at least one of coding_unit, context_before or context_after needs to be specified')
    if (!is.null(context_before) && !is.null(context_after)) stop('If no coding_unit is specified, you can only use context_before OR context_after (otherwise there wouldnt be a coding_unit at all)')
  }
  field = coding_unit
  if (is.null(field)) field = context_before
  if (is.null(field)) field = context_after
  l = as.list(environment())
  l$field = field
  l
}


#' S3 print method for textFields objects
#'
#' @param x an textFields object, created with \link{variable}
#' @param ... not used
#'
#' @method print textFields
#' @examples
#' @export
print.textFields <- function(x, ...){
  for (l in x) {
    cat(l$field, '\n')
    for (arg in names(l)) {
      if (arg == 'field') next
      if (is.null(l[[arg]])) next
      if (l[[arg]] == F) next
      cat(arg, ':\t', l[[arg]], '\n')
    }
    cat('\n')
  }
}


#' S3 print method for codingjobUnits objects
#'
#' @param x an codingjobUnits object, created with \link{create_units}
#' @param ... not used
#'
#' @method print codingjobUnits
#' @examples
#' @export
print.codingjobUnits <- function(x, ...){
  cat('List of', length(x), 'units\n\nExample (only first unit):\n\n')
  pretty_json = jsonlite::toJSON(x[[1]], pretty=T)
  cat(pretty_json)
}

#' S3 print method for createUnitsBundle objects
#'
#' @param x an createUnitsBundle object, created with \link{create_units}
#' @param ... not used
#'
#' @method print createUnitsBundle
#' @examples
#' @export
print.createUnitsBundle <- function(x, ...){
  cat(jsonlite::toJSON(x, pretty=T))
}
