#' Create units for the cssAnnotator
#'
#' Create the basis for a units object. The content of the units can then be
#' designed in a pipe of \code{\link{set_text}}, \code{\link{set_meta}}, \code{\link{set_image}} and \code{\link{set_markdown}} functions
#'
#' @param data  A data.frame
#' @param id    A column in the data.frame with unique values.
#' @param variables A vector of column names. These column names can then be referenced from the codebook.
#'                  For example, if there is a column "topic", you could ask the question: "is this sentence about {topic}".
#'                  The {topic} part will then be replaced in the question with the value for this unit in the "topic" column.
#'
#' @return
#' @export
#'
#' @examples
create_units <- function(data, id, type=NULL, variables=NULL) {
  if (!id %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', id))
  if ('.TYPE' %in% colnames(data)) stop('data cannot have a column called ".TYPE" (what a coincidence, right?)')
  if (anyDuplicated(data[[id]])) stop(sprintf('The id column (%s) needs to have unique values', id))
  for (variable in variables) {
    if (!variable %in% colnames(d)) stop(sprintf('"%s" is not a column name in d', variable))
  }
  if (!is.null(type))

  l = list(df = dplyr::as_tibble(data), id=id, fields=c(), variables=variables)
  l$df$.TYPE = 'code'

  structure(l, class = c('createUnitsBundle', 'list'))
}

function() {
  data = data.frame(id = c(1,2,3,4,5),
                    letter = letters[1:5],
                    date=c('2020-01-01','2020-01-02','2020-01-03','2020-01-04','2020-01-05'),
                    source=c('imagination'),
                    title=c('Cat','Cat','Dog','Dog','Car'),
                    text= c('I like cats.',
                            "Cats are awesome.",
                            "Some people like dogs.",
                            "Dogs are pretty awesome too.",
                            "Other people like cars"),
                    image=c('https://cdn.pixabay.com/photo/2017/07/25/01/22/cat-2536662_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2014/04/13/20/49/cat-323262_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2018/01/09/11/04/dog-3071334_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2017/09/25/13/14/dog-2785077_960_720.jpg',
                            'https://cdn.pixabay.com/photo/2016/11/29/09/32/auto-1868726_960_720.jpg'),
                    caption=c('Cat!','Caaaaaat','Doggie!!','Dog','Crrr'),
                    markdown=c('**useless markdown text**'),
                    animal=c('Cat',NA,'Dog',NA, 'Neither :('))


units = create_units(data, 'id') |>
  set_meta('source') |>
  set_meta('date') |>
  set_text('title', size=1.3, bold=T) |>
  set_text('text') |>
  set_image('image', caption='caption') |>
  set_markdown('markdown', align='center')

prepare_units(units, NULL)
}



#' Set text content
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a "character" column in the data that contains coding unit text.
#'                    Note that this can also be empty if the current text field is only context (before or after the coding unit).
#'                    For example, the data.frame could be a keyword in context listing with the columns "pre", "keyword" and "post" (see for instance quanteda's kwic function).
#'                    These could then be set to the "before", "column" and "after" arguments, respectively.
#' @param before The text can have a context before and after the coding unit. This context will be shown to coders in grey,
#'                   and they cannot annotate it (in annotate mode). The 'before' argument can be the name of a "character" column. If specified,
#'                   the value of this column will be included in the current text_field as context. NOTE that if a text_field has a 'before' context, all text_fields before it
#'                   will automatically also be considered as context. (i.e. context can only occur before of after the coding unit, not within it)
#' @param after See 'before' argument
#' @param label      A character value to label the text field. Coders will then see this label where this field starts.
#' @param ...        Style settings, passed to \code{\link{style}}
#' @param offset     If the text is a part of a bigger text, you can include the offset for the character position where it starts. This is relevant if you want to import
#'                   or export span annotations
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_text <- function(data, column=NULL, before=NULL, after=NULL, label=NULL, ..., offset=0) {
  if (!is.null(column)) column = name_as_character(substitute(column))
  if (!is.null(before)) before = name_as_character(substitute(before))
  if (!is.null(after)) after = name_as_character(substitute(after))

  for (col in c(column, before, after, label)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }
  if (is.null(column)) {
    if (is.null(before) && is.null(after)) stop('at least one of "column", "before" or "after" needs to be specified')
    if (!is.null(before) && !is.null(after)) stop('If no "column" is specified, you can only use "before" OR "after" (otherwise there wouldnt be a column at all)')
  }

  field = column
  if (is.null(field)) field = before
  if (is.null(field)) field = after
  l = list(field=field,
           coding_unit=column,
           context_before=before,
           context_after = after,
           style = style(...),
           offset=offset,
           label=label)

  if (is.null(data$text)) data$text = list()
  data$text[[length(data$text)+1]] = l
  if (field %in% data$fields) stop(sprintf('field name (%s) already exists', field))
  data$fields = c(data$fields, field)
  data
}

#' Set meta-data content
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The column with the meta data
#' @param label      A character value to label the meta field.
#' @param bold       Meta data by default uses 'bold' style setting.
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_meta <- function(data, column, label=NULL, bold=TRUE, ...) {
  column = name_as_character(substitute(column))
  for (col in c(column, label)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  l = list(field = column,
           style = style(bold=bold, ...),
           label=label)

  if (is.null(data$meta)) data$meta = list()
  data$meta[[length(data$meta)+1]] = l
  if (column %in% data$fields) stop(sprintf('field name (%s) already exists', column))
  data$fields = c(data$fields, column)
  data
}

#' Set image content
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a column in data that contains paths/urls to image files
#' @param caption     The name of a column in data that contains the image caption
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_image <- function(data, column, caption=NULL, ...) {
  column = name_as_character(substitute(column))
  for (col in c(column, caption)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  l = list(field = column,
           style = style(...))
  if (!is.null(caption)) l$caption = caption

  if (is.null(data$image)) data$image = list()
  data$image[[length(data$image)+1]] = l
  if (column %in% data$fields) stop(sprintf('field name (%s) already exists', column))
  data$fields = c(data$fields, column)
  data
}

#' Set markdown content
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a column in data that contains markdown strings
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_markdown <- function(data, column, ...) {
  column = name_as_character(substitute(column))
  for (col in c(column)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  l = list(field = column,
           style = style(...))

  if (is.null(data$markdown)) data$markdown = list()
  data$markdown[[length(data$markdown)+1]] = l
  if (column %in% data$fields) stop(sprintf('field name (%s) already exists', column))
  data$fields = c(data$fields, column)
  data
}

test <- function(x) {
  print(as.character(substitute(x)))
  print(class(substitute(x)))
}

test(x = banaan)

#' Set training units
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param select      A logical expression to select which units are training units. Perhaps the best approach is to
#'                    already have a column in the data that indicates the type of units (code, train or test). The select
#'                    expression would then be something like: type == 'train'.
#' @param ...         \code{\link{condition}} functions, to indicate
#'
#' @return
#' @export
#'
#' @examples
set_train <- function(data, select) {
  s = substitute(select)
  data$.TYPE = ifelse(eval(s, data), 'train', data$.TYPE)
  eval(s, data)
}

test <- function(x ) {
  print(as.character(substitute(x)))
  print(class(substitute(x)))

}

name_as_character <- function(substituted) {
  if (class(substituted)[1] %in% c('name', 'character'))
    as.character(substituted)
  else
    eval(substituted)
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
  units = table(x$df$.TYPE)
  units = units[match(unique(x$df$.TYPE), names(units))] # sort by occurence
  cat(sprintf('- units:'), paste(paste0(names(units), ' (', units, ')'), collapse=', '))
  cat(sprintf('\n- fields: %s', paste(x$fields, collapse=', ')))
}

#' Set styling
#'
#' This function produces a list of inline CSS style properties.
#'
#' @param text_size   The text size as a ratio. The default 1 means use the standard size. 0.5 means half this size, 2 means twice this size, etc.
#' @param bold        If True, make text bold
#' @param italic      If True, make test italic
#' @param align       How to align the text
#' @param ...         Any CSS inline style element can be used. Note that some style settings might not play nicely with certain annotator features
#'                    (such as colors in combination with span annotations)
#'
#' @return A list of CSS Style properties
#' @export
#'
#' @examples
#' # nice setting for titles
#' style(text_size = 1.4, bold=T)
style <- function(text_size=1, bold=F, italic=F, align=c('justify','center','left','right'), ...) {
  align = match.arg(align)
  s = list(textAlign = align, ...)
  if (class(text_size) == 'numeric' && text_size != 1) s$fontSize = paste0(text_size, 'em')
  if (bold) s$fontWeight = 'bold'
  if (italic) s$fontStyle = 'italic'
  s
}
