#' Create units for the cssAnnotator
#'
#' Create the basis for a units object. The content of the units can then be
#' designed in a pipe of \code{\link{set_text}}, \code{\link{set_meta}}, \code{\link{set_image}} and \code{\link{set_markdown}} functions
#'
#' @param data  A data.frame
#' @param id    A column in the data.frame with unique values.
#' @param type  A column in the data.frame with types. Valid types are: "code", "train", "test" and "survey"
#' @param variables A vector of column names. These column names can then be referenced from the codebook.
#'                  For example, if there is a column "topic", you could ask the question: "is this sentence about {topic}".
#'                  The {topic} part will then be replaced in the question with the value for this unit in the "topic" column.
#'
#' @return
#' @export
#'
#' @examples
create_units <- function(data, id='id', type=NULL, variables=NULL) {
  if (!id %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', id))
  if (!is.null(type) && !type %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', type))
  if ('.TYPE' %in% colnames(data)) stop('data cannot have a column called ".TYPE" (what a coincidence, right?)')
  if ('.POSITION' %in% colnames(data)) stop('data cannot have a column called ".POSITION" (what a coincidence, right?)')
  if (anyDuplicated(data[[id]])) stop(sprintf('The id column (%s) needs to have unique values', id))
  for (variable in variables) {
    if (!variable %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', variable))
  }

  l = list(df = dplyr::as_tibble(data), id=id, fields=c(), content_order=c(), variables=variables)
  l$df$.TYPE = if (!is.null(type)) l$df[[type]] else 'code'
  l$df$.POSITION = NA

  structure(l, class = c('createUnitsBundle', 'list'))
}

function() {
  library(annotinder)
  data = data.frame(id = c(1,2,3,4,5),
                    type = c('train','code','test','code','test'),
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
                    animal=c('Cat',NA,'Dog',NA, 'Neither :('),
                    animal_hint=c("Hint: look closely at those ears and paws.", NA, NA, NA,NA))

  units = create_units(data, 'id', 'type') |>
    set_meta('source') |>
    set_meta('date') |>
    set_text('title', size=2, bold=T, align='center') |>
    set_text('text', align='center') |>
    set_image('image', caption='caption') |>
    set_markdown('markdown', align='center') |>
    set_train('animal', damage=10, message='# OH NOES!!\n\nThis was a training unit, and it seems you got it wrong!', submessage='animal_hint') |>
    set_test('animal', damage=10)

  ## add a set_type or something. Because its not cool that set_train would now need to select for every variable

  codebook = create_codebook(
    sentiment = question('animal', 'What animal is this?', type = 'annotinder',
                         codes = c('Cat','Dog','Neither :('))
  )

  backend_connect('http://localhost:5000', 'test@user.com')
  job = create_job('test', units, codebook)

  js = list(
    jobset())

  upload_job('5', units=units, codebook=codebook, rules=rules_fixedset(randomize=T))


  u = prepare_units(units)
  jsonlite::toJSON(u[[1]], pretty = T, auto_unbox = T)
}



## set_survey(position=c('pre','post'))


#' Set text content
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a "character" column in the data that contains coding unit text.
#'                    Note that this can also be empty if the current text field is only context (before or after the coding unit).
#'                    For example, the data.frame could be a keyword in context listing with the columns "pre", "keyword" and "post" (see for instance quanteda's kwic function).
#'                    These could then be set to the "before", "column" and "after" arguments, respectively.
#' @param before     The text can have a context before and after the coding unit. This context will be shown to coders in grey,
#'                   and they cannot annotate it (in annotate mode). The 'before' argument must be the name of a "character" column. If specified,
#'                   the value of this column will be included in the current text_field as context. NOTE that if a text_field has a 'before' context, all text_fields before it
#'                   will automatically also be considered as context. (i.e. context can only occur before of after the coding unit, not within it)
#' @param after      See 'before' argument
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
  if (!is.null(column) && length(column) != 1) stop('Can only choose one column. If you want to add multiple text fields, you can use set_text multiple times')
  if (!is.null(before) && length(before) != 1) stop('Can only choose one "before" column. If you want to add multiple text fields, you can use set_text multiple times')
  if (!is.null(after) && length(after) != 1) stop('Can only choose one "after" column. If you want to add multiple text fields, you can use set_text multiple times')

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
  if (length(field) != 1) stop('Only one text field can be set at a time. Note that you can call set_text multiple times')

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
  data$content_order = c(data$content_order, field)
  data
}

#' Set meta-data content
#'
#' You can select columns to show in the unit as meta data. This will always be displayed at the top-middle of the
#' unit, with one the left hand a label and on the right hand the value (e.g., "DATE     2010-01-01").
#' With set_meta you can set multiple columns at once, but if you want different styling for meta fields
#' you can also use set_meta multiple times
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param columns     The columns with the meta data.
#' @param labels       By default, the column names are used as labels, but in uppercase and underscores replaced by whitespace.
#'                    You can customize labels by providing a character vector of the same length as the columns argument.
#'                    We recommend labels in uppercase because it fits the design, but you do you.
#' @param bold       Meta data by default uses 'bold' style setting.
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_meta <- function(data, columns, labels=NULL, bold=TRUE, ...) {
  if (!is.null(labels)) {
    if (length(columns) != length(labels)) stop('Length of the label argument should be the same as the length of the columns argument')
  } else {
    labels = gsub('_', ' ', toupper(columns))
  }

  for (i in 1:length(columns)) {
    column = columns[i]
    label = labels[i]
    if (!column %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
    l = list(field = column,
             style = style(bold=bold, ...),
             label=label)

    if (is.null(data$meta)) data$meta = list()
    data$meta[[length(data$meta)+1]] = l
    if (column %in% data$fields) stop(sprintf('field name (%s) already exists', column))
    data$fields = c(data$fields, column)
  }
  data
}

#' Set image content
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a column in data that contains paths/urls to image files
#' @param base64      If TRUE, store the image as a base64 in the codingjob json file
#' @param caption     The name of a column in data that contains the image caption
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_image <- function(data, column, base64=FALSE, caption=NULL, ...) {
  if (length(column) != 1) stop('Only one image can be set at a time. Note that you can call set_image multiple times')

  for (col in c(column, caption)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  l = list(field = column,
           base64=base64,
           style = style(...))
  if (!is.null(caption)) l$caption = caption

  if (is.null(data$image)) data$image = list()
  data$image[[length(data$image)+1]] = l
  if (column %in% data$fields) stop(sprintf('field name (%s) already exists', column))
  data$fields = c(data$fields, column)
  data$content_order = c(data$content_order, column)
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
  if (length(column) != 1) stop('Only one markdown string can be set at a time. Note that you can call set_markdown multiple times')

  for (col in c(column)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  l = list(field = column,
           style = style(...))

  if (is.null(data$markdown)) data$markdown = list()
  data$markdown[[length(data$markdown)+1]] = l
  if (column %in% data$fields) stop(sprintf('field name (%s) already exists', column))
  data$fields = c(data$fields, column)
  data$content_order = c(data$content_order, column)
  data
}

#' Set test units
#'
#' Setup training units, and provide the correct answers/annotations for these units.
#' If the answer/annotation is wrong, coders will see a message and need to retry.
#' Note that which units are train units needs to be indicated with the 'type' argument in \code{\link{create_units}}
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a column in data that has the correct annotation values. The name of the column needs to correspond to a variable/question
#'                    in the codebook, or alternatively use the 'variable' argument. If you need to set training answers for multiple columns (e.g.,
#'                    for units with multiple questions) you can use the set_train function multiple times.
#' @param variable The name of the variable as used in the codebook. If not specified, the name of the column will be used.
#' @param damage   The amount of damage a coder should receive.
#' @param operator How should the annotation value be compared to the column value? Default is "==" (equals). Alternatives are "!=" (not equals),
#'                 "<=", "<", ">=" or ">".
#' @param message     A markdown string that will be displayed when a coder gives an incorrect answer. If not given, the message will be:
#'                    \code{"### You gave an incorrect answer.\n\nThis is a **training** unit. \nPlease have another look and select a different answer"}.
#' @param submessage  Optionally, The name of a column in data with a specific markdown string to display when this condition is not met.
#'                 This message will be displayed beneath the on_wrong message.
#'
#' @return
#' @export
#'
#' @examples
set_train <- function(data, column, message=NULL, submessage=NULL, variable=column, damage=0, operator='==') {
  if (!any(data$df$.TYPE == 'train')) stop('There are no train units specified. You can do this in the "create_units" function with the "type" argument')
  if (!operator %in% c('==','<=','<','>=','>','!=')) stop("invalid operator. Has to be one of: '==','<=','<','>=','>','!='")

  for (col in c(column, submessage)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  if (is.null(data$train)) data$train = list()
  data$train[[length(data$train)+1]] = list(column=column, variable=variable,
                                            damage=damage, operator=operator, message=message, submessage=submessage)

  data
}

#' Set test units
#'
#' Setup test units (also known as gold units).
#' If the answer/annotation is wrong, coders receive damage.
#' Note that which units are test units needs to be indicated with the 'type' argument in \code{\link{create_units}}
#'
#' @param data        A createUnitsBundle object, as created with \code{\link{create_units}}
#' @param column      The name of a column in data that has the correct annotation values. The name of the column needs to correspond to a variable/question
#'                    in the codebook, or alternatively use the 'variable' argument. If you need to set training answers for multiple columns (e.g.,
#'                    for units with multiple questions) you can use the set_test function multiple times.
#' @param variable    The name of the variable as used in the codebook. If not specified, the name of the column will be used.
#' @param damage      The amount of damage a coder should receive for getting a value wrong.
#' @param operator    How should the annotation value be compared to the column value? Default is "==" (equals).
#'                    But to screen on age you could for instance use "<=".
#' @param on_wrong    A markdown string that will be displayed when a coder gives an incorrect answer. If not given, no message will be displayed.
#'
#' @return
#' @export
#'
#' @examples
set_test <- function(data, column, variable=NULL, damage=10, operator='==') {
  if (!any(data$df$.TYPE == 'test')) stop('There are no test units specified. You can do this in the "create_units" function with the "type" argument')
  if (is.null(variable)) variable = column
  if (!operator %in% c('==','<=','<','>=','>','!=')) stop("invalid operator. Has to be one of: '==','<=','<','>=','>','!='")

  for (col in c(column)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  if (is.null(data$test)) data$test = list()
  data$test[[length(data$test)+1]] = list(column=column, variable=variable,
                                          damage=damage, operator=operator)

  data
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
  cat(sprintf('units:'), paste(paste0(names(units), ' (', units, ')'), collapse=', '))
  cat(sprintf('\nfields: %s', paste(x$fields, collapse=', ')))
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
