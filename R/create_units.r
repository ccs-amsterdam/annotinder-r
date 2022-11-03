#' Create units for the cssAnnotator
#'
#' Create the basis for a units object. The content of the units can then be
#' designed in a pipe of \code{\link{set_text}}, \code{\link{set_meta}}, \code{\link{set_image}} and \code{\link{set_markdown}} functions
#'
#' @param data  A data.frame
#' @param id    Name of a column in the data.frame with unique values.
#' @param type  Name of a column in the data.frame with types. Valid types are: "code", "train", "test" and "survey"
#' @param use_subfields Rows with duplicate ids can be grouped together as units with subfields. To use this feature you must
#'                      explicitly set use_subfields to TRUE, and set is_subfield = TRUE in all the fields that are subfields.
#' @param variables A vector of column names. These column names can then be referenced from the codebook.
#'                  For example, if there is a column "topic", you could ask the question: "is this sentence about {topic}".
#'                  The {topic} part will then be replaced in the question with the value for this unit in the "topic" column.
#'
#' @return
#' @export
#'
#' @examples
create_units <- function(data, id='id', type=NULL, subfields=NULL, variables=NULL) {
  if (!id %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', id))
  if ('.TYPE' %in% colnames(data)) stop('data cannot have a column called ".TYPE" (what a coincidence, right?)')
  if ('.POSITION' %in% colnames(data)) stop('data cannot have a column called ".POSITION" (what a coincidence, right?)')

  if (anyDuplicated(data[[id]])) {
    if (is.null(subfields)) stop(sprintf('The id column (%s) needs to have unique values. Alternatively, use the subfields argument if you want to group fields from duplicate ids together', id))
  }
  if (!is.null(type)) {
    if( !type %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', type))
    if (!unique_in_groups(data[[id]], data[[type]])) stop('Duplicate ids need to have the same type.')
  }

  for (variable in variables) {
    if (!variable %in% colnames(data)) stop(sprintf('"%s" is not a column name in d', variable))
    if (!unique_in_groups(data[[id]], data[[variable]])) stop('Duplicate ids need to have the same values for variables.')
  }

  l = list(df = dplyr::as_tibble(data), id=id, fields=c(), content_order=c(), variables=variables, subfields=subfields)
  l$df$.TYPE = if (!is.null(type)) l$df[[type]] else 'code'
  l$df$.POSITION = NA

  structure(l, class = c('createUnitsBundle', 'list'))
}

function() {
  library(annotinder)

  d = data.frame(id = c(1,1,2,3), text = c('a','b','c','d'), date=c(2,2,3,4))
  create_units(d, subfields='banaan') |>
    set_text('text') |>
    set_meta('date')


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
    set_text('title', text_size=2, bold=T, align='center') |>
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
#' @param column      The name of a "character" column in the data.
#' @param before     The text can have a context before and after the coding unit. For example, the data.frame could be a keyword in context listing with the columns "pre", "keyword" and "post" (see for instance quanteda's kwic function).
#'                    These could then be set to the "before", "column" and "after" arguments, respectively. NOTE that if a before or after context is specified, all other text fields before or after
#'                    the current will also be considered context.
#' @param after      See 'before' argument
#' @param label      An expression or character value to label the text field. Coders will then see this label where this field starts.
#' @param split      A string for splitting the text. The field will then be split over multiple text fields. See the per_field argument in \code{\link{question}}
#'                   for a cool way to use this to repeat a question for multiple parts of a text.
#'                   Note that split will only work on the column. The before/after context will be kept before the first part and after the last
#' @param ...        Style settings, passed to \code{\link{style}}
#' @param offset     An expression (most likely a column). If the text is a part of a bigger, original text, you can include the offset for the character position where it starts. This is relevant if you want to import
#'                   or export span annotations for which the offset refers to the original text.
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_text <- function(data, column, before=NULL, after=NULL, label=NULL, split=NULL, ..., offset=0) {
  if (!is.null(column) && length(column) != 1) stop('Can only choose one column. If you want to add multiple text fields, you can use set_text multiple times')
  if (!is.null(before) && length(before) != 1) stop('Can only choose one "before" column. If you want to add multiple text fields, you can use set_text multiple times')
  if (!is.null(after) && length(after) != 1) stop('Can only choose one "after" column. If you want to add multiple text fields, you can use set_text multiple times')

  for (col in c(column, before, after)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }
  check_subfield(data, column)

  l = list(field=column,
           coding_unit=column,
           context_before=before,
           context_after = after,
           style = style(...),
           offset = substitute(offset),
           split=split,
           label=substitute(label))

  if (is.null(data$text)) data$text = list()
  data$text[[length(data$text)+1]] = l
  if (column %in% data$fields) stop(sprintf('field name (%s) already exists', field))
  data$fields = c(data$fields, column)
  data$content_order = c(data$content_order, column)
  data
}

#' Set meta-data content
#'
#' You can select columns to show in the unit as meta data. This will always be displayed at the top-middle of the
#' unit, with one the left hand a label and on the right hand the value (e.g., "DATE     2010-01-01").
#' With set_meta you can set multiple columns at once. Meta data cannot be grouped, so duplicate ids need to have the
#' same values in the meta columns.
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

  ids = data$df[[data$id]]
  for (i in 1:length(columns)) {
    column = columns[i]
    label = labels[i]
    if (!column %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))

    ## duplicate ids need to have the same meta values
    if (!unique_in_group(ids, data$df[[column]])) stop(sprintf('Invalid values for META field "%s". Duplicate ids need to have the same values', column))

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
#' @param caption     An expression (most likely a column name) or character value to use as image caption.
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
  check_subfield(data, column)

  l = list(field = column,
           base64=base64,
           style = style(...))
  if (!is.null(caption)) l$caption = substitute(caption)

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
#' @param split      A string for splitting the markdown string. The field will then be split over multiple markdown fields. See the per_field argument in \code{\link{question}}
#'                   for a cool way to use this to repeat a question for multiple parts of a text.
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return A createUnitsBundle object
#' @export
#'
#' @examples
set_markdown <- function(data, column, split=NULL, ...) {
  if (length(column) != 1) stop('Only one markdown string can be set at a time. Note that you can call set_markdown multiple times')

  for (col in c(column)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }
  check_subfield(data, column)


  l = list(field = column,
           split=split,
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
#' @param field    Optionally, the name of a specific field.
#' @param damage   The amount of damage a coder should receive. Can be a number or an expression that returns a number.
#' @param operator How should the annotation value be compared to the column value? Default is "==" (equals). Alternatives are "!=" (not equals),
#'                 "<=", "<", ">=" or ">".
#' @param message     A markdown string that will be displayed when a coder gives an incorrect answer. If not given, the message will be:
#'                    \code{"### You gave an incorrect answer.\n\nThis is a **training** unit. \nPlease have another look and select a different answer"}.
#' @param submessage  Optionally, an expression (e.g., the name of a column) in data with a specific markdown string to display when this condition is not met.
#'                    Use this to give specific feedback/hints.
#'
#' @return
#' @export
#'
#' @examples
set_train <- function(data, column, message=NULL, submessage=NULL, variable=column, field=NULL, damage=0, operator='==') {
  if (!any(data$df$.TYPE == 'train')) stop('There are no train units specified. You can do this in the "create_units" function with the "type" argument')
  if (!operator %in% c('==','<=','<','>=','>','!=')) stop("invalid operator. Has to be one of: '==','<=','<','>=','>','!='")

  for (col in c(column, submessage)) {
    if (!col %in% colnames(data$df)) stop(sprintf('"%s" is not a column name in data', col))
  }

  if (is.null(data$train)) data$train = list()
  data$train[[length(data$train)+1]] = list(column=column, variable=variable, field=field, operator=operator,
                                            damage=substitute(damage),
                                            message=message,
                                            submessage=substitute(submessage))

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
#' @param field       Optionally, the name of a specific field
#' @param damage      The amount of damage a coder should receive for getting a value wrong.
#' @param operator    How should the annotation value be compared to the column value? Default is "==" (equals).
#'                    But to screen on age you could for instance use "<=".
#' @param on_wrong    A markdown string that will be displayed when a coder gives an incorrect answer. If not given, no message will be displayed.
#'
#' @return
#' @export
#'
#' @examples
set_test <- function(data, column, variable=NULL, field=NULL, damage=10, operator='==') {
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

#' Customize the layout of the unit fields
#'
#' By default, the fields are presented in a single column, in the given order (i.e. the order of set_text, set_markdown and set_image functions).
#'
#'
#' @param data
#' @param areas A list with character vectors to specify the grid-template-areas (see \url{https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}).
#'              Each item in the list represents a row, and each value in the character vector the column in that row.
#'              The values need to be the names of fields (i.e. the column names). If you want a position in the grid to be empty, use a dot ".".
#' @param rows  A numeric vector of the same length as the number of rows. Each value indicates the relative space given to the row. So c(1,2) means that
#'              the second row will be twice as high as the first row.
#' @param columns Same as rows, but for columns.
#'
#' @return
#' @export
#'
#' @examples
set_grid <- function(data, areas, rows=NULL, columns=NULL) {
  data$grid = list(areas=lapply(areas, as.list))
  if (!is.null(rows)) data$grid$rows = as.list(rows)
  if (!is.null(columns)) data$grid$columns = as.list(columns)
}

areas = list(c('test','dit'),
             c('feestje'))
areas = lapply(areas, as.list)

#' S3 print method for createUnitsBundle objects
#'
#' @param x an createUnitsBundle object, created with \link{create_units}
#' @param ... not used
#'
#' @method print createUnitsBundle
#' @examples
#' @export
print.createUnitsBundle <- function(x, ...){
  units = table(x$df$.TYPE[!duplicated(x$df[[x$id]])])
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
#' @param align       How to align the text. Can be 'justify','center','left' or 'right'
#' @param ...         Any CSS inline style element can be used. Note that some style settings might not play nicely with certain annotator features
#'                    (such as colors in combination with span annotations)
#'
#' @return A list of CSS Style properties
#' @export
#'
#' @examples
#' # nice setting for titles
#' style(text_size = 1.4, bold=T)
style <- function(text_size=NULL, bold=NULL, italic=NULL, align=NULL, ...) {
  s = list(textAlign = substitute(align),
           text_size=substitute(text_size),
           bold=substitute(bold),
           italic=substitute(italic))
  s = s[!sapply(s, is.null)]
  expressions = rlang::exprs(...)
  if (length(expressions) > 0) s = c(s, expressions)
  s
}

check_subfield <- function(data, column) {
  if (!is.null(data$subfields) && !column %in% data$subfields) {
    if (!unique_in_group(data$df[[data$id]], data$df[[column]])) stop(sprintf('Invalid values for "%s". Fields that are not marked as subfields must have unique values across rows with the same id.', column))
  }
}

unique_in_group <- function(ids, values) {
  n_unique_values = nrow(unique(data.frame(id=ids, value=values)))
  n_unique_values == length(unique(ids))
}
