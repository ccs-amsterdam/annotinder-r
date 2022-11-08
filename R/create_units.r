#' Create units for the cssAnnotator
#'
#' Create the basis for a units object. The content of the units can then be
#' designed in a pipe of \code{\link{set_text}}, \code{\link{set_meta}}, \code{\link{set_image}} and \code{\link{set_markdown}} functions
#'
#' @param data  A data.frame
#' @param ...   Unit content is specified using the set_ functions (set_text, set_markdown, set_image, etc.).
#'              For example, if data has a column called header, and you want to create a title using this column,
#'              use: `set_text('title', header, bold=T, text_size=1.3)`
#' @param id    Name of a column in data with unique values. These ids will be used to link annotation to units.
#' @param type  Name of a column in data with types. Valid types are: "code", "train", "test" and "survey"
#' @param subfields Selected fields (text/image/markdown) of rows with identical id's can be grouped together into a single unit.
#'                  The subfields arguments should then be a character vector indicating which fields need to be grouped.
#'                  Fields that are not grouped should have identical values across all rows (with the same id).
#'                  When fields are grouped, they are enumerated as field.1, field.2, etc.
#'                  This is particularly usefull when combined with the per_field argument in \code{\link{question}}
#' @param variables A vector of column names in data. These column names can then be referenced from the codebook.
#'                  For example, if there is a column "topic", you could ask the question: "is this sentence about {topic}".
#'                  The {topic} part will then be replaced in the question with the value for this unit in the "topic" column.
#' @param meta A vector of column names in data. These names and their values will be shown at the top of a unit.
#'             Can also be a named vector, in which case the names will be used as the labels that coders get to see.
#' @param grid_areas A list with character vectors to specify the grid-template-areas (see \url{https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}).
#'              Each item in the list represents a row, and each value in the character vector the column in that row.
#'              The values need to be the names of fields (i.e. the column names). If you want a position in the grid to be empty, use a dot ".".
#' @param grid_cols A numeric vector of the same length as the number of columns in areas. Each value indicates the relative space given to the column. So c(1,2) means that
#'              the second column will be twice as wide as the first column.

#'
#' @return
#' @export
#'
#' @examples
create_units <- function(data, ..., id='id', type=NULL, subfields=NULL, variables=NULL, meta=NULL, grid_areas=NULL, grid_cols=NULL) {
  if (!id %in% colnames(data)) stop(sprintf('"%s" is not a column name in data', id))
  ids = data[[id]]
  if (anyDuplicated(ids)) {
    if (is.null(subfields)) stop(sprintf('The id column (%s) needs to have unique values. Alternatively, use the subfields argument if you want to group fields from duplicate ids together', id))
  }
  if (!is.null(type)) {
    if( !type %in% colnames(data)) stop(sprintf('"%s" is not a column name in data', type))
    if (!unique_in_groups(ids, data[[type]])) stop('Duplicate ids need to have the same type.')
  }
  for (variable in variables) {
    if (!variable %in% colnames(data)) stop(sprintf('"%s" is not a column name in data', variable))
    if (!unique_in_groups(ids, data[[variable]])) stop('Duplicate ids need to have the same values for variables.')
  }
  for (metafield in meta) {
    if (!metafield %in% colnames(data)) stop(sprintf('"%s" is not a column name in data', metafield))
    if (!unique_in_groups(ids, data[[metafield]])) stop('Duplicate ids need to have the same values for meta columns.')
  }

  calls = process_create_unit_calls(...)

  groups = split(1:nrow(data), ids)
  unique_ids = unique(ids)
  units = vector('list', length(unique_ids))
  for (i in 1:length(unique_ids)) {
    id = unique_ids[i]
    rows = data[groups[[id]],,drop=F]
    firstrow = rows[1,,drop=F]  ## for values that should be unique anyway

    text_fields = create_content_fields(rows, calls$text, subfields)
    image_fields = create_content_fields(rows, calls$image, subfields)
    markdown_fields = create_content_fields(rows, calls$markdown, subfields)

    meta_fields = create_meta_fields(firstrow, meta)
    variables = create_variables(firstrow, variables)

    codebook = NULL
    if (length(calls$questions) > 0) codebook = create_questions(firstrow, calls$questions)

    grid = create_field_grid(grid_areas, grid_cols, calls$field_order)
    if (!is.null(subfields)) grid = expand_grid(grid, nrow(rows), subfields)

    conditionals = NULL
    if (!is.null(type)) {
      if (firstrow[[type]] == 'train') conditionals = create_conditionals(rows, calls$train, subfields)
      if (firstrow[[type]] == 'test') conditionals = create_conditionals(rows, calls$test, subfields)
    } else {
      if (length(calls$train) > 0 || length(calls$test)> 0) stop('set_test and set_train only work if the type argument is set')
    }
    #importedAnnotations = create_imported_annotations(ann_list[[i]])

    unit = list()
    if (length(text_fields) > 0) unit$text_fields = text_fields
    if (length(image_fields) > 0) unit$image_fields = image_fields
    if (length(markdown_fields) > 0) unit$markdown_fields = markdown_fields
    if (length(meta_fields) > 0) unit$meta_fields = meta_fields
    if (length(variables) > 0) unit$variables = variables

    units[[i]] = list(id = id,
                      type = if (!is.null(type)) firstrow[[type]] else 'code',
                      unit = unit)
    if (!is.null(conditionals)) units[[i]]$conditionals = conditionals
    if (!is.null(grid)) units[[i]]$unit$grid = grid
    if (!is.null(codebook)) units[[i]]$unit$codebook = codebook

    units[[i]] = structure(units[[i]], class=c('codingjobUnit','list'))
    #if (!is.null(importedAnnotations)) units[[i]]$unit$importedAnnotations = importedAnnotations
  }

  structure(units, class=c('codingjobUnits', 'list'))
}

#' Create a single unit
#'
#' Works like \code{\link{create_units}}, but for a single unit.
#' The values can then directly be provided in the set_ functions.
#'
#' @param id   A unique id
#' @param type The unit type. Can be 'code', 'test', 'train' or 'survey'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' create_unit('id',
#'    set_text('text','This is the unit text'),
#' )
#'
#' ## this is also a good way to create custom training units
#' create_unit('id', type='train',
#'    set_text('text', This is the unit text'),
#'    set_question('variable', question = "Is this a text?", codes=c('yes','no')),
#'    set_train('variable', 'yes', message='WRONG!!\n\ntry again')
#' )
create_unit <- function(id, ..., type='code') {
  d = data.frame(id=id, type=type)
  create_units(d, type='type', ...)[[1]]
}



process_create_unit_calls <- function(...) {
  text = list()
  image = list()
  markdown = list()
  field_order = c()

  questions = list()
  train = list()
  test = list()

  fields = list(...)
  for (field in fields) {

    if (field$type %in% c('text','image','markdown')) {
      if (field$name %in% field_order) stop(sprintf('Duplicate field name: %s', field$name))

      field_order = c(field_order, field$name)
      if (field$type == 'text')
        text[[length(text) + 1]] = field
      if (field$type == 'image')
        image[[length(image) + 1]] = field
      if (field$type == 'markdown')
        markdown[[length(markdown) + 1]] = field
    }

    if (field$type == 'question')
      questions[[length(questions) + 1]] = field
    if (field$type == 'train')
      train[[length(train) + 1]] = field
    if (field$type == 'test')
      test[[length(test) + 1]] = field

  }

  list(text=text,
       image=image,
       markdown=markdown,
       field_order=field_order,
       questions=questions,
       train=train,
       test=test)
}



#' Set text content
#'
#' @param name       The name of the field. Must be unique within a unit.
#' @param value      The content of the field. Can be given as a single string, the name of a column in data (for create_units), or any expression.
#' @param before     The text can have a context before and after the coding unit. For example, the data.frame could be a keyword in context listing with the columns "pre", "keyword" and "post" (see for instance quanteda's kwic function).
#'                    These could then be set to the "before", "column" and "after" arguments, respectively. NOTE that if a before or after context is specified, all other text fields before or after
#'                    the current will also be considered context.
#' @param after      See 'before' argument
#' @param label      An expression or character value to label the text field. Coders will then see this label where this field starts.
#' @param ...        Style settings, passed to \code{\link{style}}
#' @param offset     An expression (most likely a column). If the text is a part of a bigger, original text, you can include the offset for the character position where it starts. This is relevant if you want to import
#'                   or export span annotations for which the offset refers to the original text.
#'
#' @return Only meant to be used inside of \code{\link{create_units}} or \code{\link{create_unit}}.
#' @export
#'
#' @examples
set_text <- function(name, value, before=NULL, after=NULL, label=NULL, ..., offset=0) {
  list(type = 'text',
       name=name,
       value=substitute(value),
       context_before= substitute(before),
       context_after = substitute(after),
       offset = substitute(offset),
       label=substitute(label),
       style = style(...))
}

#' Set image content
#'
#' @param name       The name of the field. Must be unique within a unit.
#' @param value      The filename of the image. Can be given as a single filename, the name of a column in data (for create_units) that has filenames, or an expression.
#' @param base64     If TRUE, store the image as a base64 in the codingjob json file
#' @param caption    The image caption. Can be a single string, or a column in data (for create_units), or an expression.
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return Only meant to be used inside of \code{\link{create_units}} or \code{\link{create_unit}}.
#' @export
#'
#' @examples
set_image <- function(name, value, base64=FALSE, caption=NULL, ...) {
  l = list(type = 'image',
           name = name,
           value = substitute(value),
           base64 = base64,
           style = style(...))
  caption = substitute(caption)
  if (!is.null(caption)) l$caption = caption
  l
}

#' Set markdown content
#'
#' @param field      The content of the field. Can be given as a single string, the name of a column in data (for create_units), or any expression.
#' @param ...        Style settings, passed to \code{\link{style}}
#'
#' @return Only meant to be used inside of \code{\link{create_units}} or \code{\link{create_unit}}.
#' @export
#'
#' @examples
set_markdown <- function(name, value, ...) {
  list(type='markdown',
       name = name,
       value = substitute(value),
       style = style(...))
}


#' Create a unit specific codebook
#'
#' Codebooks can be defined at different levels: codingjob > jobset > unit. The most specific codebook will be used.
#' This allows creating special units that have their own codebook (e.g., for survey-like questions), or using codebooks
#' with dynamic codes.
#'
#' @param name      A character value indicating the name of the question. This will also be the variable name in annotations. Coders won't see this name.
#' @param question_txt  The question text. Can either be a character value, or an expression (e.g., to use a column in the data)
#' @param codes     The codes that the coder can choose from. See \code{\link{question}} for more details. Note that with set_question you can refer
#'                  to columns in the data to create dynamic questions.
#' @param ...       Other arguments passed to \code{\link{question}}
#'
#' @return Only meant to be used inside of \code{\link{create_units}} or \code{\link{create_unit}}.
#' @export
#'
#' @examples
set_question <- function(name, question=NULL, codes=NULL, ...) {
  list(type = 'question',
       name = name,
       question = substitute(question),
       codes = substitute(codes),
       ell = list(...))
}

#' Set training units
#'
#' Setup training units, and provide the correct answers/annotations for these units.
#' If the answer/annotation is wrong, coders will see a message and need to retry.
#' Note that which units are train units needs to be indicated with the 'type' argument in \code{\link{create_units}}
#'
#' @param variable  The name of the variable as used in the codebook. If not specified, the name of the column will be used.
#' @param value     The value to which the given answer will be compared. Either a single string or the name of a column (in create_units).
#' @param field     Optionally, the name of a field (in case of a field specific annotation).
#' @param message     A markdown string that will be displayed when the given answer does not match value. If not given, the message will be:
#'                    \code{"### You gave an incorrect answer.\n\nThis is a **training** unit. \nPlease have another look and select a different answer"}.
#' @param submessage  An additional unit-specific message to display beneath the general message.
#'                    This argument takes an expression, so you can refer to a column in the data (in create_units),
#'                    and use other columns to create a custom message.
#' @param damage   The amount of damage a coder should receive. Can be a number or an expression that returns a number.
#' @param operator How should the annotation value be compared to the column value? Default is "==" (equals). Alternatives are "!=" (not equals),
#'                 "<=", "<", ">=" or ">".
#'
#' @return
#' @export
#'
#' @examples
set_train <- function(variable, value, field=NULL, message=NULL, submessage=NULL, damage=0, operator='==') {
  if (!operator %in% c('==','<=','<','>=','>','!=')) stop("invalid operator. Has to be one of: '==','<=','<','>=','>','!='")

  list(type='train',
       variable=variable, value=substitute(value), field=field, operator=operator,
       damage=substitute(damage),
       message=message,
       submessage=substitute(submessage))
}

#' Set test units
#'
#' Setup test units (also known as gold units).
#' If the answer/annotation is wrong, coders receive damage.
#' Note that which units are test units needs to be indicated with the 'type' argument in \code{\link{create_units}}
#'
#' @param variable  The name of the variable as used in the codebook. If not specified, the name of the column will be used.
#' @param value     The value to which the given answer will be compared. Either a single string or the name of a column (in create_units).
#' @param field     Optionally, the name of a field (in case of a field specific annotation).
#' @param damage    The amount of damage a coder should receive. Can be a number or an expression that returns a number.
#' @param on_wrong  A markdown string that will be displayed when a coder gives an incorrect answer. If not given, no message will be displayed.
#'                  Can also be the name of a column (in create_units) for unit specific messages.
#' @param operator How should the annotation value be compared to the column value? Default is "==" (equals). Alternatives are "!=" (not equals),
#'                 "<=", "<", ">=" or ">".
#'
#' @return
#' @export
#'
#' @examples
set_test <- function(variable, value, field=NULL, damage=0, on_wrong=NULL, operator='==') {
  if (!operator %in% c('==','<=','<','>=','>','!=')) stop("invalid operator. Has to be one of: '==','<=','<','>=','>','!='")

  list(type='test',
       variable=variable, value=substitute(value), field=field, operator=operator,
       damage=substitute(damage), on_wrong=substitute(on_wrong))
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

unique_in_groups <- function(ids, values) {
  n_unique_values = nrow(unique(data.frame(id=ids, value=values)))
  n_unique_values == length(unique(ids))
}


create_meta_fields <- function(rowdict, meta_cols) {
  labels = names(meta_cols)
  values = as.character(meta_cols)
  lapply(seq_along(values), function(i) {
    meta_field = list(name = values[i], value=rowdict[[values[i]]])
    if (!is.null(names(meta_cols))) meta_field$label = names(meta_cols)[i]
    meta_field
  })
}

create_questions <- function(rowdict, questions) {
  codebook_items = list(q)

  codebook = list()
  for (i in seq_along(questions)) {
    ci = questions[[i]]

    args = list(
      name = ci$name,
      question = eval_value(rowdict, ci$question),
      codes = eval_value(rowdict, ci$codes)
    )

    codebook[[length(codebook)+1]] = do.call(question, args = c(args, ci$ell))
  }
  if (length(codebook) == 0) return(NULL)
  do.call(create_codebook, args=codebook)
}

create_variables <- function(rowdict, variable_cols) {
  l = list()
  for (vc in variable_cols) {
    l[[vc]] = rowdict[[vc]]
  }
  l
}

create_content_fields <- function(rows, field_cols, subfields) {
  fields = list()
  for (f in field_cols) {
    is_subfield = f$name %in% subfields
    for (i in 1:nrow(rows)) {
      if (!is_subfield && i > 1) break
      rowdict = rows[i,,drop=F]

      field = list(name= if (is_subfield) paste(f$name, i, sep='.') else f$name,
                   value = eval_value(rowdict, f$value))

      style = eval_style(rowdict, f$style)
      if (length(style) > 0) field$style = style

      for (attr in names(f)) {
        if (attr %in% c('name','value','style')) next
        field[[attr]] = eval_value(rowdict, f[[attr]])
      }
      fields[[length(fields) + 1]] = field
    }
  }
  fields
}



create_conditionals <- function(rows, conditional_settings, subfields) {
  conditionals = list()
  for (i in seq_along(conditional_settings)) {
    cs = conditional_settings[[i]]
    for_subfield = if (is.null(cs$field)) FALSE else cs$field %in% subfields
    for (row_i in 1:nrow(rows)) {
      if (!for_subfield && row_i > 1) break

      rowdict = rows[row_i,,drop=F]
      con_i = length(conditionals) + 1
      conditionals[[con_i]] = list(variable = cs$variable,
                               conditions = list(list(value = eval_value(rowdict, cs$value),
                                                      operator=cs$operator)),
                               damage = eval_value(rowdict, cs$damage),
                               message= cs$message,
                               submessage = eval_value(rowdict, cs$submessage))

      if (!is.null(cs$field)) {
        field = cs$field
        if (field %in% subfields) field = paste(field, row_i, sep='.')
        conditionals[[con_i]]$field = field
      }
      if (!is.null(cs$submessage)) conditionals[[con_i]]$conditions[[1]]$submessage = eval_value(rowdict, cs$submessage)

    }
  }
  if (length(conditionals) == 0) return(NULL)
  conditionals
}

create_imported_annotations <- function(ann) {
  if (is.null(ann) || nrow(ann) == 0) return(NULL)
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

create_field_grid <- function(grid_areas, grid_cols, content_order) {
  if (is.null(grid_areas)) {
    grid_areas = as.list(content_order)
  } else {
    used_fields = unique(unlist(grid_areas))
    missing = used_fields[!used_fields %in% content_order]
    if (length(missing) > 0) {
      stop(sprintf('Invalid field names used in grid_areas (%s)', paste(missing, collapse=',')))
    }
  }

  ## use list to make sure json sees it as array of arrays (not array of strings)
  grid_areas = lapply(grid_areas, as.list)
  grid = list(areas = grid_areas)
  if (!is.null(grid_cols)) grid$columns = as.list(grid_cols)
  grid
}

expand_grid <- function(grid, n_rows, subfields) {
  if (is.null(subfields) || length(subfields) == 0) return(grid)
  newareas = list()
  for (row in grid$areas) {
    has_subfield = sapply(row, `%in%`, subfields)
    if (!any(has_subfield)) {
      newareas[[length(newareas) + 1]] = row
      next
    }
    for (i in 1:n_rows) {
      newrow = row
      newrow[has_subfield] = paste(newrow[has_subfield], i, sep='.')
      newareas[[length(newareas) + 1]] = newrow
    }
  }
  grid$areas = newareas
  grid
}

eval_style <- function(rowdict, style) {
  CSS_style = list()
  for (property in names(style)) {
    value = eval(style[[property]], envir = rowdict)
    if (is.null(value)) next

    if (property == 'text_size') {
      CSS_style$fontSize = paste0(value, 'em')
      next
    }
    if (property == 'bold' && value == TRUE) {
      CSS_style$fontWeight = 'bold'
      next
    }
    if (property == 'italic' && value == TRUE) {
      CSS_style$fontStyle = 'italic'
      next
    }
    if (property == 'align') {
      CSS_style$textAlign = value
      next
    }
    CSS_style[[property]] = value
  }
  CSS_style
}

eval_value <- function(rowdict, e) {
  ## evaluate the expression using only the environment of the row dictionary
  #eval(e, envir = rowdict, enclos = NULL)
  ## evaluate also allowing objects from the global env
  eval(e, envir = rowdict)
}

