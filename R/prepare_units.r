prepare_units <- function(createUnitsBundle) {
  d = createUnitsBundle$df
  ids = d[[createUnitsBundle$id]]

  groups = split(1:nrow(d), ids)
  unique_ids = unique(ids)

  units = vector('list', length(unique_ids))
  for (i in 1:length(unique_ids)) {
    id = unique_ids[i]
    rows = d[groups[[id]],]
    firstrow = rows[1,]  ## for values that should be unique anyway

    meta_fields = create_meta_fields(rows[1,], createUnitsBundle$meta)
    variables = create_variables(rows[1,], createUnitsBundle$variables)

    subfields = createUnitsBundle$subfields
    text_fields = create_text_fields(rows, createUnitsBundle$text, subfields)
    image_fields = create_image_fields(rows, createUnitsBundle$image, subfields)
    markdown_fields = create_markdown_fields(rows, createUnitsBundle$markdown, subfields)
    grid = create_field_grid(createUnitsBundle$grid, createUnitsBundle$content_order)

    conditionals = NULL
    if (firstrow$.TYPE == 'train') conditionals = create_conditionals(rows, createUnitsBundle$train, subfields)
    if (firstrow$.TYPE == 'test') conditionals = create_conditionals(rows, createUnitsBundle$test, subfields)
    #importedAnnotations = create_imported_annotations(ann_list[[i]])

    units[[i]] = list(id = id,
                      type = firstrow$.TYPE,
                      unit = list(text_fields=text_fields,
                                  meta_fields=meta_fields,
                                  image_fields=image_fields,
                                  markdown_fields=markdown_fields,
                                  variables=variables))

    if (!is.null(firstrow$.POSITION) && !is.na(firstrow$.POSITION)) units[[i]]$position = firstrow$.POSITION
    if (!is.null(conditionals)) units[[i]]$conditionals = conditionals
    if (!is.null(grid)) units[[i]]$unit$grid = grid
    #if (!is.null(importedAnnotations)) units[[i]]$unit$importedAnnotations = importedAnnotations
  }

  structure(units, class=c('codingjobUnits', 'list'))
}

create_meta_fields <- function(rowdict, meta_cols) {
  lapply(seq_along(meta_cols), function(i) {
    mf = meta_cols[[i]]
    meta_field = list(name = mf$field, value=rowdict[[mf$field]])
    meta_field$style = eval_style(rowdict, mf$style)

    if (!is.null(mf$label)) meta_field$label = mf$label
    meta_field
  })
}

create_variables <- function(rowdict, variable_cols) {
  l = list()
  for (vc in variable_cols) {
    l[[vc]] = rowdict[[vc]]
  }
  l
}

create_text_fields <- function(rows, text_cols, subfields) {
  lapply(seq_along(text_cols), function(i) {
    tf = text_cols[[i]]

    text_field = list(name=tf$field)
    firstrow = rows[1,]
    if (!is.null(tf$offset)) text_field$offset = eval(tf$offset, envir = firstrow)
    if (!is.null(tf$label)) text_field$label = eval(tf$label, envir = firstrow)
    if (!is.null(tf$context_before)) text_field$context_before = firstrow[[tf$context_before]]
    if (!is.null(tf$context_after)) text_field$context_after = firstrow[[tf$context_after]]

    if (text_field$name %in% subfields || !is.null(tf$split)) {
      text_field$value = list()
      for (row_i in 1:nrow(rows)) {
        values = rows[[tf$coding_unit]][row_i]
        if (!is.null(tf$split)) values = stringi::stri_split(values, fixed = split)[[1]]
        offset = c(0, nchar(values)[-length(values)])

        for (value_i in 1:length(values)) {
          text_field$value[['']] = list(
            value = values[value_i],
            offset = if (value_i == 1) 0 else nchar(values[value_i-1]),
            style = eval_style(rows[row_i,], tf$style)
          )
        }
      }
      names(text_field$value) = NULL  ## otherwise jsonlite will think its an object
    } else {
      row = rows[1,]
      text_field$value = row[[tf$coding_unit]]
      text_field$style = eval_style(row, tf$style)
    }

    text_field
  })
}


create_image_fields <- function(rows, image_cols, subfields) {
  lapply(seq_along(image_cols), function(i) {
    rf = image_cols[[i]]

    image_field = list(name = rf$field, base64=rf$base64)

    if (image_field$name %in% subfields) {
      image_field$value = list()
      for (row_i in 1:nrow(rows)) {
        rowdict = rows[row_i,]
        value = list (
          value = rowdict[[rf$field]],
          style = eval_style(rowdict, rf$style)
        )
        if (!is.null(rf$caption)) value$caption = eval(rf$caption, rowdict)
        image_field$value[['']] = value
      }
      names(image_field$value) = NULL  # otherwise jsonlite will think its an object
    } else {
      row = rows[1,]
      image_field$value = row[[rf$field]]
      image_field$style = eval_style(row, rf$style)
      if (!is.null(rf$caption)) image_field$caption = eval(rf$caption, row)
    }

    image_field
  })
}

create_markdown_fields <- function(rows, markdown_cols, subfields) {
  lapply(seq_along(markdown_cols), function(i) {
    mf = markdown_cols[[i]]

    markdown_field = list(name = mf$field)
    if (markdown_field$name %in% subfields || !is.null(mf$split)) {
      markdown_field$value = list()
      for (row_i in 1:nrow(rows)) {
        values = rows[[mf$field]][row_i]
        if (!is.null(mf$split)) values = stringi::stri_split(values, fixed = split)[[1]]
        for (value in values) {
          text_field$value[['']] = list(
            value = value,
            style = eval_style(rows[row_i,], mf$style)
          )
        }
      }
      names(markdown_field$value) = NULL # otherwise jsonlite will think its an object
    } else {
      row = rows[1,]
      markdown_field$value = row[[mf$field]]
      markdown_field$style = eval_style(row, mf$style)
    }
    markdown_field
  })
}


create_conditionals <- function(rows, conditional_settings, subfields) {
  conditionals = list()
  for (i in seq_along(conditional_settings)) {
    cs = conditional_settings[[i]]
    for (row_i in 1:nrow(rows)) {
      rowdict = rows[row_i,]
      conditionals[[i]] = list(variable = cs$column,
                               conditions = list(list(value = rowdict[[cs$column]],
                                                      operator=cs$operator)),
                               damage = eval(cs$damage, rowdict),
                               message= cs$message, rowdict)

      if (!is.null(conditional_settings$field)) {
        field = rowdict[[conditional_settings$field]]
        if (cs$column %in% subfields) field = paste(field, row_i, sep='.')
        conditionals[[i]]$field = field
      }
      if (!is.null(cs$message)) conditionals[[i]]$conditions[[1]]$submessage = eval(cs$submessge, rowdict)
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

create_field_grid <- function(grid, content_order) {
  if (is.null(grid) && length(content_order) > 1) {
    grid = list(areas=as.list(content_order))
  }
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


#' S3 print method for codingjobUnits objects
#'
#' @param x an codingjobUnits object, created with \link{create_units}
#' @param ... not used
#'
#' @method print codingjobUnits
#' @examples
#' @export
print.codingjobUnits <- function(x, ...){
  cat('List of', length(x), 'units\n')
}
