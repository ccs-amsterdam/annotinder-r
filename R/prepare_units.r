prepare_units <- function(createUnitsBundle) {
  d = createUnitsBundle$df

  units = vector('list', nrow(d))
  for (i in 1:nrow(d)) {
    rowdict = as.list(d[i,])
    id = rowdict[[createUnitsBundle$id]]
    text_fields = create_text_fields(rowdict, createUnitsBundle$text)
    meta_fields = create_meta_fields(rowdict, createUnitsBundle$meta)
    image_fields = create_image_fields(rowdict, createUnitsBundle$image)
    markdown_fields = create_markdown_fields(rowdict, createUnitsBundle$markdown)
    grid = create_field_grid(createUnitsBundle$grid, createUnitsBundle$content_order)
    variables = create_variables(rowdict, createUnitsBundle$variables)

    conditionals = NULL
    if (rowdict$.TYPE == 'train') conditionals = create_conditionals(rowdict, createUnitsBundle$train)
    if (rowdict$.TYPE == 'test') conditionals = create_conditionals(rowdict, createUnitsBundle$test)
    #importedAnnotations = create_imported_annotations(ann_list[[i]])

    units[[i]] = list(id = jsonlite::unbox(id),
                      type = rowdict$.TYPE,
                      unit = list(text_fields=text_fields,
                                  meta_fields=meta_fields,
                                  image_fields=image_fields,
                                  markdown_fields=markdown_fields,
                                  variables=variables))

    if (!is.null(rowdict$.POSITION) && !is.na(rowdict$.POSITION)) units[[i]]$position = rowdict$.POSITION
    if (!is.null(conditionals)) units[[i]]$conditionals = conditionals
    if (!is.null(grid)) units[[i]]$unit$grid = grid
    #if (!is.null(importedAnnotations)) units[[i]]$unit$importedAnnotations = importedAnnotations

  }

  structure(units, class=c('codingjobUnits', 'list'))
}

create_text_fields <- function(rowdict, text_cols) {
  lapply(seq_along(text_cols), function(i) {
    tf = text_cols[[i]]
    text = if (is.null(tf$coding_unit)) '' else rowdict[[tf$coding_unit]]
    text_field = list(name=tf$field, value=text, style=tf$style)
    if (!is.null(tf$label)) text_field$label = tf$label
    if (!is.null(tf$context_before)) text_field$context_before = rowdict[[tf$context_before]]
    if (!is.null(tf$context_after)) text_field$context_after = rowdict[[tf$context_after]]
    text_field
  })
}

create_meta_fields <- function(rowdict, meta_cols) {
  lapply(seq_along(meta_cols), function(i) {
    mf = meta_cols[[i]]
    meta_field = list(name = mf$field, value=rowdict[[mf$field]], style=mf$style)
    if (!is.null(mf$label)) meta_field$label = mf$label
    meta_field
  })
}

create_image_fields <- function(rowdict, image_cols) {
  lapply(seq_along(image_cols), function(i) {
    rf = image_cols[[i]]
    image_field = list(name = rf$field, value=rowdict[[rf$field]], base64=rf$base64, style=rf$style)
    if (!is.null(rf$caption)) image_field$caption = rowdict[[rf$caption]]
    image_field
  })
}

create_markdown_fields <- function(rowdict, markdown_cols) {
  lapply(seq_along(markdown_cols), function(i) {
    mf = markdown_cols[[i]]
    markdown_field = list(name = mf$field, value=rowdict[[mf$field]], style=mf$style)
    markdown_field
  })
}

create_variables <- function(rowdict, variable_cols) {
  l = list()
  for (vc in variable_cols) {
    l[[vc]] = rowdict[[vc]]
  }
  l
}

create_conditionals <- function(rowdict, conditional_settings) {
  conditionals = list()
  for (i in seq_along(conditional_settings)) {
    cs = conditional_settings[[i]]
    conditionals[[i]] = list(variable = cs$column,
                             conditions = list(list(value = rowdict[[cs$column]],
                                                    operator=cs$operator)),
                             damage=cs$damage,
                             message=cs$message)
    if (!is.null(cs$message)) conditionals[[i]]$conditions[[1]]$submessage = rowdict[[cs$submessage]]
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
  if (is.null(grid) && length(content_order) > 1) grid = content_order
  grid
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
