prepare_units <- function(createUnitsBundle) {
  d = createUnitsBundle$df

  # if (is.null(annotations) || nrow(annotations) == 0) {
  #   ann_list = NULL
  # } else {
  #   ann_list = split(annotations, annotations$id)
  #   ann_list = ann_list[match(d[[createUnitsBundle$id]], names(ann_list))]
  # }

  units = vector('list', nrow(d))
  for (i in 1:nrow(d)) {
    rowdict = as.list(d[i,])
    id = rowdict[[createUnitsBundle$id]]
    text_fields = create_text_fields(rowdict, createUnitsBundle$text)
    meta_fields = create_meta_fields(rowdict, createUnitsBundle$meta)
    image_fields = create_image_fields(rowdict, createUnitsBundle$image)
    markdown_field = create_markdown_field(rowdict, createUnitsBundle$markdown)
    variables = create_variables(rowdict, createUnitsBundle$variables)

    conditions = NULL
    if (rowdict$.TYPE == 'train') conditions = create_conditions(rowdict, createUnitsBundle$train)
    if (rowdict$.TYPE == 'test') conditions = create_conditions(rowdict, createUnitsBundle$test)
    #importedAnnotations = create_imported_annotations(ann_list[[i]])

    ## to do: add "gold". Then when creating coding job check if gold answers match with questions
    units[[i]] = list(id = jsonlite::unbox(id),
                      type = rowdict$.TYPE,
                      unit = list(text_fields=text_fields,
                                  meta_fields=meta_fields,
                                  image_fields=image_fields,
                                  markdown_field=markdown_field,
                                  variables=variables))

    if (!is.null(conditions)) units[[i]]$conditions = conditions
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
    image_field = list(name = rf$field, value=rowdict[[rf$field]], style=rf$style)
    if (!is.null(rf$caption)) image_field$caption = rf$caption
    image_field
  })
}

create_markdown_field <- function(rowdict, markdown_cols) {
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

create_conditions <- function(rowdict, condition_settings) {
  conditions = list()
  for (i in seq_along(condition_settings)) {
    cs = condition_settings[[i]]
    conditions[[i]] = list(variable = cs$column,
                           value=rowdict[[cs$column]],
                           damage=cs$damage,
                           operator=cs$operator)
    if (!is.null(cs$message)) conditions[[i]]$message = rowdict[[cs$message]]
  }
  if (length(conditions) == 0) return(NULL)
  conditions
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
