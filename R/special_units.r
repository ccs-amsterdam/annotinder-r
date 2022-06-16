#' Create a special unit for asking questions
#'
#' Sometimes you don't want to ask a coder about a unit, but rather about themselves, or something.
#' For instance, to include some basic survey questions at the start of a coding set.
#'
#' @param ...     Questions to ask, created with \code{\link{question}}. Can be multiple (just as in \code{\link{create_codebook}}.
#' @param markdown You can add a unit above the question by providing a markdown string. If empty. the coder will only see the question, See \url{https://commonmark.org/help/} for help.
#' @param text_window_size The annotation interface has two parts: text (on top) and the answer form (at the bottom). By default, the text window size is about 70 (70%),
#'                         and users can change the size themselves (because the best size might differ depending on device). Here you can optionally set a fixed alternative.
#'                         This can for instance be useful if you have a question with big or many buttons, in which case you could set the text window size to something small.
#'                         Can also be set to 'auto' so the annotator tries a nice fi (but be sure to test beforehand whether this makes sense for your codebook)
#'
#' @return
#' @export
#'
#' @examples
create_question_unit <- function(..., markdown=NULL, text_window_size=NULL) {
  if (is.null(title) && is.null(text) && is.null(markdown)) stop('Either title+text or markdown need to be given')
  if (is.null(markdown)) {
    if (is.null(title)) title = ''
    if (is.null(text)) text = ''
    units = create_units(data.frame(id=NA, title=title, text=text), id='id', text=text_fields(
      text_field('title', bold=T, center=T, size='1.4'),
      text_field('text')
    ))
  } else {
    units = create_units(data.frame(id=NA, markdown=markdown), id='id', markdown='markdown')
  }
  units = prepare_units(units, NULL)
  unit = units[[1]]
  unit$unit$codebook = create_codebook(...)
  if (!is.null(text_window_size)) {
    if (text_window_size != 'auto') {
      if (!class(text_window_size) == 'numeric') stop('text_window_size must be numeric or "auto"')
      if (text_window_size < 0 || text_window_size > 80) stop('text window size must be a value between 0 and 80')
    }
    unit$unit$text_window_size = text_window_size
  }
  unit
}

#' Create a special unit for informing coders
#'
#' @param id      A unique id for the unit
#' @param title   A title
#' @param text    The information text
#' @param markdown Alternatively, provide the content as a markdown string. If used, title+text are ignored.
#'                 See \url{https://commonmark.org/help/} for help.
#' @param button  The text to show on the button
#'
#' @return
#' @export
#'
#' @examples
create_info_unit <- function(id, title=NULL, text=NULL, markdown=NULL, button="Continue") {
  if (is.null(title) && is.null(text) && is.null(markdown)) stop('Either title+text or markdown need to be given')
  if (is.null(markdown)) {
    if (is.null(title)) title = ''
    if (is.null(text)) text = ''
    units = create_units(data.frame(id=id, title=title, text=text), id='id', text=text_fields(
      text_field('title', bold=T, center=T, size='1.4'),
      text_field('text')
    ))
  } else {
    units = create_units(data.frame(id=id, markdown=markdown), id='id', markdown='markdown')
  }
  units = prepare_units(units, NULL)
  unit = units[[1]]

  unit$unit$codebook = list(type='questions', questions=list(list(name='confirm', type='confirm', button=button)))
  unit
}
