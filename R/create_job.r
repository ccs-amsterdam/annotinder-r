#' Create a CCS Annotator codingjob
#'
#' @param title     A character string, for the title of the codingjob
#' @param units     A codingjobUnits object, as created with \code{\link{create_units}}
#' @param codebook  A codebook object, as created with \code{\link{create_codebook}}
#'
#' @return   A codingjob object
#' @export
#'
#' @examples
create_job <- function(title, units, codebook) {
  cj_package = list(title=jsonlite::unbox(title),
                    provenance=list(),
                    codebook=codebook,
                    units=prepare_units(units),
                    rules= list(ruleset = jsonlite::unbox('crowdcoding')))
  structure(cj_package, class=c('codingjob', 'list'))
}




function() {


  codebook = create_codebook('annotate',
                  codebook_variable("sentiment", "Assign sentiment to words", codes(
                   code('negative', color='red'),
                   code('neutral', color='grey'),
                   code('positive', color='green')
                  )))

  d = data.frame(id=c(1,2),
                title = c('title 1', 'title 2'),
                text=c('text 1','text 2'),
                pre=c('<','<'), post=c('>','>'),
                date=c('2010','2020'),
                topic=c('a','b'))

  units =  create_units(d, id='id', meta='date', variables='topic',
      text_field('title', bold=T, size = 2),
      text_field('text', context_before = 'pre', context_after = 'post')
  )


  create_job('newjob', units, codebook)
  #backend_connect('http://localhost:5000/', 'test@user.com')
}
