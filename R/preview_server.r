# Enable CORS Filtering
# see: https://github.com/rstudio/plumber/issues/66#issuecomment-845739601
#' @filter cors
cors <- function(req, res) {
  safe_domains <- c("*")

  if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason

    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
}

### state
annotations = list()

### helper functions
is_coded <- function(codingjob) {
  sapply(1:length(codingjob$units), function(i) {
    if (i > length(annotations)) return(FALSE)
    if (is.null(annotations[[i]])) return(FALSE)
    annotations[[i]]$status != 'IN_PROGRESS'
  })
}


### GET

#*
#* @param x ...
#* @get /login
function(res, req) {
  headers = req$HEADERS
  auth = headers['authorization']
  if (is.null(auth) || auth != paste('Bearer', token)) {
    res$status = 401
    list(error = "Invalid token")
  } else {
    list(email='', is_admin=jsonlite::unbox(F), jobs=list())
  }
}

#*
#* @param x ...
#* @get /codingjob/<job_id>/codebook
function(job_id) {
  codingjob$codebook
}

#*
#* @param x ...
#* @get /codingjob/<job_id>/progress
function(job_id) {
  n_coded = sum(is_coded(codingjob))
  l = list(n_coded = n_coded, n_total=length(codingjob$units),
       seek_forwards=F, seek_backwards=T)
  lapply(l, jsonlite::unbox)
}

#*
#* @param x ...
#* @get /codingjob/<job_id>/unit
function(res, req, job_id) {
  suppressWarnings({
    index = as.numeric(req$argsQuery[['index']])
  })


  if (length(index) == 0 || is.na(index)) {
    coded = is_coded(codingjob)
    index = if (all(coded)) Inf else which(!coded)[1]
  } else index = index + 1  # (index starts at 0)

  if (index > length(codingjob$units)) {
    res$status = 404
    return(list(error = "no units left"))
  }

  index = min(index, length(codingjob$units))
  unit = codingjob$units[[index]]
  unit$id = jsonlite::unbox(index)

  if (index <= length(annotations) && !is.null(annotations[[index]])) {
    unit$status = annotations[[index]]$status
    unit$annotation = annotations[[index]]$annotation
  }

  unit
}

#*
#* @param x ...
#* @get /codingjob/<job_id>
function(job_id) {
  list()
}


### POST

# #*
# #* @param x ...
# #* @post /codingjob
# function() {
#   body = req$argsBody
#   body
# }

#*
#* @param x ...
#* @post /codingjob/<job_id>/unit/<unit_id>/annotation
function(req, job_id, unit_id) {
  body = req$argsBody
  unit_id = as.numeric(unit_id)
  if (length(annotations) == 0) annotations <<- vector('list', length(codingjob$units))
  annotations[[unit_id]] <<- body
}

