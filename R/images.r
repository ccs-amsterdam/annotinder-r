function() {
  png_files = list.files('~/projects/images', pattern='\\.png', full.names = T)
  dplyr::tibble(
    filename = gsub('.*/','',png_files),
    base64 = sapply(png_files, base64enc::base64encode)
  )

x = base64enc::base64encode("~/projects/images/2ekamer.png")


library(annotinder)
sentiment = question('sentiment', 'What is the sentiment of this image?',
                                codes = c(Negative = 'red', Neutral = 'grey', Positive = 'green'))

d = data.frame(id=1:2, image = list.files('~/projects/images', full.names = T))

codingjob = create_job('Image sentiment',
                       create_units(d, id='id', image='image'),
                       create_codebook(sentiment))

jsonlite::write_json(codingjob$units, '~/projects/ccs-annotator-client/public/units/images.json')


backend_connect('https://amcat4.labs.vu.nl/api/annotator', 'test@user.com')
upload_job(codingjob)

}


