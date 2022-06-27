library(tidyverse)
library(ccsAnnotator)

words = read_csv('~/Downloads/words.csv')

units = create_units(words, id='id', text=text_fields(text_field('word', size=1.5, bold=T, center=T)))

codebook = create_codebook(
  question(name= 'immigration',
           question = "Is this word related to immigration? Swipe right, left or up!", type = 'annotinder',
           codes = c(No='crimson', Yes='lightgreen', `Skip`='yellow')))

jobsets = list(
  jobset('words1', unit_set = paste0('w', 1:40)),
  jobset('words2', unit_set = paste0('w', 21:60)),
  jobset('words3', unit_set = paste0('w', 41:80)),
  jobset('words4', unit_set = paste0('w', 61:100)),
  jobset('words5', unit_set = paste0('w', 81:120)),
  jobset('words6', unit_set = paste0('w', 100:139)),
  jobset('words7', unit_set = paste0('w', c(121:139, 1:21)))
)

deb = debrief('# Thanks for the help!\n\nIf you have any questions or comments about this tool, please reach out to [k.welbers@vu.nl](mailto:k.welbers@vu.nl)\n\n*If you want to share the love of coding, please share the QR code below!*', qr=TRUE)


md = "
# Welcome!

Thank you for helping us demo and test this new tool that we're working on.
Once you press the continue button below, you will be shown a number of words, and we want
to ask you to indicate when you think these texts are related to **immigration**.

If you think this is the case, **swipe right!** If you think this is not the case, **swipe left!**
You can also swipe up to skip an item.
"
pre = create_question_unit(id ='intro', markdown = md, question('introduction', type = 'confirm'))


create_job('test job', units = units, codebook = codebook) |>
  create_job_db(overwrite = T) |>
  start_annotator(background=T)

backend_connect('https://kasperwelbers.com/ica-annotator', 'k.welbers@vu.nl')
upload_job(title='ICA Immigration demo 2', units=units, codebook=codebook, jobsets = jobsets, debrief = deb, pre=pre)

