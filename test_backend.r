library(ccsAnnotator)
library(dplyr)

data = data.frame(id = c(1,2,3,4,5),
                   text= c('I like cats.',
                           "Cats are awesome.",
                           "Some people like dogs.",
                           "Dogs are pretty awesome too.",
                           "Other people like cars"),
                   animal=c('Cat',NA,'Dog',NA, 'Neither :('))

units = data %>%
  filter(is.na(animal)) create_units(data, id='id', text='text', gold='animal')

u = prepare_units(units, NULL)
print.codingjobUnits <- function(x) print('wtf')


length(u)

as_tibble(u)

animal = question('animal', 'What animal is this text about?',
                  codes=c('Dog','Cat','Neither :('))
codebook = create_codebook(animal)

pre = create_question_unit(markdown='# hi\n\nThis is a pre question',
                           question('pre', 'Is this a pre question?', codes=c('Think so!', 'Must be true, yeah')))
post = create_question_unit(markdown='# hi\n\nThis is a post question',
                           question('pre', 'Is this a post question?', codes=c('Yep!', 'I think so yes')))

codingjob = create_job('test', units, codebook=codebook, pre=pre, post=post, train=gold_units)

create_gold_rules <- function(train_n, which='last', )

pre = set_special_id(pre, 'pre')
post = set_special_id(post, 'post')
codingjob$units = c(pre, codingjob$units, post)

sapply(codingjob$units, function(x) is.null(x[['gold']]))

job$units

codingjob = create_job(title, units, create_codebook(animal))


backend_connect('https://kasperwelbers.com/annotator', 'test@user.com')

upload_job('Swipe right! adjusted',
           units=create_units(d, id='id', image='stimulus'),
           codebook=create_codebook(animal),
           jobsets=jobsets,
           debrief=debrief('Bedankt voor het deelnemen aan dit onderzoek!'),
           pre=list(intro, trust, social, nieuws, insta_nieuws, swipeinfo),
           post=list(post, afsluiting))
