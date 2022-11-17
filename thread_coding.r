library(annotinder)
library(tidyverse)

tweets = read_csv('~/Downloads/klimaatgesprekken-clean.csv') %>%
  mutate(type = ifelse(tweetorder == 0, 'tweet','comment'),
         author = ifelse(tweetorder %% 2 == 0, 'Author','Comment')) %>%
  arrange(conversationid, tweetorder) %>%
  select(conversation = conversationid, type, author, tweet=text) %>%
  mutate(tweet = gsub('@USER','',tweet)) %>%
  mutate(tweet = gsub('\n+',' ', tweet)) %>%
  mutate(tweet = stringi::stri_trim(tweet))


first_tweet = tweets %>% filter(type == 'tweet') %>% select(conversation, tweet)
comments = tweets %>% filter(type == 'comment') %>% select(conversation, comment = tweet, author)
d = first_tweet %>% full_join(comments, by='conversation')
d

units = create_units(d, id='conversation', subfields='comment',
  set_markdown('tweet', tweet,
           border= '5px solid grey',
           borderColor = 'teal',
           borderRadius = '10px',
           margin = '10px 25% 10px 10px'),
  set_markdown('comment', comment,
             align= 'justify',
             border= '5px solid grey',
             borderColor=ifelse(author == 'Author', 'teal', 'maroon'),
             borderRadius = '10px',
             margin = ifelse(author == 'Author', '10px 25% 10px 10px', '10px 10px 10px 25%'))
)


codebook = create_codebook(
  test = question('klimaatverandering', 'Is het eerste bericht in deze conversatie blablabla?', type='buttons',
            field='tweet',
            codes = c('Nee','Ja')),
  dit = question('reactie', 'Wat is de toon van deze reactie?', type='buttons',
            per_field = 'comment',
            codes = c('Boos','Blij','Irritant'))
)

create_job('Thread coding', units, codebook) %>%
  create_job_db(overwrite=T) %>%
  start_annotator(background = T)



x = prepare_units(units)

jsonlite::toJSON(x[[1]], pretty = T)
names(x[[1]]$unit$text_fields[[2]]$value) = NULL
