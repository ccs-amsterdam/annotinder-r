
<!-- README.md is generated from README.Rmd. Please edit that file -->

Here’s a quick tutorial of current features.

# Installation

Not yet on CRAN, so install from github. Note that the repository name
is “annotinder-r”, but the package name is “annotinder”.

``` r
remotes::install_github("ccs-amsterdam/annotinder-r")
library(annotinder)
```

# Getting started

We’ll first look at how to make a local codingjob (on your own device),
that you can code in R or in a browser. This is useful for two reasons:

- 1.  It let’s you annotate stuff with a nice interface.

- 2.  You can use this to test unit and codebook settings, before you
      deploy codingjobs to a server.

To create a codingjob you need to key components: A set of units and a
codebook.

## Units

To create units we use the `create_units` function. For this example we
use a set of State of the Union speech paragraphs included in this
package.

In this data we have an `id` column with a unique id. It is mandatory in
AnnoTinder to explicitly provide unique id’s for all your units, because
you’ll want to do this properly to later on link annotations back to
units.

For now, the only data we’ll include in our unit is the text column in
the mini_sotu_par data.frame. We do so by providing a named argument
that assigns a text field. In the current example this might look
overkill, but later on you’ll see why this pays off in terms of
flexibility.

``` r
units <- create_units(mini_sotu_par,
  id = "id",
  set_text("text", text)
)
```

You can check the units object, which simply tells you that this is a
list of n units (at some point we’ll add some more usefull details).

``` r
units
```

## Codebook

The codebook describes what you want to code. There are (currently) two
general modes of annotations:

- In the `questions` mode, coders will see the unit and then have to
  answer one or multiple questions.
- In the `annotation` mode, coders will be able to select specific words
  and phrases in the unit to label them.

The two modes cannot be combined in the same codebook (but you can use
multiple codebooks in a codingjob, which we’ll address later).

We’ll start with a simple codebook with a single question about
sentiment. We need to provide a unique name for the question, the
question itself as shown to the coder, and the codes that the coder can
choose from. Codes can be a simple character vector or a data.frame (for
more advanced features), and we have a nice shorthand for creating
colored codes by using a named vector.

``` r
sentiment <- question("sentiment", "What is the sentiment of this text?",
  codes = c(crimson = "Negative", grey = "Neutral", lightgreen = "Positive")
)
```

Now we can add this to the codebook.

``` r
codebook <- create_codebook(sentiment)
```

## Creating a codingjob

Now we can create a codingjob! When coding locally from R, we first
create a job object, and then create a job database using sqlite. By
default the database will be stored in your working directory.

``` r
job <- create_job("simple_example", units, codebook)
job_db <- create_job_db(job, overwrite = T)
job_db
```

As you see, job_db is simply the path to the database.

We can now start the annotator by passing a job database to
`start_annotator`. By default, this is run as a background process that
persists until the main session is closed. This way you can continue
your current session, and even directly import annotations made. The
annotator will then also run in your Viewer pane (though this is a bit
experimental).

``` r
start_annotator(job_db)
```

You can now start annotating! If you want to retrieve the annotations
from your db, simply pass the path to the database to
`gimme_annotations`.

``` r
gimme_annotations(job_db)
```

# Advanced codebooks

Now let’s look at some more interesting codebooks. We’ll use the same
simple units as above (though we’ll add some meta data, because we can).

``` r
units <- create_units(mini_sotu_par,
  id = "id", meta = c("name", "year"),
  set_text("text", text)
)
```

## Annotation mode

Let’s first look at the other annotation mode. We’ll use roughly the
same question, but this time for labeling.

``` r
sentiment <- annotation_variable("sentiment", "Select words or phrases and label their sentiment", codes = c(crimson = "Negative", gre = "Neutral", lightgreen = "Positive"))
codebook <- create_codebook(sentiment)
```

We’ll again start a server, but this time we set `overwrite=TRUE` in
`create_job_db`. This way we can just keep creating the job called
`example`. If overwrite is not set, you would get an error message
telling you that you can’t just overwrite another codingjob. Also, we’ll
use a pipe because we’re cool like that.

``` r
create_job("example", units, codebook) %>%
  create_job_db(overwrite = T) %>%
  start_annotator(background = T)
```

Now you can select words and label them! (The first unit is only 3
words, to click “Go to next unit” for a more interesting annotation
experience).

With the mouse you can do this like you would normally select text, but
it automatically snaps to words. You can also use the keyboard, which
can greatly speed up annotations once you’re used to it. Use the arrow
keys to navigate, hold spacebar to make a selection, and ctrl+Enter to
go to the next unit. In general, we strive to have every feature of
AnnoTinder supported for Mouse, Keyboard and Touch.

If you ask for annotations, you see that now you also get the offset and
length, which indicate the exact character positions of the annotation,
and the selected text.

``` r
gimme_annotations()
```

## Other question formats

There are various alternative types of questions you can ask, including
open text, searchable dropdowns (for long codebooks) and scales. We’ll
discuss scales and open text later when we get to the option to include
survey units. Let’s first focus on the AnnoTinder feature.

``` r
sentiment <- question("sentiment", "Swipe left for negative, right for positive, and up for neutral",
  type = "annotinder",
  codes = c(crimson = "Negative", grey = "Neutral", lightgreen = "Positive")
)
codebook <- create_codebook(sentiment)

create_job("example", units, codebook) %>%
  create_job_db(overwrite = T) %>%
  start_annotator(background = T)
```

This is basically the same codebook, but now you can swipe to code. In R
this isn’t very useful, but when you start deploying jobs to a server,
this works really well with mobile phone.

… more stuff to add

# Advanced units

You can actually go pretty wild in designing the units. Next to text you
can also add images and markdown. Using markdown makes it easier to add
some styling, but the downside is that you can’t use it for labeling in
annotation mode. You can also create training and testing units in which
you provide the ‘correct’ answers. Anyway, here’s an example of
combining a lot of stuff.

!! the test units don’t really make sense for a local R server, and it
will just print the amount of damage a coder would receive. When using
the Annotinder server, this damage will be processed silently and can be
used to disqualify annotators.

``` r
data <- data.frame(
  id = c(1, 2, 3, 4, 5),
  type = c("train", "code", "test", "code", "test"),
  letter = letters[1:5],
  date = c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05"),
  source = c("imagination"),
  title = c("Cat", "Cat", "Dog", "Dog", "Car"),
  text = c(
    "I like cats.",
    "Cats are awesome.",
    "Some people like dogs.",
    "Dogs are pretty awesome too.",
    "Other people like cars"
  ),
  image = c(
    "https://cdn.pixabay.com/photo/2017/07/25/01/22/cat-2536662_960_720.jpg",
    "https://cdn.pixabay.com/photo/2014/04/13/20/49/cat-323262_960_720.jpg",
    "https://cdn.pixabay.com/photo/2018/01/09/11/04/dog-3071334_960_720.jpg",
    "https://cdn.pixabay.com/photo/2017/09/25/13/14/dog-2785077_960_720.jpg",
    "https://cdn.pixabay.com/photo/2016/11/29/09/32/auto-1868726_960_720.jpg"
  ),
  caption = c("Cat!", "Caaaaaat", "Doggie!!", "Dog", "Crrr"),
  markdown = c("**useless markdown text**"),
  animal = c("Cat", NA, "Dog", NA, "Neither :("),
  animal_hint = c("Hint: look closely at those ears and paws.", NA, NA, NA, NA)
)

units <- create_units(data,
  id = "id", type = "type", meta = c("date", "source"),
  set_text("title", title, text_size = 2, bold = T, align = "center"),
  set_text("text", text, align = "center"),
  set_image("image", image, caption = caption),
  set_markdown("markdown", markdown, align = "center"),
  set_train("animal", animal,
    message = "# OH NOES!!\n\nThis was a training unit, and it seems you got it wrong!",
    submessage = animal_hint
  ),
  set_test("animal", animal, damage = 10)
)

animal <- question("animal", "What animal is this?", type = "annotinder", codes = c("Cat", "Dog", "Neither :("))
codebook <- create_codebook(animal)

create_job("example", units, codebook) %>%
  create_job_db(overwrite = T) %>%
  start_annotator(background = T)
```
