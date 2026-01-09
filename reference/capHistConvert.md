# Convert between capture history data.frame formats.

Use to convert between simple versions of several capture history
data.frame formats – “individual”, “frequency”, “event”, “MARK”, and
“RMark”. The primary use is to convert to the “individual” format for
use in
[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md).

## Usage

``` r
capHistConvert(
  df,
  cols2use = NULL,
  cols2ignore = NULL,
  in.type = c("frequency", "event", "individual", "MARK", "marked", "RMark"),
  out.type = c("individual", "event", "frequency", "MARK", "marked", "RMark"),
  id = NULL,
  event.ord = NULL,
  freq = NULL,
  var.lbls = NULL,
  var.lbls.pre = "event",
  include.id = ifelse(is.null(id), FALSE, TRUE)
)
```

## Arguments

- df:

  A data.frame that contains the capture histories and, perhaps, a
  unique fish identifier or frequency variable. See details.

- cols2use:

  A string or numeric vector that indicates columns in `df` to use.
  Negative numeric values will not use those columns. Cannot use both
  `cols2use` and `col2ignore`.

- cols2ignore:

  A string or numeric vector that indicates columns in `df` to ignore.
  Typical columns to ignore are those that are not either in `id=` or
  `freq=` or part of the capture history data. Cannot use both
  `cols2use` and `col2ignore`.

- in.type:

  A single string that indicates the type of capture history format to
  convert **FROM**.

- out.type:

  A single string that indicates the type of capture history format to
  convert **TO**.

- id:

  A string or numeric that indicates the column in `df` that contains
  the unique identifier for an individual fish. This argument is only
  used if `in.type="event"`, `in.type="individual"`, or, possibly,
  `in.type="RMark"`.

- event.ord:

  A string that contains a vector of ordered levels to be used when
  `in.type="event"`. The default is to order alphabetically which may
  not be desirable if, for example, the events are labeled as ‘first’,
  ‘second’, ‘third’, and ‘fourth’. In this case, use
  `event.ord=c("first","second","third","fourth")`.

- freq:

  A string or numeric that indicates the column in `df` that contains
  the frequency of individual fish corresponding to a capture history.
  This argument is only used if `in.type="MARK"`, `in.type="frequency"`,
  or, possibly, `in.type="RMark"`.

- var.lbls:

  A string vector of labels for the columns that contain the returned
  individual or frequency capture histories. If `var.lbls=NULL` or the
  length is different then the number of events then default labels
  using `var.lbls.pre` will be used. This argument is only used if
  `out.type="frequency"` or `out.type="individual"`.

- var.lbls.pre:

  A single string used as a prefix for the labels of the columns that
  contain the returned individual or frequency capture histories. This
  prefix will be appended with a number corresponding to the sample
  event. This argument is only used if `out.type="frequency"` or
  `out.type="individual"` and will be ignored if a proper vector is
  given in `var.lbls`.

- include.id:

  A logical that indicates whether a unique fish identifier
  variable/column should be included in the output data.frame. This
  argument is only used if `out.type="individual"` or
  `out.type="RMark"`.

## Value

A data frame of the proper type given in `out.type` is returned. See
details.

## Details

[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
requires capture histories to be recorded in the “individual” format. In
this format, the data frame contains (at least) as many columns as
sample events and as many rows as individually tagged fish. Optionally,
the data.frame may also contain a column with unique fish identifiers
(e.g., tag numbers). Each cell in the capture history portion of the
data.frame contains a ‘0’ if the fish of that row was NOT seen in the
event of that column and a ‘1’ if the fish of that row WAS seen in the
event of that column. For example, suppose that five fish were marked on
four sampling events; fish ‘17’ was captured on the first two events;
fish ‘18’ was captured on the first and third events; fish ‘19’ was
captured on only the third event; fish ‘20’ was captured on only the
fourth event; and fish ‘21’ was captured on the first and second events.
The “individual” capture history date.frame for these data looks like:

|      |        |        |        |        |
|------|--------|--------|--------|--------|
| fish | event1 | event2 | event3 | event4 |
| 17   | 1      | 1      | 0      | 0      |
| 18   | 1      | 0      | 1      | 0      |
| 19   | 0      | 0      | 1      | 0      |
| 20   | 0      | 0      | 0      | 1      |
| 21   | 1      | 1      | 0      | 0      |

The “frequency” format data.frame (this format is used in Rcapture) has
unique capture histories in separate columns, as in the “individual”
format, but also includes a column with the frequency of individuals
that had the capture history of that row. It will not contain a fish
identifier variable. The same data from above looks like:

|        |        |        |        |      |
|--------|--------|--------|--------|------|
| event1 | event2 | event3 | event4 | freq |
| 1      | 1      | 0      | 0      | 2    |
| 1      | 0      | 1      | 0      | 1    |
| 0      | 0      | 1      | 0      | 1    |
| 0      | 0      | 0      | 1      | 1    |

The “event” format data.frame has a column with the unique fish
identifier and a column with the event in which the fish of that row was
observed. The same data from above looks like:

|      |       |
|------|-------|
| fish | event |
| 17   | 1     |
| 18   | 1     |
| 21   | 1     |
| 17   | 2     |
| 21   | 2     |
| 18   | 3     |
| 19   | 3     |
| 20   | 4     |

MARK (http://www.phidot.org/software/mark/index.html) is the
“gold-standard” software for analyzing complex capture history
information. In the “MARK” format the 0s and 1s of the capture histories
are combined together as a string without any spaces. Thus, the “MARK”
format has the capture history strings in one column with an additional
column that contains the frequency of individuals that exhibited the
capture history of that row. The final column ends with a semi-colon.
The same data from above looks like:

|      |      |
|------|------|
| ch   | freq |
| 0001 | 1;   |
| 0010 | 1;   |
| 1010 | 1;   |
| 1100 | 2;   |

The `RMark` and marked are packages used to replace some of the
functionality of MARK or to interact with MARK. The “RMark” or “marked”
format requires the capture histories as one string (must be a character
string and called ‘ch’), as in the “MARK” format, but without the
semicolon. The data.frame may be augmented with an identifier for
individual fish OR with a frequency variable. If augmented with a unique
fish identification variable then the same data from above looks like:

|      |      |
|------|------|
| fish | ch   |
| 17   | 1100 |
| 18   | 1010 |
| 19   | 0010 |
| 20   | 0001 |
| 21   | 1100 |

However, if augmented with a frequency variable then the same data from
above looks like:

|      |      |
|------|------|
| ch   | freq |
| 0001 | 1    |
| 0010 | 1    |
| 1010 | 1    |
| 1100 | 2    |

Each of the formats can be used to convert from (i.e., in `in.type=`) or
to convert to (i.e., in `out.type=`) with the exception that only the
individual fish identifier version can be converted to when
`out.type="RMark"`.

## Note

The formats as used here are simple in the sense that one is only
allowed to have the individual fish identifier or the frequency variable
in addition to the capture history information. More complex analyses
may use a number of covariates. For these more complex analyses, one
should work directly with the Rcapture, `RMark`, or marked packages.

This function also assumes that all unmarked captured fish are marked
and returned to the population (i.e., no losses at the time of marking
are allowed).

## Warning

`capHistConvert` may give unwanted results if the data are
`in.type="event"` but there are unused levels for the variable, as would
result if the data.frame had been subsetted on the event variable. The
unwanted results can be corrected by using `droplevels` before
`capHistConvert`. See the last example for an example.

## IFAR Chapter

9-Abundance from Capture-Recapture Data.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

## See also

See
[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
to summarize “individual” capture histories into a format usable in
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
and
[`mrOpen`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md).
Also see Rcapture, `RMark`, or marked packages for handling more complex
analyses.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## A small example of 'event' format
( ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
#>   fish   yr
#> 1   17 1987
#> 2   18 1987
#> 3   21 1987
#> 4   17 1988
#> 5   21 1988
#> 6   18 1989
#> 7   19 1989
#> 8   20 1990
# convert to 'individual' format
( ex1.E2I <- capHistConvert(ex1,id="fish",in.type="event") )
#>   fish 1987 1988 1989 1990
#> 1   17    1    1    0    0
#> 2   18    1    0    1    0
#> 3   19    0    0    1    0
#> 4   20    0    0    0    1
#> 5   21    1    1    0    0
# convert to 'frequency' format
( ex1.E2F <- capHistConvert(ex1,id="fish",in.type="event",out.type="frequency") )
#>   1987 1988 1989 1990 freq
#> 1    0    0    0    1    1
#> 2    0    0    1    0    1
#> 3    1    0    1    0    1
#> 4    1    1    0    0    2
# convert to 'MARK' format
( ex1.E2M <- capHistConvert(ex1,id="fish",in.type="event",out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# convert to 'RMark' format
( ex1.E2R <- capHistConvert(ex1,id="fish",in.type="event",out.type="RMark") )
#>   fish   ch
#> 1   17 1100
#> 2   18 1010
#> 3   19 0010
#> 4   20 0001
#> 5   21 1100

## convert converted 'individual' format ...
# to 'frequency' format (must ignore "id")
( ex1.I2F <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="frequency") )
#>   1987 1988 1989 1990 freq
#> 1    0    0    0    1    1
#> 2    0    0    1    0    1
#> 3    1    0    1    0    1
#> 4    1    1    0    0    2
# to 'MARK' format
( ex1.I2M <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# to 'RMark' format
( ex1.I2R <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="RMark") )
#>   fish   ch
#> 1   17 1100
#> 2   18 1010
#> 3   19 0010
#> 4   20 0001
#> 5   21 1100
# to 'event' format
( ex1.I2E <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="event") )
#>   fish event
#> 1   17  1987
#> 2   18  1987
#> 3   21  1987
#> 4   17  1988
#> 5   21  1988
#> 6   18  1989
#> 7   19  1989
#> 8   20  1990

#' ## convert converted 'frequency' format ...
# to 'individual' format
( ex1.F2I <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency") )
#>   1987 1988 1989 1990
#> 1    0    0    0    1
#> 2    0    0    1    0
#> 3    1    0    1    0
#> 4    1    1    0    0
#> 5    1    1    0    0
( ex1.F2Ia <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",include.id=TRUE) )
#>   id 1987 1988 1989 1990
#> 1  1    0    0    0    1
#> 2  2    0    0    1    0
#> 3  3    1    0    1    0
#> 4  4    1    1    0    0
#> 5  5    1    1    0    0
# to 'Mark' format
( ex1.F2M <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
                            out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# to 'RMark' format
( ex1.F2R <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
                            out.type="RMark") )
#>     ch
#> 1 0001
#> 2 0010
#> 3 1010
#> 4 1100
#> 5 1100
( ex1.F2Ra <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
                             out.type="RMark",include.id=TRUE) )
#>   id   ch
#> 1  1 0001
#> 2  2 0010
#> 3  3 1010
#> 4  4 1100
#> 5  5 1100
# to 'event' format
( ex1.F2E <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
                            out.type="event") )
#>   id event
#> 1  3  1987
#> 2  4  1987
#> 3  5  1987
#> 4  4  1988
#> 5  5  1988
#> 6  2  1989
#> 7  3  1989
#> 8  1  1990

## convert converted 'MARK' format ...
# to 'individual' format
( ex1.M2I <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK") )
#>   event1 event2 event3 event4
#> 1      0      0      0      1
#> 2      0      0      1      0
#> 3      1      0      1      0
#> 4      1      1      0      0
#> 5      1      1      0      0
( ex1.M2Ia <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",include.id=TRUE) )
#>   id event1 event2 event3 event4
#> 1  1      0      0      0      1
#> 2  2      0      0      1      0
#> 3  3      1      0      1      0
#> 4  4      1      1      0      0
#> 5  5      1      1      0      0
# to 'frequency' format
( ex1.M2F <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="frequency") )
#>   event1 event2 event3 event4 freq
#> 1      0      0      0      1    1
#> 2      0      0      1      0    1
#> 3      1      0      1      0    1
#> 4      1      1      0      0    2
# to 'RMark' format
( ex1.M2R <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="RMark") )
#>     ch
#> 1 0001
#> 2 0010
#> 3 1010
#> 4 1100
#> 5 1100
( ex1.M2Ra <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="RMark",include.id=TRUE) )
#>   id   ch
#> 1  1 0001
#> 2  2 0010
#> 3  3 1010
#> 4  4 1100
#> 5  5 1100
# to 'event' format
( ex1.M2E <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="event") )
#>   id  event
#> 1  3 event1
#> 2  4 event1
#> 3  5 event1
#> 4  4 event2
#> 5  5 event2
#> 6  2 event3
#> 7  3 event3
#> 8  1 event4
 
## convert converted 'RMark' format ...
# to 'individual' format
( ex1.R2I <- capHistConvert(ex1.E2R,id="fish",in.type="RMark") )
#>   fish event1 event2 event3 event4
#> 1   17      1      1      0      0
#> 2   18      1      0      1      0
#> 3   19      0      0      1      0
#> 4   20      0      0      0      1
#> 5   21      1      1      0      0
# to 'frequency' format
( ex1.R2F <- capHistConvert(ex1.E2R,id="fish",in.type="RMark",out.type="frequency") )
#>   event1 event2 event3 event4 freq
#> 1      0      0      0      1    1
#> 2      0      0      1      0    1
#> 3      1      0      1      0    1
#> 4      1      1      0      0    2
# to 'MARK' format
( ex1.R2M <- capHistConvert(ex1.E2R,id="fish",in.type="RMark",out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# to 'event' format
( ex1.R2E <- capHistConvert(ex1.E2R,id="fish",in.type="RMark",out.type="event") )
#>   fish  event
#> 1   17 event1
#> 2   18 event1
#> 3   21 event1
#> 4   17 event2
#> 5   21 event2
#> 6   18 event3
#> 7   19 event3
#> 8   20 event4

## Remove semi-colon from MARK format to make a RMark 'frequency' format
ex1.E2R1 <- ex1.E2M
ex1.E2R1$freq <- as.numeric(sub(";","",ex1.E2R1$freq))
ex1.E2R1
#>     ch freq
#> 1 0001    1
#> 2 0010    1
#> 3 1010    1
#> 4 1100    2
# convert this to 'individual' format
( ex1.R2I1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark") )
#>   event1 event2 event3 event4
#> 1      0      0      0      1
#> 2      0      0      1      0
#> 3      1      0      1      0
#> 4      1      1      0      0
#> 5      1      1      0      0
( ex1.R2I1a <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",include.id=TRUE) )
#>   id event1 event2 event3 event4
#> 1  1      0      0      0      1
#> 2  2      0      0      1      0
#> 3  3      1      0      1      0
#> 4  4      1      1      0      0
#> 5  5      1      1      0      0
# convert this to 'frequency' format
( ex1.R2F1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="frequency") )
#>   event1 event2 event3 event4 freq
#> 1      0      0      0      1    1
#> 2      0      0      1      0    1
#> 3      1      0      1      0    1
#> 4      1      1      0      0    2
# convert this to 'MARK' format
( ex1.R2M1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# convert this to 'event' format
( ex1.R2E1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="event") )
#>   id  event
#> 1  3 event1
#> 2  4 event1
#> 3  5 event1
#> 4  4 event2
#> 5  5 event2
#> 6  2 event3
#> 7  3 event3
#> 8  1 event4


########################################################################
## A small example using character ids
( ex2 <- data.frame(fish=c("id17","id18","id21","id17","id21","id18","id19","id20"),
                    yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
#>   fish   yr
#> 1 id17 1987
#> 2 id18 1987
#> 3 id21 1987
#> 4 id17 1988
#> 5 id21 1988
#> 6 id18 1989
#> 7 id19 1989
#> 8 id20 1990
# convert to 'individual' format
( ex2.E2I <- capHistConvert(ex2,id="fish",in.type="event") )
#>   fish 1987 1988 1989 1990
#> 1 id17    1    1    0    0
#> 2 id18    1    0    1    0
#> 3 id19    0    0    1    0
#> 4 id20    0    0    0    1
#> 5 id21    1    1    0    0
# convert to 'frequency' format
( ex2.E2F <- capHistConvert(ex2,id="fish",in.type="event",out.type="frequency") )
#>   1987 1988 1989 1990 freq
#> 1    0    0    0    1    1
#> 2    0    0    1    0    1
#> 3    1    0    1    0    1
#> 4    1    1    0    0    2
# convert to 'MARK' format
( ex2.E2M <- capHistConvert(ex2,id="fish",in.type="event",out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# convert to 'RMark' format
( ex2.E2R <- capHistConvert(ex2,id="fish",in.type="event",out.type="RMark") )
#>   fish   ch
#> 1 id17 1100
#> 2 id18 1010
#> 3 id19 0010
#> 4 id20 0001
#> 5 id21 1100

## convert converted 'individual' format ...
# to 'frequency' format
( ex2.I2F <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="frequency") )
#>   1987 1988 1989 1990 freq
#> 1    0    0    0    1    1
#> 2    0    0    1    0    1
#> 3    1    0    1    0    1
#> 4    1    1    0    0    2
# to 'MARK' format
( ex2.I2M <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="MARK") )
#>     ch freq
#> 1 0001   1;
#> 2 0010   1;
#> 3 1010   1;
#> 4 1100   2;
# to 'RMark' format
( ex2.I2R <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="RMark") )
#>   fish   ch
#> 1 id17 1100
#> 2 id18 1010
#> 3 id19 0010
#> 4 id20 0001
#> 5 id21 1100
# to 'event' format
( ex2.I2E <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="event") )
#>   fish event
#> 1 id17  1987
#> 2 id18  1987
#> 3 id21  1987
#> 4 id17  1988
#> 5 id21  1988
#> 6 id18  1989
#> 7 id19  1989
#> 8 id20  1990

## demo use of var.lbls
( ex2.E2Ia <- capHistConvert(ex2,id="fish",in.type="event",var.lbls.pre="Sample") )
#>   fish 1987 1988 1989 1990
#> 1 id17    1    1    0    0
#> 2 id18    1    0    1    0
#> 3 id19    0    0    1    0
#> 4 id20    0    0    0    1
#> 5 id21    1    1    0    0
( ex2.E2Ib <- capHistConvert(ex2,id="fish",in.type="event",
              var.lbls=c("first","second","third","fourth")) )
#>   fish first second third fourth
#> 1 id17     1      1     0      0
#> 2 id18     1      0     1      0
#> 3 id19     0      0     1      0
#> 4 id20     0      0     0      1
#> 5 id21     1      1     0      0

## demo use of event.ord
( ex2.I2Ea <- capHistConvert(ex2.E2Ib,id="fish",in.type="individual",out.type="event") )
#>   fish  event
#> 1 id17  first
#> 2 id18  first
#> 3 id21  first
#> 4 id17 second
#> 5 id21 second
#> 6 id18  third
#> 7 id19  third
#> 8 id20 fourth
( ex2.E2Ibad <- capHistConvert(ex2.I2Ea,id="fish",in.type="event") )
#>   fish first fourth second third
#> 1 id17     1      0      1     0
#> 2 id18     1      0      0     1
#> 3 id19     0      0      0     1
#> 4 id20     0      1      0     0
#> 5 id21     1      0      1     0
( ex2.E2Igood <- capHistConvert(ex2.I2Ea,id="fish",in.type="event",
                 event.ord=c("first","second","third","fourth")) )
#>   fish first second third fourth
#> 1 id17     1      1     0      0
#> 2 id18     1      0     1      0
#> 3 id19     0      0     1      0
#> 4 id20     0      0     0      1
#> 5 id21     1      1     0      0

## ONLY RUN IN INTERACTIVE MODE
if (FALSE) { # \dontrun{

########################################################################
  if (require(Rcapture)) {
    ## A larger example of 'frequency' format (data from Rcapture package)
    data(bunting,package="Rcapture")
    head(bunting)
    # convert to 'individual' format
    bun.F2I <- capHistConvert(bunting,in.type="frequency",freq="freq")
    head(bun.F2I)
    # convert to 'MARK' format
    bun.F2M <- capHistConvert(bunting,id="id",in.type="frequency",freq="freq",out.type="MARK")
    head(bun.F2M)
    # convert converted 'individual' back to 'MARK' format
    bun.I2M <- capHistConvert(bun.F2I,id="id",in.type="individual",out.type="MARK")
    head(bun.I2M)
    # convert converted 'individual' back to 'frequency' format
    bun.I2F <- capHistConvert(bun.F2I,id="id",in.type="individual",
               out.type="frequency",var.lbls.pre="Sample")
    head(bun.I2F)
  }


########################################################################
if (require(marked)) {
    ## A larger example of 'marked' or 'RMark' format, but with a covariate
    ##   and when the covariate is removed there is no frequency or individual
    ##   fish identifier.
    data(dipper,package="marked")
    head(dipper)
    # isolate males and females
    dipperF <- subset(dipper,sex=="Female")
    dipperM <- subset(dipper,sex=="Male")
    # convert females to 'individual' format
    dipF.R2I <- capHistConvert(dipperF,cols2ignore="sex",in.type="RMark")
    head(dipF.R2I)
    # convert males to 'individual' format
    dipM.R2I <- capHistConvert(dipperM,cols2ignore="sex",in.type="RMark")
    head(dipM.R2I)
    # add sex variable to each data.frame and then combine
    dipF.R2I$sex <- "Female"
    dipM.R2I$sex <- "Male"
    dip.R2I <- rbind(dipF.R2I,dipM.R2I)
    head(dip.R2I)
    tail(dip.R2I)
 }
} # } # end \dontrun


## An example of problem with unused levels
## Create a set of test data with several groups
( df <- data.frame(fish=c("id17","id18","id21","id17","id21","id18","id19","id20","id17"),
                   group=c("B1","B1","B1","B2","B2","B3","B4","C1","C1")) )
#>   fish group
#> 1 id17    B1
#> 2 id18    B1
#> 3 id21    B1
#> 4 id17    B2
#> 5 id21    B2
#> 6 id18    B3
#> 7 id19    B4
#> 8 id20    C1
#> 9 id17    C1
#  Let's assume the user wants to subset the data from the "B" group
( df1 <- subset(df,group %in% c("B1","B2","B3","B4")) )
#>   fish group
#> 1 id17    B1
#> 2 id18    B1
#> 3 id21    B1
#> 4 id17    B2
#> 5 id21    B2
#> 6 id18    B3
#> 7 id19    B4
#  Looks like capHistConvert() is still using the unused factor
#  level from group C
capHistConvert(df1,id="fish",in.type="event")
#>   fish B1 B2 B3 B4
#> 1 id17  1  1  0  0
#> 2 id18  1  0  1  0
#> 3 id19  0  0  0  1
#> 4 id21  1  1  0  0
# use droplevels() to remove the unused groups and no problem
df1 <- droplevels(df1)
capHistConvert(df1,id="fish",in.type="event")
#>   fish B1 B2 B3 B4
#> 1 id17  1  1  0  0
#> 2 id18  1  0  1  0
#> 3 id19  0  0  0  1
#> 4 id21  1  1  0  0
```
