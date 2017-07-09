01 DateTime
================

Reviewing commands about dates
------------------------------

``` r
knitr::opts_chunk$set(echo = TRUE)
today <- Sys.Date()
today
```

    ## [1] "2017-07-08"

``` r
class(today)
```

    ## [1] "Date"

``` r
now <- Sys.time()
now
```

    ## [1] "2017-07-08 11:04:21 BRT"

``` r
class(now)
```

    ## [1] "POSIXct" "POSIXt"

``` r
monday <- as.Date("2017-07-03")
monday
```

    ## [1] "2017-07-03"

``` r
class(monday)
```

    ## [1] "Date"

``` r
aDate <- "04/07/2017"
aDate
```

    ## [1] "04/07/2017"

``` r
class(aDate)
```

    ## [1] "character"

``` r
tuesday <- as.Date(aDate, "%d/%m/%Y")
tuesday
```

    ## [1] "2017-07-04"

``` r
class(tuesday)
```

    ## [1] "Date"

``` r
mondayMorning <- as.Date("2017-07-03 05:30:00")
mondayMorning
```

    ## [1] "2017-07-03"

``` r
class(mondayMorning)
```

    ## [1] "Date"

``` r
aDate <- "04/07/2017"
tuesday <- strptime(aDate, "%d/%m/%Y")
tuesday
```

    ## [1] "2017-07-04 BRT"

``` r
class(tuesday)
```

    ## [1] "POSIXlt" "POSIXt"

``` r
tuesdayMorning <- strptime("04/07/2017 at 05:20:17", "%d/%m/%Y at %H:%M:%S")
tuesdayMorning
```

    ## [1] "2017-07-04 05:20:17 BRT"

``` r
class(tuesdayMorning)
```

    ## [1] "POSIXlt" "POSIXt"

``` r
tuesdayMorning
```

    ## [1] "2017-07-04 05:20:17 BRT"

``` r
tM <- as.POSIXlt(tuesdayMorning)
tM$sec
```

    ## [1] 17

``` r
tM$wday
```

    ## [1] 2

``` r
birthEAP <- as.Date("1809/01/19")
birthEAP
```

    ## [1] "1809-01-19"

``` r
today <- Sys.Date()
today
```

    ## [1] "2017-07-08"

``` r
diff <- today-birthEAP
diff
```

    ## Time difference of 76141 days

``` r
class(diff)
```

    ## [1] "difftime"

``` r
today <- Sys.Date()
today
```

    ## [1] "2017-07-08"

``` r
future <- today+100 # days
future
```

    ## [1] "2017-10-16"

``` r
class(future)
```

    ## [1] "Date"
