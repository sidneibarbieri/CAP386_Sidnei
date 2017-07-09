Operations with Vectors in R
================

``` r
knitr::opts_chunk$set(echo = FALSE)
tempHi <- c(41.2,44.8,53.9,64.5,73.9,82.7,87.2,85.1,78.2,67.0,56.3,46.0)
tempHi
```

    ##  [1] 41.2 44.8 53.9 64.5 73.9 82.7 87.2 85.1 78.2 67.0 56.3 46.0

``` r
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
names(tempHi) <- months
tempHi
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 41.2 44.8 53.9 64.5 73.9 82.7 87.2 85.1 78.2 67.0 56.3 46.0

``` r
otherTemp <- c(Jan=41.2,Feb=44.8,Mar=53.9,Apr=64.5,May=73.9,Jun=82.7,Jul=87.2,Aug=85.1,Sep=78.2,Oct=67.0,Nov=56.3,Dec=46.0)
otherTemp
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 41.2 44.8 53.9 64.5 73.9 82.7 87.2 85.1 78.2 67.0 56.3 46.0

``` r
tempLo <- c(23.5,26.1,33.6,42.0,51.8,60.8,65.8,63.9,56.6,43.7,34.7,27.3)

range <- tempHi-tempLo
range 
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 17.7 18.7 20.3 22.5 22.1 21.9 21.4 21.2 21.6 23.3 21.6 18.7

``` r
tempHiC <- (5/9) * (tempHi - 32)
tempHiC
```

    ##       Jan       Feb       Mar       Apr       May       Jun       Jul 
    ##  5.111111  7.111111 12.166667 18.055556 23.277778 28.166667 30.666667 
    ##       Aug       Sep       Oct       Nov       Dec 
    ## 29.500000 25.666667 19.444444 13.500000  7.777778

``` r
tooCold <- (((5/9) * (tempLo - 32)) < 0)
tooCold
```

    ##  [1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [12]  TRUE

``` r
names(tooCold) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
tooCold
```

    ##   Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec 
    ##  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE

``` r
tempHi[7] 
```

    ##  Jul 
    ## 87.2

``` r
tempHi[12] 
```

    ## Dec 
    ##  46

``` r
tooCold[7]
```

    ##   Jul 
    ## FALSE

``` r
tooCold[12]
```

    ##  Dec 
    ## TRUE

``` r
tempHi[c(11:12,1:2)]
```

    ##  Nov  Dec  Jan  Feb 
    ## 56.3 46.0 41.2 44.8

``` r
tempHi[seq(2,12,2)]
```

    ##  Feb  Apr  Jun  Aug  Oct  Dec 
    ## 44.8 64.5 82.7 85.1 67.0 46.0

``` r
tempHi[seq(2,12,3)]
```

    ##  Feb  May  Aug  Nov 
    ## 44.8 73.9 85.1 56.3

``` r
tempHiC[tempHiC > 20] 
```

    ##      May      Jun      Jul      Aug      Sep 
    ## 23.27778 28.16667 30.66667 29.50000 25.66667

``` r
min(tempHiC)
```

    ## [1] 5.111111

``` r
max(tempHiC)
```

    ## [1] 30.66667

``` r
sd(tempHiC)
```

    ## [1] 9.178011

``` r
mean(tempHiC)
```

    ## [1] 18.37037

``` r
tempHiC[tempHiC > mean(tempHiC)] 
```

    ##      May      Jun      Jul      Aug      Sep      Oct 
    ## 23.27778 28.16667 30.66667 29.50000 25.66667 19.44444

``` r
hotter <- tempHiC + 10
hotter
```

    ##      Jan      Feb      Mar      Apr      May      Jun      Jul      Aug 
    ## 15.11111 17.11111 22.16667 28.05556 33.27778 38.16667 40.66667 39.50000 
    ##      Sep      Oct      Nov      Dec 
    ## 35.66667 29.44444 23.50000 17.77778

``` r
hotter-tempHiC
```

    ## Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec 
    ##  10  10  10  10  10  10  10  10  10  10  10  10

``` r
hotter <- tempHiC + c(3,3,2,2,2,1,1,1,1,2,2,3)
hotter
```

    ##       Jan       Feb       Mar       Apr       May       Jun       Jul 
    ##  8.111111 10.111111 14.166667 20.055556 25.277778 29.166667 31.666667 
    ##       Aug       Sep       Oct       Nov       Dec 
    ## 30.500000 26.666667 21.444444 15.500000 10.777778

``` r
hotter-tempHiC
```

    ## Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec 
    ##   3   3   2   2   2   1   1   1   1   2   2   3

``` r
hotter <- tempHiC + c(3,2,1)
hotter
```

    ##       Jan       Feb       Mar       Apr       May       Jun       Jul 
    ##  8.111111  9.111111 13.166667 21.055556 25.277778 29.166667 33.666667 
    ##       Aug       Sep       Oct       Nov       Dec 
    ## 31.500000 26.666667 22.444444 15.500000  8.777778

``` r
hotter-tempHiC
```

    ## Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec 
    ##   3   2   1   3   2   1   3   2   1   3   2   1

``` r
hotter <- tempHiC + c(3,2,1,0,-1)
```

    ## Warning in tempHiC + c(3, 2, 1, 0, -1): comprimento do objeto maior não é
    ## múltiplo do comprimento do objeto menor

``` r
hotter
```

    ##       Jan       Feb       Mar       Apr       May       Jun       Jul 
    ##  8.111111  9.111111 13.166667 18.055556 22.277778 31.166667 32.666667 
    ##       Aug       Sep       Oct       Nov       Dec 
    ## 30.500000 25.666667 18.444444 16.500000  9.777778

``` r
hotter-tempHiC
```

    ## Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec 
    ##   3   2   1   0  -1   3   2   1   0  -1   3   2

``` r
avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,NA,17.1,18.8,19.4,20.3,21.4)
names(avgTempSJC) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
avgTempSJC
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 22.2 22.4 21.6 19.6 17.0 16.1   NA 17.1 18.8 19.4 20.3 21.4

``` r
min(avgTempSJC)
```

    ## [1] NA

``` r
max(avgTempSJC)
```

    ## [1] NA

``` r
mean(avgTempSJC)
```

    ## [1] NA

``` r
sd(avgTempSJC)
```

    ## [1] NA

``` r
min(avgTempSJC,na.rm = TRUE)
```

    ## [1] 16.1

``` r
max(avgTempSJC,na.rm = TRUE)
```

    ## [1] 22.4

``` r
mean(avgTempSJC,na.rm = TRUE)
```

    ## [1] 19.62727

``` r
sd(avgTempSJC,na.rm = TRUE)
```

    ## [1] 2.196857

``` r
is.na(avgTempSJC)
```

    ##   Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec 
    ## FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE

``` r
!is.na(avgTempSJC)
```

    ##   Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec 
    ##  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE

``` r
avgTempSJC[is.na(avgTempSJC)]
```

    ## Jul 
    ##  NA

``` r
avgTempSJC[!is.na(avgTempSJC)]
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Aug  Sep  Oct  Nov  Dec 
    ## 22.2 22.4 21.6 19.6 17.0 16.1 17.1 18.8 19.4 20.3 21.4
