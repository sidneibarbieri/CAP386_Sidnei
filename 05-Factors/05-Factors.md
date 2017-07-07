Factors - Categorical Variables
================

``` r
knitr::opts_chunk$set(echo = TRUE)

avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,15.6,17.1,18.8,19.4,20.3,21.4)

names(avgTempSJC) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

avgTempSJC
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 22.2 22.4 21.6 19.6 17.0 16.1 15.6 17.1 18.8 19.4 20.3 21.4

``` r
descTempSJC <- vector(length = length(avgTempSJC))
descTempSJC
```

    ##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [12] FALSE

``` r
descTempSJC[avgTempSJC>20] <- 'Hot' 
descTempSJC
```

    ##  [1] "Hot"   "Hot"   "Hot"   "FALSE" "FALSE" "FALSE" "FALSE" "FALSE"
    ##  [9] "FALSE" "FALSE" "Hot"   "Hot"

``` r
descTempSJC[avgTempSJC<18] <- 'Cool' 
descTempSJC
```

    ##  [1] "Hot"   "Hot"   "Hot"   "FALSE" "Cool"  "Cool"  "Cool"  "Cool" 
    ##  [9] "FALSE" "FALSE" "Hot"   "Hot"

``` r
class(descTempSJC)
```

    ## [1] "character"

``` r
descTempSJC[(avgTempSJC >= 18) & (avgTempSJC <= 20)] <- 'Mild'
descTempSJC
```

    ##  [1] "Hot"  "Hot"  "Hot"  "Mild" "Cool" "Cool" "Cool" "Cool" "Mild" "Mild"
    ## [11] "Hot"  "Hot"

``` r
class(descTempSJC)
```

    ## [1] "character"

``` r
descTempSJC <- factor(descTempSJC)
descTempSJC
```

    ##  [1] Hot  Hot  Hot  Mild Cool Cool Cool Cool Mild Mild Hot  Hot 
    ## Levels: Cool Hot Mild

``` r
class(descTempSJC)
```

    ## [1] "factor"

``` r
descTempSJC <- cut(avgTempSJC,
                   breaks=c(-Inf,18,20, Inf),
                   labels=c("Cool","Mild","Hot"))
descTempSJC
```

    ##  [1] Hot  Hot  Hot  Mild Cool Cool Cool Cool Mild Mild Hot  Hot 
    ## Levels: Cool Mild Hot

``` r
class(descTempSJC)
```

    ## [1] "factor"
