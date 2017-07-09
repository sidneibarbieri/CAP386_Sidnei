Vectors
================

First steps
-----------

``` r
l <- c(FALSE,FALSE,TRUE,FALSE)
n <- c(12.333,12.389,12.918,13,14.021)
cc <- c('programming','r','python')

length(l)
```

    ## [1] 4

``` r
class(l) 
```

    ## [1] "logical"

``` r
length(n)
```

    ## [1] 5

``` r
class(n)
```

    ## [1] "numeric"

``` r
length(cc)
```

    ## [1] 3

``` r
class(cc)
```

    ## [1] "character"

Creating empty Vectors
----------------------

``` r
v <- vector(length = 10)
length(v)
```

    ## [1] 10

``` r
class(v)
```

    ## [1] "logical"

``` r
v
```

    ##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

``` r
v <- vector(length = 10,mode = "numeric")
length(v)
```

    ## [1] 10

``` r
class(v)
```

    ## [1] "numeric"

``` r
v
```

    ##  [1] 0 0 0 0 0 0 0 0 0 0

What if we mix types?
---------------------

``` r
m1 <- c('R','is','Easy',FALSE)
class(m1) 
```

    ## [1] "character"

``` r
m1
```

    ## [1] "R"     "is"    "Easy"  "FALSE"

``` r
m2 <- c(1,2,3,123.123)
class(m2)
```

    ## [1] "numeric"

``` r
m2
```

    ## [1]   1.000   2.000   3.000 123.123

``` r
m3 <- c('programming',3000,'lines')
class(m3)
```

    ## [1] "character"

``` r
m3
```

    ## [1] "programming" "3000"        "lines"

``` r
m4 <- c(-2,-1,FALSE,TRUE,2)
class(m4)
```

    ## [1] "numeric"

``` r
m4
```

    ## [1] -2 -1  0  1  2
