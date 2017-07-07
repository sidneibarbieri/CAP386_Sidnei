2 Baltimore Rainfall
================

``` r
knitr::opts_chunk$set(echo = FALSE)
# Precipitação mensal média em Baltimore
precpolBaltimore <- c(3.47, 3.02, 3.93, 3.00, 3.89, 3.43, 3.85, 3.74, 3.98, 3.16, 3.12, 3.35)

# Conversão para milímetros
precmmBaltimore <- (precpolBaltimore * 25.4) # 1 polegada = 25,4 milímetros

months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Baltimore: Precipitação mensal média em polegada
names(precpolBaltimore) <- months 
precpolBaltimore
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 3.47 3.02 3.93 3.00 3.89 3.43 3.85 3.74 3.98 3.16 3.12 3.35

``` r
# Baltimore: Precipitação mensal média em milímetros
names(precmmBaltimore) <- months 
precmmBaltimore
```

    ##     Jan     Feb     Mar     Apr     May     Jun     Jul     Aug     Sep 
    ##  88.138  76.708  99.822  76.200  98.806  87.122  97.790  94.996 101.092 
    ##     Oct     Nov     Dec 
    ##  80.264  79.248  85.090

``` r
# Minimum
min(precmmBaltimore)
```

    ## [1] 76.2

``` r
# Maximum
max(precmmBaltimore)
```

    ## [1] 101.092

``` r
# Average 
mean(precmmBaltimore)
```

    ## [1] 88.773
