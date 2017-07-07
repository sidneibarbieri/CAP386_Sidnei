1 Understanding and Fixing Code
================

``` r
knitr::opts_chunk$set(echo = TRUE)
# Understanding and Fixing Code

# Fix the part of the code that shows the cold months in Baltimore (Lecture02/02- PrimitiveTypes) so we know which are those months.
# Temperatures for Baltimore (Low) 
lowBaltimore <- c(23.5, 26.1, 33.6, 42.0, 51.8, 60.8, 65.8, 63.9, 56.6, 43.7, 34.7, 27.3)

# Temperatures for Baltimore (High) 
highBaltimore <- c(41.2, 44.8, 53.9, 64.5, 73.9, 82.7, 87.2, 85.1, 78.2, 67.0, 56.3, 46.0)

# Meses do ano
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Data Frame com as temperaturas mínimas e máximas 
dfBaltimore <- data.frame(Low=lowBaltimore,High=highBaltimore,row.names = months)
dfBaltimore
```

    ##      Low High
    ## Jan 23.5 41.2
    ## Feb 26.1 44.8
    ## Mar 33.6 53.9
    ## Apr 42.0 64.5
    ## May 51.8 73.9
    ## Jun 60.8 82.7
    ## Jul 65.8 87.2
    ## Aug 63.9 85.1
    ## Sep 56.6 78.2
    ## Oct 43.7 67.0
    ## Nov 34.7 56.3
    ## Dec 27.3 46.0

``` r
# Vetor com as temperaturas médias
avgBaltimore <- rowMeans(dfBaltimore[,1:2])
avgBaltimore
```

    ##   Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec 
    ## 32.35 35.45 43.75 53.25 62.85 71.75 76.50 74.50 67.40 55.35 45.50 36.65

``` r
# Data Frame com as temperaturas mínimas, máximas e a média
dfBaltimore2 <- data.frame(Low_ºF=lowBaltimore,High_ºF=highBaltimore, Average_ºF=avgBaltimore,row.names = months)
dfBaltimore2
```

    ##     Low_ºF High_ºF Average_ºF
    ## Jan   23.5    41.2      32.35
    ## Feb   26.1    44.8      35.45
    ## Mar   33.6    53.9      43.75
    ## Apr   42.0    64.5      53.25
    ## May   51.8    73.9      62.85
    ## Jun   60.8    82.7      71.75
    ## Jul   65.8    87.2      76.50
    ## Aug   63.9    85.1      74.50
    ## Sep   56.6    78.2      67.40
    ## Oct   43.7    67.0      55.35
    ## Nov   34.7    56.3      45.50
    ## Dec   27.3    46.0      36.65

``` r
# Conversão para Celsius
lowBaltimoreC <- (5/9) * (lowBaltimore - 32)
highBaltimoreC <- (5/9) * (highBaltimore - 32)
avgBaltimoreC <- (5/9) * (avgBaltimore - 32)


# Data Frame com as temperaturas mínimas, máximas e a média em Celsius (com redução para casas decimais)
dfBaltimoreC <- data.frame(Low_ºC=round(lowBaltimoreC,2),High_ºC=round(highBaltimoreC,2), Average_ºC=round(avgBaltimoreC,2),row.names = months)
dfBaltimoreC
```

    ##     Low_ºC High_ºC Average_ºC
    ## Jan  -4.72    5.11       0.19
    ## Feb  -3.28    7.11       1.92
    ## Mar   0.89   12.17       6.53
    ## Apr   5.56   18.06      11.81
    ## May  11.00   23.28      17.14
    ## Jun  16.00   28.17      22.08
    ## Jul  18.78   30.67      24.72
    ## Aug  17.72   29.50      23.61
    ## Sep  13.67   25.67      19.67
    ## Oct   6.50   19.44      12.97
    ## Nov   1.50   13.50       7.50
    ## Dec  -2.61    7.78       2.58

``` r
# Data Frame indicando se o mês é quente, frio ou normal
frioBaltimoreC <- ifelse(dfBaltimoreC$Average_ºC<10,"Frio",ifelse(dfBaltimoreC$Average_ºC>25,"Quente","Normal")) 
names(frioBaltimoreC) <- months
frioBaltimoreC
```

    ##      Jan      Feb      Mar      Apr      May      Jun      Jul      Aug 
    ##   "Frio"   "Frio"   "Frio" "Normal" "Normal" "Normal" "Normal" "Normal" 
    ##      Sep      Oct      Nov      Dec 
    ## "Normal" "Normal"   "Frio"   "Frio"

``` r
# What would happen if we still had a NA on the vector for the temperatures in SJC? (Lecture02/05-Factors)?
# Média mensal das temperaturas em SJC
avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,NA,17.1,18.8,19.4,20.3,21.4)
names(avgTempSJC) <- months
avgTempSJC
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
    ## 22.2 22.4 21.6 19.6 17.0 16.1   NA 17.1 18.8 19.4 20.3 21.4

``` r
# What happens if we try to run sjcTemps["Jan":"Jul",c(1,3)]? Why? (see Lecture02/06-DataFrames).

# avgTempSJC["Jan": "Jul", c (1,3)]
# Error in "Jan":"Jul" : Argumento NA/NaN
# Além disso: Warning messages:
# 1: NAs introduzidos por coerção 
# 2: NAs introduzidos por coerção 

# Tratando NAs
semNAavgTempSJC <- avgTempSJC[!is.na(avgTempSJC)]
semNAavgTempSJC
```

    ##  Jan  Feb  Mar  Apr  May  Jun  Aug  Sep  Oct  Nov  Dec 
    ## 22.2 22.4 21.6 19.6 17.0 16.1 17.1 18.8 19.4 20.3 21.4

``` r
# We can change Data Frames with sjcTemps3["Aug",] <- c(1,2,3) and sjcTemps3["Jul",] <- sjcTemps3["Jul",]+c(3,4) but we cannot change with sjcTemps3["Jul",] <- c(3,4). Why? (see Lecture02/06-DataFrames).
# Contruindo o DF
maxTempSJC <- c(29.7,30.1,29.5,27.3,25.1,24.3,24.1,26.2,27.2,27.3,28,28.7)
avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,15.6,17.1,18.8,19.4,20.3,21.4)
minTempSJC <- c(16.2,16.5,15.7,13.2,10.1,8.9,8.2,9.9,11.9,13.4,14.2,15.3)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
sjcTemps <- data.frame(Max=maxTempSJC,Avg=avgTempSJC,Min=minTempSJC,row.names = months)
sjcTemps
```

    ##      Max  Avg  Min
    ## Jan 29.7 22.2 16.2
    ## Feb 30.1 22.4 16.5
    ## Mar 29.5 21.6 15.7
    ## Apr 27.3 19.6 13.2
    ## May 25.1 17.0 10.1
    ## Jun 24.3 16.1  8.9
    ## Jul 24.1 15.6  8.2
    ## Aug 26.2 17.1  9.9
    ## Sep 27.2 18.8 11.9
    ## Oct 27.3 19.4 13.4
    ## Nov 28.0 20.3 14.2
    ## Dec 28.7 21.4 15.3

``` r
class(sjcTemps)
```

    ## [1] "data.frame"

``` r
sjcTemps3 <- sjcTemps

# 1º Comando proposto
sjcTemps3["Aug",]
```

    ##      Max  Avg Min
    ## Aug 26.2 17.1 9.9

``` r
sjcTemps3["Aug",] <- c(1,2,3)
sjcTemps3["Aug",]
```

    ##     Max Avg Min
    ## Aug   1   2   3

``` r
# 2º Comando proposto
sjcTemps3["Jul",]
```

    ##      Max  Avg Min
    ## Jul 24.1 15.6 8.2

``` r
sjcTemps3["Jul",] <- sjcTemps3["Jul",]+c(3,4)
sjcTemps3["Jul",]
```

    ##      Max  Avg  Min
    ## Jul 27.1 19.6 11.2

``` r
# 3º Comando proposto
# sjcTemps3["Jul",] <- c(3,4)
# Error in `[<-.data.frame`(`*tmp*`, "Jul", , value = c(3, 4)) : 
# replacement has 2 items, need 3

# Run some examples that show that the recycling rule reuses the smaller (shorter) vector.
vetor <- c(1,1,1,1,1)
vetor
```

    ## [1] 1 1 1 1 1

``` r
recycling <- vetor + c(1,0,1)
```

    ## Warning in vetor + c(1, 0, 1): comprimento do objeto maior não é múltiplo
    ## do comprimento do objeto menor

``` r
recycling
```

    ## [1] 2 1 2 2 1

``` r
recycling <- recycling + c(-1,0,-1,-1)
```

    ## Warning in recycling + c(-1, 0, -1, -1): comprimento do objeto maior não é
    ## múltiplo do comprimento do objeto menor

``` r
recycling
```

    ## [1] 1 1 1 1 0

``` r
# What does stringsAsFactors=FALSE in read.csv() do? Why do we need it? (see Lecture02/06-DataFrames).
taubateWeather <- read.table("https://raw.githubusercontent.com/rafaeldcsantos/CAP386/master/Data/Taubate.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
taubateWeather
```

    ##    TAUBATE Maximum.Absolute.Temperature Maximum.Average.Temperature
    ## 1      Jan                         36.7                        30.2
    ## 2      Feb                         40.1                        30.5
    ## 3      Mar                         36.2                        29.9
    ## 4      Apr                         34.2                        27.8
    ## 5      May                         32.6                        25.4
    ## 6      Jun                         31.0                        24.6
    ## 7      Jul                         32.1                        24.4
    ## 8      Aug                         35.6                        26.4
    ## 9      Sep                         37.8                        27.6
    ## 10     Oct                         38.0                        27.5
    ## 11     Nov                         37.4                        28.7
    ## 12     Dec                         37.5                        29.1
    ## 13    Year                         40.1                        27.7
    ##    Average.Temperature Minimum.Average.Temperature
    ## 1                 23.3                        17.7
    ## 2                 23.5                        17.8
    ## 3                 22.9                        17.8
    ## 4                 20.8                        16.1
    ## 5                 18.1                        14.2
    ## 6                 16.7                        12.9
    ## 7                 16.4                        12.6
    ## 8                 18.2                        14.0
    ## 9                 20.0                        15.7
    ## 10                20.7                        16.4
    ## 11                21.7                        16.9
    ## 12                22.7                        17.1
    ## 13                20.4                        15.8
    ##    Minimum.Absolute.Temperature Rainfall..mm. Days.with.Rainfall....1mm
    ## 1                          10.9         233.5                        17
    ## 2                          12.6         192.1                        14
    ## 3                          11.6         173.5                        12
    ## 4                           5.4          67.1                         7
    ## 5                           4.2          40.9                         4
    ## 6                           1.1          29.3                         4
    ## 7                           0.9          31.1                         4
    ## 8                           2.6          41.7                         4
    ## 9                           3.8          64.0                         6
    ## 10                          7.8         132.8                        11
    ## 11                          8.6         146.2                        12
    ## 12                         11.0         244.6                        16
    ## 13                          0.9        1396.8                       111
    ##    Relative.Humidity Sunshine.Hours
    ## 1               76.8          170.0
    ## 2               76.7          168.2
    ## 3               76.8          179.5
    ## 4               75.9          177.3
    ## 5               76.2          176.1
    ## 6               75.7          175.6
    ## 7               72.5          188.9
    ## 8               69.1          186.2
    ## 9               69.2          158.8
    ## 10              74.0          148.8
    ## 11              73.8          161.5
    ## 12              76.8          155.0
    ## 13              74.5         2045.9

``` r
# Por padrão, a função read.csv transforma variáveis que contêm caracteres em factor (categórica). Para desabilitar isto, basta especificar na chamada à função stringsAsFactors = FALSE

# Levels 
politicos <- factor(rep("corrupto",10),levels=c("corrupto","honesto"))
politicos
```

    ##  [1] corrupto corrupto corrupto corrupto corrupto corrupto corrupto
    ##  [8] corrupto corrupto corrupto
    ## Levels: corrupto honesto

``` r
tratamentos <- factor(rep(c("Controle","Adubo A","Adubo B"),each=4))
tratamentos
```

    ##  [1] Controle Controle Controle Controle Adubo A  Adubo A  Adubo A 
    ##  [8] Adubo A  Adubo B  Adubo B  Adubo B  Adubo B 
    ## Levels: Adubo A Adubo B Controle
