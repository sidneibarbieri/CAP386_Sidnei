Exercício da 3ª Aula - 1ª Parte
================

PS: The initial part was given by professor
-------------------------------------------

1. The “hotdog” variable for the “Hot Dogs in Baltimore” example can be better defined. How can we ensure that all variations of “hot dogs” are represented?
------------------------------------------------------------------------------------------------------------------------------------------------------------

### Hot dog variations (franks, HOT DOG, hOT dOG, etc.)

``` r
knitr::opts_chunk$set(echo = TRUE)

x <- 1
y <- nrow(dfHotdog)
while (x<=y) {
  if (grepl("Hot dog",dfHotdog$ItemsSold[x],ignore.case=TRUE)){
    dfHotdog$hotdog[x] <- TRUE
  } else if (grepl("franks",dfHotdog$ItemsSold[x],ignore.case=TRUE)){
    dfHotdog$hotdog[x] <- TRUE
  } else 
    dfHotdog$hotdog[x] <- FALSE

    x <- x+1
}

dfHotdog
```

    ##                                                                  ItemsSold
    ## 1                              Grilled food, pizza slices, gyro sandwiches
    ## 2                          Hot Dogs, Sausage, Snacks, Gum, Candies, Drinks
    ## 3                          Hot dogs, Sausage, drinks, snacks, gum, & candy
    ## 4                           Hot dogs, sausages, chips, snacks, drinks, gum
    ## 5 Large & Small beef franks, soft drinks, water, all types of nuts & chips
    ## 6                                                   Hot dogs, Sodas, Chips
    ##   hotdog
    ## 1  FALSE
    ## 2   TRUE
    ## 3   TRUE
    ## 4   TRUE
    ## 5   TRUE
    ## 6   TRUE

2. I prefer Pizza. OK, create a “pizza” variable then.
------------------------------------------------------

``` r
knitr::opts_chunk$set(echo = TRUE)

x <- 1
y <- nrow(dfHotdog)
while (x<=y) {
  if (grepl("pizza",dfHotdog$ItemsSold[x],ignore.case=TRUE)){
    dfHotdog$hotdog[x] <- TRUE
  } else 
    dfHotdog$hotdog[x] <- FALSE
  
  x <- x+1
}

dfHotdog
```

    ##                                                                  ItemsSold
    ## 1                              Grilled food, pizza slices, gyro sandwiches
    ## 2                          Hot Dogs, Sausage, Snacks, Gum, Candies, Drinks
    ## 3                          Hot dogs, Sausage, drinks, snacks, gum, & candy
    ## 4                           Hot dogs, sausages, chips, snacks, drinks, gum
    ## 5 Large & Small beef franks, soft drinks, water, all types of nuts & chips
    ## 6                                                   Hot dogs, Sodas, Chips
    ##   hotdog
    ## 1   TRUE
    ## 2  FALSE
    ## 3  FALSE
    ## 4  FALSE
    ## 5  FALSE
    ## 6  FALSE

3. Get the name of the town for the “Hot Dogs in Baltimore” example.
--------------------------------------------------------------------

##### Filter the name of the town from the location variable: ("Towson 21204(39.28540000000, -76.62260000000)" "Owings Mill 21117(39.29860000000, -76.61280000000)" "Owings Mill 21117(39.28920000000, -76.62670000000)" "Owings Mill 21117(39.28870000000, -76.61360000000)"...) – search for grep examples on stackoverflow.com on how to do this. Hint: there is a lazier simpler way: we already have the name and ZIP code...

``` r
knitr::opts_chunk$set(echo = TRUE)

## Completing my Data Frame 
library(plyr)
dfHotdogC <- join_all(list(bVendors,dfHotdog)) 
```

    ## Joining by: ItemsSold, hotdog

``` r
## Hot dog location with name of the town
x <- 1
y <- nrow(dfHotdogC)
while (x<=y) {
  if (grepl("Hot dog",dfHotdogC$ItemsSold[x],ignore.case=TRUE)){
    dfHotdogC$hotdog[x] <- TRUE
  } else if (grepl("franks",dfHotdogC$ItemsSold[x],ignore.case=TRUE)){
    dfHotdogC$hotdog[x] <- TRUE
  } else 
    dfHotdogC$hotdog[x] <- FALSE
  
  x <- x+1
}

town <- vector(length = nrow(dfHotdogC),mode = "character")
library("stringr")
x <- 1
y <- nrow(dfHotdogC)
while (x<=y) {
  if (dfHotdogC$hotdog[x] == TRUE) {
    town[x] <- str_extract_all(dfHotdogC$location[x], regex("[a-z]+", TRUE))
  }
  x <- x+1
}

dfHotdogC$town <- town
colnames(dfHotdogC)
```

    ##  [1] "LicenseNum" "VendorName" "VendorAddr" "ItemsSold"  "Cart_Descr"
    ##  [6] "location"   "lat"        "long"       "hotdog"     "town"

``` r
latitude <- vector(length = nrow(dfHotdogC),mode = "numeric")
latitude <- dfHotdogC$lat
longitude <- vector(length = nrow(dfHotdogC),mode = "numeric")
longitude <- dfHotdogC$long

dfHotdogC$location <- NULL
dfHotdogC$LicenseNum <- NULL
dfHotdogC$VendorAddr <- NULL
dfHotdogC$Cart_Descr <- NULL
dfHotdogC$VendorAddr <- NULL
dfHotdogC$ItemsSold <- NULL


dfHotdogC
```

    ##                                              VendorName     lat    long
    ## 1             Abdul-Ghani, Christina, "The Bullpen Bar" 39.2854 76.6226
    ## 2                                            Ali, Fathi 39.2986 76.6128
    ## 3                                            Ali, Fathi 39.2892 76.6267
    ## 4                                            Ali, Fathi 39.2887 76.6136
    ## 5                                            Ali, Yusuf 39.2792 76.6220
    ## 6                                     Amatullah, Maidah 39.3025 76.6161
    ## 7                                         Amer, Mohamed 39.2876 76.6135
    ## 8                                        Blimline, Lisa 39.2752 76.6203
    ## 9                               Paul & Elizabeth Carter 39.2853 76.6227
    ## 10                              Paul & Elizabeth Carter 39.2772 76.6265
    ## 11                                   Ellenberger, Penny 39.2771 76.6269
    ## 12                                       Wheatley, Lisa 39.2842 76.6189
    ## 13                                        Isreal, David 39.2853 76.6190
    ## 14                                 Kastanakis, Theodore 39.2886 76.6236
    ## 15                                  Kouloumbre, Iaonnis 39.2892 76.6253
    ## 16                            Marangos, Toula & Filipos 39.2905 76.6140
    ## 17                                    Markiewicz, Robin 39.2862 76.6191
    ## 18                                Papastefanou, Stanley 39.2873 76.6250
    ## 19                                        Rouse, Donald 39.2861 76.6190
    ## 20                                        Rouse, Donald 39.2765 76.6240
    ## 21                                      Shiflett, Roger 39.2820 76.6209
    ## 22                                       Solomon, Damon 39.2852 76.6228
    ## 23                                       Solomon, Damon 39.2843 76.6178
    ## 24                                    Stansbury, Joseph 39.2913 76.6120
    ## 25                                         Reid, Gloria 39.2862 76.6189
    ## 26                                     Wheatley, Vinnie 39.2842 76.6188
    ## 27                                      Lerman, Abraham 39.2864 76.6206
    ## 28                                      Bamba, Youssouf 39.2800 76.6241
    ## 29                 Mazouz, Abdelkarim & Argoum, Mohamed 39.2895 76.6153
    ## 30                              Hynson, Jr., Raymond C. 39.2843 76.6189
    ## 31                                    Djelassi, Chaouki 39.2870 76.6176
    ## 32                                      K&B Enterprises 39.2887 76.6152
    ## 33                                  Giorgakis, Kalliopi 39.2873 76.6237
    ## 34                                         Omar, Khalid 39.2914 76.6089
    ## 35                                         Cotton, Eric 39.2851 76.6230
    ## 36                      Quint, Brad (The Beef Brothers) 39.2896 76.6176
    ## 37                           Kohlhepp, III, Mrs William 39.2850 76.6231
    ## 38                                Brewer, Donald Thomas 39.2843 76.6179
    ## 39                                     Dunlap, Patricia 39.2772 76.6268
    ## 40                                    Djelassi, Chaouki 39.2911 76.6139
    ## 41                                       Blimline, Lisa 39.2819 76.6203
    ## 42                                       Blimline, Lisa 39.2850 76.6232
    ## 43                                        Rouse, Nicole 39.2855 76.6223
    ## 44                                            Lee, Gary 39.2843 76.6181
    ## 45                                      Azzouni, Jaafer 39.2903 76.6109
    ## 46                                   Mazouz, Abdelkarim 39.2878 76.6176
    ## 47                                   Stallings, Sha-nel 39.2911 76.6126
    ## 48                                      Azzouni, Jaafer 39.2882 76.6070
    ## 49                                      Roberts, Melvin 39.2854 76.6190
    ## 50 Diakgeorgiou, Euthoxia t/a "Georgey Dee's Food Cart" 39.2925 76.6126
    ## 51                   Winfield, Brian   "Hollywood Dogs" 39.2914 76.6104
    ## 52                                        Holmes, Robin 39.2888 76.6081
    ## 53                                        Rouse, Donald 39.2793 76.6225
    ## 54                                      Strunk, Kum Cha 39.2780 76.6262
    ## 55                                      Strunk, Kum Cha 39.2856 76.6223
    ## 56                                     Johnson, Antoine 39.2985 76.6211
    ## 57                           Saldana, Maria Teresa Luna 39.2027 76.5587
    ## 58                                        Canty, Albert 39.2891 76.6282
    ## 59                                         Hummel, Gary 39.3284 76.6136
    ## 60                               Polychronis, Aristides 39.2986 76.5910
    ## 61                              Jarava, Edgar & Gustavo 39.2985 76.5921
    ## 62                              Jarava, Edgar & Gustavo 39.2964 76.5939
    ## 63                              Jarava, Edgar & Gustava 39.2973 76.5907
    ## 64                                         Hummel, Gary 39.3252 76.6220
    ## 65                                       McCoy, Patrice 39.2985 76.5919
    ## 66                                         Barr, Nomiki 39.2889 76.6326
    ## 67                                   Gilliam, Gwendolyn 39.3072 76.6148
    ## 68                                  Elmonir, Elsayed M. 39.3065 76.6177
    ## 69                                       Lewis, Cynthia 39.2856 76.6367
    ## 70                                       Parrott, Aaron 39.2933 76.6078
    ## 71           Vasquez, Emenegildo " Vasquez Fresh Fruit" 39.2998 76.5650
    ## 72                                        Johns, Farley 39.3532 76.6647
    ## 73                 Guzman, Irma  "Irma's Best Hot Dogs" 39.2847 76.5932
    ## 74                                      Azzouni, Jaafer 39.3305 76.6149
    ## 75                    Bethea, Anthony "Biggs Food Cart" 39.2599 76.6654
    ## 76                                         Brown, April 39.3111 76.6097
    ## 77                                        Arlene Gordon 39.3144 76.6770
    ##    hotdog          town
    ## 1   FALSE              
    ## 2    TRUE  Owings, Mill
    ## 3    TRUE  Owings, Mill
    ## 4    TRUE  Owings, Mill
    ## 5    TRUE     Baltimore
    ## 6    TRUE     Baltimore
    ## 7    TRUE     Baltimore
    ## 8    TRUE     Baltimore
    ## 9    TRUE     Baltimore
    ## 10   TRUE     Baltimore
    ## 11   TRUE     Baltimore
    ## 12  FALSE              
    ## 13   TRUE  Randallstown
    ## 14   TRUE     Baltimore
    ## 15   TRUE     Baltimore
    ## 16   TRUE     Baltimore
    ## 17  FALSE              
    ## 18   TRUE     Baltimore
    ## 19   TRUE     Baltimore
    ## 20   TRUE     Baltimore
    ## 21   TRUE     Baltimore
    ## 22   TRUE     Baltimore
    ## 23   TRUE     Baltimore
    ## 24  FALSE              
    ## 25  FALSE              
    ## 26   TRUE        Laurel
    ## 27   TRUE  Owings, Mill
    ## 28   TRUE     Baltimore
    ## 29   TRUE     Baltimore
    ## 30  FALSE              
    ## 31  FALSE              
    ## 32   TRUE Middle, River
    ## 33   TRUE     Baltimore
    ## 34   TRUE     Baltimore
    ## 35  FALSE              
    ## 36   TRUE     Baltimore
    ## 37   TRUE  Reisterstown
    ## 38   TRUE  Reisterstown
    ## 39   TRUE     Baltimore
    ## 40  FALSE              
    ## 41   TRUE     Baltimore
    ## 42   TRUE     Baltimore
    ## 43   TRUE     Baltimore
    ## 44  FALSE              
    ## 45  FALSE              
    ## 46   TRUE     Baltimore
    ## 47   TRUE     Baltimore
    ## 48   TRUE Windsor, Mill
    ## 49  FALSE              
    ## 50   TRUE     Baltimore
    ## 51   TRUE     Baltimore
    ## 52   TRUE    Pikesville
    ## 53  FALSE              
    ## 54   TRUE     Baltimore
    ## 55  FALSE              
    ## 56   TRUE      Edgewood
    ## 57  FALSE              
    ## 58  FALSE              
    ## 59   TRUE     Baltimore
    ## 60   TRUE     Baltimore
    ## 61   TRUE     Baltimore
    ## 62   TRUE     Baltimore
    ## 63   TRUE     Baltimore
    ## 64   TRUE     Baltimore
    ## 65   TRUE     Baltimore
    ## 66   TRUE      Pasadena
    ## 67  FALSE              
    ## 68   TRUE     Baltimore
    ## 69   TRUE     Baltimore
    ## 70   TRUE        Laurel
    ## 71  FALSE              
    ## 72   TRUE     Baltimore
    ## 73   TRUE     Baltimore
    ## 74  FALSE              
    ## 75   TRUE     Baltimore
    ## 76   TRUE     Baltimore
    ## 77   TRUE    Pikesville
