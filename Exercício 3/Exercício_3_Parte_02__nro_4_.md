4 Complete the Codebook for the “Hot Dogs in Baltimore” example.
================

##### Create a clean markdown file with all steps required to eliminate some columns, add and clean others. Make sure to include a codebook for the variables (even the ones you’ve created!)

Initial Part. Given by prefessor
--------------------------------

##### Donwload do arquivo

##### Construção do data.frame

##### Números de Licença como Factor (conveniente para ordenação)

Missão do aluno:
----------------

##### Adição de colunas para a Cidade, a Latitude e a Longitude

``` r
knitr::opts_chunk$set(echo = TRUE)
bVendors$latitude <- vector(length = nrow(bVendors),mode = "numeric")
bVendors$longitude <- vector(length = nrow(bVendors),mode = "numeric")
bVendors$town <- vector(length = nrow(bVendors),mode = "character")
```

##### Extração da Cidade, da Latitude e da Longitude a partir da coluna Location.1

``` r
knitr::opts_chunk$set(echo = TRUE)
library('stringr')

names(bVendors)[names(bVendors) == "Location.1"] <- "location"
templocation <- unlist(regmatches(bVendors$location,gregexpr("[-0-9.]+",bVendors$location)))

x <- 1
jump <- 2 # Saltar o código numérico que não será utilizado
y <- nrow(bVendors)
while (x<=y) {
  bVendors$latitude[x] <- as.numeric(templocation[jump])
  bVendors$longitude[x] <- as.numeric(templocation[jump+1])
  bVendors$town[x] <- str_replace_all(bVendors$location[x], "[^A-z]+", " ") 
  
  x <- x+1
  jump <- jump+3 # Uma mesma localização é composta por um grupo de 3 dados (código, lat, long)
}
```

##### Hot dog variations (franks, HOT DOG, hOT dOG, etc.)

``` r
knitr::opts_chunk$set(echo = TRUE)
x <- 1
y <- nrow(bVendors)
while (x<=y) {
  if (grepl("Hot dog",bVendors$ItemsSold[x],ignore.case=TRUE)){
    bVendors$hotdog[x] <- TRUE
  } else if (grepl("franks",bVendors$ItemsSold[x],ignore.case=TRUE)){
    bVendors$hotdog[x] <- TRUE
  } else 
    bVendors$hotdog[x] <- FALSE
  
  x <- x+1
}
```

##### Descartando colunas consideradas de menor relevância para a operação "Comer Hot Dog, custe o que custar!"

``` r
knitr::opts_chunk$set(echo = TRUE)
colnames(bVendors) # Muita informação
```

    ##  [1] "Id"         "LicenseNum" "VendorName" "VendorAddr" "ItemsSold" 
    ##  [6] "Cart_Descr" "St"         "location"   "latitude"   "longitude" 
    ## [11] "town"       "hotdog"

``` r
bVendors$Id <- NULL
bVendors$LicenseNum <- NULL
bVendors$VendorAddr <- NULL
bVendors$VendorAddr <- NULL
bVendors$ItemsSold <- NULL
bVendors$Cart_Descr <- NULL
bVendors$St <- NULL
bVendors$location <- NULL
```

Resultado Final
---------------

##### Quem? Qual cidade? Posso por no Waze? Será que tem hot dog?

``` r
knitr::opts_chunk$set(echo = TRUE)
bVendors
```

    ##                                              VendorName latitude longitude
    ## 1             Abdul-Ghani, Christina, "The Bullpen Bar"  39.2854  -76.6226
    ## 2                                            Ali, Fathi  39.2986  -76.6128
    ## 3                                            Ali, Fathi  39.2892  -76.6267
    ## 4                                            Ali, Fathi  39.2887  -76.6136
    ## 5                                            Ali, Yusuf  39.2792  -76.6220
    ## 6                                     Amatullah, Maidah  39.3025  -76.6161
    ## 7                                         Amer, Mohamed  39.2876  -76.6135
    ## 8                                        Blimline, Lisa  39.2752  -76.6203
    ## 9                               Paul & Elizabeth Carter  39.2853  -76.6227
    ## 10                              Paul & Elizabeth Carter  39.2772  -76.6265
    ## 11                                   Ellenberger, Penny  39.2771  -76.6269
    ## 12                                       Wheatley, Lisa  39.2842  -76.6189
    ## 13                                        Isreal, David  39.2853  -76.6190
    ## 14                                 Kastanakis, Theodore  39.2886  -76.6236
    ## 15                                  Kouloumbre, Iaonnis  39.2892  -76.6253
    ## 16                            Marangos, Toula & Filipos  39.2905  -76.6140
    ## 17                                    Markiewicz, Robin  39.2862  -76.6191
    ## 18                                Papastefanou, Stanley  39.2873  -76.6250
    ## 19                                        Rouse, Donald  39.2861  -76.6190
    ## 20                                        Rouse, Donald  39.2765  -76.6240
    ## 21                                      Shiflett, Roger  39.2820  -76.6209
    ## 22                                       Solomon, Damon  39.2852  -76.6228
    ## 23                                       Solomon, Damon  39.2843  -76.6178
    ## 24                                    Stansbury, Joseph  39.2913  -76.6120
    ## 25                                         Reid, Gloria  39.2862  -76.6189
    ## 26                                     Wheatley, Vinnie  39.2842  -76.6188
    ## 27                                      Lerman, Abraham  39.2864  -76.6206
    ## 28                                      Bamba, Youssouf  39.2800  -76.6241
    ## 29                 Mazouz, Abdelkarim & Argoum, Mohamed  39.2895  -76.6153
    ## 30                              Hynson, Jr., Raymond C.  39.2843  -76.6189
    ## 31                                    Djelassi, Chaouki  39.2870  -76.6176
    ## 32                                      K&B Enterprises  39.2887  -76.6152
    ## 33                                  Giorgakis, Kalliopi  39.2873  -76.6237
    ## 34                                         Omar, Khalid  39.2914  -76.6089
    ## 35                                         Cotton, Eric  39.2851  -76.6230
    ## 36                      Quint, Brad (The Beef Brothers)  39.2896  -76.6176
    ## 37                           Kohlhepp, III, Mrs William  39.2850  -76.6231
    ## 38                                Brewer, Donald Thomas  39.2843  -76.6179
    ## 39                                     Dunlap, Patricia  39.2772  -76.6268
    ## 40                                    Djelassi, Chaouki  39.2911  -76.6139
    ## 41                                       Blimline, Lisa  39.2819  -76.6203
    ## 42                                       Blimline, Lisa  39.2850  -76.6232
    ## 43                                        Rouse, Nicole  39.2855  -76.6223
    ## 44                                            Lee, Gary  39.2843  -76.6181
    ## 45                                      Azzouni, Jaafer  39.2903  -76.6109
    ## 46                                   Mazouz, Abdelkarim  39.2878  -76.6176
    ## 47                                   Stallings, Sha-nel  39.2911  -76.6126
    ## 48                                      Azzouni, Jaafer  39.2882  -76.6070
    ## 49                                      Roberts, Melvin  39.2854  -76.6190
    ## 50 Diakgeorgiou, Euthoxia t/a "Georgey Dee's Food Cart"  39.2925  -76.6126
    ## 51                   Winfield, Brian   "Hollywood Dogs"  39.2914  -76.6104
    ## 52                                        Holmes, Robin  39.2888  -76.6081
    ## 53                                        Rouse, Donald  39.2793  -76.6225
    ## 54                                      Strunk, Kum Cha  39.2780  -76.6262
    ## 55                                      Strunk, Kum Cha  39.2856  -76.6223
    ## 56                                     Johnson, Antoine  39.2985  -76.6211
    ## 57                           Saldana, Maria Teresa Luna  39.2027  -76.5587
    ## 58                                        Canty, Albert  39.2891  -76.6282
    ## 59                                         Hummel, Gary  39.3284  -76.6136
    ## 60                               Polychronis, Aristides  39.2986  -76.5910
    ## 61                              Jarava, Edgar & Gustavo  39.2985  -76.5921
    ## 62                              Jarava, Edgar & Gustavo  39.2964  -76.5939
    ## 63                              Jarava, Edgar & Gustava  39.2973  -76.5907
    ## 64                                         Hummel, Gary  39.3252  -76.6220
    ## 65                                       McCoy, Patrice  39.2985  -76.5919
    ## 66                                         Barr, Nomiki  39.2889  -76.6326
    ## 67                                   Gilliam, Gwendolyn  39.3072  -76.6148
    ## 68                                  Elmonir, Elsayed M.  39.3065  -76.6177
    ## 69                                       Lewis, Cynthia  39.2856  -76.6367
    ## 70                                       Parrott, Aaron  39.2933  -76.6078
    ## 71           Vasquez, Emenegildo " Vasquez Fresh Fruit"  39.2998  -76.5650
    ## 72                                        Johns, Farley  39.3532  -76.6647
    ## 73                 Guzman, Irma  "Irma's Best Hot Dogs"  39.2847  -76.5932
    ## 74                                      Azzouni, Jaafer  39.3305  -76.6149
    ## 75                    Bethea, Anthony "Biggs Food Cart"  39.2599  -76.6654
    ## 76                                         Brown, April  39.3111  -76.6097
    ## 77                                        Arlene Gordon  39.3144  -76.6770
    ##             town hotdog
    ## 1        Towson   FALSE
    ## 2   Owings Mill    TRUE
    ## 3   Owings Mill    TRUE
    ## 4   Owings Mill    TRUE
    ## 5     Baltimore    TRUE
    ## 6     Baltimore    TRUE
    ## 7     Baltimore    TRUE
    ## 8     Baltimore    TRUE
    ## 9     Baltimore    TRUE
    ## 10    Baltimore    TRUE
    ## 11    Baltimore    TRUE
    ## 12       Laurel   FALSE
    ## 13 Randallstown    TRUE
    ## 14    Baltimore    TRUE
    ## 15    Baltimore    TRUE
    ## 16    Baltimore    TRUE
    ## 17    Baltimore   FALSE
    ## 18    Baltimore    TRUE
    ## 19    Baltimore    TRUE
    ## 20    Baltimore    TRUE
    ## 21    Baltimore    TRUE
    ## 22    Baltimore    TRUE
    ## 23    Baltimore    TRUE
    ## 24    Baltimore   FALSE
    ## 25    Baltimore   FALSE
    ## 26       Laurel    TRUE
    ## 27  Owings Mill    TRUE
    ## 28    Baltimore    TRUE
    ## 29    Baltimore    TRUE
    ## 30  Glen Burnie   FALSE
    ## 31    Baltimore   FALSE
    ## 32 Middle River    TRUE
    ## 33    Baltimore    TRUE
    ## 34    Baltimore    TRUE
    ## 35    Baltimore   FALSE
    ## 36    Baltimore    TRUE
    ## 37 Reisterstown    TRUE
    ## 38 Reisterstown    TRUE
    ## 39    Baltimore    TRUE
    ## 40    Baltimore   FALSE
    ## 41    Baltimore    TRUE
    ## 42    Baltimore    TRUE
    ## 43    Baltimore    TRUE
    ## 44    Baltimore   FALSE
    ## 45 Windsor Mill   FALSE
    ## 46    Baltimore    TRUE
    ## 47    Baltimore    TRUE
    ## 48 Windsor Mill    TRUE
    ## 49    Baltimore   FALSE
    ## 50    Baltimore    TRUE
    ## 51    Baltimore    TRUE
    ## 52   Pikesville    TRUE
    ## 53    Baltimore   FALSE
    ## 54    Baltimore    TRUE
    ## 55    Baltimore   FALSE
    ## 56     Edgewood    TRUE
    ## 57    Baltimore   FALSE
    ## 58    Baltimore   FALSE
    ## 59    Baltimore    TRUE
    ## 60    Baltimore    TRUE
    ## 61    Baltimore    TRUE
    ## 62    Baltimore    TRUE
    ## 63    Baltimore    TRUE
    ## 64    Baltimore    TRUE
    ## 65    Baltimore    TRUE
    ## 66     Pasadena    TRUE
    ## 67       Towson   FALSE
    ## 68    Baltimore    TRUE
    ## 69    Baltimore    TRUE
    ## 70       Laurel    TRUE
    ## 71     Rosedale   FALSE
    ## 72    Baltimore    TRUE
    ## 73    Baltimore    TRUE
    ## 74 Windsor Mill   FALSE
    ## 75    Baltimore    TRUE
    ## 76    Baltimore    TRUE
    ## 77   Pikesville    TRUE
