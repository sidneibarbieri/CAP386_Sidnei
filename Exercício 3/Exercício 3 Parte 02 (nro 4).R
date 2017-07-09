## Initial Part. Given by prefessor
##### Donwload do arquivo
vendors <- "https://data.baltimorecity.gov/api/views/bqw3-z52q/rows.csv?accessType=DOWNLOAD"
download.file(vendors,destfile = "../TempData/BFood.csv",method="curl")
if (file.exists("../TempData/BFood.csv"))
{
  tam <- file.info("../TempData/BFood.csv")$size
  paste("File downloaded, ",tam," bytes")
} else
{
  "Error downloading file!"
}

##### Construção do data.frame
bVendors <- read.csv(file="../TempData/BFood.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
str(bVendors)

##### Números de Licença como Factor (conveniente para ordenação)
bVendors$LicenseNum <- as.factor(bVendors$LicenseNum)

##### Adição de colunas para a Cidade, a Latitude e a Longitude
bVendors$latitude <- vector(length = nrow(bVendors),mode = "numeric")
bVendors$longitude <- vector(length = nrow(bVendors),mode = "numeric")
bVendors$town <- vector(length = nrow(bVendors),mode = "character")

##### Extração da Cidade, da Latitude e da Longitude a partir da coluna Location.1
names(bVendors)[names(bVendors) == "Location.1"] <- "location"
templocation <- unlist(regmatches(bVendors$location,gregexpr("[-0-9.]+",bVendors$location)))
templocation
x <- 1
jump <- 2 # Saltar o código numérico que não será utilizado
y <- nrow(bVendors)

library('stringr')
while (x<=y) {
  bVendors$latitude[x] <- as.numeric(templocation[jump])
  bVendors$longitude[x] <- as.numeric(templocation[jump+1])
  bVendors$town[x] <- str_replace_all(bVendors$location[x], "[^A-z]+", " ") 
  
  x <- x+1
  jump <- jump+3 # Uma mesma localização é composta por um grupo de 3 dados (código, lat, long)
}


##### Hot dog variations (franks, HOT DOG, hOT dOG, etc.) 
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

##### Descartando colunas consideradas de menor relevância para a operação "Comer Hot Dog, custe o que custar!"
colnames(bVendors) # Muita informação

bVendors$Id <- NULL
bVendors$LicenseNum <- NULL
bVendors$VendorAddr <- NULL
bVendors$VendorAddr <- NULL
bVendors$ItemsSold <- NULL
bVendors$Cart_Descr <- NULL
bVendors$St <- NULL
bVendors$location <- NULL

## Resultado Final 
##### Quem? Qual cidade? Posso por no Waze? Será que tem hot dog?
bVendors