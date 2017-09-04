library(datasets)
library(dplyr) 

### Importando o arquivo

orig <- getwd()
orig
setwd(orig)

file <- "CoverType.csv"
df<-read.csv2(file, header = TRUE, sep =";", stringsAsFactors = FALSE, encoding = "UTF-8")
as.data.frame(df)
df

df <- tbl_df(df)
summary(df)

### Separando as Famílias
Type <- strsplit(as.character(df$Description),'-')
head(Type)
for(i in 1:nrow(df)) 
{
  Type$Family[i] <- Type[[i]][1]
}

df$Type <- as.character(Type$Family)
df$Type

df <-  select(df, Study, Code, Type)
head(df)

summary(Type)


