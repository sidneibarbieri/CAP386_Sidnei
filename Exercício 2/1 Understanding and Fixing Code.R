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

# Vetor com as temperaturas médias
avgBaltimore <- rowMeans(dfBaltimore[,1:2])
avgBaltimore

# Data Frame com as temperaturas mínimas, máximas e a média
dfBaltimore2 <- data.frame(Low_ºF=lowBaltimore,High_ºF=highBaltimore, Average_ºF=avgBaltimore,row.names = months)
dfBaltimore2

# Conversão para Celsius
lowBaltimoreC <- (5/9) * (lowBaltimore - 32)
highBaltimoreC <- (5/9) * (highBaltimore - 32)
avgBaltimoreC <- (5/9) * (avgBaltimore - 32)


# Data Frame com as temperaturas mínimas, máximas e a média em Celsius (com redução para casas decimais)
dfBaltimoreC <- data.frame(Low_ºC=round(lowBaltimoreC,2),High_ºC=round(highBaltimoreC,2), Average_ºC=round(avgBaltimoreC,2),row.names = months)
dfBaltimoreC

# Data Frame indicando se o mês é quente, frio ou normal
frioBaltimoreC <- ifelse(dfBaltimoreC$Average_ºC<10,"Frio",ifelse(dfBaltimoreC$Average_ºC>25,"Quente","Normal")) 
names(frioBaltimoreC) <- months
frioBaltimoreC


# What would happen if we still had a NA on the vector for the temperatures in SJC? (Lecture02/05-Factors)?
# Média mensal das temperaturas em SJC
avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,NA,17.1,18.8,19.4,20.3,21.4)
names(avgTempSJC) <- months
avgTempSJC

# What happens if we try to run sjcTemps["Jan":"Jul",c(1,3)]? Why? (see Lecture02/06-DataFrames).

# avgTempSJC["Jan": "Jul", c (1,3)]
# Error in "Jan":"Jul" : Argumento NA/NaN
# Além disso: Warning messages:
# 1: NAs introduzidos por coerção 
# 2: NAs introduzidos por coerção 

# Tratando NAs
semNAavgTempSJC <- avgTempSJC[!is.na(avgTempSJC)]
semNAavgTempSJC

# We can change Data Frames with sjcTemps3["Aug",] <- c(1,2,3) and sjcTemps3["Jul",] <- sjcTemps3["Jul",]+c(3,4) but we cannot change with sjcTemps3["Jul",] <- c(3,4). Why? (see Lecture02/06-DataFrames).
# Contruindo o DF
maxTempSJC <- c(29.7,30.1,29.5,27.3,25.1,24.3,24.1,26.2,27.2,27.3,28,28.7)
avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,15.6,17.1,18.8,19.4,20.3,21.4)
minTempSJC <- c(16.2,16.5,15.7,13.2,10.1,8.9,8.2,9.9,11.9,13.4,14.2,15.3)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
sjcTemps <- data.frame(Max=maxTempSJC,Avg=avgTempSJC,Min=minTempSJC,row.names = months)
sjcTemps
class(sjcTemps)
sjcTemps3 <- sjcTemps

# 1º Comando proposto
sjcTemps3["Aug",]
sjcTemps3["Aug",] <- c(1,2,3)
sjcTemps3["Aug",]

# 2º Comando proposto
sjcTemps3["Jul",]
sjcTemps3["Jul",] <- sjcTemps3["Jul",]+c(3,4)
sjcTemps3["Jul",]

# 3º Comando proposto
# sjcTemps3["Jul",] <- c(3,4)
# Error in `[<-.data.frame`(`*tmp*`, "Jul", , value = c(3, 4)) : 
# replacement has 2 items, need 3

# Run some examples that show that the recycling rule reuses the smaller (shorter) vector.
vetor <- c(1,1,1,1,1)
vetor

recycling <- vetor + c(1,0,1)
recycling

recycling <- recycling + c(-1,0,-1,-1)
recycling

# What does stringsAsFactors=FALSE in read.csv() do? Why do we need it? (see Lecture02/06-DataFrames).
taubateWeather <- read.table("https://raw.githubusercontent.com/rafaeldcsantos/CAP386/master/Data/Taubate.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
taubateWeather
# Por padrão, a função read.csv transforma variáveis que contêm caracteres em factor (categórica). Para desabilitar isto, basta especificar na chamada à função stringsAsFactors = FALSE

# Levels 
politicos <- factor(rep("corrupto",10),levels=c("corrupto","honesto"))
politicos

tratamentos <- factor(rep(c("Controle","Adubo A","Adubo B"),each=4))
tratamentos
