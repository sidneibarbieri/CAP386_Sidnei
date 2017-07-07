# Understanding and Fixing Code

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




