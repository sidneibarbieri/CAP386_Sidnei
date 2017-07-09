avgTempSJC <- c(22.2,22.4,21.6,19.6,17,16.1,15.6,17.1,18.8,19.4,20.3,21.4)

names(avgTempSJC) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

avgTempSJC

descTempSJC <- vector(length = length(avgTempSJC))
descTempSJC

descTempSJC[avgTempSJC>20] <- 'Hot' 
descTempSJC

descTempSJC[avgTempSJC<18] <- 'Cool' 
descTempSJC

class(descTempSJC)

descTempSJC[(avgTempSJC >= 18) & (avgTempSJC <= 20)] <- 'Mild'
descTempSJC

class(descTempSJC)

descTempSJC <- factor(descTempSJC)
descTempSJC

class(descTempSJC)

descTempSJC <- cut(avgTempSJC,
                   breaks=c(-Inf,18,20, Inf),
                   labels=c("Cool","Mild","Hot"))
descTempSJC
class(descTempSJC)
