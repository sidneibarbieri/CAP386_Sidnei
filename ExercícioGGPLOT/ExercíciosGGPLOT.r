library(datasets)
str(anscombe)

anscombe1 <- data.frame(anscombe$x1, anscombe$y1)
anscombe1
names(anscombe1) <- c("x", "y")
anscombe1$Grupo = rep("A", 11)

anscombe2 <- data.frame(anscombe$x2, anscombe$y2)
names(anscombe2) <- c("x", "y")
anscombe2$Grupo = rep("B", 11)

anscombe3 <- data.frame(anscombe$x3, anscombe$y3)
names(anscombe3) <- c("x", "y")
anscombe3$Grupo = rep("C", 11)

anscombe4 <- data.frame(anscombe$x4, anscombe$y4)
names(anscombe4) <- c("x", "y")
anscombe4$Grupo = rep("D", 11)

todo <- rbind(anscombe1, anscombe2, anscombe3, anscombe4)
todo

plot(todo)

library(ggplot2)
ggplot(todo, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'red', lwd=0.2)
  facet_wrap(~Grupo)

  
  
