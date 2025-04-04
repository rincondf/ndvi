

mod2_y <- lm(Yield ~ PLATFORM * NDRE, data = S_NDRE_alt, subset = Details == "HiRain_Hard_SP")
summary(mod2_y)


mod2_y1 <- lm(Yield ~ PLATFORM * NDRE, data = S_NDRE_alt, subset = Details == "HiRain_SWW_SP")
summary(mod2_y1)



mod2_y2 <- lm(Yield ~ PLATFORM * NDRE, data = S_NDRE_alt, subset = Details == "Metribuzin_Trial_Spray")
summary(mod2_y2)



mod2_y3 <- lm(Yield ~ PLATFORM * NDRE, data = S_NDRE_alt, subset = Details == "Metribuzin_Trial_Spray")
summary(mod2_y3)








par(mfrow = c(2, 2), oma = c(5, 5, 2, 2))


par(mar = c(2, 2, 2, 2))
plot(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
     S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
     xlim = c(0, 0.6),
     ylim = c(20, 150), xlab = "", ylab = "", xaxt = "n", cex.axis = 1.8)

axis(1, at = seq(0, 0.6, 0.1), labels = FALSE)


points(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_Hard_SP")],
       S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], col = "blue")


my.fit2_WSU <- predict(mod2_y, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "WSU" & 
                                                          S_NDRE_alt$Details == "HiRain_Hard_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
      my.fit2_WSU$fit,
      lwd = 2)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
      my.fit2_WSU$fit + my.fit2_WSU$se.fit * qt(0.075, length(my.fit2_WSU$fit) - 2),
      lty = 2)

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
      my.fit2_WSU$fit - my.fit2_WSU$se.fit * qt(0.075, length(my.fit2_WSU$fit) - 2),
      lty = 2)



my.fit2_TAMU <- predict(mod2_y, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "TAMU" & 
                                                           S_NDRE_alt$Details == "HiRain_Hard_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
      my.fit2_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
      my.fit2_TAMU$fit + my.fit2_TAMU$se.fit * qt(0.075, length(my.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_Hard_SP")], 
      my.fit2_TAMU$fit - my.fit2_TAMU$se.fit * qt(0.075, length(my.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")

###########

par(mar = c(2, 2, 2, 2))

plot(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
     S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
     xlim = c(0, 0.6),
     ylim = c(20, 150), xlab = "", ylab = "", xaxt = "n", yaxt  = "n", cex.axis = 1.8)

axis(1, at = seq(0, 0.6, 0.1), labels = FALSE)
axis(2, at = seq(20, 150, 20), labels = FALSE)

points(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_SWW_SP")],
       S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], col = "blue")


my1.fit2_WSU <- predict(mod2_y1, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "WSU" & 
                                                            S_NDRE_alt$Details == "HiRain_SWW_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
      my1.fit2_WSU$fit,
      lwd = 2)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
      my1.fit2_WSU$fit + my1.fit2_WSU$se.fit * qt(0.075, length(my1.fit2_WSU$fit) - 2),
      lty = 2)

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
      my1.fit2_WSU$fit - my1.fit2_WSU$se.fit * qt(0.075, length(my1.fit2_WSU$fit) - 2),
      lty = 2)



my1.fit2_TAMU <- predict(mod2_y1, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "TAMU" & 
                                                             S_NDRE_alt$Details == "HiRain_SWW_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
      my1.fit2_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
      my1.fit2_TAMU$fit + my1.fit2_TAMU$se.fit * qt(0.075, length(my1.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "HiRain_SWW_SP")], 
      my1.fit2_TAMU$fit - my1.fit2_TAMU$se.fit * qt(0.075, length(my1.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")


####################


par(mar = c(2, 2, 2, 2))

plot(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
     S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
     xlim = c(0, 0.6),
     ylim = c(20, 150), xlab = "", ylab = "", cex.axis = 1.8)

axis(1, at = seq(0, 0.6, 0.1), labels = FALSE)

points(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")],
       S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], col = "blue")


my2.fit2_WSU <- predict(mod2_y2, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "WSU" & 
                                                            S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit2_WSU$fit,
      lwd = 2)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit2_WSU$fit + my2.fit2_WSU$se.fit * qt(0.075, length(my2.fit2_WSU$fit) - 2),
      lty = 2)

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit2_WSU$fit - my2.fit2_WSU$se.fit * qt(0.075, length(my2.fit2_WSU$fit) - 2),
      lty = 2)




my2.fit2_TAMU <- predict(mod2_y2, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "TAMU" & 
                                                             S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit2_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit2_TAMU$fit + my2.fit2_TAMU$se.fit * qt(0.075, length(my2.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit2_TAMU$fit - my2.fit2_TAMU$se.fit * qt(0.075, length(my2.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")




######


par(mar = c(2, 2, 2, 2))


plot(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
     S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
     xlim = c(0, 0.6),
     ylim = c(20, 150), xlab = "", ylab = "", yaxt = "n", cex.axis = 1.8)

axis(2, at = seq(20, 150, 20), labels = FALSE)

points(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")],
       S_NDRE_alt$Yield[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], col = "blue")


my3.fit2_WSU <- predict(mod2_y3, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "WSU" & 
                                                            S_NDRE_alt$Details == "Metribuzin_Trial_Spray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit2_WSU$fit,
      lwd = 2)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit2_WSU$fit + my3.fit2_WSU$se.fit * qt(0.075, length(my3.fit2_WSU$fit) - 2),
      lty = 2)

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "WSU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit2_WSU$fit - my3.fit2_WSU$se.fit * qt(0.075, length(my3.fit2_WSU$fit) - 2),
      lty = 2)




my3.fit2_TAMU <- predict(mod2_y3, newdata = S_NDRE_alt[which(S_NDRE_alt$PLATFORM == "TAMU" & 
                                                             S_NDRE_alt$Details == "Metribuzin_Trial_Spray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit2_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit2_TAMU$fit + my3.fit2_TAMU$se.fit * qt(0.075, length(my3.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDRE_alt$NDRE[which(S_NDRE_alt$PLATFORM == "TAMU" & S_NDRE_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit2_TAMU$fit - my3.fit2_TAMU$se.fit * qt(0.075, length(my3.fit2_TAMU$fit) - 2),
      lty = 2, col = "blue")


title(xlab = "NDRE", cex.lab = 2, outer = TRUE)
title(ylab = "Yield", cex.lab = 2, outer = TRUE)


