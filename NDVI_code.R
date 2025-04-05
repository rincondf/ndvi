require(stargazer)

S_NDVI_alt <- read.csv(file = "Data/S_NDVI_alt.csv")


mod_y <- lm(Yield ~ PLATFORM * NDVI, data = S_NDVI_alt, subset = Details == "HiRain_Hard_SP")
summary(mod_y)

stargazer(mod_y, type = "text", covariate.labels = c("PLATFORM", "NDVI", "PLATFORM:NDVI", "Intercept"),
          title = "HiRain Hard SP")
          
          
mod_y1 <- lm(Yield ~ PLATFORM * NDVI, data = S_NDVI_alt, subset = Details == "HiRain_SWW_SP")
summary(mod_y1)


stargazer(mod_y1, type = "text", covariate.labels = c("PLATFORM", "NDVI", "PLATFORM:NDVI", "Intercept"),
          title = "HiRain SWW SP")


mod_y2 <- lm(formula = Yield ~ PLATFORM * NDVI, data = S_NDVI_alt, subset = Details == "Metribuzin_Trail_NoSpray")
summary(mod_y2)

stargazer(mod_y2, type = "text", covariate.labels = c("PLATFORM", "NDVI", "PLATFORM:NDVI", "Intercept"),
          title = "Metribuzin Trial No Spray")


mod_y3 <- lm(Yield ~ PLATFORM * NDVI, data = S_NDVI_alt, subset = Details == "Metribuzin_Trial_Spray")
summary(mod_y3)

stargazer(mod_y3, type = "text", covariate.labels = c("PLATFORM", "NDVI", "PLATFORM:NDVI", "Intercept"),
          title = "Metribuzin Trial Spray")



# FIGURE


par(mfrow = c(2, 2), oma = c(5, 5, 2, 2))


par(mar = c(2, 2, 2, 2))
plot(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
     S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
     xlim = c(0.3, 1),
     ylim = c(20, 150), xlab = "", ylab = "", xaxt = "n", cex.axis = 1.8, main = "HiRain_Hard_SP")

axis(1, at = seq(0.3, 1, 0.1), labels = FALSE)


points(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_Hard_SP")],
       S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], col = "blue")


my.fit_WSU <- predict(mod_y, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "WSU" & 
                                                      S_NDVI_alt$Details == "HiRain_Hard_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
      my.fit_WSU$fit,
      lwd = 2)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
      my.fit_WSU$fit + my.fit_WSU$se.fit * qt(0.075, length(my.fit_WSU$fit) - 2),
      lty = 2)

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
      my.fit_WSU$fit - my.fit_WSU$se.fit * qt(0.075, length(my.fit_WSU$fit) - 2),
      lty = 2)



my.fit_TAMU <- predict(mod_y, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "TAMU" & 
                                                          S_NDVI_alt$Details == "HiRain_Hard_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
      my.fit_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
      my.fit_TAMU$fit + my.fit_TAMU$se.fit * qt(0.075, length(my.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_Hard_SP")], 
      my.fit_TAMU$fit - my.fit_TAMU$se.fit * qt(0.075, length(my.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")

###########

par(mar = c(2, 2, 2, 2))

plot(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
     S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
     xlim = c(0.3, 1),
     ylim = c(20, 150), xlab = "", ylab = "", xaxt = "n", yaxt  = "n", cex.axis = 1.8,
     main = "HiRain_SWW_SP")

axis(1, at = seq(0.3, 1, 0.1), labels = FALSE)
axis(2, at = seq(20, 150, 20), labels = FALSE)

points(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_SWW_SP")],
       S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], col = "blue")


my1.fit_WSU <- predict(mod_y1, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "WSU" & 
                                                          S_NDVI_alt$Details == "HiRain_SWW_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
      my1.fit_WSU$fit,
      lwd = 2)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
      my1.fit_WSU$fit + my1.fit_WSU$se.fit * qt(0.075, length(my1.fit_WSU$fit) - 2),
      lty = 2)

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
      my1.fit_WSU$fit - my1.fit_WSU$se.fit * qt(0.075, length(my1.fit_WSU$fit) - 2),
      lty = 2)



my1.fit_TAMU <- predict(mod_y1, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "TAMU" & 
                                                           S_NDVI_alt$Details == "HiRain_SWW_SP"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
      my1.fit_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
      my1.fit_TAMU$fit + my1.fit_TAMU$se.fit * qt(0.075, length(my1.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "HiRain_SWW_SP")], 
      my1.fit_TAMU$fit - my1.fit_TAMU$se.fit * qt(0.075, length(my1.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")


####################


par(mar = c(2, 2, 2, 2))

plot(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
     S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
     xlim = c(0.3, 1),
     ylim = c(20, 150), xlab = "", ylab = "", cex.axis = 1.8,
     main = "Metribuzin_Trail_NoSpray")

axis(1, at = seq(0.3, 1, 0.1), labels = FALSE)

points(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")],
       S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], col = "blue")


my2.fit_WSU <- predict(mod_y2, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "WSU" & 
                                                          S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit_WSU$fit,
      lwd = 2)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit_WSU$fit + my2.fit_WSU$se.fit * qt(0.075, length(my2.fit_WSU$fit) - 2),
      lty = 2)

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit_WSU$fit - my2.fit_WSU$se.fit * qt(0.075, length(my2.fit_WSU$fit) - 2),
      lty = 2)




my2.fit_TAMU <- predict(mod_y2, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "TAMU" & 
                                                           S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit_TAMU$fit + my2.fit_TAMU$se.fit * qt(0.075, length(my2.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trail_NoSpray")], 
      my2.fit_TAMU$fit - my2.fit_TAMU$se.fit * qt(0.075, length(my2.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")




######


par(mar = c(2, 2, 2, 2))


plot(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
     S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
     xlim = c(0.3, 1),
     ylim = c(20, 150), xlab = "", ylab = "", yaxt = "n", cex.axis = 1.8,
     main = "Metribuzin_Trial_Spray")

axis(2, at = seq(20, 150, 20), labels = FALSE)

points(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")],
       S_NDVI_alt$Yield[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], col = "blue")


my3.fit_WSU <- predict(mod_y3, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "WSU" & 
                                                            S_NDVI_alt$Details == "Metribuzin_Trial_Spray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit_WSU$fit,
      lwd = 2)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit_WSU$fit + my3.fit_WSU$se.fit * qt(0.075, length(my3.fit_WSU$fit) - 2),
      lty = 2)

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "WSU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit_WSU$fit - my3.fit_WSU$se.fit * qt(0.075, length(my3.fit_WSU$fit) - 2),
      lty = 2)




my3.fit_TAMU <- predict(mod_y3, newdata = S_NDVI_alt[which(S_NDVI_alt$PLATFORM == "TAMU" & 
                                                             S_NDVI_alt$Details == "Metribuzin_Trial_Spray"), c(2, 3, 4)], se.fit = TRUE)


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit_TAMU$fit,
      lwd = 2, col = "blue")


lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit_TAMU$fit + my3.fit_TAMU$se.fit * qt(0.075, length(my3.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")

lines(S_NDVI_alt$NDVI[which(S_NDVI_alt$PLATFORM == "TAMU" & S_NDVI_alt$Details == "Metribuzin_Trial_Spray")], 
      my3.fit_TAMU$fit - my3.fit_TAMU$se.fit * qt(0.075, length(my3.fit_TAMU$fit) - 2),
      lty = 2, col = "blue")


title(xlab = "NDVI", cex.lab = 2, outer = TRUE)
title(ylab = "Yield", cex.lab = 2, outer = TRUE)
