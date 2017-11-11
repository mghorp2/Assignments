
par(xpd=NA)
plot(NA, NA, type = "n", xlim = c(0,100), ylim = c(0,100), xlab = "X", ylab = "Y")
lines(x = c(50,50), y = c(0,100))
text(x = 50, y = 108, labels = c("t1"), col = "Blue")
# t2: y = 75; (0, 75) (40, 75)
lines(x = c(0,50), y = c(70,70))
text(x = -8, y = 70, labels = c("t2"), col = "Blue")
# t3: x = 75; (75,0) (75, 100)
lines(x = c(70,70), y = c(0,100))
text(x = 70, y = 108, labels = c("t3"), col = "Blue")
# t4: x = 20; (20,0) (20, 75)
lines(x = c(25,25), y = c(0,70))
text(x = 25, y = -8, labels = c("t4"), col = "Blue")
# t5: y=25; (75,25) (100,25)
lines(x = c(70,100), y = c(35,35))
text(x = 108, y = 35, labels = c("t5"), col = "Blue")

text(x = (50+70)/2, y = 50, labels = c("R1"))
text(x = 20, y = (100+70)/2, labels = c("R2"))
text(x = (70+100)/2, y = (100+35)/2, labels = c("R3"))
text(x = (70+100)/2, y = 35/2, labels = c("R4"))
text(x = 30, y = 70/2, labels = c("R5"))
text(x = 10, y = 70/2, labels = c("R6"))



