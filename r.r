# Le generáljuk az x értékeket
x <- seq(0, 40, 1)

# definiáljuk a lineáris egyenletet 4x + 3y = k ,vissza adjuk y értékét
equation <- function(x, k) {
  return((k - 4 * x) / 3)
}

# Definiáljuk a feltételeket és legeneráljuk a y értékeket
c1 <- 40 - x
c2 <- 60 - 2 * x

# Kiszámoljuk a minimumot
y_min <- pmin(c1, c2)


# Megkeressük a metszetet
common_elements <- intersect(c1, c2)
same_indices <- which(c1 == c2) #y érték
same_elements <- c1[same_indices] #x érték

# Ábrázoljuk a feltételeket és a LMH
plot(x, x, type = "n", ylab = "y", xlab = "x", main = "Linear Programing", xaxs = "i", yaxs = "i")  #koordináta renszer beállítása
polygon(c(x, rev(x)), c(y_min, rep(0, length(x))), col = "azure2", density = NA,)   #LMH ábrázolása színel
polygon(c(x, rev(x)), c(y_min, rep(0, length(x))), col = "green", density = 9, angle = 45) #LMH ábrázolása vonalakal
lines(x, c1, type = "l", col = "black") #korlátozó feltétel 1
lines(x, c2, type = "l", col = "Red")   #korlátozó feltétel 2

# Horizontális és vertikális vonal ábrázolása a metszésponthoz
abline(h = same_elements, col = "blue", lty = 2)    # horizontális vonal
abline(v = same_indices - 1, col = "blue", lty = 2) # vertikális vonal

# A metszéspont ábrázolása és  koordináták kiírása
points(same_indices - 1, same_elements, pch = 19, col = "Orange",cex = 2) # metszéspont ábrázolása
text(same_indices - 1, same_elements, paste("(", same_indices - 1, ",", same_elements, ")", sep = ""), pos = 3, col = "black", cex = 2)    # koordináták kiírása

# Kiszámoljuk a lineáris egyenletet a metszéspontnál
k <- 4 * (same_indices-1) + 3 * same_elements

k_values <- seq(0, k, by = k/5) #5db x értek generálása a profit szintvonalakhoz 0-tol a metszéspontig
for (k in k_values) {
  lines(x, equation(x, k), col = "purple", lty = 3) # profit szintvonalak ábrázolása
}

# feliratozás
legend("topright", legend = c("Korlátozó feltétel 1", "Korlátozó feltétel 2", "Optimális megoldás", "Profit szintvonalnak"), col = c("black", "red", "Orange", "purple"), lty = c(1, 1, NA, 3), pch = c(NA, NA, 19, NA))
