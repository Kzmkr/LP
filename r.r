# Create a sequence of x values
x <- seq(0, 40, 1)

# Define the linear function for 4x + 3y = k
equation <- function(x, k) {
  return((k - 4 * x) / 3)
}

# Define the constraints
c1 <- 40 - x
c2 <- 60 - 2 * x

# Calculate the minimum values
y_min_value <- min(c1, c2)

# Find intersection
common_elements <- intersect(c1, c2)
same_indices <- which(c1 == c2)
same_elements <- c1[same_indices]

# Plot the constraints and shaded area
plot(x, x, type = "n", ylab = "y", xlab = "x", main = "Linear Programing", xaxs = "i", yaxs = "i")
polygon(c(x, rev(x)), c(y_min, rep(0, length(x))), col = "azure2", density = NA,)
polygon(c(x, rev(x)), c(y_min, rep(0, length(x))), col = "green", density = 9, angle = 45)
lines(x, c1, type = "l", col = "black")
lines(x, c2, type = "l", col = "Red")

# Add horizontal and vertical lines at intersection points
abline(h = same_elements, col = "blue", lty = 2)
abline(v = same_indices - 1, col = "blue", lty = 2)

# Add points and labels at intersection points
points(same_indices - 1, same_elements, pch = 19, col = "Orange",cex = 2)
text(same_indices - 1, same_elements, paste("(", same_indices - 1, ",", same_elements, ")", sep = ""), pos = 3, col = "black", cex = 2)

# Calculate the constant k for the parallel line
k <- 4 * (same_indices-1) + 3 * same_elements

k_values <- seq(0, k, by = k/5) # Range of k values for parallel lines
for (k in k_values) {
  y_parallel <- equation(x, k)
  lines(x, y_parallel, col = "purple", lty = 3)
}

# Add a legend
legend("topright", legend = c("Korlátozó feltétel 1", "Korlátozó feltétel 2", "Optimális megoldás", "Profit szintvonalnak"), col = c("black", "red", "Orange", "purple"), lty = c(1, 1, NA, 3), pch = c(NA, NA, 19, NA))
