library(ggplot2)
library(tibble)

x <- tibble(name = c("biden", "trump"), votes = c(10, 5))

ggplot(data = x, aes(x = name, y = votes)) +
  geom_bar(stat = "identity", fill = c("red", "blue")) +
  theme_minimal()
