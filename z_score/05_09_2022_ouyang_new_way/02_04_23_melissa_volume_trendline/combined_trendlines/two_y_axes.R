
library(ggplot2)

colors <- c("Salary"= "red", "Vacation Days"="blue")

# Create example data
data <- data.frame(age = c(20, 25, 30, 35, 40),
                   salary = c(50000, 60000, 70000, 80000, 90000),
                   vacation_days = c(10, 15, 10, 25, 30))

# Create ggplot2 plot with two y-axes
ggplot(data, aes(x = age)) +
  # geom_line(aes(y = salary), color = "red") +
  # geom_line(aes(y = vacation_days*3000), color = "blue") +
  geom_line(aes(y = salary, color = "Salary")) +
  geom_line(aes(y = vacation_days*3000, color = "Vacation Days")) +
  scale_y_continuous(name = "Salary", sec.axis = sec_axis(~./3000, name = "Vacation Days")) +
  scale_color_manual(values = colors)
  