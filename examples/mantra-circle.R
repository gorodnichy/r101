
# shift labels ----

# This code draws a circle, then draws marks (dot and label beside the dot)
# on this circle, then connects using line previous dot in a list  to the next one in the list
# 0 degrees starts from point (0,1) going clockwise.
# These dots(marks) correspond to the words of "Om-ah-hum-vajra-guru-padma-siddhi-hum" mantra.

# use of data.table and ggplot is demonstrated.

vajra_mantra_cicle <- function (my_colour="green", equalspacing=F) {
  # Install and load necessary packages
  if (!requireNamespace("ggplot2")) {
    install.packages("ggplot2")
  }
  if (!requireNamespace("data.table")) {
    install.packages("data.table")
  }
  library(ggplot2)
  library(data.table)

  # Create a data.table to store the information for each mark
  marks_data <- data.table(
    id = 1:9,
    label_eng = c("Om", "Ah", "Hum", "Vajra", "Guru", "Padma", "Siddhi", "", ""),
    label_san = c("ॐ", "अः", "हूं", "वज्र", "गुरु", "पद्म", "सिद्धि", "", ""),
    degrees = c(270, 90, 0, 135, 315, 45, 225, 0, 270)
  )

  d = 360 / 5
  if (equalspacing)
    marks_data$degrees = c(240, 120, 0, 2*d, 360-d, d, 3*d, 0, 240)

  marks_data [, label :=   paste0(label_san, " \n", label_eng) ]

  # Function to calculate the coordinates on the circle based on degrees
  get_circle_coordinates <- function(degrees, radius = 1) {
    radians <- (90 - degrees) * pi / 180
    x <- radius * cos(radians)
    y <- radius * sin(radians)
    return(data.table(x = x, y = y))
  }

  # Calculate coordinates for each mark on the circle
  marks_data[, c("x", "y") := as.list(get_circle_coordinates(degrees))]

  # Create a data.table for circle points
  circle_points <- get_circle_coordinates(seq(0, 360, 10))

  # Create a ggplot with a circle and marks
  p <- ggplot() +
    # Draw the circle
    geom_path(data = circle_points, aes(x, y), color = "black") +
    coord_fixed() +
    # theme(plot.margin = unit(rep(0, 4), "cm"))  + # Remove plot margins
    theme_void()


  # Add the marks as points and labels (positioned outside the circle)
  p <- p +
    geom_point(data = marks_data[1:7], aes(x, y), size = 3, color = my_colour) +
    geom_text(data = marks_data[1:7], aes(x = 1.2 * x, y = 1.2 * y, label = label), color = my_colour)

  # Add lines connecting the dots (excluding the last two)
  p <- p +
    geom_segment(
      data = marks_data[1:7],  # Exclude last two rows
      aes(x = x, y = y, xend = shift(x, type = "lead"), yend = shift(y, type = "lead")),
      color = my_colour
    )

  # Connect dot 7 to dot 8 (which is 3)
  p <- p +
    geom_segment(
      data = marks_data[c(7, 8)],
      aes(x = x, y = y, xend = x[1], yend = y[1]),
      color = my_colour
    )

  # Connect dot 8 (which is 3) to dot 9 (which is 1)
  p <- p +
    geom_segment(
      data = marks_data[c(8, 9)],
      aes(x = x, y = y, xend = x[1], yend = y[1]),
      color = my_colour
    )

  print(p)


}
vajra_mantra_cicle()
vajra_mantra_cicle("red", T)
