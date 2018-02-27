x = seq(from = 0, to = 2*pi, length.out = 1000)

nb_move = 5 # recommanded = 5

plot(0, 0, xlim = c(0,1), ylim = c(0, 1), type = "n")

for(t in seq(from = 0, to = 2*pi, length.out = nb_move)) {

  lines(x/(2*pi), (sin(x+t)+x-sin(t))/(2*pi), type = "l", col = "blue")

}

lines(x/(2*pi), (sin(x) + x)/(2*pi), type = "l", col = "red")

lines(x/(2*pi), (cos(x) + x - 1)/(2*pi), type = "l")