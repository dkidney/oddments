
require(oddments)

coords = cbind(x = c(1,1,2,2), y = c(1,2,2,1))

s = 300

png("examples/rotate.coords.plot1.png", width = s, height = s)
par(mar = c(2,2,0,0))
plot(coords, type = "n", xlim = c(-0.5,2.5), ylim = c(-0.5,2.5), asp = 1)
polygon(coords)
dev.off()

axis = c(1,1)
new = rotate.coords(coords, angle = 2/3*pi, axis = axis)

png("examples/rotate.coords.plot2.png", width = s, height = s)
par(mar = c(2,2,0,0))
plot(coords, type = "n", xlim = c(-0.5,2.5), ylim = c(-0.5,2.5), asp = 1)
polygon(coords)
points(axis[1], axis[2], col=2, pch=19)
polygon(new, border = 4)
dev.off()

new2 = rotate.coords(coords, angle = 120, axis = axis, clockwise = FALSE, units = "degrees") 

png("examples/rotate.coords.plot3.png", width = s, height = s)
par(mar = c(2,2,0,0))
plot(coords, type = "n", xlim = c(-0.5,2.5), ylim = c(-0.5,2.5), asp = 1)
polygon(coords)
points(axis[1], axis[2], col=2, pch=19)
polygon(new, border = 4)
polygon(new2, border = 3)
dev.off()

