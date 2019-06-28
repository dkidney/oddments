\dontrun{
    
# original coordinates
x = cbind(x = c(1,1,2,2), y = c(1,2,2,1))
plot(x, type = "n", xlim = c(-0.5,2.5), ylim = c(-0.5,2.5), asp = 1, xlab = "x")
polygon(x)

# axis of rotation
axis = c(1,1)
points(axis[1], axis[2], col=2, pch=19)

# rotate clockwise by 2/3 pi radians
rotated = rotate_coords(x, angle = 2/3*pi, axis = axis)
polygon(rotated, border = 4)

# rotate anti-clockwise by 2/3 pi radians
rotated = rotate_coords(x, angle = 2/3*pi, axis = axis, clockwise = FALSE)
polygon(rotated, border = 3)
}
