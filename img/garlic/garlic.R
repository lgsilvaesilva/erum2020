library(imager)
library(sf)
# garlic <- load.image('img/garlic-voronoi.jpg')
garlic <- load.image('img/garlic-voronoi-bb.png')

# x <- 1:900
# y <- 1200:0
x <- 0:356
y <- 399:0
z <- expand.grid(x, y)

par(mar = rep(0, 4))
plot(garlic)
id <- identify(z)
# id <- c(362251, 381267, 406568, 437763, 484675, 491053, 503754, 534865, 535019, 601647, 639644, 651925, 677314, 743816)

par(mar = rep(0, 4))
plot(garlic)
points(z[id, ], col = 'red', pch = 19)

new_bb <- c(min(x), min(y), max(x), max(y))
names(new_bb) <- c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") <- "bbox"
attr(cents_garlic_sf, "bbox") <- new_bb

cents_garlic <- z[id, ]
cents_garlic_sf <- st_as_sf(cents_garlic, coords = c("Var1","Var2"), remove = FALSE)
plot(cents_garlic_sf$geometry, add = T, col = "red", pch = 19)

v <- do.call(c, st_geometry(cents_garlic_sf))
xx <- st_voronoi(v, dTolerance = 1)

png('garlic-00.png')
par(mar = rep(0, 4), bg = 'black')
plot(garlic)
dev.off()
plot(xx, add = T, border = 'gray60', col = NA)  

png(sprintf('garlic-%s.png', formatC(i, digits = 2, 2, flag = '0')))
par(mar = rep(0, 4), bg = 'transparent')
plot(garlic)
plot(do.call(c, xx[1:i]), add = T, border = 'gray60', col = scales::alpha('indianred', .3))  
dev.off()

set.seed(1)
for(i in 1:length(xx)) {
  png(sprintf('garlic-%s.png', formatC(i, digits = 2, 2, flag = '0')))
  par(mar = rep(0, 4), bg = 'black')
  plot(garlic)
  plot(do.call(c, xx[1:i]), add = T, border = 'gray60', col = scales::alpha('indianred', .3))  
  dev.off()
}

