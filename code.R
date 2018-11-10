barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("194", "197", "95", "15", "156")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("P", "M", "F", "Q", "D", "I", "U", "N", "T", "J", "H", "L")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("55", "197", "156", "97", "72")
plot.PCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("C", "AA", "S", "G", "H", "A", "L", "R")
plot.PCA(res, select = drawn, axes = 3:4, choix = 'var', title = '', cex = cex)
drawn <-
c("194", "12", "17", "156", "95")
plot.PCA(res, select = drawn, axes = 5:6, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("E", "R", "O", "B", "A", "G", "V", "K")
plot.PCA(res, select = drawn, axes = 5:6, choix = 'var', title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("194", "197", "95", "15", "156")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
res.hcpc$desc.var
