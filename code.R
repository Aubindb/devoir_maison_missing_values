barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("194", "197", "95", "156", "15")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("F", "P", "M", "D", "I", "U", "T", "J", "N", "H", "L")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("55", "197", "156", "97", "72")
plot.PCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("AA", "C", "S", "G", "R", "A", "H")
plot.PCA(res, select = drawn, axes = 3:4, choix = 'var', title = '', cex = cex)
drawn <-
c("194", "12", "17", "156", "52")
plot.PCA(res, select = drawn, axes = 5:6, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("E", "B", "A", "O", "R", "K", "M")
plot.PCA(res, select = drawn, axes = 5:6, choix = 'var', title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("194", "197", "95", "156", "15")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
res.hcpc$desc.var
