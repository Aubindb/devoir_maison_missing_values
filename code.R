barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("194", "55", "197", "97", "15")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("GNI, PPP (current international $) [NY.GNP.MKTP.PP.CD]", "GDP (current US$) [NY.GDP.MKTP.CD]", 
"GNI, Atlas method (current US$) [NY.GNP.ATLS.CD]", "Life expectancy at birth, total (years) [SP.DYN.LE00.IN]", 
"Mortality rate, under-5 (per 1,000 live births) [SH.DYN.MORT]", 
"Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]", 
"Foreign direct investment, net inflows (BoP, current US$) [BX.KLT.DINV.CD.WD]", 
"GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]", 
"Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]", 
"Mobile cellular subscriptions (per 100 people) [IT.CEL.SETS.P2]", 
"Adolescent fertility rate (births per 1,000 women ages 15-19) [SP.ADO.TFRT]", 
"Personal remittances, paid (current US$) [BM.TRF.PWKR.CD.DT]", 
"GNI per capita, Atlas method (current US$) [NY.GNP.PCAP.CD]"
)
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("197", "156", "95", "55", "165")
plot.PCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("Net barter terms of trade index (2000 = 100) [TT.PRI.MRCH.XD.WD]", 
"Industry (including construction), value added (% of GDP) [NV.IND.TOTL.ZS]", 
"Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]", 
"Merchandise trade (% of GDP) [TG.VAL.TOTL.GD.ZS]", "Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]", 
"Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]", "CO2 emissions (metric tons per capita) [EN.ATM.CO2E.PC]", 
"Foreign direct investment, net inflows (BoP, current US$) [BX.KLT.DINV.CD.WD]"
)
plot.PCA(res, select = drawn, axes = 3:4, choix = 'var', title = '', cex = cex)
drawn <-
c("55", "12", "17", "194", "156")
plot.PCA(res, select = drawn, axes = 5:6, choix = 'ind', invisible = 'quali', title = '', cex = cex)
drawn <-
c("Foreign direct investment, net (BoP, current US$) [BN.KLT.DINV.CD]", 
"Inflation, consumer prices (annual %) [FP.CPI.TOTL.ZG]", "Gross capital formation (% of GDP) [NE.GDI.TOTL.ZS]", 
"Inflation, GDP deflator (annual %) [NY.GDP.DEFL.KD.ZG]", "Population, total [SP.POP.TOTL]", 
"Personal remittances, paid (current US$) [BM.TRF.PWKR.CD.DT]", 
"GNI per capita, Atlas method (current US$) [NY.GNP.PCAP.CD]", 
"Immunization, measles (% of children ages 12-23 months) [SH.IMM.MEAS]"
)
plot.PCA(res, select = drawn, axes = 5:6, choix = 'var', title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("194", "55", "197", "97", "15")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
res.hcpc$desc.var
