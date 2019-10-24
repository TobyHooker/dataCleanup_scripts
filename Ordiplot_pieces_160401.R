## ordiplot pieces...  160401


p12 <- ordiplot(inv.nmds.3, display="sp", choices=c(1,2), type="n")
    points(inv.nmds.3, display="species", choices=c(1,2), col=inv.3.scores4$Water.Src)
    orditorp(inv.nmds.3, display="sp", choices=c(1,2), pcol=inv.3.scores4$Water.Src)

p13 <- ordiplot(inv.nmds.3, display="sp", choices=c(1,3), type="n")
    points(inv.nmds.3, display="species", choices=c(1,3), col=inv.3.scores4$Water.Src)
    orditorp(inv.nmds.3, display="sp", choices=c(1,3), pcol=inv.3.scores4$Water.Src)

p23 <- ordiplot(inv.nmds.3, display="sp", choices=c(2,3), type="n")
    points(inv.nmds.3, display="species", choices=c(2,3), col=inv.3.scores4$Water.Src)
    orditorp(inv.nmds.3, display="sp", choices=c(2,3), pcol=inv.3.scores4$Water.Src)










