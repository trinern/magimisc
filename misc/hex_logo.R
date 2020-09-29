#install.packages("hexSticker")

# Dette fordrer riktignok at du har samme bilde hos deg..
# Men oppsettet kan replikeres med annet bilde
imgurl <- "C:/Users/tjebsen001/Downloads/85c7c6cc37476c6192548a655b99d9e2.png"



sticker(imgurl, package="MagiMisc", p_size=20, s_x=1, s_y=.8, s_width=.9, s_height = 6,
        h_fill = "dodgerblue4", h_color = "black",
        filename="C:\\Users\\tjebsen001\\Downloads\\imgfile.png")

