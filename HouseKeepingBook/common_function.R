#차트 테마
th = theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 10)
           , axis.text.y=element_text(size = 10)
           , plot.title = element_text(hjust = 0.5, face = "bold")
           , title = element_text(hjust = 0.5, size = 14)
           , legend.text = element_text(size = 11)
           , text=element_text(family = "Kakao Regular"))

th2 = theme(axis.text.x=element_text(hjust = 1, size = 10)
            , axis.text.y=element_text(size = 10)
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 14)
            , legend.text = element_text(size = 11)
            , text=element_text(family = "Kakao Regular"))

th3 = theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 12)
            , plot.title = element_text(hjust = 0.5, face = "bold")
            , title = element_text(hjust = 0.5, size = 14)
            , legend.text = element_text(size = 12)
            , text=element_text(family = "Kakao Regular"))

# Here we define spaces as the big separator
point = format_format(big.mark = ",", scientific = FALSE)