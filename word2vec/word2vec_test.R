install.packages("devtools")
#Rtools는 Windows에 깔때 별도로 깔아야 한다.
install.packages("KoNLP")
install_github("mukul13/rword2vec")
library(devtools)
library(rword2vec)
ls("package:rword2vec")

library(KoNLP)
require(doParallel)
registerDoParallel()

#----------------------------------------
model=word2vec(train_file = "./이명박취임연설문.txt",output_file = "./vec.bin", binary=1)

distance(file_name = "./vec.bin", search_word = "대한",num = 10)

ana=word_analogy(file_name = "vec2.bin",search_words = "노무현 많은 때문에",num = 20)


bin_to_txt("./vec.bin","./vector.txt")


vocab_count(file_name="./이명박취임연설문.txt",vocab_file="./vocab.txt",min_count = 20)
d=read.table("./vocab.txt")
#-----------------------------------------------------------------------------------

vocTxt = readChar("./out.txt", file.info("./out.txt")$size, useBytes = TRUE)

useSejongDic()
vocNoun = extractNoun(vocTxt)

vocNoun2 = NULL
for(i in 1:length(vocNoun)){
  vocNoun2 = paste(vocNoun2, vocNoun[i])
}
writeChar(vocNoun2, "./vocNoun.txt")

model2=word2vec(train_file = "./out.txt",output_file = "./vecNoun.bin",binary=2)
distance(file_name = "./vecNoun.bin", search_word = "일자리",num = 15)
word_analogy(file_name = "./vecNoun.bin", search_words = "젊은이 취업 젊은이",num = 10)

#vocab_count(file_name="./vecNoun.bin", vocab_file="./vocNounVocab.txt", min_count = 0)

bin_to_txt("./vecNoun.bin","./vectorNoun.txt")
vectorNoun = read.table("./vectorNoun.txt",skip=1, fill = TRUE)

#PCA
fit = princomp(vectorNoun[,2:101], cor=TRUE)


#----------------------------------------------------------------------
plot(vectorNoun[,4:5])
text(vectorNoun[,4],vectorNoun[,5], vectorNoun[,1], cex= 0.7)


install.packages("Rtsne")
library(Rtsne)
rtsne_out <- Rtsne(as.matrix(vectorNoun[1:209,2:101]))
jpeg("d:\\barneshutplot.jpg", width=2400, height=1800)
plot(rtsne_out$Y, t='n', main="BarnesHutSNE")
text(rtsne_out$Y, labels=vectorNoun[,1], family="AppleGothic")
as.matrix(vectorNoun)
