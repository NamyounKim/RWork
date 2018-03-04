library(rword2vec)

# word2vec 모델링
model2=word2vec(train_file = "../FastCampus/trainTxt.txt", output_file = "vec.bin", binary=1, window = 6)

#가까운 단어 찾기
dist=distance(file_name = "vec.bin",search_word = "청원",num = 20)

#txt output 파일 만들기
bin_to_txt("vec.bin","vector.txt")

#코사인 유사도 행렬을 만들기 위한 전처리
data=as.data.frame(read.table("vector.txt",skip=1))
rownames(data) = data[,1]
data = data[,-1]
data_mat = as.matrix(data)

# 코사인 유사도 행렬 만들기
library(lsa)
cosineSim = cosine(t(data_mat))
#cosine(data_mat["청원",], data_mat["청와대",])

# cosineSim 행렬로 단어 네트워크 맵 만드시면 됩니다.