imdb_dir <- "./data/aclImdb"
train_dir <- file.path(imdb_dir, "train")
labels <- c()
texts <- c()
for (label_type in c("neg", "pos")) {
  label <- switch(label_type, neg = 0, pos = 1)
  dir_name <- file.path(train_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"), 
                           full.names = TRUE)) {
    texts <- c(texts, readChar(fname, file.info(fname)$size))
    labels <- c(labels, label)
  }
}



library(keras)
maxlen <- 100                 # We will cut reviews after 100 words
training_samples <- 200       # We will be training on 200 samples
validation_samples <- 10000   # We will be validating on 10000 samples
max_words <- 10000            # We will only consider the top 10,000 words in the dataset

#단어 목록 만들기
tokenizer = text_tokenizer(num_words = max_words) %>% fit_text_tokenizer(texts)

# 각 문서를 단어 인덱스로 표현
sequences = texts_to_sequences(tokenizer, texts)

# 각 단어별 인덱스 값
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

# 각 리뷰의 단어수 동일하게 하기 -> 랜덤하게 100개로 맞춤
data = pad_sequences(sequences, maxlen = maxlen)
labels = as.array(labels)
cat("Shape of data tensor:", dim(data), "\n")


# Split the data into a training set and a validation set
# But first, shuffle the data, since we started from data
# where sample are ordered (all negative first, then all positive).
indices = sample(1:nrow(data))
training_indices = indices[1:training_samples]
validation_indices = indices[(training_samples + 1): 
                                (training_samples + validation_samples)]
x_train <- data[training_indices,]
y_train <- labels[training_indices]
x_val <- data[validation_indices,]
y_val <- labels[validation_indices]


# glove 다운로드
glove_dir = '~/Downloads/glove.6B'
lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt"))
embeddings_index = new.env(hash = TRUE, parent = emptyenv())
for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}