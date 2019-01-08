library(text2vec)


# 이전에 만들었던 형태소분석 결과를 가져옴
targetData = readRDS("./raw_data/parsed_data.RDS")

# Create iterator over tokens
tokens = space_tokenizer(targetData)

# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)

# 특정 빈도 이하의 단어는 삭제
vocab = prune_vocabulary(vocab, term_count_min = 3L)

# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)

# TCM(term-co-occurrence matrix) 만들기: 윈도우 내 동시 출현 단어
tcm = create_tcm(it, vectorizer, skip_grams_window = 6L)

# 스레드 개수 지정
RcppParallel::setThreadOptions(numThreads = 4)

#word2vec 세팅
glove = GlobalVectors$new(word_vectors_size = 100, vocabulary = vocab, x_max = 10)

#학습
wv_main = fit_transform(tcm, glove, n_iter = 20)

# 최종 word2vec
word_vectors = wv_main + t(glove$components)

# 단어간 계산
berlin = word_vectors["청원", , drop = FALSE] - 
  word_vectors["청와대", , drop = FALSE] + 
  word_vectors["국회의원", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

cos_sim2 = sim2(x = word_vectors, y = word_vectors["ais", , drop = FALSE], method = "cosine", norm = "l2")
head(sort(cos_sim2[,1], decreasing = TRUE), 20)
nearest_to(model, model[["ais"]], 20)

subVec = model[rownames(model)=="임금",] - model[rownames(model) == "근로",] + model[rownames(model) == "취업",]
nearest_to(model, subVec, 20)



library(Rtsne)

## Curating the database for analysis with both t-SNE and PCA
sub_word_vectors = word_vectors[nchar(rownames(word_vectors))>1,]
sub_word_vectors = sub_word_vectors[rownames(sub_word_vectors) %in% vocab[vocab$term_count > 50,]$term, ]
nrow(sub_word_vectors)

Labels = rownames(sub_word_vectors)
train$label<-as.factor(train$label)

## for plotting
colors = rainbow(length(unique(Labels)))
names(colors) = unique(Labels)

## Executing the algorithm on curated data
tsne = Rtsne(sub_word_vectors, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
#exeTimeTsne = system.time(Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

## Plotting
library(extrafont)
par(family="AppleGothic")
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=Labels, col=colors[Labels])
