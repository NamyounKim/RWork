library(textTinyR)
library(fastTextR)
library(NLP4kec)
library(reticulate)
install.packages("/Users/kakao/Downloads/NLP4kec_1.2.0.tgz", repos = NULL, type = .Platform$pkgType)


concat = readRDS("./raw_data/petitions_content_2018.RDS")
content = concat$content

content = r_parser_r(content, useEn = T, language = "ko", korDicPath = "./dictionary/user_dictionary.txt")


concat$pasred = content

readr::write_csv(concat, "./petition.csv")
clust_vec = textTinyR::tokenize_transform_vec_docs(object = concat, as_token = T,
                                                   to_lower = T, 
                                                   remove_punctuation_vector = F,
                                                   remove_numbers = F, 
                                                   trim_token = T,
                                                   split_string = T,
                                                   split_separator = " \r\n\t.,;:()?!//", 
                                                   remove_stopwords = T,
                                                   language = "english", 
                                                   min_num_char = 3, 
                                                   max_num_char = 100,
                                                   stemmer = "porter2_stemmer", 
                                                   path_2folder = "./",
                                                   threads = 4,
                                                
                                                   verbose = T)

PATH_INPUT = "./output_token_single_file.txt"

PATH_OUT = "./rt_fst_model.vec"

vecs = fastTextR::skipgram_cbow(input_path = PATH_INPUT, output_path = PATH_OUT, 
                                method = "skipgram", lr = 0.075, lrUpdateRate = 100, 
                                dim = 300, ws = 5, epoch = 5, minCount = 1, neg = 5, 
                                wordNgrams = 2, loss = "ns", bucket = 2e+06,
                                minn = 0, maxn = 0, thread = 6, t = 1e-04, verbose = 2)

vecs = fastTextR::fasttext(input = PATH_INPUT, method = "skipgram", control = ft.control(window_size = 4L))

unq = unique(unlist(clust_vec$token, recursive = F))

utl = sparse_term_matrix$new(vector_data = concat
                                        ,file_data = NULL
                                        ,document_term_matrix = TRUE
                                        ,)

tm = utl$Term_Matrix(sort_terms = FALSE, to_lower = T, remove_punctuation_vector = F,
                     remove_numbers = F, trim_token = T, split_string = T, 
                     stemmer = "porter2_stemmer",
                     split_separator = " \r\n\t.,;:()?!//", remove_stopwords = T,
                     language = "english", min_num_char = 3, max_num_char = 100,
                     print_every_rows = 100000, normalize = NULL, tf_idf = F, 
                     threads = 6, verbose = T)




gl_term_w = utl$global_term_weights()
utl$term_associations("어린이집")
utl$most_frequent_terms()


use_python("/anaconda3/bin/python")
use_virtualenv("myenv")
conda_list()
conda_install("r-reticulate", "scipy")
conda_install("r-reticulate", "pandas")

sc = import("scipy")
pd = import("pandas")

# import numpy and specify no automatic Python to R conversion
np <- import("numpy", convert = FALSE)

# do some array manipulations with NumPy
a <- np$array(c(1:4))
sum <- a$cumsum()

# convert to R explicitly at the end
py_to_r(sum)
