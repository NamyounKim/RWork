install.packages("jug")
library(jug)

jug() %>%
  use(path = NULL, function(req, res, err){
    "test 1,2,3!"
  }) %>%
  serve_it()
