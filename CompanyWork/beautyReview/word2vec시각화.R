raw_review_strength$brand_nm = gsub(pattern = "에뛰드 하우스", "에뛰드하우스", raw_review_strength$brand_nm)
raw_review_weakness$brand_nm = gsub(pattern = "에뛰드 하우스", "에뛰드하우스", raw_review_weakness$brand_nm)


brand_score_weakness = raw_review_weakness %>% group_by(brand_nm) %>% dplyr::summarise(발색 = mean(a1),
                                                                                         커버력= mean(a2),
                                                                                         밀착력_지속력= mean(a3),
                                                                                         세정력= mean(a4),
                                                                                         향= mean(a5),
                                                                                         흡수력= mean(a6),
                                                                                         보습력= mean(a7),
                                                                                         안티에이징_미백= mean(a8),
                                                                                         천연= mean(a9),
                                                                                         휴대성= mean(a10),
                                                                                         가성비= mean(a11),
                                                                                         사용편리성= mean(a12))

brand_score_weakness = brand_score_weakness %>% filter(brand_nm %in% c("설화수","헤라","프리메라"))
brand_score_weakness = brand_score_weakness %>% filter(brand_nm %in% c("라네즈","아이오페","마몽드"))
brand_score_weakness = brand_score_weakness %>% filter(brand_nm %in% c("에뛰드하우스", "이니스프리 본품","아리따움"))

ggRadar2(brand_score_weakness, aes(colour=brand_nm), rescale = F, ylim = 0.1)

brand_score_strength = raw_review_strength %>% group_by(brand_nm) %>% dplyr::summarise(발색 = mean(a1),
                                                                                         커버력= mean(a2),
                                                                                         밀착력_지속력= mean(a3),
                                                                                         세정력= mean(a4),
                                                                                         향= mean(a5),
                                                                                         흡수력= mean(a6),
                                                                                         보습력= mean(a7),
                                                                                         안티에이징_미백= mean(a8),
                                                                                         천연= mean(a9),
                                                                                         휴대성= mean(a10),
                                                                                         가성비= mean(a11),
                                                                                         사용편리성= mean(a12))

brand_score_strength = brand_score_strength %>% filter(brand_nm %in% c("설화수","헤라","프리메라"))
brand_score_strength = brand_score_strength %>% filter(brand_nm %in% c("라네즈","아이오페","마몽드"))
brand_score_strength = brand_score_strength %>% filter(brand_nm %in% c("에뛰드하우스", "이니스프리 본품","아리따움"))

ggRadar2(brand_score_strength, aes(colour=brand_nm), rescale = F, ylim = 0.1)