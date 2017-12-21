library(readr)
library(readxl)
library(NLP4kec)

test1 = readxl::read_xlsx("/Users/kimnamyoun/GitHub/RWork/FastCampus/Blog_TestSet_Spam.xlsx", col_names = T)
test = "이날 사고는 최근 잇따른 사고와 대책에도 불구하고 안전관리 전반의 실질적인 개선은 이뤄지지 않고 있음을 말해준다. 무엇보다 각종 대책에도 공사 현장의 관행엔 큰 변화가 없다는 지적이 나온다. 건설업계 전반의 외주화 바람 속에 타워크레인 운용 또한 영세업체에 하도급으로 맡겨지는 비율이 크게 늘면서, 안전보다는 공기 단축 등 ‘속도전’을 강조하는 관행이 바뀌지 않고 있다는 것이다. 정회운 한국노총 전국타워크레인 설·해체노조 위원장은 “현재 건설업체 가운데 자체 크레인을 보유하고 있는 경우는 극히 드물고 전체의 85~90% 이상은 외주 임대업체가 맡고 있다”며 “자체 장비 보유가 수지타산이 안 맞아서인지 2000년대 들어서 외주화 비율이 급격히 높아졌다”고 말했다. 박종국 시민안전감시센터장은 “타워크레인 업종이 완전히 외주화돼 영세업체가 많다”며 “신형 장비를 사려면 최소 5억원이 들고 원가 마진을 회수하는 데만 7~8년이 걸리기 때문에, 값싼 중국산 크레인을 구입해 전국을 떠돌며 작업하는 실정”이라고 말했다."
r_parser_r(test, useEn = T, language = "ko",korDicPath = "./dictionary.txt")

head(test1)

start = Sys.time()
result2 = file_parser_r("/Users/kimnamyoun/GitHub/RWork/FastCampus/Blog_TestSet_Spam.xlsx", useEn = T, language = "ko",korDicPath = "./dictionary.txt")
Sys.time() - start

start = Sys.time()
result1 = r_parser_r(test1$content, useEn = T, language = "ko",korDicPath = "./dictionary.txt")
Sys.time() - start

