# 실행하는 명령어 > system.time(source('~/R/DURPrdlstInfoService/DURPrdlstInfoService.R', encoding = 'UTF-8'))
#Rprof("Rprof.out")
# ------------------------------step 1------------------------------
print("------------------------------step 1------------------------------")
# 필요한 라이브러리를 주석 제거하여 추가
#install.packages("XML") # XML : API에서 xml 데이터 수집을 위한 패키지
#install.packages("data.table")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("progress")
#install.packages("httr")
require(XML) # XML 데이터 처리를 위한 패키지
require(data.table) # data.frame 보다 더 빠르고 효율적인 data.table 사용을 위한 패키지
require(stringr) # stringr : 문자열 처리 및 파이프 연산자 사용을 위한 패키지
require(ggplot2) # ggplot2 : 시각화를 위한 패키지
require(progress) # progress bar를 사용하기 위한 패키지
require(dplyr) # data.frame에서 mutate를 사용하기 위한 패키지
require(httr) # R language에서 request를 하기 위한 패키지

# ------------------------------step 2------------------------------
print("------------------------------step 2------------------------------")
# chkurl에 접속해서 xml 파싱을 통해서 totalCount를 가져온다.
chkurl <- "http://apis.data.go.kr/1470000/DURPrdlstInfoService/getUsjntTabooInfoList?serviceKey=qx1rTzz3GHMRrD7mzJ%2BVoJf2FGOA9L89k1GCg6W9AVlx2IBQ8h%2FwckiodyqwAAjwx2pByE83SYr5Ahk5j4Q%2BUQ%3D%3D&numOfRows=100&pageNo=1"
item <- list()
item_temp_dt<-data.table()
raw.data <- xmlTreeParse(chkurl, useInternalNodes = TRUE,encoding = "utf-8")
rootNode <- xmlRoot(raw.data)
chkCode <- xpathSApply(rootNode,"//resultCode",xmlValue) # rootNode에서 resultCode element value를 가져온다.
if(chkCode==99) { # 99가 error 발생 코드
  print(chkurl)
  stop("LIMITED NUMBER OF SERVICE REQUESTS EXCEEDS ERROR. PLEASE TRY AGAIN TOMORROW")
} else {
  totalCount <- xpathSApply(rootNode,"//totalCount",xmlValue) # rootNode에서 totalCount element value를 가져온다.
}

# ------------------------------step 3------------------------------
print("------------------------------step 3------------------------------")
# 공공데이터 API를 위한 변수 설정
# 공공데이터 API는 1일 1000회 트래픽 제한(개발계정)/100000회(운영계정)
# API를 위한 변수 설정
service_key <- "qx1rTzz3GHMRrD7mzJ%2BVoJf2FGOA9L89k1GCg6W9AVlx2IBQ8h%2FwckiodyqwAAjwx2pByE83SYr5Ahk5j4Q%2BUQ%3D%3D"
# totalCount를 numeric으로 변환하여 100으로 나눠서 올림한 수를 저장
endcnt <- ceiling(as.numeric(totalCount)/100)
# 조회할 pageNo list 생성, 1부터 endcnt까지
pagelist <-c(1:endcnt)

# ------------------------------step 4------------------------------
print("------------------------------step 4------------------------------")
# 데이터를 수집할 url list 작성
# paste0 : 벡터를 문자열들로 변환한 후 연결한다. paste( )와 달리 sep가 항상 빈 문자열. 즉, 공백없이 이어준다.
urllist <- list()
cnt <- 0
# progress bar의 사전정보가 담긴 객체 pb 생성. 반복횟수는 length(pagelist)로 저장
pb <- progress_bar$new(total = length(pagelist))
for(i in 1:length(pagelist)){
  cnt = cnt+1 # R에서는 index 번호가 1번부터 시작한다.
  urllist[cnt] <- paste0("http://apis.data.go.kr/1470000/DURPrdlstInfoService/getUsjntTabooInfoList?","serviceKey=",service_key,"&numOfRows=100","&pageNo=",pagelist[cnt])
  pb$tick()
  Sys.sleep(0.001)
}
print("수집할 데이터 URL LIST 생성 완료")
Sys.sleep(1) # 프로그램의 안정성을 위해서 잠시 대기

# ------------------------------step 5------------------------------
print("------------------------------step 5------------------------------")
# 데이터 수집과정
# openapi를 이용해 공공데이터를 수집
# 데이터는 XML 형식으로 리턴되므로 적절한 형태로 편집이 필요한 단계
# 데이터 수집 과정
# progress bar의 사전정보가 담긴 객체 pb 생성. 전체 반복횟수는 length(urllist)로 저장
pb <- progress_bar$new(total = length(urllist))
total <- list() # 최종 결과물이 저장되는 list 변수
err_list <- list() # error list 변수
err_total <- list() # 최종 에러가 저장되는 list 변수
errcnt_i <- 0 # error 카운트
errcnt_j <- 0 # error 카운트
trycnt <- 0 # while문을 얼마나 반복하는지 카운트하는 변수
for(i in 1:length(urllist)){
  item <- list()
  item_temp_dt<-data.table()
  while(TRUE){
    # urllist[i]는 list 타입이라서, as.character로 character 타입으로 변환해서 GET 요청한다.
    # reponse에서 status_code이 200(OK)으로 연결이 정상일 때, while문을 탈출한다. (무한대기=폴링방식)
    tryCatch(status_code <- GET(as.character(urllist[i]))$status_code,
             error = function(e) {
               #trycnt = trycnt + 1
               #print(paste0("재연결 시도 : ",trycnt," 회"))
               print("재연결 시도")
               Sys.sleep(1.9989) # 1.9989초(실제론 2.0초)를 대기하고, 다시 while문을 반복한다.
             },
             warning = function(w) print("warning"),
             finally = NULL)
    if(status_code == 200) {
      break;
    }
  }
  raw.data <- xmlTreeParse(urllist[i], useInternalNodes = TRUE,encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  chk_resultCode <- xpathSApply(rootNode,"//resultCode",xmlValue) # rootNode에서 resultCode element value를 가져온다.
  chk_totalCount <- xpathSApply(rootNode,"//totalCount",xmlValue) # rootNode에서 resultCode element value를 가져온다.
  while(as.numeric(chk_totalCount)==0){ # 폴딩방식으로 무한대기
    print("API-Server returns NULL. Reconnecting...")
    chk_totalCount <- xpathSApply(rootNode,"//totalCount",xmlValue) # rootNode에서 resultCode element value를 가져온다.
    Sys.sleep(1)
  }
  if(as.numeric(chk_resultCode)==99){ # 99가 error 발생 코드
    print(urllist[i])
    stop("LIMITED NUMBER OF SERVICE REQUESTS EXCEEDS ERROR. PLEASE TRY AGAIN TOMORROW")
  }else {
    items <- rootNode[[2]][['items']]
    # 'DUR_SEQ','TYPE_CODE','TYPE_NAME','MIX','INGR_CODE','INGR_KOR_NAME','INGR_ENG_NAME','ITEM_SEQ','ITEM_NAME','ENTP_NAME',
    # 'CHART', 'FORM_CODE','ETC_OTC_CODE','CLASS_CODE','FORM_NAME','ETC_OTC_NAME','CLASS_NAME','MAIN_INGR','MIXTURE_DUR_SEQ','MIXTURE_MIX',
    # 'MIXTURE_INGR_CODE','MIXTURE_INGR_KOR_NAME','MIXTURE_INGR_ENG_NAME','MIXTURE_ITEM_SEQ','MIXTURE_ITEM_NAME','MIXTURE_ENTP_NAME','MIXTURE_FORM_CODE','MIXTURE_ETC_OTC_CODE','MIXTURE_CLASS_CODE','MIXTURE_FORM_NAME',
    # 'MIXTURE_ETC_OTC_NAME','MIXTURE_CLASS_NAME','MIXTURE_MAIN_INGR','NOTIFICATION_DATE','PROHBT_CONTENT','ITEM_PERMIT_DATE','MIXTURE_ITEM_PERMIT_DATE','MIXTURE_CHART','CHANGE_DATE','MIXTURE_CHANGE_DATE'
    size <- xmlSize(items)
    for(j in 1:size){
      item_temp <- xmlSApply(items[[j]],xmlValue)
      rowcnt <- nrow(data.frame(item_temp))
      if(rowcnt==42){
        item_temp_dt <- data.table( DUR일련번호 = xpathSApply(items[[j]],"DUR_SEQ",xmlValue),
                                    DUR유형코드 = xpathSApply(items[[j]],"TYPE_CODE",xmlValue),
                                    DUR유형 = xpathSApply(items[[j]],"TYPE_NAME",xmlValue),
                                    단일_복합 = xpathSApply(items[[j]],"MIX",xmlValue),
                                    DUR성분코드 = xpathSApply(items[[j]],"INGR_CODE",xmlValue),
                                    DUR성분 = xpathSApply(items[[j]],"INGR_KOR_NAME",xmlValue),
                                    DUR성분_영문 = xpathSApply(items[[j]],"INGR_ENG_NAME",xmlValue),
                                    복합제 = xpathSApply(items[[j]],"MIX_INGR",xmlValue),
                                    품목기준코드 = xpathSApply(items[[j]],"ITEM_SEQ",xmlValue),
                                    품목명 = xpathSApply(items[[j]],"ITEM_NAME",xmlValue),
                                    
                                    업체명 = xpathSApply(items[[j]],"ENTP_NAME",xmlValue),
                                    성상 = xpathSApply(items[[j]],"CHART",xmlValue),
                                    제형구분코드 = xpathSApply(items[[j]],"FORM_CODE",xmlValue),
                                    전문일반구분코드 = xpathSApply(items[[j]],"ETC_OTC_CODE",xmlValue),
                                    약효분류코드 = xpathSApply(items[[j]],"CLASS_CODE",xmlValue),
                                    제형 = xpathSApply(items[[j]],"FORM_NAME",xmlValue),
                                    전문_일반 = xpathSApply(items[[j]],"ETC_OTC_NAME",xmlValue),
                                    약효분류 = xpathSApply(items[[j]],"CLASS_NAME",xmlValue),
                                    주성분 = xpathSApply(items[[j]],"MAIN_INGR",xmlValue),
                                    병용금기DUR번호 = xpathSApply(items[[j]],"MIXTURE_DUR_SEQ",xmlValue),
                                    
                                    병용금기복합제 = xpathSApply(items[[j]],"MIXTURE_MIX",xmlValue),
                                    병용금기DUR성분코드 = xpathSApply(items[[j]],"MIXTURE_INGR_CODE",xmlValue),
                                    병용금기DUR성분 = xpathSApply(items[[j]],"MIXTURE_INGR_KOR_NAME",xmlValue),
                                    병용금기DUR성분_영문 = xpathSApply(items[[j]],"MIXTURE_INGR_ENG_NAME",xmlValue),
                                    병용금기품목기준코드 = xpathSApply(items[[j]],"MIXTURE_ITEM_SEQ",xmlValue),
                                    병용금기품목명 = xpathSApply(items[[j]],"MIXTURE_ITEM_NAME",xmlValue),
                                    병용금기업체명 = xpathSApply(items[[j]],"MIXTURE_ENTP_NAME",xmlValue),
                                    병용금기제형구분코드 = xpathSApply(items[[j]],"MIXTURE_FORM_CODE",xmlValue),
                                    병용금기전문일반구분코드 = xpathSApply(items[[j]],"MIXTURE_ETC_OTC_CODE",xmlValue),
                                    병용금기약효분류코드 = xpathSApply(items[[j]],"MIXTURE_CLASS_CODE",xmlValue),
                                    
                                    병용금기제형 = xpathSApply(items[[j]],"MIXTURE_FORM_NAME",xmlValue),
                                    병용금기전문_일반 = xpathSApply(items[[j]],"MIXTURE_ETC_OTC_NAME",xmlValue),
                                    병용금기약효분류 = xpathSApply(items[[j]],"MIXTURE_CLASS_NAME",xmlValue),
                                    병용금기주성분 = xpathSApply(items[[j]],"MIXTURE_MAIN_INGR",xmlValue),
                                    고시일자 = xpathSApply(items[[j]],"NOTIFICATION_DATE",xmlValue),
                                    금기내용 = xpathSApply(items[[j]],"PROHBT_CONTENT",xmlValue),
                                    비고 = xpathSApply(items[[j]],"REMARK",xmlValue),
                                    품목허가일자 = xpathSApply(items[[j]],"ITEM_PERMIT_DATE",xmlValue),
                                    병용금기품목허가일자 = xpathSApply(items[[j]],"MIXTURE_ITEM_PERMIT_DATE",xmlValue),
                                    병용금기성상 = xpathSApply(items[[j]],"MIXTURE_CHART",xmlValue),
                                    
                                    변경일자 = xpathSApply(items[[j]],"CHANGE_DATE",xmlValue),
                                    병용변경일자 = xpathSApply(items[[j]],"MIXTURE_CHANGE_DATE",xmlValue)
        )
      }else{
        print(paste("ERROR!! rowcnt=", rowcnt))
        # data.table로는 두 번째 열의 type이 list라서 write.csv에서 처리하지 못함, 그래서 data.frame을 사용
        err_list_temp <- data.frame(ROW = rowcnt, position = j) %>% mutate(URL = list(urllist[i]) %>% unlist())
        errcnt_j <- errcnt_j + 1
        err_list[[errcnt_j]]<-err_list_temp
      }
      item[[j]]<-item_temp_dt
    }#for문 끝
    total[[i]]<-rbindlist(item)
    if(errcnt_j > 0) {
      errcnt_i <- errcnt_i + 1
      err_total[[errcnt_i]]<-rbindlist(err_list)
    }
  }#else문 끝
  cat("\n",paste(i,"/",length(urllist),"완료"),"\n")
  pb$tick()
}#for문 끝
print("api request & xml parsing 완료")
result_UsjntTabooInfo_data <- rbindlist(total)
result_err_data <- rbindlist(err_total)

# ------------------------------step 6------------------------------
# 데이터 저장 과정
print("------------------------------step 6------------------------------")
save(result_UsjntTabooInfo_data, file="getUsjntTabooInfoList.Rdata")
write.csv(
  result_err_data,           # 파일에 저장할 데이터 프레임 또는 행렬
  file="err_url_list_dt.csv",# 데이터를 저장할 파일명
  row.names=TRUE             # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
)
write.csv(
  result_UsjntTabooInfo_data, # 파일에 저장할 데이터 프레임 또는 행렬
  file="getUsjntTabooInfoList.csv", # 데이터를 저장할 파일명
  row.names=TRUE              # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
)
print("Rdata, CSV 저장 완료")
#Rprof(NULL)#시간측정종료