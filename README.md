# 공공데이터 API를 이용한 DUR품목정보 데이터 수집 (웹 크롤러)(web crawler)
- 시작날짜 : 2019년 01월 30일 (수)
- 프로젝트명 : DUR품목정보에서 병용금기 정보조회 데이터 수집
- 수집하는 데이터 : 공공데이터(www.data.go.kr)-식품의약품안전처 의약품 관련 정보-DUR품목정보(DURPrdlstInfoService)-병용금기정보조회(getUsjntTabooInfoList) [링크](https://www.data.go.kr/subMain.jsp?param=T1BFTkFQSUAxNTAyMDYyNw==#/L3B1YnIvcG90L215cC9Jcm9zTXlQYWdlL29wZW5EZXZHdWlkZVBhZ2UkQF4wMTJtMSRAXnB1YmxpY0RhdGFQaz0xNTAyMDYyNyRAXnB1YmxpY0RhdGFEZXRhaWxQaz11ZGRpOmZhMmViZjJjLTY0NTMtNGY5ZC1iZDg5LWVmOGUzYzc3ZTE5ZiRAXm9wcnRpblNlcU5vPTE2NzQ2JEBebWFpbkZsYWc9dHJ1ZQ==)
- 제작언어 : R lanaguage [링크](https://www.r-project.org/)
- 구성파일 : DURPrdlstInfoService.R (R 코드파일)

R.version
```
platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          3                           
minor          5.2                         
year           2018                        
month          12                          
day            20                          
svn rev        75870                       
language       R                           
version.string R version 3.5.2 (2018-12-20)
nickname       Eggshell Igloo            
```
----

## DURPrdlistInfoService.R 크롤링 중 발생한 문제점

- 주의 : 프로그램의 원활한 동작 및 csv 샘플 확보를 위해서 원활하게 동작하는 것을 목표로 작성된 코드라서 비효율적인 부분이 많이 존재합니다.

#### 첫 번째 ) API-Server Connection Failure
- **문제점** : 알 수 없는 원인으로 API서버와 연결이 불가능할 때, request 요청이 불가능해져서 에러가 발생하고 실행을 멈춘다.

- **해결 방법** : 연결이 성립할 때까지 계속해서 연결을 시도하는 폴링 방식으로 해결
```R
while(TRUE){
    # urllist[i]는 list 타입이라서, as.character로 character 타입으로 변환해서 GET 요청한다.
    # reponse에서 status_code이 200(OK)으로 연결이 정상일 때, while문을 탈출한다. (무한대기=폴링방식)
    tryCatch(status_code <- GET(as.character(urllist[i]))$status_code,
             error = function(e) {
                 print("재연결 시도")
                 Sys.sleep(1.9989) # 1.9989초(실제론 2.0초)를 대기하고, 다시 while문을 반복한다.
             },
             warning = function(w) print("warning"),
             finally = NULL)
    if(status_code == 200) {
      break;
    }
  }
```

----

#### 두 번째 ) Error in UseMethod("xmlSApply")
- **문제점** : `xmlSApply`을 사용하는 객체(object)가 NULL일 때 에러가 발생하고 실행을 멈춘다.`Error in UseMethod("xmlSApply") : no applicable method for 'xmlSApply' applied to an object of class "NULL"`

- **추가설명** : `httr` 패키지에 있는 `GET`으로 요청해서 공공데이터 서버에서 status_code = 200으로 정상처리 됐지만, 공공데이터 서버에서 정상적인 데이터를 주지 않아서 출력되는 데이터 값이 `NULL`이기 때문에 발생하는 에러이다. 밑에 코드는 해당 문제의 경우에서 얻어온 XML 코드이다.

```xml
<response>
  <header>
    <resultCode>00</resultCode>
    <resultMsg>NORMAL SERVICE.</resultMsg>
  </header>
  <body>
    <numOfRows>100</numOfRows>
    <pageNo>895</pageNo>
    <totalCount>0</totalCount>
    <items/>
  </body>
</response>
```

위 에러코드에서 `totalCount`와 `<items/>`에 정상적인 값이 있어야 하는데, 아무런 데이터도 포함하고 있지 않다. 아래 정상코드와 비교하면 차이를 알 수 있다.
```xml
<response>
	<header>
		<resultCode>00</resultCode>
		<resultMsg>NORMAL SERVICE.</resultMsg>
	</header>
	<body>
		<numOfRows>100</numOfRows>
		<pageNo>895</pageNo>
		<totalCount>351010</totalCount>
		<items>...</items>
	</body>
</response>
```

- **해결방법** : `totalCount`의 값이 `0`이면 해당 `URL`을 다시 `request`하도록 변경
```R
while(as.numeric(chk_totalCount)==0){ # 폴딩방식으로 무한대기
    print("API-Server returns NULL. Reconnecting...")
    chk_totalCount <- xpathSApply(rootNode,"//totalCount",xmlValue) # rootNode에서 resultCode element value를 가져온다.
    Sys.sleep(1)
  }
```

----

#### 세 번째)  Unable to establish connection with R session
- **문제점** : R코드를 동작하면서 발생하는 알 수 없는 에러로 인하여 간헐적으로 R session이 끊기는 현상이다.
- **미해결** : 간헐적으로 발생해서 원인을 파악하지 못했다.

----

#### 네 번째) Single Thread VS Muti-Threads

![img](https://t1.daumcdn.net/cfile/tistory/21212C4C5895DD5E11)

- **문제점** : R은 싱글쓰레드로 제작된 언어이기 때문에 처리해야할 데이터의 양이 늘어나면 처리속도가 기하급수적으로 감소한다. 

![img](https://blogfiles.pstatic.net/MjAxOTAyMDFfMTMy/MDAxNTQ4OTg0NjI0MTIz.1D7DAjPGb9Ytk6AU8RVXvnWRm3YDp3ZoxFxHeiB5Ew8g.zosc1nBTaPjJ2_I8kzfXgAQ2a5eD27BkWPzvt_m5MzMg.PNG.jjscan/time.png)

  병용금기를 약 35만 건을 CSV파일로 처리하는데, 초기에는 1시간에 33%를 완료했지만, 점점 느려져서 17시간이 걸렸다.

- **해결방법** : R 병렬 프로그래밍을 구현하여 처리 속도를 향상한다.

- **추가설명** : MPI(Message Passing Interface)를 위한 R 패키지로 Rmpi를 선택하였다. 추가적으로 윈도우 환경에서 MPI 서브시스템 구현을 위해서 MS-MPI[링크](https://docs.microsoft.com/en-us/message-passing-interface/microsoft-mpi)를 사용한다. 리눅스 환경에서는 OpenMPI를 이용한다. 

R에서 Rmpi를 사용하려면, MS-MPI를 설치한 이후에 Rstudio 바로가기를 `"C:\Program Files\Microsoft MPI\Bin\mpiexec.exe" -n 1 "C:\Program Files\RStudio\bin\rstudio.exe"`처럼 수정하여 생성하고 실행한다.
```
# in Rstudio to spawn R slaves.
mpi.spawn.Rslaves()
# Check if Rmpi is running properly,
mpi.setup.rngstream(iseed=123)
mpi.parReplicate(80, mean(rnorm(1000000)))
mpi.close.Rslaves()
mpi.exit()
```


** 계속 업데이트 중.... **

- **참고문헌** : R 병렬 프로그래밍 (사이먼 채플 외 지음)

----

