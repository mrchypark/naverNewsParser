library(stringr)

##정치
# 더미 list01 생성
list01<-list()
# 날짜 변경 for 문
for(j in 0:1095){ 
    # 날짜 2012년 1월 1일 부터 +1095일까지
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    # 페이지를 인식하기 위해 조건을 줌
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  # 네이버 정치면 주소
  test<-eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=269&sid1=100&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[500:1000]
  test<-test[-grep("img",test)]
  listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
  list01<-c(list01,listif)

  Sys.sleep(0.5)
  list01<-unlist(list01)
  list01<-unique(list01)
  lennow<-length(list01)
#  print(lennow)
#  print(i)
#  print(kk)
  i<-i+1

}}

list01<-gsub('<a href=\"','',list01)
list01<-gsub('\">','',list01)
list01<-substr(list01,1,108)
list01<-list01[-grep("hot",list01)]
list01<-list01[-grep("endic",list01)]
write.csv(list01,"list01.csv",row.names=F)

print(length(list01))
print("part 2 start")

##경제

list02<-list()
for(j in 0:1095){ 
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  test<-eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=263&sid1=101&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[500:1000]
  test<-test[-grep("img",test)]
  listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
  list02<-c(list02,listif)

  Sys.sleep(0.5)
  list02<-unlist(list02)
  list02<-unique(list02)
  lennow<-length(list02)
#  print(lennow)
#  print(i)
#  print(kk)
  i<-i+1

}}

list02<-gsub('<a href=\"','',list02)
list02<-gsub('\">','',list02)
list02<-substr(list02,1,108)
list02<-list02[-grep("hot",list02)]
list02<-list02[-grep("endic",list02)]
write.csv(list02,"list02.csv",row.names=F)

print(length(list02))
print("part 3 start")

##사회

list03<-list()
for(j in 0:1095){ 
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  test<-eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=257&sid1=102&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[500:1000]
  test<-test[-grep("img",test)]
  listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
  list03<-c(list03,listif)

  Sys.sleep(0.5)
  list03<-unlist(list03)
  list03<-unique(list03)
  lennow<-length(list03)
#  print(lennow)
#  print(i)
#  print(kk)
  i<-i+1

}}

list03<-gsub('<a href=\"','',list03)
list03<-gsub('\">','',list03)
list03<-substr(list03,1,108)
list03<-list03[-grep("hot",list03)]
list03<-list03[-grep("endic",list03)]
write.csv(list03,"list03.csv",row.names=F)

print(length(list03))
print("part 4 start")

##생활/문화

list04<-list()
for(j in 0:1095){ 
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  test<-eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=245&sid1=103&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[500:1000]
  test<-test[-grep("img",test)]
  listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
  list04<-c(list04,listif)

  Sys.sleep(0.5)
  list04<-unlist(list04)
  list04<-unique(list04)
  lennow<-length(list04)
#  print(lennow)
#  print(i)
#  print(kk)
  i<-i+1

}}

list04<-gsub('<a href=\"','',list04)
list04<-gsub('\">','',list04)
list04<-substr(list04,1,108)
list04<-list01[-grep("hot",list04)]
list04<-list01[-grep("endic",list04)]
write.csv(list04,"list04.csv",row.names=F)

print(length(list04))
print("part 5 start")

##세계

list05<-list()
for(j in 0:1095){ 
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  test<-eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=322&sid1=104&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[500:1000]
  test<-test[-grep("img",test)]
  listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
  list05<-c(list05,listif)
  
  Sys.sleep(0.5)
  list05<-unlist(list05)
  list05<-unique(list05)
  lennow<-length(list05)
#  print(lennow)
#  print(i)
#  print(kk)
  i<-i+1

}}

list05<-gsub('<a href=\"','',list05)
list05<-gsub('\">','',list05)
list05<-substr(list05,1,108)
list05<-list05[-grep("hot",list05)]
list05<-list05[-grep("endic",list05)]
write.csv(list05,"list05.csv",row.names=F)

print(length(list05))
print("part 6 start")

##과학

list06<-list()
for(j in 0:1095){ 
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  test<-eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=228&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[500:1000]
  test<-test[-grep("img",test)]
  listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
  list06<-c(list06,listif)
  
  Sys.sleep(0.5)
  list06<-unlist(list06)
  list06<-unique(list06)
  lennow<-length(list06)
#  print(lennow)
#  print(i)
#  print(kk)
  i<-i+1

}}

list06<-gsub('<a href=\"','',list06)
list06<-gsub('\">','',list06)
list06<-substr(list06,1,108)
list06<-list06[-grep("hot",list06)]
list06<-list06[-grep("endic",list06)]
write.csv(list06,"list06.csv",row.names=F)

print(length(list06))
print("part 7 start")

##스포츠

list07<-list()
for(j in 0:1095){ 
    dd<-as.Date(j, origin = "2012-01-01")
    kk<-gsub('-',"",as.character(dd))
    len<-0
    lennow<-1
    i<-1
while(len!=lennow){
  
  len<-lennow
  test<-eval(parse(text=paste0("readLines('http://sports.news.naver.com/sports/index.nhn?category=sports_general&ctg=news&mod=lst&type=news&date=",kk,"&page=",i,"',warn=F)")))
  test<-test[698:1300]
  test<-test[grep("<tr><td><a href=\"",test)]
  listif<-paste0("http://sports.news.naver.com/", substr(test, 35, 128))
  list07<-c(list07,listif)

  Sys.sleep(0.5)
  list07<-unlist(list07)
  list07<-unique(list07)
  lennow<-length(list07)
  print(lennow)
  print(i)
  print(kk)
  i<-i+1

}}

write.csv(list07,"list07.csv",row.names=F)
print(length(list07))
print("End")


#########################################################################################################


list<-data.frame(category=8,title="test",author="test",postTime="time",chgTime="time",contents="test")
for(i in 1:6){
  test<-eval(parse(text=gsub(" ","",paste("list0",i))))
  dd<-length(test)
  for(j in 1:dd){
    tt<-test[j]
    tst<-readLines(tt,warn=F)

    tst<-gsub('&nbsp;',"",tst)
    tst<-gsub('&lt;',"<",tst)
    tst<-gsub('&gt;',">",tst)
    tst<-gsub('&amp;',"&",tst)
    tst<-gsub('&quot;','"',tst)

    cate<-tst[grep(":category2",tst)]
    title<-tst[grep("og:title",tst)]
    author<-tst[grep(":author",tst)]
    postTime<-tst[grep("기사입력",tst)]
    chgTime<-tst[grep("최종수정",tst)]
    if(length(grep("본문 내용",tst))>1){
    con<-tst[grep("본문 내용",tst)[1]:grep("본문 내용",tst)[2]]}else{
    con<-tst[grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)[ grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)>grep("본문 내용",tst)][1]]}

    cate<-gsub("[^(가-힣ㄱ-ㅎㅏ-ㅣ)]","",cate)
    title<-gsub('<meta property=\"og:title\"\t\t\tcontent=\"',"",title)
    title<-gsub('\"/>',"",title)
    author<-gsub('<meta property=\"og:article:author\"\tcontent=\"',"",author)
    author<-gsub('\"/>',"",author)
    author<-gsub(" 네이버 뉴스","",author)
    author<-gsub("[[:punct:]]", "", author)
    author<-gsub(" ", "", author)
    postTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",postTime)
    postTime<-gsub("기사입력 ","",postTime)
    if (length(chgTime)!=0){
      chgTime<-gsub("\t","",chgTime)
      chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
      chgTime<-gsub("최종수정 ","",chgTime)
      }else{chgTime<-postTime}
    con<-gsub("\t","",con)
    con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
    con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
    con<-paste(con, collapse = "")

    listTem<-data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con)
    list<-rbind(list,listTem)

Sys.sleep(0.5)
print(paste(i,j,j/dd,"%"))
}}


list<-data.frame(category=8,title="test",author="test",postTime="time",chgTime="time",contents="test")
i<-7
dd<-length(list07)
  for(j in 1:dd){
    tt<-list07[j]
    tst<-readLines(tt,warn=F)

    tst<-gsub('&nbsp;',"",tst)
    tst<-gsub('&lt;',"<",tst)
    tst<-gsub('&gt;',">",tst)
    tst<-gsub('&amp;',"&",tst)
    tst<-gsub('&quot;','"',tst)

    title<-tst[grep("og:title",tst)]
    author<-tst[grep(":author\"",tst)]
    postTime<-tst[grep("기사입력",tst)+1]
    chgTime<-tst[grep("최종수정",tst)]
    if(length(grep("기사 내용",tst))>1){
    con<-tst[grep("기사 내용",tst)[1]:grep("기사 내용",tst)[2]]}else{
    con<-tst[grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)[ grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)>grep("기사 내용",tst)][1]]}

    cate<-"스포츠"
    title<-gsub('<meta property=\"og:title\"       content=\"',"",title)
    title<-gsub('\"/>',"",title)
    author<-gsub('<meta property=\"og:article:author\" content=\"',"",author)
    author<-gsub('\"/>',"",author)
    author<-gsub("네이버 스포츠 ","",author)
    author<-gsub("[[:punct:]]", "", author)
    author<-gsub(" ", "", author)
    postTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",postTime)
    postTime<-gsub(" ","",postTime)
    postTime<-paste0(substr(postTime,1,10)," ",substr(postTime,11,15))
    if (length(chgTime)!=0){
      chgTime<-gsub("\t","",chgTime)
      chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
      chgTime<-gsub("[[:punct:]]", "", chgTime)
      chgTime<-gsub("최종수정", "", chgTime)
      chgTime<-gsub(" ", "", chgTime)
      chgTime<-paste0(substr(chgTime,1,4),"-",substr(chgTime,5,6),"-",substr(chgTime,7,8)," ",substr(chgTime,9,10),":",substr(chgTime,11,12))
      }else{chgTime<-postTime}
    con<-gsub("\t","",con)
    con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
    con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
    con<-paste(con, collapse = "")

    listTem<-data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con)
    list<-rbind(list,listTem)

Sys.sleep(0.5)
print(paste(i,j,j/dd,"%"))
}
