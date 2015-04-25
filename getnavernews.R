getNaverNews <- function()
{
    startDate <- as.Date(readline(prompt="Enter start date(yyyy-mm-dd): "))
    while(!grepl("^(19|20)..[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",startDate))
        {
            startDate <- readline(prompt="Please ;) enter start date(yyyy-mm-dd): ")
        }

    endDate <- as.Date(readline(prompt="Enter end date(yyyy-mm-dd): "))
    while(!grepl("^(19|20)..[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",endDate))
        {
            endDate <- readline(prompt="Please ;) enter end date(yyyy-mm-dd): ")
        }
    while(startDate>endDate)
        {
            endDate <- readline(prompt="Please ;) enter end date later than start date(yyyy-mm-dd): ")
        }

    print("1: politics 2: economy 3: society 4: culture 5: world news 6: IT/science 7: sports 8: entertainment")
    print("If you don't enter 1 or 2, it will be done with nothing. Thank you.")
    selectAll <- readline(prompt="Do you want colect all categories?(yes:1, no:2): ")
    if(selectAll!=1)
        {
            selectPolitics <- readline(prompt="Do you want politics category?(yes:1, no:2): ")
            selectEconomy <- readline(prompt="Do you want economy category?(yes:1, no:2): ")
            selectSociety <- readline(prompt="Do you want society category?(yes:1, no:2): ")
            selectCulture <- readline(prompt="Do you want culture category?(yes:1, no:2): ")
            selectWorld <- readline(prompt="Do you want world news category?(yes:1, no:2): ")
            selectScience <- readline(prompt="Do you want IT/science category?(yes:1, no:2): ")
            selectSports <- readline(prompt="Do you want sports category?(yes:1, no:2): ")
            selectEntertain <- readline(prompt="Do you want entertainment category?(yes:1, no:2): ")
        }
    else
        {
            selectPolitics<-1
            selectEconomy<-1
            selectSociety<-1
            selectCulture<-1
            selectWorld<-1
            selectScience<-1
            selectSports<-1
            selectEntertain<-1
        }

    print(paste("Please wait. Results will save at",getwd()))
    print("Start step 1. Scraping URL.")


    end<-as.integer(endDate-startDate)

    listAll<-list()
    list01<-list()
    list02<-list()
    list03<-list()
    list04<-list()
    list05<-list()
    list06<-list()
    list07<-list()
    list08<-list()


    if(selectPolitics==1)
        {

            print("Start Politics part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=269&sid1=100&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=269&sid1=100&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[500:1000]
                            test<-test[-grep("img",test)]
                            listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
                            list01<-c(list01,listif)

                            Sys.sleep(0.01)
                            list01<-unlist(list01)
                            list01<-unique(list01)
                            lennow<-length(list01)
                            print(paste("I'm scraping politics part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }

            list01<-gsub('<a href=\"','',list01)
            list01<-gsub('\">','',list01)
            list01<-substr(list01,1,108)
            if(length(grep("hot",list01))>0)
                {
                    list01<-list01[-grep("hot",list01)]
                }
            if(length(grep("endic",list01))>0)
                {
                    list01<-list01[-grep("endic",list01)]
                }
            if(length(grep("target",list01))>0)
                {
                    list01<-list01[-grep("target",list01)]
                }
            if(length(grep("class",list01))>0)
                {
                    list01<-list01[-grep("class",list01)]
                }
        }

    if(selectEconomy==1)
        {

            print("Start economy part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=263&sid1=101&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=263&sid1=101&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[500:1000]
                            test<-test[-grep("img",test)]
                            listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
                            list02<-c(list02,listif)

                            Sys.sleep(0.5)
                            list02<-unlist(list02)
                            list02<-unique(list02)
                            lennow<-length(list02)
                            print(paste("I'm scraping economy part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }

            list02<-gsub('<a href=\"','',list02)
            list02<-gsub('\">','',list02)
            list02<-substr(list02,1,108)
            if(length(grep("hot",list02))>0)
                {
                    list02<-list02[-grep("hot",list02)]
                }
            if(length(grep("endic",list02))>0)
                {
                    list02<-list02[-grep("endic",list02)]
                }
            if(length(grep("target",list02))>0)
                {
                    list02<-list02[-grep("target",list02)]
                }
            if(length(grep("class",list02))>0)
                {
                    list02<-list02[-grep("class",list02)]
                }
        }

    if(selectSociety==1)
        {

            print("Start society part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=257&sid1=102&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=257&sid1=102&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[500:1000]
                            test<-test[-grep("img",test)]
                            listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
                            list03<-c(list03,listif)

                            Sys.sleep(0.5)
                            list03<-unlist(list03)
                            list03<-unique(list03)
                            lennow<-length(list03)
                            print(paste("I'm scraping society part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }

            list03<-gsub('<a href=\"','',list03)
            list03<-gsub('\">','',list03)
            list03<-substr(list03,1,108)
            if(length(grep("hot",list03))>0)
                {
                    list03<-list03[-grep("hot",list03)]
                }
            if(length(grep("endic",list03))>0)
                {
                    list03<-list03[-grep("endic",list03)]
                }
            if(length(grep("target",list03))>0)
                {
                    list03<-list03[-grep("target",list03)]
                }
            if(length(grep("class",list03))>0)
                {
                    list03<-list03[-grep("class",list03)]
                }
        }

    if(selectCulture==1)
        {

            print("Start culture part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=245&sid1=103&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=245&sid1=103&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[500:1000]
                            test<-test[-grep("img",test)]
                            listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
                            list04<-c(list04,listif)

                            Sys.sleep(0.5)
                            list04<-unlist(list04)
                            list04<-unique(list04)
                            lennow<-length(list04)
                            print(paste("I'm scraping culture part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }

            list04<-gsub('<a href=\"','',list04)
            list04<-gsub('\">','',list04)
            list04<-substr(list04,1,108)
            if(length(grep("hot",list04))>0)
                {
                    list04<-list04[-grep("hot",list04)]
                }
            if(length(grep("endic",list04))>0)
                {
                    list04<-list04[-grep("endic",list04)]
                }
            if(length(grep("target",list04))>0)
                {
                    list04<-list04[-grep("target",list04)]
                }
            if(length(grep("class",list04))>0)
                {
                    list04<-list04[-grep("class",list04)]
                }
        }

    if(selectWorld==1)
        {

            print("Start world news part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=322&sid1=104&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=322&sid1=104&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[500:1000]
                            test<-test[-grep("img",test)]
                            listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
                            list05<-c(list05,listif)

                            Sys.sleep(0.5)
                            list05<-unlist(list05)
                            list05<-unique(list05)
                            lennow<-length(list05)
                            print(paste("I'm scraping world news part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }

            list05<-gsub('<a href=\"','',list05)
            list05<-gsub('\">','',list05)
            list05<-substr(list05,1,108)
            if(length(grep("hot",list05))>0)
                {
                    list05<-list05[-grep("hot",list05)]
                }
            if(length(grep("endic",list05))>0)
                {
                    list05<-list05[-grep("endic",list05)]
                }
            if(length(grep("target",list05))>0)
                {
                    list05<-list05[-grep("target",list05)]
                }
            if(length(grep("class",list05))>0)
                {
                    list05<-list05[-grep("class",list05)]
                }
        }

    if(selectScience==1)
        {

            print("Start IT/science part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=228&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://news.naver.com/main/list.nhn?sid2=228&sid1=105&mid=shm&mode=LS2D&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[500:1000]
                            test<-test[-grep("img",test)]
                            listif<-unique(gsub("\t","",test[grep("href=\"http",test)]))
                            list06<-c(list06,listif)

                            Sys.sleep(0.5)
                            list06<-unlist(list06)
                            list06<-unique(list06)
                            lennow<-length(list06)
                            print(paste("I'm scraping IT/Science part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }

            list06<-gsub('<a href=\"','',list06)
            list06<-gsub('\">','',list06)
            list06<-substr(list06,1,108)
            if(length(grep("hot",list06))>0)
                {
                    list06<-list06[-grep("hot",list06)]
                }
            if(length(grep("endic",list06))>0)
                {
                    list06<-list06[-grep("endic",list06)]
                }
            if(length(grep("target",list06))>0)
                {
                    list06<-list06[-grep("target",list06)]
                }
            if(length(grep("class",list06))>0)
                {
                    list06<-list06[-grep("class",list06)]
                }
        }


    if(selectSports==1)
        {

            print("Start sports part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-gsub('-',"",as.character(dd))
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://sports.news.naver.com/sports/index.nhn?category=sports_general&ctg=news&mod=lst&type=news&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://sports.news.naver.com/sports/index.nhn?category=sports_general&ctg=news&mod=lst&type=news&date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[698:1300]
                            test<-test[grep("<tr><td><a href=\"",test)]
                            listif<-paste0("http://sports.news.naver.com/", substr(test, 35, 128))
                            list07<-c(list07,listif)

                            Sys.sleep(0.5)
                            list07<-unlist(list07)
                            list07<-unique(list07)
                            lennow<-length(list07)
                            print(paste("I'm scraping sports part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }
        }

    if(selectEntertain==1)
        {

            print("Start entertainment part.")

            for(j in 0:end)
                {
                    dd<-as.Date(j, origin = startDate)
                    kk<-as.character(dd)
                    len<-0
                    lennow<-1
                    i<-1
                    while(len!=lennow)
                        {

                            len<-lennow
                            ncnt<-1

                            options(warn=-1)
                            test<-tryCatch(eval(parse(text=paste0("readLines('http://entertain.naver.com/home/mainNews?date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            if(grepl("Read error",test))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            test<-tryCatch(eval(parse(text=paste0("readLines('http://entertain.naver.com/home/mainNews?date=",kk,"&page=",i,"',warn=F)"))),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            test<-test[grep('class="tit"',test)]
                            listif<-paste0("http://entertain.naver.com", substr(test, 12, 39))
                            list08<-c(list08,listif)

                            Sys.sleep(0.5)
                            list08<-unlist(list08)
                            list08<-unique(list08)
                            lennow<-length(list08)
                            print(paste("I'm scraping entertainment part",lennow,"links. I'm at",kk,"date,",i,"page."))
                            i<-i+1

                        }
                }
        }

    listAll<-c(list01,list02,list03,list04,list05,list06,list07,list08)
    write.csv(listAll,"listURL.csv",row.names=F)
    print(paste("We get",length(listAll),"links!"))
    print(paste("Get url list is Done! It is saved at",getwd(),"named listURL.csv."))
    listLen<-length(listAll)
    listAll<-c(list01,list02,list03,list04,list05,list06)

##########################################################################################################
########################                                                     #############################
########################                 step 2 getting contents             #############################
########################                                                     #############################
##########################################################################################################

    print("Start step 2. Crawling contents. This is last step.")
    dataAll<-data.frame(category=8,title="test",author="test",postTime="time",chgTime="time",contents="test")
    errorURL<-list()

    if(listLen>0)
        {
            if(length(listAll)>0)
                {
                    for(j in 1:dd)
                        {
                            tt<-as.character(listAll[j])
                            if(nchar(tt)==144|nchar(tt)==138)
                                {
                                    substr(tt,1,108)->tt
                                }

                            ncnt<-1
                            tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            options(warn=-1)
                            if(grepl("Read error",tst))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            if(ncnt==3)
                                {
                                    errorURL<<-c(errorURL,tt)
                                }
                                    else
                                {
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
                                    if(length(grep("본문 내용",tst))>1)
                                        {
                                            con<-tst[grep("본문 내용",tst)[1]:grep("본문 내용",tst)[2]]
                                        }
                                    else
                                        {
                                            con<-tst[grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)[ grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)>grep("본문 내용",tst)][1]]
                                        }

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
                                    if (length(chgTime)!=0)
                                        {
                                            chgTime<-gsub("\t","",chgTime)
                                            chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
                                            chgTime<-gsub("최종수정 ","",chgTime)
                                        }
                                    else
                                        {
                                            chgTime<-postTime
                                        }
                                    con<-gsub("\t","",con)
                                    con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
                                    con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
                                    con<-paste(con, collapse = "")

                                    listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
                                    options(warn=-1)
                                    if(!grepl("Read error",listTem))
                                        {
                                           
                                            dataAll<-rbind(dataAll,listTem)
                                            print(paste(cate,j,j/dd*100,"%"))
                                        }
                                        
                                    else
                                        {
                                            print(paste("This link is not right.",tt))
                                            print(paste(cate,j,j/dd*100,"% above"))
                                            errorURL<<-c(errorURL,tt)
                                        }
                                    options(warn=1)
                                    Sys.sleep(0.5)
                                }
                        }
                }
            if(selectSports==1)
                {
                    dd<-length(list07)
                    for(j in 1:dd)
                        {
                            tt<-list07[j]
                            ncnt<-1
                            tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            options(warn=-1)
                            if(grepl("Read error",tst))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            tst<-tryCatch(readLines(tt,warn=F),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)

                            if(ncnt==3)
                                {
                                    errorURL<<-c(errorURL,tt)
                                }
                            else
                                {
                                    tst<-gsub('&nbsp;',"",tst)
                                    tst<-gsub('&lt;',"<",tst)
                                    tst<-gsub('&gt;',">",tst)
                                    tst<-gsub('&amp;',"&",tst)
                                    tst<-gsub('&quot;','"',tst)

                                    title<-tst[grep("og:title",tst)]
                                    author<-tst[grep(":author\"",tst)]
                                    postTime<-tst[grep("기사입력",tst)+1]
                                    chgTime<-tst[grep("최종수정",tst)]
                                    if(length(grep("기사 내용",tst))>1)
                                        {
                                            con<-tst[grep("기사 내용",tst)[1]:grep("기사 내용",tst)[2]]
                                        }
                                    else
                                        {
                                            con<-tst[grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)[ grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)>grep("기사 내용",tst)][1]]
                                        }

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
                                    if (length(chgTime)!=0)
                                        {
                                            chgTime<-gsub("\t","",chgTime)
                                            chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
                                            chgTime<-gsub("[[:punct:]]", "", chgTime)
                                            chgTime<-gsub("최종수정", "", chgTime)
                                            chgTime<-gsub(" ", "", chgTime)
                                            chgTime<-paste0(substr(chgTime,1,4),"-",substr(chgTime,5,6),"-",substr(chgTime,7,8)," ",substr(chgTime,9,10),":",substr(chgTime,11,12))
                                        }
                                    else
                                        {
                                            chgTime<-postTime
                                        }
                                    con<-gsub("\t","",con)
                                    con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
                                    con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
                                    con<-paste(con, collapse = "")

                                    listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
                                    options(warn=-1)
                                    if(!grepl("Read error",listTem))
                                        {
                                           
                                            dataAll<-rbind(dataAll,listTem)
                                            print(paste(cate,j,j/dd*100,"%"))
                                        }
                                    
                                    else
                                        {
                                            print(paste("This link is not right.",tt))
                                            print(paste(cate,j,j/dd*100,"% above"))
                                            errorURL<<-c(errorURL,tt)
                                        }
                                    options(warn=1)
                                    Sys.sleep(0.5)
                                }
                        }
                }

            if(selectEntertain==1)
                {
                    dd<-length(list08)
                    for(j in 1:dd)
                        {
                            tt<-as.character(list08[j])
                            if(nchar(tt)==144|nchar(tt)==138)
                                {
                                    substr(tt,1,108)->tt
                                }
                            tst<-readLines(tt,warn=F,encoding="UTF-8")
                            ncnt<-1
                            tst<-tryCatch(readLines(tt,warn=F,encoding="UTF-8"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                            options(warn=-1)
                            if(grepl("Read error",tst))
                                {
                                    while(ncnt>3)
                                        {
                                            Sys.sleep(1)
                                            tst<-tryCatch(readLines(tt,warn=F,encoding="UTF-8"),  error = function(e) print("Read error, Please wait. It will be start after 1 sec."))
                                            ncnt<-ncnt+1
                                        }
                                }
                            options(warn=1)
                            if(ncnt==5)
                                {
                                    errorURL<<-c(errorURL,tt)
                                }
                            else
                                {


                                    tst<-gsub('&nbsp;',"",tst)
                                    tst<-gsub('&lt;',"<",tst)
                                    tst<-gsub('&gt;',">",tst)
                                    tst<-gsub('&amp;',"&",tst)
                                    tst<-gsub('&quot;','"',tst)
                                    tst<-gsub('&#039;',"'",tst)

                                    cate<-"TV연애"
                                    title<-tst[grep("og:title",tst)]
                                    author<-tst[grep(":author",tst)]
                                    postTime<-tst[grep("기사입력",tst)]
                                    chgTime<-tst[grep("최종수정",tst)]
                                    if(length(grep("본문 내용",tst))>1)
                                        {
                                            con<-tst[grep("본문 내용",tst)[1]:grep("본문 내용",tst)[2]]
                                        }
                                    else
                                        {
                                            con<-tst[grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)[ grep("[(가-힣ㄱ-ㅎㅏ-ㅣ)]",tst)>grep("본문 내용",tst)][1]]
                                        }

                                    cate<-gsub("[^(가-힣ㄱ-ㅎㅏ-ㅣ)]","",cate)
                                    title<-gsub('<meta property=\"og:title\"\t\t\tcontent=\"',"",title)
                                    title<-gsub('\"/>',"",title)
                                    author<-gsub('<meta property=\"og:article:author\"\tcontent=\"',"",author)
                                    author<-gsub('\"/>',"",author)
                                    author<-gsub(" 네이버TV연예","",author)
                                    author<-gsub("[[:punct:]]", "", author)
                                    author<-gsub(" ", "", author)
                                    postTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",postTime)
                                    postTime<-gsub("기사입력 ","",postTime)
                                    if (length(chgTime)!=0)
                                        {
                                            chgTime<-gsub("\t","",chgTime)
                                            chgTime<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",chgTime)
                                            chgTime<-gsub("최종수정 ","",chgTime)
                                        }
                                    else
                                        {
                                            chgTime<-postTime
                                        }
                                    con<-gsub("\t","",con)
                                    con<-gsub("<!--(([^-]*)|([-]{1})|([-]{2})([^>]{1}))*?-->","",con)
                                    con<-gsub("<(/)?([a-zA-Z]*)(\\s[a-zA-Z]*=[^>]*)?(\\s)*(/)?>","",con)
                                    if(length(grep("script",con))==2)
                                        {
                                            con<-con[c(-grep("script",con)[1]:-grep("script",con)[2])]
                                        }
                                    con<-paste(con, collapse = "")
                                    con<-gsub("<!--.*-->","",con)

                                    listTem<-tryCatch(data.frame(category=cate,title=title,author=author,postTime=postTime,chgTime=chgTime,contents=con),  error = function(e) print("Read error."))
                                    options(warn=-1)
                                    if(!grepl("Read error",listTem))
                                        {
                                           
                                            dataAll<-rbind(dataAll,listTem)
                                            print(paste(cate,j,j/dd*100,"%"))
                                        }
                                    else
                                        {
                                            print(paste("This link is not right.",tt))
                                            print(paste(cate,j,j/dd*100,"% above"))
                                            errorURL<<-c(errorURL,tt)
                                        }
                                    options(warn=1)
                                    Sys.sleep(0.5)
                                }
                        }
                }


            dataAll<-dataAll[-1,]
            write.csv(dataAll,"dataAll.csv",row.names=F)
            print(paste("Get contents is Done! It is saved at",getwd(),"named dataAll.csv."))
        }
    else
        {
            print("No url collected.")
        }
}
getNaverNews()