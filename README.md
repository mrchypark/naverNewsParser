# naverNewsParser

2012년 부터 2014년 까지 네이버에 게시된 뉴스를 list로 불러옵니다.<br>
(참고로 그냥 실행시키시면 일주일도 더 걸릴껍니다.)<br>
<br>
1단계는 네이버에 직접 게시된 링크를 불러옵니다.<br>
7개 카테고리(정치, 경제, 사회, 생활/문화, 세계, IT/과학, 스포츠)의 뉴스를 불러옵니다.<br>
2단계는 링크를 불러서 6개 콜론(카테고리, 제목, 신문사, 게제시간, 수정시간, 내용)으로 정리해서 저장합니다.<br>
네이버의 블락을 피하기 위해 매 요청마다 0.5초의 딜레이를 주었습니다.<br>
본 코드는 R로 제작되었습니다.<br>
코드 효율 및 최적화는 고려하지 않았습니다.<br>
수정 요청 대 환영입니다.<br>
<br>
뉴스의 저작권은 상업목적으로는 불허(돈을 내야함)하며 연구의 목적을 위해 수집하였습니다.<br>
한국온라인 신문협회(KONA) http://www.kona.or.kr/ (국민일보, 동아닷컴, 디지틀조선일보, 매경인터넷, 미디어칸, 세계닷컴,
전자신문인터넷, 조인스닷컴, 한겨레엔, 한경닷컴, 한국아이닷컴)의 뉴스는 ③ 영리를 목적으로 하지 아니하고 개인적으로 이용하는 경우 복제를 허가하고 있습니다.<br>
하지만 데이터 자체를 계시하는 경우를 불법으로 간주하므로 수집하는 코드만을 공개합니다.<br>
수집이 완료된 자료는 혹여 메일로 요청하실 경우 영리목적을 하지 않을 것을 증빙해주시면 보내드리겠습니다.<br>
감사합니다.<br>
<br>
This source code is to parse naver news in Korea.<br>
As you can see this code is by R.<br>
Naver news categories are 7(politics,economy,sociaty,culture,world news, IT/sceince, sports).<br>
Always welcome your participation.<br>



 
