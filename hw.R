library(readr)
inter_student105 <- 
  read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
View(inter_student105)
inter_student106 <- 
  read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=57bc363ccce160a21b217889cf0a8413")
View(inter_student106)
inter_student107 <- 
  read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=3fcc20ce2557c2ba27468f8b73c8f312")
View(inter_student107)
inter_school105 <- 
  read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
View(inter_school105)
inter_school106 <-
  read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=a0dc5559421a6ad525fdc461bb87ff83")
View(inter_school106)
inter_school107 <-
  read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1f89cd92b57dc3e08b7337101fa70d52")
View(inter_school107)
exchange_student <- read_csv("C:/Users/user/Downloads/exchange student.csv", 
                             locale = locale(encoding = "BIG5"))
exchange_student_total <- read_csv("C:/Users/user/Downloads/exchange_student_total.csv", 
                                   locale = locale(encoding = "BIG5"))
View(exchange_student_total)
inter_student105$國別<-gsub("中國大陸","大陸地區",inter_student105$國別)

exchange_student$`進修交流國家(地區)別`<-gsub("大陸地區","中國",exchange_student$`進修交流國家(地區)別`)
exchange_student$`進修交流國家(地區)別`<-gsub("朝鮮民主主義人民共和國(北韓)","北韓",exchange_student$`進修交流國家(地區)別`)
exchange_student$`進修交流國家(地區)別`<-gsub("印度尼西亞共和國","印尼",exchange_student$`進修交流國家(地區)別`)
exchange_student$`進修交流國家(地區)別`<-gsub("埃及阿拉伯共和國","埃及",exchange_student$`進修交流國家(地區)別`)
exchange_student$`進修交流國家(地區)別`<-gsub("蒙古國","蒙古",exchange_student$`進修交流國家(地區)別`)
exchange_student$`進修交流國家(地區)別`<-gsub("德意志聯邦共和國","德國",exchange_student$`進修交流國家(地區)別`)
exchange_student$`進修交流國家(地區)別`<-
  gsub("共和國|王國|侯國|聯邦|社會主義共和國|聯合大公國|合眾國|和平之國|民主社會主義共和國|伊斯蘭共和國|大公國|人民共和國",
       "",exchange_student$`進修交流國家(地區)別`)
exchange_student_total$國別[33]<-"阿拉伯"

library(tidyr)
library(dplyr)

inter_student105<-gather(inter_student105,stu,num,-洲別,-國別)
student105<-
  inter_student105%>%group_by(國別)%>%summarise(Total=sum(as.numeric(num)))
inter_student106<-gather(inter_student106,stu,num,-洲別,-國別)
student106<-
  inter_student106%>%group_by(國別)%>%summarise(Total=sum(as.numeric(num)))
inter_student107<-gather(inter_student107,stu,num,-洲別,-國別)
student107<-
  inter_student107%>%group_by(國別)%>%summarise(Total=sum(as.numeric(num)))


inter_student<-full_join(student105,student106,by="國別")
inter_student<-full_join(inter_student,student107,by="國別")
inter_student[is.na(inter_student)]<-0
inter_student$國別<-gsub("大陸地區","中國",inter_student$國別)

inter_student<-gather(inter_student,year,num,-國別)
inter_student<-inter_student%>%group_by(國別)%>%
  summarise(total=sum(as.numeric(num),na.rm = T))%>%arrange(desc(total))

head(inter_student,10)

View(inter_student)
View(student)

inter_school105<-gather(inter_school105,stu,num,-學校類型,-學校代碼,-學校名稱)
school105<-
  inter_school105%>%group_by(學校名稱)%>%summarise(total=sum(num))

inter_school106<-gather(inter_school106,stu,num,-學校類型,-學校代碼,-學校名稱)
school106<-
  inter_school106%>%group_by(學校名稱)%>%summarise(total=sum(num))

inter_school107<-gather(inter_school107,stu,num,-學校類型,-學校代碼,-學校名稱)
school107<-
  inter_school107%>%group_by(學校名稱)%>%summarise(total=sum(num))

inter_school<-full_join(school105,school106,by="學校名稱")
inter_school<-full_join(inter_school,school107,by="學校名稱")
inter_school[is.na(inter_school)]<-0
View(inter_school)
inter_school<-gather(inter_school,year,num,-學校名稱)
school<-inter_school%>%group_by(學校名稱)%>%
  summarise(total=sum(num))%>%arrange(desc(total))

school<-school[-1,]
head(school,10)


library(ggplot2)
ggplot()+geom_bar(data = inter_student,aes(x=國別,y=total),stat = "identity")
library(choroplethrMaps)
library(choroplethr)
library(readxl)
CountryName <- read_excel("C:/Users/user/Downloads/CountryName.xlsx")
colnames(CountryName)<-c("arc","region","國別")
CountryName$region<-tolower(CountryName$region)
country<-inner_join(inter_student,CountryName,by="國別")
country<-country[!duplicated(country$region),]
country<-rename(country,value=total)

country_choropleth(country,title = "各國來台念書的學生人數",num_colors=4,legend="人數")




exchange_student$學校名稱
exchange_country<-exchange_student%>%
  select(`進修交流國家(地區)別`,`本國學生出國進修交流至少1學期(修讀學分)以上人數小計`,
         `本國學生出國進修交流未滿1學期(修讀學分)人數小計`)
View(exchange_country)
exchange_school<-exchange_student%>%
  select(學校名稱,`本國學生出國進修交流至少1學期(修讀學分)以上人數小計`,
             `本國學生出國進修交流未滿1學期(修讀學分)人數小計`)
View(exchange_school)

exchange_country<-gather(exchange_country,stu,num,-`進修交流國家(地區)別`)
exchange_country<-exchange_country%>%group_by(`進修交流國家(地區)別`)%>%
  summarise(total=sum(num))%>%arrange(desc(total))

head(exchange_country,10)
View(exchange_country)

exchange_country<-rename(exchange_country,國別=`進修交流國家(地區)別`)
countrymap<-inner_join(exchange_country,CountryName,by="國別")
countrymap<-countrymap[!duplicated(countrymap$region),]
countrymap<-rename(countrymap,value=total)
class(countrymap)
country_choropleth(countrymap,title = "台灣大專院校學生去各國的留學人數",num_colors=4,legend="人數")
View(exchange_country)

ggplot()+geom_bar(data = exchange_country,aes(x=國別,y=total),stat = "identity")

exchange_school<-gather(exchange_school,stu,num,-學校名稱)
exchange_school<-exchange_school%>%group_by(學校名稱)%>%
  summarise(total=sum(num))%>%arrange(desc(total))

head(exchange_school,10)
exchange_student_total<-rename(exchange_student_total,num=總人數)
exchange_student_total<-exchange_student_total%>%group_by(國別)%>%
  summarise(value=sum(num))%>%arrange(desc(value))
exchange_student_total$國別[33]<-"阿拉伯"
View(exchange_student_total)
head(exchange_student_total,10)
total_student<-inner_join(exchange_student_total,CountryName,by="國別")
total_student<-total_student[!duplicated(total_student$region),]
county_choropleth(total_student,title = "台灣學生去各國的留學人數",num_colors=4,legend="國家")
#這是R Code Chunk
View(total_student)

library(treemap)
treemap(inter_student,
        index = "國別",
        vSize="total",
        vColor="total",
        type="value",
        title="各國來台留學人數比例")
treemap(exchange_country,
        index = "國別",
        vSize="total",
        vColor="total",
        type="value",
        title="台灣去各國留學人數比例")
?treemap
