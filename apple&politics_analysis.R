library(plotrix)
library(tidyr)
library(lubridate)
library(av)
library(ggplot2)
library(readxl)
library(dplyr)
library(gganimate)
library(gifski)
theme_set(theme_bw())
Comments_apple <- as.data.frame(read_excel('Comments_apple.xlsx'))
PostHistory_apple <- as.data.frame(read_excel('PostHistory_apple.xlsx'))
Posts_apple <- as.data.frame(read_excel('Posts_apple.xlsx'))
Tags_apple <- as.data.frame(read_excel('Tags_apple.xlsx'))
Users_apple <- as.data.frame(read_excel('Users_apple.xlsx'))

Comments_politics <- as.data.frame(read_excel('Comments_politics.xlsx'))
PostHistory_politics <- as.data.frame(read_excel('PostHistory_politics.xlsx'))
Posts_politics <- as.data.frame(read_excel('Posts_politics.xlsx'))
Tags_politics <- as.data.frame(read_excel('Tags_politics.xlsx'))
Users_politics <- as.data.frame(read_excel('Users_politics.xlsx'))



#Aktywność pod postami appla, trendy, wpływ pandemii:
Apple2016 <- PostHistory_apple %>% filter(grepl('2016', `Attribute:CreationDate`))
Apple2017 <- PostHistory_apple %>% filter(grepl('2017', `Attribute:CreationDate`))
Apple2018 <- PostHistory_apple %>% filter(grepl('2018', `Attribute:CreationDate`))
Apple2019 <- PostHistory_apple %>% filter(grepl('2019', `Attribute:CreationDate`))
Apple2020 <- PostHistory_apple %>% filter(grepl('2020', `Attribute:CreationDate`))
Apple2021 <- PostHistory_apple %>% filter(grepl('2021', `Attribute:CreationDate`))

Users_apple_2016 <- Users_apple %>% filter(grepl('2016', `Attribute:CreationDate`))
Users_apple_2017 <- Users_apple %>% filter(grepl('2017', `Attribute:CreationDate`))
Users_apple_2018 <- Users_apple %>% filter(grepl('2018', `Attribute:CreationDate`))
Users_apple_2019 <- Users_apple %>% filter(grepl('2019', `Attribute:CreationDate`))
Users_apple_2020 <- Users_apple %>% filter(grepl('2020', `Attribute:CreationDate`))
Users_apple_2021 <- Users_apple %>% filter(grepl('2021', `Attribute:CreationDate`))

Posts_new_users_2016 <- inner_join(Apple2016,Users_apple_2016,by=c('Attribute:UserId'='Attribute:Id'))
Posts_new_users_2016 <- count(unique(Posts_new_users_2016['Attribute:DisplayName']))

Posts_new_users_2017 <- inner_join(Apple2017,Users_apple_2017,by=c('Attribute:UserId'='Attribute:Id'))
Posts_new_users_2017 <- count(unique(Posts_new_users_2017['Attribute:DisplayName']))

Posts_new_users_2018 <- inner_join(Apple2018,Users_apple_2018,by=c('Attribute:UserId'='Attribute:Id'))
Posts_new_users_2018 <- count(unique(Posts_new_users_2018['Attribute:DisplayName']))

Posts_new_users_2019 <- inner_join(Apple2019,Users_apple_2019,by=c('Attribute:UserId'='Attribute:Id'))
Posts_new_users_2019 <- count(unique(Posts_new_users_2019['Attribute:DisplayName']))

Posts_new_users_2020 <- inner_join(Apple2020,Users_apple_2020,by=c('Attribute:UserId'='Attribute:Id'))
Posts_new_users_2020 <- count(unique(Posts_new_users_2020['Attribute:DisplayName']))

Posts_new_users_2021 <- inner_join(Apple2021,Users_apple_2021,by=c('Attribute:UserId'='Attribute:Id'))
Posts_new_users_2021 <- count(unique(Posts_new_users_2021['Attribute:DisplayName']))


Number_new_users <- data.frame(Lata=2016:2020, Nowi_użytkownicy=c(Posts_new_users_2016$n,Posts_new_users_2017$n,
                                                                  Posts_new_users_2018$n,Posts_new_users_2019$n,
                                                                  Posts_new_users_2020$n))

Plot1 <- Number_new_users %>% ggplot(aes(x=Lata,y=Nowi_użytkownicy,fill=Nowi_użytkownicy)) +  geom_bar(stat='identity')+geom_point(size=1.5) 
Plot1
Plot1 + transition_time(Lata)+labs(title='Animacja zmiany aktywności nowych użytkowników')





#Wokół których produktow fimry apple było najawieksze zainteresowanie w czasie pandemii?
Posts_apple_1 <- Posts_apple %>% select(c('Attribute:Id','Attribute:Tags','Attribute:CreationDate')) %>%
                                        filter(grepl('2020|2021', `Attribute:CreationDate`))
Comments_apple_1 <- Comments_apple %>% filter(grepl('2020|2021', `Attribute:CreationDate`)) %>%
  group_by(`Attribute:PostId`) %>% summarise(Number_responds=n())
Posts_responds <- inner_join(Posts_apple_1,Comments_apple_1,by=c('Attribute:Id'='Attribute:PostId')) %>%
  select(c('Attribute:Tags','Number_responds'))
iphone <- Posts_responds %>% filter(grepl('<iphone>', `Attribute:Tags`))  
imac <- Posts_responds %>% filter(grepl('<imac>', `Attribute:Tags`))  
apple_watch <- Posts_responds %>% filter(grepl('<apple-watch>', `Attribute:Tags`))  
macbook_pro <- Posts_responds %>% filter(grepl('<macbook-pro>', `Attribute:Tags`))  
ipad <- Posts_responds %>% filter(grepl('<ipad>', `Attribute:Tags`)) 

Apple_products <- data.frame(Produkty=c('iPhone','iMac','AppleWatch','MacbookPro','iPad'),
                             Responds=c(sum(iphone$Number_responds),sum(imac$Number_responds),
                                        sum(apple_watch$Number_responds),sum(macbook_pro$Number_responds),
                                        sum(ipad$Number_responds)))

slices <- Apple_products$Responds

Plot2 <- pie3D(slices,explode=0.1,radius=2,
      main="Zainteresowanie poszczególnymi produktami firmy Apple w latach 2020-2021")

#Zainteresowanie wyborami w poszczególnych miastach USA
Posts_politics1 <- Posts_politics %>% filter(grepl('2020-10|2020-11', `Attribute:CreationDate`)) %>%
  filter(grepl('voting|election|united_states|donald-trump|joe-biden', `Attribute:Tags`))
Frequency_posts <- inner_join(Posts_politics1,Users_politics,by=c('Attribute:OwnerUserId'='Attribute:Id')) %>%
  group_by(`Attribute:Location`) %>% summarise(Frequency_posts=n())
Comments_politics1 <- inner_join(Posts_politics1,Comments_politics,by=c('Attribute:Id'='Attribute:PostId')) %>%
  select('Attribute:UserId') %>% group_by(`Attribute:UserId`) %>% summarise(Frequency_comments=n())
Frequency_comments <- inner_join(Users_politics,Comments_politics1,by=c('Attribute:Id'='Attribute:UserId')) %>%
  select(c('Attribute:Id','Frequency_comments','Attribute:Location')) %>% group_by(`Attribute:Location`) %>%
  summarise(Frequency_comments=sum(Frequency_comments))
NewYork_activity_comments <- Frequency_comments %>% filter(grepl('New York|NY|NewYork', `Attribute:Location`))
Houston_activity_comments <- Frequency_comments %>% filter(grepl('Houston', `Attribute:Location`))
Seattle_activity_comments <- Frequency_comments %>% filter(grepl('Seattle', `Attribute:Location`)) 
Minnesota_activity_comments <- Frequency_comments %>% filter(grepl('Minnesota', `Attribute:Location`)) 
Denver_activity_comments <- Frequency_comments %>% filter(grepl('Denver', `Attribute:Location`)) 
Atlanta_activity_comments <- Frequency_comments %>% filter(grepl('Atlanta', `Attribute:Location`)) 
SanJose_activity_comments <- Frequency_comments %>% filter(grepl('San Jose', `Attribute:Location`)) 

NewYork_activity_posts <- Frequency_posts %>% filter(grepl('New York|NY|NewYork', `Attribute:Location`))
Houston_activity_posts <- Frequency_posts %>% filter(grepl('Houston', `Attribute:Location`))
Seattle_activity_posts <- Frequency_posts %>% filter(grepl('Seattle', `Attribute:Location`)) 
Minnesota_activity_posts <- Frequency_posts %>% filter(grepl('Minnesota', `Attribute:Location`)) 
Denver_activity_posts <- Frequency_posts %>% filter(grepl('Denver', `Attribute:Location`)) 
Atlanta_activity_posts <- Frequency_posts %>% filter(grepl('Atlanta', `Attribute:Location`)) 
SanJose_activity_posts <- Frequency_posts %>% filter(grepl('San Jose', `Attribute:Location`)) 

Activity_in_election <- data.frame(Miasta=c('NewYork','Houston','Seattle','Minnesota','Denver','Atlanta','SanJose'),
                                   Aktywność=c(sum(NewYork_activity_comments$Frequency_comments)+sum(NewYork_activity_posts$Frequency_posts),
                                   sum(Houston_activity_comments$Frequency_comments)+sum(Houston_activity_posts$Frequency_posts),
                                   sum(Seattle_activity_comments$Frequency_comments)+sum(Seattle_activity_posts$Frequency_posts),
                                   sum(Minnesota_activity_comments$Frequency_comments)+sum(Minnesota_activity_posts$Frequency_posts),
                                   sum(Denver_activity_comments$Frequency_comments)+sum(Denver_activity_posts$Frequency_posts),
                                   sum(Atlanta_activity_comments$Frequency_comments)+sum(Atlanta_activity_posts$Frequency_posts),
                                   sum(SanJose_activity_comments$Frequency_comments)+sum(SanJose_activity_posts$Frequency_posts)))

Plot3 <- Activity_in_election %>% ggplot(aes(x=Miasta,y=Aktywność,fill=Aktywność)) +  geom_bar(stat='identity') + scale_fill_distiller(palette = "Greens", direction = 1)
Plot3

'Trump vs. Biden'
Trump_popularity <- Posts_politics %>% filter(grepl('trump|donald-trump', `Attribute:Tags`))
Biden_popularity <- Posts_politics %>% filter(grepl('biden', `Attribute:Tags`))
Trump_popularity$year <- substr(Trump_popularity$`Attribute:CreationDate`, 0, 4)
Trump_popularity <- Trump_popularity %>% group_by(`year`) %>% summarize(Popularity=n())
Trump_popularity$Candidate <- 'Donald Trump'
Biden_popularity$year <- substr(Biden_popularity$`Attribute:CreationDate`, 0, 4)
Biden_popularity <- Biden_popularity %>% group_by(`year`) %>% summarize(Popularity=n()) 
Biden_popularity$Candidate <- 'Joe Biden'
Comparison <- bind_rows(Biden_popularity,Trump_popularity)
Comparison <- transform(Comparison,year=as.numeric(year))
Plot4 <- Comparison %>% ggplot(aes(x=year,y=Popularity,group=Candidate,color=Candidate))+geom_line()+geom_point()+ggtitle('Animacja popularności poszczególnych kandydatów') + transition_reveal(year)
Plot4
