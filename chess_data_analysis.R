stringsAsFactors = FALSE

Badges <- read.csv("Chess_data_frames/Badges.csv")
Comments <- read.csv("Chess_data_frames/Comments.csv")
PostLinks <- read.csv("Chess_data_frames/PostLinks.csv")
Posts <- read.csv("Chess_data_frames/Posts.csv")
Tags <- read.csv("Chess_data_frames/Tags.csv")
Users <- read.csv("Chess_data_frames/Users.csv")
Votes <- read.csv("Chess_data_frames/Votes.csv")

##########################################################
liczba_postow_w_latach <- sqldf::sqldf(
  "
  SELECT substring(X_CreationDate, 1, 4) as rok, COUNT(X_Id) as Liczba_postow 
  FROM Posts
  GROUP BY substring(X_CreationDate, 1, 4)
  "
)

plot_liczba_postow_w_latach <- barplot(
  height = liczba_postow_w_latach$Liczba_postow,
  names = liczba_postow_w_latach$rok,
  main = "Liczba postów w kolejnych latach",
  ylim = c(0, 1.15*max(liczba_postow_w_latach$Liczba_postow))
)

text(
  x = plot_liczba_postow_w_latach,
  y = liczba_postow_w_latach$Liczba_postow,
  label = liczba_postow_w_latach$Liczba_postow,
  pos = 3,
  cex = 0.8,
  col = "black"
)

##########################################################

liczba_postow_2020 <- sqldf::sqldf(
  "
  SELECT strftime('%m', X_CreationDate) as miesiac, COUNT(X_Id) as Liczba_postow 
  FROM Posts
  WHERE substring(X_CreationDate, 1, 4) == '2020'
  GROUP BY strftime('%m', X_CreationDate)
  "
)

plot_liczba_postow_2020 <- barplot(
  height = liczba_postow_2020$Liczba_postow,
  names = liczba_postow_2020$miesiac,
  main = "Liczba postów w kolejnych miesi¹cach 2020",
  ylim = c(0, 1.15*max(liczba_postow_2020$Liczba_postow))
)

text(
  x = plot_liczba_postow_2020,
  y = liczba_postow_2020$Liczba_postow,
  label = liczba_postow_2020$Liczba_postow,
  pos = 3,
  cex = 0.8,
  col = "black"
)

liczba_postow_2021 <- sqldf::sqldf(
  "
  SELECT strftime('%m', X_CreationDate) as miesiac, COUNT(X_Id) as Liczba_postow 
  FROM Posts
  WHERE substring(X_CreationDate, 1, 4) == '2021'
  GROUP BY strftime('%m', X_CreationDate)
  "
)

plot_liczba_postow_2021 <- barplot(
  height = liczba_postow_2021$Liczba_postow,
  names = liczba_postow_2021$miesiac,
  main = "Liczba postów w kolejnych miesi¹cach 2021",
  ylim = c(0, 1.15*max(liczba_postow_2021$Liczba_postow))
)

text(
  x = plot_liczba_postow_2021,
  y = liczba_postow_2021$Liczba_postow,
  label = liczba_postow_2021$Liczba_postow,
  pos = 3,
  cex = 0.8,
  col = "black"
)

liczba_uzytkownikow_w_latach <- sqldf::sqldf(
  "
  SELECT substring(X_CreationDate, 1, 4) as rok_zalozenia, COUNT(X_Id) as Liczba_uzytkownikow 
  FROM Users
  GROUP BY substring(X_CreationDate, 1, 4)
  "
)

plot_liczba_uzytkownikow_w_latach <- barplot(
  height = liczba_uzytkownikow_w_latach$Liczba_uzytkownikow,
  names = liczba_uzytkownikow_w_latach$rok_zalozenia,
  main = "Liczba nowych u¿ytkowników w kolejnych latach",
  ylim = c(0, 1.15*max(liczba_uzytkownikow_w_latach$Liczba_uzytkownikow))
)

text(
  x = plot_liczba_uzytkownikow_w_latach,
  y = liczba_uzytkownikow_w_latach$Liczba_uzytkownikow,
  label = liczba_uzytkownikow_w_latach$Liczba_uzytkownikow,
  pos = 3,
  cex = 0.8,
  col = "black"
)

liczba_uzytkownikow_2020 <- sqldf::sqldf(
  "
  SELECT strftime('%m', X_CreationDate) as miesiac, COUNT(X_Id) as Liczba_uzytkownikow 
  FROM Users
  WHERE substring(X_CreationDate, 1, 4) == '2020'
  GROUP BY strftime('%m', X_CreationDate)
  "
)

plot_liczba_uzytkownikow_2020 <- barplot(
  height = liczba_uzytkownikow_2020$Liczba_uzytkownikow,
  names = liczba_uzytkownikow_2020$miesiac,
  main = "Liczba nowych u¿ytkowników w kolejnych miesi¹cach 2020",
  ylim = c(0, 1.15*max(liczba_uzytkownikow_2020$Liczba_uzytkownikow))
)

text(
  x = plot_liczba_uzytkownikow_2020,
  y = liczba_uzytkownikow_2020$Liczba_uzytkownikow,
  label = liczba_uzytkownikow_2020$Liczba_uzytkownikow,
  pos = 3,
  cex = 0.8,
  col = "black"
)


liczba_uzytkownikow_2021 <- sqldf::sqldf(
  "
  SELECT strftime('%m', X_CreationDate) as miesiac, COUNT(X_Id) as Liczba_uzytkownikow 
  FROM Users
  WHERE substring(X_CreationDate, 1, 4) == '2021'
  GROUP BY strftime('%m', X_CreationDate)
  "
)

plot_liczba_uzytkownikow_2021 <- barplot(
  height = liczba_uzytkownikow_2021$Liczba_uzytkownikow,
  names = liczba_uzytkownikow_2021$miesiac,
  main = "Liczba nowych u¿ytkowników w kolejnych miesi¹cach 2021",
  ylim = c(0, 1.15*max(liczba_uzytkownikow_2021$Liczba_uzytkownikow))
)

text(
  x = plot_liczba_uzytkownikow_2021,
  y = liczba_uzytkownikow_2021$Liczba_uzytkownikow,
  label = liczba_uzytkownikow_2021$Liczba_uzytkownikow,
  pos = 3,
  cex = 0.8,
  col = "black"
)

liczba_komentarzy_w_latach <- sqldf::sqldf(
  "
  SELECT substring(X_CreationDate, 1, 4) as rok, COUNT(X_Id) as Liczba_komentarzy 
  FROM Comments
  GROUP BY substring(X_CreationDate, 1, 4)
  "
)

plot_liczba_komentarzy_w_latach <- barplot(
  height = liczba_komentarzy_w_latach$Liczba_komentarzy,
  names = liczba_komentarzy_w_latach$rok,
  main = "Liczba komentarzy w kolejnych latach",
  ylim = c(0, 1.15*max(liczba_komentarzy_w_latach$Liczba_komentarzy))
)

text(
  x = plot_liczba_komentarzy_w_latach,
  y = liczba_komentarzy_w_latach$Liczba_komentarzy,
  label = liczba_komentarzy_w_latach$Liczba_komentarzy,
  pos = 3,
  cex = 0.8,
  col = "black"
)

###################################################################

liczba_komentarzy_2020 <- sqldf::sqldf(
  "
  SELECT strftime('%m', X_CreationDate) as miesiac, COUNT(X_Id) as Liczba_komentarzy 
  FROM Comments
  WHERE substring(X_CreationDate, 1, 4) == '2020'
  GROUP BY strftime('%m', X_CreationDate)
  "
)

plot_liczba_komentarzy_2020 <- barplot(
  height = liczba_komentarzy_2020$Liczba_komentarzy,
  names = liczba_komentarzy_2020$miesiac,
  main = "Liczba komentarzy w kolejnych miesi¹cach 2020",
  ylim = c(0, 1.15*max(liczba_komentarzy_2020$Liczba_komentarzy))
)

text(
  x = plot_liczba_komentarzy_2020,
  y = liczba_komentarzy_2020$Liczba_komentarzy,
  label = liczba_komentarzy_2020$Liczba_komentarzy,
  pos = 3,
  cex = 0.8,
  col = "black"
)


#Wnioski:
  # W 2020 roku znacznie przyby³o postów i u¿ytkowników w stosunku do poprzednich lat
  # Serial Queen's Gambit przyczyni³ siê do wzrostu liczby u¿ytkowników (pierwszy odcinek
  # wyemitowano w paŸdzierniku 2020 - wtedy nast¹pi³ wzrost, natomiast
  # nie przyczyni³ siê do wzrostu postów



