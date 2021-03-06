#####################################################################################################
# M�nga f�retag samt organisationer har stor nytta av att ta reda p� vad allm�nheten tycker om just det 
# f�retaget/organisationen. Ett s�tt att ta reda p� detta �r att unders�ka vad folk skriver om dem p� 
# sociala medier.  Ett stort socialt medium, som anv�nds aktivt av mer �r 300 miljoner m�nniskor 
# v�rlden �ver, �r Twitter. En analys av tweets riktade mot f�retag/organisationer genom t.ex. 
# en given hashtag, kan hj�lpa f�retag/organisationer att f� en klarare bild �ver vad allm�nhetens 
# attityd gentemot dem �r. Vid lansering av en ny produkt �r denna analysmetod speciellt effektiv, 
# d� den snabbt och enkelt ger feedback fr�n allm�nheten ang�ende vad de tycker om produkten.

# Denna typ av analys kallas f�r sentiment analysis och i detta projektet �r m�let att ta reda p� vad 
# som skrivs p� Twitter om f�retaget Nintendo, som precis annonserat att Nintendo of Americas 
# president, Reggie Fils-Aime, ska g� i pension och ers�ttas av Doug Bowser. 
# Vi �r allts� ute efter att ta reda p� allm�nhetens reaktion p� detta och projektet kommer ge svar
# p� fr�gan om vad folk skriver om Nintendo samt hur dess attityd gentemot f�retaget �r genom en 
# analys av 1000 engelskspr�kiga tweets som inkluderar hashtagen Nintendo. Tweetsen h�mtas och sparas
# i en csv-fil med hj�lp av Twitters API samt R-paketet "rtweet". 

# F�r att ta reda p� vad som skrivs om f�retaget p� Twitter redovisas tv� grafer, en barplot samt ett 
# wordcloud, inneh�llandes information �ver vilka ord som mest frekvent f�rekommer bland de 1000 
# tweetsen. F�r att liknande ord, t.ex. run, running, runs osv ska kategoriseras under ett och samma 
# namn anv�nds en metod som kallas stemming, vilket leder till att vissa ord i graferna kan ha lite 
# annorlunda stavning samt grammatisk b�jning. Detta �r dock ett pris som jag tycker det �r v�rt att 
# betala f�r att f� m�jligheten att sl� samman liknande ord under ett namn.

# En analys av orden som redovisas i dessa tv� grafer kan ge en acceptabel bild �ver vad allm�nheten 
# tycker om Nintendo efter annonseringen av president-bytet. Det finns dock ett effektivare s�tt att 
# ta reda p� om de 1000 tweetsen inneh�llandes hashtagen Nintendo kan kategoriseras som positiva, 
# negativa, f�rhoppningsfulla osv genom en metod som g�r igenom varje ord i varje tweet och 
# po�ngs�tter varje ord med avseende p� om de �r positiva, negativa, f�rhoppningsfulla osv. 
# Efter att varje ord i varje tweet analyserats av denna metod kan vi genom en barplot t.ex. redovisa 
# hur m�nga po�ng kategorin "positive" samlat ihop. Metoden i fr�ga kan kategorisera ett ord till tio 
# olika kategorier, och det �r fullt m�jligt att ett ord kan kategoriseras till fler �n en kategori.

# Projektet �r uppdelat i tre delar d�r den f�rsta �r till�gnad �t f�rberedning av datamaterialet, 
# d� text fr�n sociala medier ofta �r data som beh�ver f�rberedas och tv�ttas ordentligt innan 
# analysfasen. Del tv� handlar om att ta fram de tv� graferna som redovisar vilka ord som f�rekommer 
# mest frekvent i tweetsen. Slutligen handlar den sista delen om implementeringen av v�r sentiment 
# analysis f�r att ta reda p� den allm�nna attityden gentemot f�retaget Nintendo enligt dessa 1000 tweets.
# Projektet avslutas med en kort kommentar �ver det uppn�dda resultatet.
#####################################################################################################


#######################################################################
########################F�rbered datamaterialet########################
#######################################################################


# R-paket som �r n�dv�ndiga f�r att utf�ra projektet.
install.packages("quanteda")
install.packages("wordcloud2")
install.packages("syuzhet")

library(quanteda)
library(wordcloud2)
library(syuzhet)


# Importera csv-filen med namn "TweetsNintendo" inneh�llandes 1000 tweets.
Data <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = F)


# Ta bort l�nkar i tweetsen.
Data$Tweet <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", Data$Tweet)


# Ta bort koden f�r emojis i tweetsen.
Data$Tweet <- gsub("<.*?>", "", Data$Tweet)


# Ta bort ord som b�rjar med "#" eller "@" i tweetsen.
# Detta f�r att t.ex. ordet #Nintendo skulle f�rekomma i varje tweet, och tecknet "@" anv�nds f�r att
# l�nka till en annan twitter-anv�ndare. Ord som b�rjar med @ �r s�ledes �verfl�diga d� de inte 
# hj�lper oss att utvinna n�gon information �ver vad personen som tweetat tycker om f�retaget Nintendo.
Data$Tweet <- gsub("[@#]\\w+ *", "", Data$Tweet)


# G�r s� att varje ord i en tweet blir en "token", samt ta bort punkter, symboler, bindestreck och
# ord som endast �r nummer.
# F�r att kunna anv�nda funktionen steaming beh�ver varje ord bli en egen token.
Data.tokens <- tokens(Data$Tweet, what = "word", 
                      remove_numbers = TRUE, remove_punct = TRUE,
                      remove_symbols = TRUE, remove_hyphens = TRUE)


# Konvertera alla tokens till gemener.
Data.tokens <- tokens_tolower(Data.tokens)


# Ta bort engelska "stop-words" i v�ra tweets.
Data.tokens <- tokens_select(Data.tokens, stopwords(), 
                             selection = "remove")


# Utf�r stemming p� v�ra tokens.
Data.tokens <- tokens_wordstem(Data.tokens, language = "english")


# Skapa en document-feature matris som sedan konverteras till en matris.
# Vid konverteringen anv�nds funktionen transpose s� att kolumnerna inneh�ller tweetsen och raderna 
# orden.
dfm <- dfm(Data.tokens, tolower = FALSE)
dfm <- t(as.matrix(dfm))


#######################################################################
###Skapa tv� grafer inneh�llande de mest frekvent f�rekommande orden###
#######################################################################


# Skapa en barplot som redovisar vilka ord som f�rekommer mest frekvent i v�ra 1000 tweets.  
# Endast ord som f�rekommer minst 40 g�nger redovisas i grafen.
words <- rowSums(dfm)
words <- subset(words, words >= 40)
barplot(words, las = 2, main = 'Mest frekvent f�rekommande orden', col = rainbow(40), ylim = c(0,350))


# Skapa ett "wordcloud" som redovisar vilka ord som f�rekommer mest frekvent i v�ra 1000 tweets.
# Genom att dra musen �ver ett ord i grafen redovisas antal g�nger det givna ordet f�rekommer i
# de 1000 tweetsen.
words_sorted <- sort(rowSums(dfm), decreasing = TRUE)
words_df <- data.frame(names(words_sorted), words_sorted)
colnames(words_df) <- c('ord', 'freq')
head(words_df)
cloud <- wordcloud2(words_df, size = 0.7, shape = 'circle', minSize = 2, rotateRatio = 0.30)
cloud


#######################################################################
##########################Sentiment Analysis###########################
#######################################################################


# Utf�r sentiment analysis p� v�ra 1000 tweets.
tweets <- iconv(Data$Tweet)
Sentiment_Scores <- get_nrc_sentiment(tweets)


# Skapa en barplot som redovisar resultatet av v�r sentiment analysis.
par(mar=c(6,4,4,2))
barplot(colSums(Sentiment_Scores), las = 2, col = rainbow(10), 
        main = 'Sentiment analysis av #Nintendo tweets', ylab = 'Antal', ylim = c(0,1000))


#####################################################################################################
# Resultatet av ovanst�ende sentiment analysis visar att attityden gentemot f�retaget Nintendo �r 
# mestadels positiv. Resultatet st�mmer v�l �verens med vad man kan f�rv�nta sig efter inspektion av 
# de tv� graferna inneh�llande information �ver de ord som f�rekommer mest frekvent i de 1000 tweetsen,
# d� man ser att ord som �r positivt laddade f�rekommer i st�rre utstr�ckning �n negativt laddade ord.
# En f�rklaring till varf�r staplarna "positive" samt "anticipation" �r st�rst i grafen skulle kunna 
# f�rklaras av att m�nga tackar den gamla presidenten, , Reggie Fils-Aime, f�r ett bra utf�rt arbete 
# p� Nintendo of America, samt att m�nga �r f�rv�ntansfulla �ver vad den nya presidenten, Doug Bowser,
# kan �stadkomma.
#####################################################################################################'