#####################################################################################################
# Många företag samt organisationer har stor nytta av att ta reda på vad allmänheten tycker om just det 
# företaget/organisationen. Ett sätt att ta reda på detta är att undersöka vad folk skriver om dem på 
# sociala medier.  Ett stort socialt medium, som används aktivt av mer är 300 miljoner människor 
# världen över, är Twitter. En analys av tweets riktade mot företag/organisationer genom t.ex. 
# en given hashtag, kan hjälpa företag/organisationer att få en klarare bild över vad allmänhetens 
# attityd gentemot dem är. Vid lansering av en ny produkt är denna analysmetod speciellt effektiv, 
# då den snabbt och enkelt ger feedback från allmänheten angående vad de tycker om produkten.

# Denna typ av analys kallas för sentiment analysis och i detta projektet är målet att ta reda på vad 
# som skrivs på Twitter om företaget Nintendo, som precis annonserat att Nintendo of Americas 
# president, Reggie Fils-Aime, ska gå i pension och ersättas av Doug Bowser. 
# Vi är alltså ute efter att ta reda på allmänhetens reaktion på detta och projektet kommer ge svar
# på frågan om vad folk skriver om Nintendo samt hur dess attityd gentemot företaget är genom en 
# analys av 1000 engelskspråkiga tweets som inkluderar hashtagen Nintendo. Tweetsen hämtas och sparas
# i en csv-fil med hjälp av Twitters API samt R-paketet "rtweet". 

# För att ta reda på vad som skrivs om företaget på Twitter redovisas två grafer, en barplot samt ett 
# wordcloud, innehållandes information över vilka ord som mest frekvent förekommer bland de 1000 
# tweetsen. För att liknande ord, t.ex. run, running, runs osv ska kategoriseras under ett och samma 
# namn används en metod som kallas stemming, vilket leder till att vissa ord i graferna kan ha lite 
# annorlunda stavning samt grammatisk böjning. Detta är dock ett pris som jag tycker det är värt att 
# betala för att få möjligheten att slå samman liknande ord under ett namn.

# En analys av orden som redovisas i dessa två grafer kan ge en acceptabel bild över vad allmänheten 
# tycker om Nintendo efter annonseringen av president-bytet. Det finns dock ett effektivare sätt att 
# ta reda på om de 1000 tweetsen innehållandes hashtagen Nintendo kan kategoriseras som positiva, 
# negativa, förhoppningsfulla osv genom en metod som går igenom varje ord i varje tweet och 
# poängsätter varje ord med avseende på om de är positiva, negativa, förhoppningsfulla osv. 
# Efter att varje ord i varje tweet analyserats av denna metod kan vi genom en barplot t.ex. redovisa 
# hur många poäng kategorin "positive" samlat ihop. Metoden i fråga kan kategorisera ett ord till tio 
# olika kategorier, och det är fullt möjligt att ett ord kan kategoriseras till fler än en kategori.

# Projektet är uppdelat i tre delar där den första är tillägnad åt förberedning av datamaterialet, 
# då text från sociala medier ofta är data som behöver förberedas och tvättas ordentligt innan 
# analysfasen. Del två handlar om att ta fram de två graferna som redovisar vilka ord som förekommer 
# mest frekvent i tweetsen. Slutligen handlar den sista delen om implementeringen av vår sentiment 
# analysis för att ta reda på den allmänna attityden gentemot företaget Nintendo enligt dessa 1000 tweets.
# Projektet avslutas med en kort kommentar över det uppnådda resultatet.
#####################################################################################################


#######################################################################
########################Förbered datamaterialet########################
#######################################################################


# R-paket som är nödvändiga för att utföra projektet.
install.packages("quanteda")
install.packages("wordcloud2")
install.packages("syuzhet")

library(quanteda)
library(wordcloud2)
library(syuzhet)


# Importera csv-filen med namn "TweetsNintendo" innehållandes 1000 tweets.
Data <- read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = F)


# Ta bort länkar i tweetsen.
Data$Tweet <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", Data$Tweet)


# Ta bort koden för emojis i tweetsen.
Data$Tweet <- gsub("<.*?>", "", Data$Tweet)


# Ta bort ord som börjar med "#" eller "@" i tweetsen.
# Detta för att t.ex. ordet #Nintendo skulle förekomma i varje tweet, och tecknet "@" används för att
# länka till en annan twitter-användare. Ord som börjar med @ är således överflödiga då de inte 
# hjälper oss att utvinna någon information över vad personen som tweetat tycker om företaget Nintendo.
Data$Tweet <- gsub("[@#]\\w+ *", "", Data$Tweet)


# Gör så att varje ord i en tweet blir en "token", samt ta bort punkter, symboler, bindestreck och
# ord som endast är nummer.
# För att kunna använda funktionen steaming behöver varje ord bli en egen token.
Data.tokens <- tokens(Data$Tweet, what = "word", 
                      remove_numbers = TRUE, remove_punct = TRUE,
                      remove_symbols = TRUE, remove_hyphens = TRUE)


# Konvertera alla tokens till gemener.
Data.tokens <- tokens_tolower(Data.tokens)


# Ta bort engelska "stop-words" i våra tweets.
Data.tokens <- tokens_select(Data.tokens, stopwords(), 
                             selection = "remove")


# Utför stemming på våra tokens.
Data.tokens <- tokens_wordstem(Data.tokens, language = "english")


# Skapa en document-feature matris som sedan konverteras till en matris.
# Vid konverteringen används funktionen transpose så att kolumnerna innehåller tweetsen och raderna 
# orden.
dfm <- dfm(Data.tokens, tolower = FALSE)
dfm <- t(as.matrix(dfm))


#######################################################################
###Skapa två grafer innehållande de mest frekvent förekommande orden###
#######################################################################


# Skapa en barplot som redovisar vilka ord som förekommer mest frekvent i våra 1000 tweets.  
# Endast ord som förekommer minst 40 gånger redovisas i grafen.
words <- rowSums(dfm)
words <- subset(words, words >= 40)
barplot(words, las = 2, main = 'Mest frekvent förekommande orden', col = rainbow(40), ylim = c(0,350))


# Skapa ett "wordcloud" som redovisar vilka ord som förekommer mest frekvent i våra 1000 tweets.
# Genom att dra musen över ett ord i grafen redovisas antal gånger det givna ordet förekommer i
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


# Utför sentiment analysis på våra 1000 tweets.
tweets <- iconv(Data$Tweet)
Sentiment_Scores <- get_nrc_sentiment(tweets)


# Skapa en barplot som redovisar resultatet av vår sentiment analysis.
par(mar=c(6,4,4,2))
barplot(colSums(Sentiment_Scores), las = 2, col = rainbow(10), 
        main = 'Sentiment analysis av #Nintendo tweets', ylab = 'Antal', ylim = c(0,1000))


#####################################################################################################
# Resultatet av ovanstående sentiment analysis visar att attityden gentemot företaget Nintendo är 
# mestadels positiv. Resultatet stämmer väl överens med vad man kan förvänta sig efter inspektion av 
# de två graferna innehållande information över de ord som förekommer mest frekvent i de 1000 tweetsen,
# då man ser att ord som är positivt laddade förekommer i större utsträckning än negativt laddade ord.
# En förklaring till varför staplarna "positive" samt "anticipation" är störst i grafen skulle kunna 
# förklaras av att många tackar den gamla presidenten, , Reggie Fils-Aime, för ett bra utfört arbete 
# på Nintendo of America, samt att många är förväntansfulla över vad den nya presidenten, Doug Bowser,
# kan åstadkomma.
#####################################################################################################'