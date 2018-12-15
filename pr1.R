library(ggplot2)
library(GGally)

load('movies_merged')
df = movies_merged

# dim(df)
df2 <- df[df$Type == "movie",]
# dim(df2)

toNumeric = function(n){
  strtoi(unlist(strsplit(df2[n,"Runtime"], " "))[1])
}

get.seq = mapply(toNumeric, 1:nrow(df2))
df2$Runtime = unlist(get.seq)

#2
df2.year.runtime = subset(df2, select = c(Year, Runtime))
df2.year.runtime.NoNA = na.omit(df2.year.runtime)

ggplot(df2.year.runtime.NoNA, aes("",Runtime)) + geom_boxplot()+coord_flip()+scale_x_discrete()
ggplot(df2.year.runtime.NoNA, aes(x=Runtime))+geom_histogram()

df2.year.runtime.NoNA$Year = ( df2.year.runtime.NoNA$Year%/% 10) * 10

ggplot(df2.year.runtime.NoNA, aes(reorder(Year, Year, median), Runtime) ) + geom_boxplot() + scale_x_discrete("") 

df2.year.runtime.NoNA.aggregate = aggregate(df2.year.runtime.NoNA[, "Runtime"], list(df2.year.runtime.NoNA$Year), mean)
names(df2.year.runtime.NoNA.aggregate) = c("Year", "Runtime")
# ggplot(df2.year.runtime.NoNA.aggregate, aes(x=Year, y=Runtime))+geom_line()
ggplot(df2.year.runtime.NoNA.aggregate, aes(x=Year, y=Runtime,fill=Runtime)) + geom_bar(stat="identity")

df2.budget.runtime = subset(df2, select = c(Budget, Runtime))
df2.budget.runtime.NoNA = na.omit(df2.budget.runtime)

qplot(Budget, Runtime, data = df2.budget.runtime.NoNA, size = I(1.2)) +
  stat_smooth(color = "red", size = I(2), se = F)

# 3
head(df2$Genre)
View(df2$Genre)
library(tm)

df2.genre.runtime = subset(df2, select = c(Genre, Runtime))

sum(df2.genre.runtime$Genre == "N/A")
sum(df2.genre.runtime$Genre == "NA")

df2.genre.runtime$Genre = ifelse(df2.genre.runtime$Genre=="N/A", "NA", df2.genre.runtime$Genre)

sum(df2.genre.runtime$Genre == "N/A")
sum(df2.genre.runtime$Genre == "NA")

dim(df2.genre.runtime)

df2.genre.runtime = subset(df2.genre.runtime, df2.genre.runtime$Genre!="NA")
dim(df2.genre.runtime)

df2.genre.runtime$Genre = gsub(",", " ", df2.genre.runtime$Genre)
corp = Corpus(VectorSource(df2.genre.runtime$Genre))

matrix = DocumentTermMatrix(corp)
Genre.df = data.frame(as.matrix(matrix))
no.of.Genres = ncol(Genre.df)
Genre.df$Runtime = df2.genre.runtime$Runtime
df2.genre.runtime = Genre.df

head(df2.genre.runtime)
sorted.genre = sort(colSums(df2.genre.runtime[,1:no.of.Genres]), decreasing = TRUE)[1:10]
genre_proportion=sorted.genre/nrow(df2.genre.runtime)
pie(genre_proportion)


genre_prop = data.frame(names(genre_proportion), genre_proportion)
names(genre_prop) = c("genre", "prop")

ggplot(genre_prop, aes(x=genre, y=prop,fill=prop)) + geom_bar(stat="identity")

df2.genre.runtime.top10 = df2.genre.runtime[c(names(sorted.genre), "Runtime")]
dim(df2.genre.runtime.top10)
df2.genre.runtime.top10 = na.omit(df2.genre.runtime.top10)

library(reshape2)
# melt the data to a long format
df2.genre.runtime.top10.long <- melt(data = df2.genre.runtime.top10, id.vars = "Runtime")
names(df2.genre.runtime.top10.long)
dim(df2.genre.runtime.top10.long)
df2.genre.runtime.top10.long = subset(df2.genre.runtime.top10.long, value==1, c(Runtime, variable))
names(df2.genre.runtime.top10.long)

View(df2.genre.runtime.top10.long)
ggplot(df2.genre.runtime.top10.long, aes(x=factor(variable), y=Runtime))+geom_boxplot()

ggplot(df2.genre.runtime.top10.long, aes(x=Runtime))+geom_histogram()+facet_wrap(~variable)

df2.genre.runtime.NoNA = na.omit(df2.genre.runtime)
nrow(df2.genre.runtime.NoNA)
df2.genre.runtime.NoNA.long = as.data.frame(df2.genre.runtime.NoNA.long)
df2.genre.runtime.NoNA.long$Runtime = as.numeric(as.character(df2.genre.runtime.NoNA.long$Runtime))
ggplot(df2.genre.runtime.NoNA.long, aes(factor(genre), log10(Runtime))) + geom_boxplot()

# 4
names(df2)
tail(df2$Year)
tail(df2$Released)
tail(df2$Date)
summary(df2)
check = function(i){
  flag = TRUE
  year = as.character(df2[i,"Year"])
  released = as.character(df2[i,"Released"])
  date = df2[i,"Date"]
  released.year = ifelse(is.na(released), released, unlist(strsplit(released, "-")) [1])
  print(year)
  print(date)
  print(released.year)
  if(released!="NA" && released!=year)
    flag = FALSE
  if(date!="NA" && date!=year)
    flag = FALSE
  flag
}
print(check(1000))

median(df2.genre.runtime.NoNA$Runtime)

summary(df2$Year)
summary(df2$Date)
summary(df2$Released)
summary(df2$Gross)

df2$Released = as.Date(df2$Released, "%Y-%m-%d")
df2$ReleasedYear = as.numeric(format(df2$Released, "%Y"))
summary(df2$ReleasedYear)

df2$DateNew = ifelse((is.na(df2$Date) & !(is.na(df2$Date)&is.na(df2$ReleasedYear))), df2$Year, df2$Date)
df2$DateNew = ifelse(is.na(df2$Date) , df2$Year, df2$Date)

summary(df2$DateNew)

df2$ReleasedYear = ifelse(is.na(df2$ReleasedYear), df2$Year, df2$ReleasedYear)
summary(df2$ReleasedYear)

df2$YearReleasedDiff = ifelse((df2$Year == df2$ReleasedYear & df2$ReleasedYear==df2$DateNew), "Y", "N")
summary(as.factor(df2$YearReleasedDiff))

df2$YearReleasedDiff2 = ifelse(
  ((df2$Year == df2$ReleasedYear & df2$Year == df2$DateNew) |
    (df2$Year+1 == df2$ReleasedYear & df2$Year+1 == df2$DateNew) |
    (df2$Year-1 == df2$ReleasedYear & df2$Year-1 == df2$DateNew)),
  "Y","N")

df2$YearReleasedDiff2 = ifelse(
     (abs(df2$Year-df2$ReleasedYear)<=1 & abs(df2$Year-df2$DateNew)<=1 & abs(df2$ReleasedYear-df2$DateNew)<=1),
  "Y","N")
summary(as.factor(df2$YearReleasedDiff2))

# Original dataset - no of Gross Records
org.gross = length(df2$Gross[!is.na(df2$Gross)])
print(org.gross)

# Filter out mismatch Year, Date Released Year using "YearReleasedDiff"
df2.Year.Date.Released = subset(df2, YearReleasedDiff=="Y")
nrow(df2.Year.Date.Released)
# Method 1 Gross Records
df2.Year.Date.Released.gross = length(df2.Year.Date.Released$Gross[!is.na(df2.Year.Date.Released$Gross)])
print(df2.Year.Date.Released.gross)
print((org.gross-df2.Year.Date.Released.gross)*100/org.gross)

# Filter out mismatch Year, Date and Released Year using "YearReleasedDiff2"
df2.Year.Date.Released2 = subset(df2, YearReleasedDiff2=="Y")
nrow(df2.Year.Date.Released2)
# Method 2 Gross Records
df2.Year.Date.Released2.gross = length(df2.Year.Date.Released2$Gross[!is.na(df2.Year.Date.Released2$Gross)])
print(df2.Year.Date.Released2.gross)
print((org.gross-df2.Year.Date.Released2.gross)*100/org.gross)

# 5

df2.gross.budget.runtime.genre = subset(df2.Year.Date.Released2, select = c(Gross, Budget, Runtime, Genre))
df2.gross.budget.runtime.genre$Genre = ifelse(df2.gross.budget.runtime.genre$Genre=="N/A", "NA", df2.gross.budget.runtime.genre$Genre)

df2.gross.budget.runtime.genre$Genre = gsub(",", " ", df2.gross.budget.runtime.genre$Genre)
corp = Corpus(VectorSource(df2.gross.budget.runtime.genre$Genre))

matrix = DocumentTermMatrix(corp)
Genre.df = data.frame(as.matrix(matrix))
no.of.Genres = ncol(Genre.df)
Genre.df$Gross = df2.gross.budget.runtime.genre$Gross
Genre.df$Budget = df2.gross.budget.runtime.genre$Budget
Genre.df$Runtime = df2.gross.budget.runtime.genre$Runtime
df2.gross.budget.runtime.genre = Genre.df

ggplot(df2.gross.budget.runtime.genre, aes(log(Budget), log(Gross))) + geom_point() +stat_smooth()+ ggtitle("log(Budget) vs. log(Gross)")

df2.gross.budget.runtime.genre$shortRuntime = ifelse(df2.gross.budget.runtime.genre$Runtime<=103, df2.gross.budget.runtime.genre$Runtime, NA)
df2.gross.budget.runtime.genre$longRuntime = ifelse(df2.gross.budget.runtime.genre$Runtime>103, df2.gross.budget.runtime.genre$Runtime, NA)

ggplot(df2.gross.budget.runtime.genre, aes(Runtime, log(Gross))) + geom_point() + stat_smooth()+ggtitle("Runtime vs. Gross")
ggplot(df2.gross.budget.runtime.genre, aes(shortRuntime, log(Gross))) + geom_point() + stat_smooth()+ggtitle("Short Runtime vs. Gross")
ggplot(df2.gross.budget.runtime.genre, aes(longRuntime, log(Gross))) + geom_point() + stat_smooth()+ggtitle("Long Runtime vs. Gross")

df2.gross.budget.runtime.genre$shortRuntime = NULL
df2.gross.budget.runtime.genre$longRuntime = NULL


# Genre
# Let's do so preparations first
df2.top10 = df2.gross.budget.runtime.genre[c(names(sorted.genre), "Gross")]
name = names(df2.top10)

genres = c()
gross.val = c()

for(i in 1:10){
  genres = c(genres, i)
  tmp = df2.top10
  tmp[,i] = ifelse(tmp[,i]==1, 1,NA)
  tmp = subset(tmp, !is.na(tmp[,i]))
  tmp = subset(tmp, !is.na(tmp$Gross))
  if(nrow(tmp) > 0)
  {
    val = aggregate(tmp$Gross, list(tmp[,i]), FUN = mean)
    gross.val = c(gross.val, val$x)
  }
  else
  { 
    gross.val = c(gross.val, 0)
  } 
}
gross.genre = data.frame(name[1:10], gross.val, genres)
ggplot(gross.genre, aes(y=gross.val, x=genres,fill=gross.val)) +
  geom_bar(stat="identity")

gross.genre$genres = NULL
print(gross.genre)

gross.genre = df2.gross.budget.runtime.genre[c(names(sorted.genre), "Gross")]

gross.genre = melt(data = gross.genre, id.vars = "Gross")
gross.genre = subset(gross.genre, value==1, c(Gross, variable))
ggplot(gross.genre, aes(x=factor(variable), y=Gross))+geom_boxplot()

df.month = df2.Year.Date.Released2
df.month$Released = as.Date(df.month$Released, "%Y-%m-%d")
df.month$Month = as.numeric(format(df.month$Released, "%m"))

GetMonthName = function(num){
  switch(as.numeric(num),"Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
}

df.month$Month = factor(
  lapply(as.numeric(df.month$Month, units="months"), GetMonthName),
  levels=c("Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))

cleanDF = df.month[!is.na(df.month$Month),]
ggplot(cleanDF, aes(reorder(Month, Gross, median), Gross)) + geom_boxplot() + coord_flip() + 
  scale_y_continuous(labels = scales::comma) + xlab("Month") + ggtitle("Gross vs. release Month")

# 6
View(df)
df = subset(df2, select = c("Awards", "Gross"))
df$AwardsNum <- sapply(df$Awards, 
                       function(awards) as.numeric( unlist(
                         regmatches(awards, gregexpr("+[0-9]+", awards))[[1]]
                       ) ) )
df$Wins = 0
df$Nominations = 0
invalidAwards = 0

for(i in 1:nrow(df)){
  if(is.na(df[i,]$Awards) || df[i,]$Awards=="N/A")  
  {
    invalidAwards = invalidAwards +1
    next
  }
  alist = strsplit(df[i,]$Awards, " ")
  # print(alist[[1]])

  Won = c(Won, grep("win",alist[[1]], value=FALSE, ignore.case=TRUE))
  Won = c(Won, grep("Won",alist[[1]], value=FALSE, ignore.case=TRUE))
  
  Nomination = grep("nominat+",alist[[1]], value=FALSE, ignore.case=TRUE)
  
  Won = sort(Won)
  Nomination = sort(Nomination)
  
  k=1
  l=1
  
  for(j in 1:length(df[i,]$AwardsNum[[1]]))
  {
    if(k<=length(Won) & l<=length(Nomination))
    {
      if(Won[k]<Nomination[l])
      {
        df[i,]$Wins = df[i,]$Wins + as.numeric(df[i,]$AwardsNum[[1]][j])          
        k = k+1
      }
      else
      {
        df[i,]$Nominations = df[i,]$Nominations + as.numeric(df[i,]$AwardsNum[[1]][j])          
        l = l+1
      }
    } else if(k<=length(Won))
    {
      df[i,]$Wins = df[i,]$Wins + as.numeric(df[i,]$AwardsNum[[1]][j])          
      k = k+1
    } else if(l<=length(Nomination))
    {
      df[i,]$Nominations = df[i,]$Nominations + as.numeric(df[i,]$AwardsNum[[1]][j])          
      l = l+1
    }
  }
}
print(nrow(df)-invalidAwards)
sum(df$Wins!=0 | df$Nominations!=0)

df$AwardsNum = NULL
df$Awards = NULL

ggplot(df, aes(Gross)) +
  geom_point(data=df, aes(y=Wins, color="Wins")) +
  geom_point(data=df, aes(y=Nominations, color="Nominations")) + ggtitle("Gross vs Wins/Nominations")
ggplot(df, aes(x=Gross, y=Wins)) +
  geom_point() + ggtitle("Gross vs Wins")
ggplot(df, aes(x=Gross, y=Nominations)) +
  geom_point() + ggtitle("Gross vs Nominations")
help(head)

names(df2)
df = subset(df2, select = c())
summary(df2$imdbRating)

# 7
ggpairs(df2[,c("imdbVotes","imdbRating")], title="IMDB Rating Relationships")

ggpairs(df2[, c("tomatoReviews","tomatoFresh","tomatoRotten","tomatoMeter","tomatoRating", "tomatoUserMeter", "tomatoUserRating","tomatoUserReviews")], 
        title = "Pairwise relationships between tomato ratings.")

ggpairs(df2[, c("imdbVotes","imdbRating","tomatoMeter","tomatoRating", "tomatoUserMeter", "tomatoUserRating")], 
                 title = "Pairwise relationships between IMDB and tomato ratings.")

ggplot(df2) + 
  geom_point(aes(x=log(Domestic_Gross),log(Gross))) +
  geom_smooth(aes(x=log(Domestic_Gross),log(Gross))) + 
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) + ggtitle("Domestic Gross vs. Gross")
  
head(df2$Language)
tail(df2$Language)

# Subset Language
mv = subset(df, select = c(Language))
head(mv)
tail(mv)

# Number of rows with Language as "N/A"
sum(mv$Language == "N/A")

# Number of rows with Language as "NA"
sum(mv$Language == "NA")

# Replace N/A with NA
mv$Language = ifelse(mv$Language=="N/A", "NA", mv$Language)

# Recheck
sum(mv$Language == "N/A")
sum(mv$Language == "NA")

dim(mv$Language)

# Remove rows with Language as "NA"
mv = subset(mv, mv$Language!="NA")
dim(mv)

# Replace Language with a collection of binary columns
mv$Language = gsub(",", " ", mv$Language)
corp = Corpus(VectorSource(mv$Language))

matrix = DocumentTermMatrix(corp)
mv = data.frame(as.matrix(matrix))
no.of.lang = ncol(mv)

# no of languages
no.of.lang
head(mv)

# find top ten Language
mv.sorted = sort(colSums(mv[,1:no.of.lang]), decreasing = TRUE)[1:10]

# caculate their proportion
lang_prop=mv.sorted/nrow(mv)
lang_prop

# Create a pie chart of proportion
pie(lang_prop)

lang_prop = data.frame(names(lang_prop), lang_prop)
names(lang_prop) = c("language", "prop")

# create a bar plot of proportion
ggplot(lang_prop, aes(x=language, y=prop,fill=prop)) + geom_bar(stat="identity")


#10
# Subset Genre and Gross 
df2.genre.gross = subset(df2, select = c(Genre, Gross))

# Replace N/A with NA
df2.genre.gross$Genre = ifelse(df2.genre.gross$Genre=="N/A", "NA", df2.genre.gross$Genre)

# Remove rows with Genre as "NA"
df2.genre.runtime = subset(df2.genre.gross, df2.genre.gross$Genre!="NA")

# Replace Genre with a collection of binary columns
df2.genre.gross$Genre = gsub(",", " ", df2.genre.gross$Genre)
corp = Corpus(VectorSource(df2.genre.gross$Genre))

matrix = DocumentTermMatrix(corp)
Genre.df = data.frame(as.matrix(matrix))
no.of.Genres = ncol(Genre.df)

Genre.df$TotalNoGenres = rowSums(Genre.df)
Genre.df$Gross = df2.genre.gross$Gross

df2.genre.gross = Genre.df
df2.genre.gross = na.omit(df2.genre.gross)
ggplot(df2.genre.gross, aes(x=factor(TotalNoGenres), y=log(Gross))) +  
  geom_boxplot() +ggtitle("Total no of genres assigned vs. gross")

no.Genres.Gross = aggregate(df2.genre.gross$Gross, list(df2.genre.gross$TotalNoGenres), FUN = mean)
names(no.Genres.Gross) = c("no.of.genres", "gross")
ggplot(no.Genres.Gross, aes(y=gross, x=no.of.genres, fill=gross)) +  
  geom_bar(stat="identity")
