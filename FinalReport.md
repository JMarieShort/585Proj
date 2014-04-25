Twitter Advisor Analysis
========================================================
**Jessica Short**

Background & Objective
--------------------------------------------------------
Many insurance and financial products are sold through advisors. There is a biweekly Twitter meetup for advisors to talk about relevant industry trends and strategies. These conversations are identified with specific hashtags. This project walks through the process of collecting tweets in this conversation series, creating some summary statistics about the participants and mapping relationships between them.

Data Collection
--------------------------------------------------------
Data collection was completed in R, utilizing the twitteR package in order to pull data from Twitter, using my personal developer account. I also utilized dplyr and lubridate to get the data in suitable format. Packages ggplot2 and igraph were utilized for graphing. The final report was compiled using R Markdown and knitr.
### Twitter Developer Accounts
In order to have access to Twitter information through the API, I set up a developer account following the directions provided by [this blog post](http://thinktostart.wordpress.com/2013/05/22/twitter-authentification-with-r/). The consumerKey and consumerSecret are masked below, any attempt to re-create the data collection would require your own developer account credentials. 


```r
library(RCurl)
library(twitteR)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "#################"
consumerSecret <- "####################################"

twitCred <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
    requestURL = reqURL, accessURL = accessURL, authURL = authURL)

twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

registerTwitterOAuth(twitCred)

```


### Bi-Weekly Tweet Collection
Tweets needed to be collected on a regular basis because tweets are no longer searchable through the API after about 2 weeks. The code below was used to collect all the relevant tweets on a bi-weekly basis. The data was then saved off to be analyzed later. 

Collection is also affected by rate limiting as described on the [Twitter developers website](https://dev.twitter.com/). Rate limiting is the reason for the inclusion of sys.sleep() in the functions used below. For future analysis, I would consider the use of the streameR package, because it allows access to the streaming API, which has less restrictive rate limits for this type of analysis.


```r
term<-########
rsTweets <- searchTwitter(term, n=500)
tweets.df2<-twListToDF(rsTweets)

users<-unique(llply(rsTweets,screenName))

#have to use a function so the system won't rate limit 
at<-data.frame()
for (user in users){
   # Download latest 100 tweets from the user's timeline
   tweets <- twListToDF(userTimeline(user,n=100,includeRts=TRUE))
   at<-rbind(at,tweets)
   Sys.sleep(25)
}

saveRDS(tweets.df2, file=paste0("AT_",today(),".Rdata"))
saveRDS(at, file=paste0("UserHist_",today(),".Rdata"))

```

#### Friendship Collection
In order to collect a measure of a relationship between 2 participating users, I am using friendship, where your friends are defined as users that you follow.

I waited until all the sessions of interest were completed in order to collect the user details and friendships. Ideally, I would like to see relationships and user statistics at the time of participation to evaluate changes. However, this collection method is satisfactory for an exploratory effort. 


```r
friends_df <- ldply(at_uids, function(x) {
    Sys.sleep(60)
    x1 <- getUser(x)$getFriendIDs()
    x2 <- x1[x1 %in% at_uids]
    return(data.frame(cbind(node_id = x, friend_id = x2, friend = 1)))
})
saveRDS(friends_df, file = "friends415.Rdata")
```


### Data Reshaping 
Tweets were collected on the following dates
  * 03-05-2014
  * 03-20-2014
  * 04-06-2014
  
In the section below, the data from all collections are combined, and participant summaries created. 

#### Tweet Details
One of the interesting features in collecting the data was that retweets are returned in the searches. In contrast, retweets are not included in the user's history by default; this can be adjusted in the twitteR function UserTimeline by setting the includeRts option. I chose to eliminate retweets from the talk sessions, since the user who re-tweets is not generating their own content, and the original author will get credit for that re-tweet.


```r
collectDates <- c("2014-03-05", "2014-03-20", "2014-04-06")

# AT_test is the list of Conversation tweets
AT_test <- ldply(collectDates, function(x) {
    res <- readRDS(paste0("AT_", x, ".Rdata"))
    res$ext_dt <- x
    res
})
# drop the retweets from the AT_test collection
AT_test_noRT <- subset(AT_test, !isRetweet)
saveRDS(AT_test_noRT, file = "AT_TW.Rdata")

# UH_test is all the tweets for users that participated in the conversations
UH_test <- ldply(collectDates, function(x) {
    res <- readRDS(paste0("UserHist_", x, ".Rdata"))
    res$ext_dt <- x
    res
})
UH_test2 <- UH_test[!duplicated(UH_test$id), ]
saveRDS(UH_test2, file = "UserHistFinal.Rdata")


# get users account details
at_users <- twListToDF(lookupUsers(unique(AT_test_noRT$screenName)))
saveRDS(at_users, file = "at_users.Rdata")

```

#### Summaries
The steps above resulted in 4 dataframes that were saved to avoid re-computation. We can now load the dataframes and continue with the analysis.

First the data is cleaned. Then, summary statistics are calculated regarding each user and details about their account.





```r

library(plyr)
library(lubridate)
library(ggplot2)
library(igraph)
library(RColorBrewer)
# Read in UH_test2, AT_test_noRT, and at_users, friends

friend_edge <- readRDS("friends415.Rdata")
AT_test_noRT <- readRDS("AT_TW.Rdata")
UH_test2 <- readRDS("UserHistFinal.Rdata")
at_users <- readRDS("at_users.Rdata")

# structural changes to friend df
friend_edge$node_id <- as.character(friend_edge$node_id)
friend_edge$friend_id <- as.character(friend_edge$friend_id)
friend_edge$friend <- as.character(friend_edge$friend)

# categorize user's account as 'new'/'existing'
at_users$yr_splt <- (year(at_users$created) > 2012)
at_users$yr_splt <- factor(at_users$yr_splt, levels = c(TRUE, FALSE), labels = c("New Accounts (2013 or 2014)", 
    "Prior Existing Accounts"))

# categorize if the tweet is about insurance/financial services etc
UH_test2$tpc <- grepl("Retire|401k|insurance|invest", UH_test2$text, ignore.case = TRUE)

# clean up source in User History
UH_test2$src <- sapply(strsplit(gsub("</a>", "", UH_test2$statusSource), ">"), 
    function(x) ifelse(length(x) > 1, x[2], x[1]))


# AT participation stats
AT_summ <- ddply(AT_test_noRT, .(screenName), summarise, n_AT_tw = length(id), 
    n_AT_ses = length(unique(ext_dt)), n_AT_RT = sum(retweetCount))

# general stats on user history
userStats <- ddply(UH_test2, .(screenName), summarise, min_dt = min(created), 
    max_dt = max(created), n_tw = length(screenName), avg_RT = (sum(retweetCount)/n_tw), 
    pct_fin = mean(tpc), mode_src = names(which.max(table(src))))

userStats <- subset(userStats, screenName %in% AT_summ$screenName)

userStatsFinal <- merge(at_users, merge(userStats, AT_summ, by = "screenName"), 
    by = "screenName")
# move id to first column
userStatsFinal <- userStatsFinal[, c(13, 1:12, 14:26)]


# Add some additional information to UserStats search term masked - idea is
# to identify users associated with the company
userStatsFinal$aflg <- grepl(term2, userStatsFinal$description, ignore.case = TRUE)

```

The following variables are now available for each user:   


```
id 
screenName 
description 
statusesCount 
followersCount 
favoritesCount 
friendsCount 
url 
name 
created 
protected 
verified 
location 
listedCount 
followRequestSent 
profileImageUrl 
yr_splt 
min_dt 
max_dt 
n_tw 
avg_RT 
pct_fin 
mode_src 
n_AT_tw 
n_AT_ses 
n_AT_RT 
aflg 
```



Advisor Analysis 
-------------------------------------------------------
The primary objective of this analysis was to gain a greater understanding about the online presence of advisors and how they are using Twitter to build their personal brand. With that goal in mind, I subset the analysis to users that were identified to be advisors. 



```r
adv_UserStats <- subset(userStatsFinal, aflg == TRUE)
# also removing the Chat host
adv_UserStats <- subset(adv_UserStats, followersCount < 4000)
adv_UserHist <- subset(UH_test2, screenName %in% adv_UserStats$screenName)
# High / Med / Low for % of tweets relating to Financial Services
adv_UserStats$fina_dec <- with(adv_UserStats, cut(pct_fin, quantile(pct_fin, 
    (0:3)/3), label = FALSE, include.lowest = TRUE))

adv_UserStats$fina_dec <- factor(adv_UserStats$fina_dec, levels = c(3, 2, 1), 
    labels = c("High", "Moderate", "Low"))
```

#### Location Information
Location information could be included in a number of places. 

```r
table(is.na(adv_UserHist$longitude))
```

```
## 
## FALSE  TRUE 
##    54  3287
```

```r
table(adv_UserStats$location)
```

```
## 
##                                         32216           Appleton, WI 
##                      9                      1                      1 
##                Atlanta           Atlanta, GA        Atlanta, Georgia 
##                      1                      1                      2 
##   Atlanta, Georgia USA                Chicago            Chicago, IL 
##                      1                      1                      3 
##   DC/Maryland/Virginia             Des Moines            Des Moines  
##                      1                      2                      1 
##         Des Moines, Ia         Des Moines, IA        Des Moines, IA  
##                      1                      5                      1 
##       Des Moines, Iowa            Detroit, MI                Georgia 
##                      1                      1                      1 
##       Jacksonville, Fl       Jacksonville, FL Jacksonville, Florida  
##                      1                      3                      1 
##        Jacksonvlle, FL         Johnston, Iowa        Kansas City, KS 
##                      1                      1                      1 
##            New England                    NYC       Omaha, Nebraska  
##                      1                      1                      1 
##      Overland Park, KS         Parsippany, NJ      San Francisco, CA 
##                      1                      1                      1 
##         Schaumburg, IL               Virginia     Washington, DC RIS 
##                      1                      1                      1
```

* Not a lot of available location information in latitude/longitude by tweet. 
* Location in user profile is free form, but with some cleaning it could be valuable.

#### Account Creation

```r
check <- data.frame(dt = sort(adv_UserStats$created), n = c(1:nrow(adv_UserStats)))
ggplot(aes(x = round_date(dt, "day"), y = n), data = check) + geom_line() + 
    labs(x = "Date", y = "Number Accounts") + theme(aspect.ratio = 0.75)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

This plot shows how many of these accounts existed across time. We can see a sharp increase in the rate of account creation beginning in the second half of 2012. 
#### Connection Method
In this section, I explore the applications being used to tweet.

```r
# How are they accessing twitter? Including 'work related' vs. non-work
# related tweets
pcts <- ddply(adv_UserHist, .(src), summarise, pct_fin = round(mean(tpc) * 100, 
    2), y = length(id) - (sum(tpc)/2))
pcts <- pcts[with(pcts, order(-y)), ]


ggplot() + geom_bar(aes(x = reorder(src, src, function(x) length(x)), fill = tpc), 
    data = adv_UserHist) + geom_text(aes(x = src, y = y, label = pct_fin), data = pcts) + 
    coord_flip() + labs(x = "Twitter Access Source", fill = "Financial Services \n information in Tweet") + 
    xlim(pcts$src[8:1]) + scale_fill_brewer(type = "qual", palette = 3)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

The bars represent the number of tweets in the users' histories posted using each application. The color represents whether the tweet contained financial services information. The percentages represent the percent of financial services tweets within each group. 

* Most of these tweets are being posted using [HootSuite](https://hootsuite.com/), followed by web and the Twitter iPhone app. 
* The mobile applications don't seem to be used for 'work-related' tweets as often as the web/tweet button.


#### Retweets and followers

```r
ggplot(data = adv_UserStats, aes(x = log(followersCount), y = (n_AT_RT), colour = factor(fina_dec))) + 
    geom_point(aes(size = 3)) + facet_wrap(~yr_splt, nrow = 2) + labs(x = "Log (followers)", 
    y = "Retweets of chat contributions", colour = "% Financial Tweets") + scale_color_manual(values = brewer.pal(4, 
    "Set1")) + guides(size = FALSE)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

This plot attempts to explore a few metrics of influence. One is the number of followers (shown on the x axis) and the other is the number of retweets of chat content (shown on the y axis). We can see from this plot that
* Established accounts tend to have more followers
* One user is being retweeted a lot more than any other new user
* Some users with the largest number of followers have very few retweets. 

#### Advisor Relationships
In this section I explore network graphs to visualize relationships among users. I am currently using friend relationships to create network edges. Replies and Retweets are also good options for describing the connection between users.Since I am only measuring relationships in a pre-determined subset of the population, this dataframe could easily be transformed to change the directionality of the relationship and measure followers. 
  

```r
friends_a <- subset(friend_edge, node_id %in% adv_UserStats$id & friend_id %in% 
    adv_UserStats$id)
twgr3 <- graph.data.frame(d = friends_a, vertices = adv_UserStats)
# Note that graph.data.frame from the igraph package has an issue with dates
# in the vertex #dataframe, because class returns a vector of length 2 for
# that data type.
```

 

```r
 al <- layout.auto(twgr3)
frl <- layout.fruchterman.reingold(twgr3)

plot(twgr3, layout=al,
     vertex.label=NA,#V(twgr2)$screenName, 
    edge.arrow.size=0.1,
    vertex.color =V(twgr3)$n_AT_ses,
    vertex.size = log(V(twgr3)$followersCount),
    main = 'Twitter Network')

legend(1, 
       1.25,
       legend = c('# Sessions','1', 
                  '2',
                  '3'), 
       col = c('white','black','red','green'), 
       lty=1,
       cex = .7)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


The plot allows us to see the Twitter based connections among these advisors, but a true analysis would require more research into the theory of network diagrams. 

Future Work
--------------------------------------------------------
This is primarily an exploratory effort to understand the data that is available via Twitter, and the ease of access through R and the twitteR package. There are many potential extensions of this information. 

* Search Twitter for tweets regarding retirement, financial services, etc. and then scrape the profile descriptions and urls of those users to see what other web presence those users have. See if we can link Twitter handles to linked in profiles, and pull any information from LinkedIn.
* Try Sentiment Analysis of the tweets collected, what type of attitude is most effective?



References
--------------------------------------------------------
Jeff Gentry (2013). twitteR: R based Twitter client. R package version 1.1.7.
  http://CRAN.R-project.org/package=twitteR
  
Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with
  lubridate. Journal of Statistical Software, 40(3), 1-25. URL
  http://www.jstatsoft.org/v40/i03/.


Hillebrand, J. (2013 May 22). Twitter Authentication with R. http://thinktostart.wordpress.com/2013/05/22/twitter-authentification-with-r/.

McFarland, Daniel, Solomon Messing,
 Mike Nowak, and Sean Westwood. 2010. "Social Network Analysis          
 Labs in R." Stanford University.  
 
R Core Team (2014). R: A language and environment for statistical computing. R
  Foundation for Statistical Computing, Vienna, Austria. URL
  http://www.R-project.org/.
  
Rogers, Everett M. (1962). Diffusion of Innovations. Glencoe: Free Press. ISBN 0-612-62843-4.

H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
 
Hadley Wickham and Romain Francois (2014). dplyr: dplyr: a grammar of data
  manipulation. R package version 0.1.2. http://CRAN.R-project.org/package=dplyr

Yihui Xie (2013). knitr: A general-purpose package for dynamic report generation
  in R. R package version 1.5.

     
