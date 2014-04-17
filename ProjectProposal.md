Stat 585X: Final Project Proposal
========================================================
**Jessica Short**

Proposed Topic
-------------------------
Many insurance and financial products are sold through advisors. There is a bi-weekly twitter meetup for advisors to talk about relevant industry trends and strategies. Conversations among these professionals are coordinated using specific hashtags. 

I propose to collect the tweets that compose these conversations, as well as the other twitter information of participants.

The primary objective of this analysis is to gain more information about the online presence of the advisors and to understand how they are using twitter to build their personal brand. 

A secondary objective is to see if Everett Rogers' Diffusion of innovations theory could be applied to the group of advisors in an attempt to identify the innovators, early adopters, and opinion leaders of the group. Hopefuly this could lead to identify advisors who would be more receptive to new technologies in general.

Data Availability
-------------------------
Twitter data through the API is only indexed for approximately 2 weeks. After this time, the search feature will no longer return Tweets. 

A user's history can be pulled based on a given number of tweets. So, for two users who have been on twitter for the same amount of time, more history will be returned for the user with lower tweet frequency. 

In order to collect an appropriate amount of data, The code to search for the advisorTalk tweets was run on March 5, and March 19. There will be one additional collection for the meeting on April 2 before the project is completed.

Data Preparation and Analysis
-------------------------
I will to do the analysis in R, utilizing the twitteR package in order to pull data from twitter, using my personal developer account. I will also utilize dplyr and lubridate to get the data in suitable format. I plan to use ggplot2 for graphing. Final report will be compiled using knitr.

The first part of the analysis will involve calculating some basic statistics for each identified participant. Some examples might be:
* Number of tweets in the Advisor Talk series
* Number of session of Advisor Talk that they participated in
* Rate of tweets outside of the series

The second part of the analysis will involve text mining to understand what the advisors are tweeting about most often.

* Are the accounts used for just business or business and personal use?
* What are the main business topics mentioned? Life Insurance, Retirement, etc.
* Are advisors linking to particular news sources on a regular basis?

If time permits, to address the secondary objective, the third part of the analysis will involve creating a relationship map of participants. The map would be based on retweets and replies. The goal would be to identify some advisors that are more influential on twitter than others?

References
-------------------------



  
Jeff Gentry (2013). twitteR: R based Twitter client. R package version 1.1.7.
  http://CRAN.R-project.org/package=twitteR
  
Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with
  lubridate. Journal of Statistical Software, 40(3), 1-25. URL
  http://www.jstatsoft.org/v40/i03/.

R Core Team (2014). R: A language and environment for statistical computing. R
  Foundation for Statistical Computing, Vienna, Austria. URL
  http://www.R-project.org/.
  
Rogers, Everett M. (1962). Diffusion of Innovations. Glencoe: Free Press. ISBN 0-612-62843-4.

H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
 
Hadley Wickham and Romain Francois (2014). dplyr: dplyr: a grammar of data
  manipulation. R package version 0.1.2. http://CRAN.R-project.org/package=dplyr

Yihui Xie (2013). knitr: A general-purpose package for dynamic report generation
  in R. R package version 1.5.
