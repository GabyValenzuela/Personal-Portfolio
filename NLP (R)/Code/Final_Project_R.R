install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)  


library(readxl)
case <- read_excel("/Users/gabyvalenzuela/Documents/31. Personal Portfolio/NLP (R)/Datasets/Case_group.xls")


### NA data = cleaning dataset
for ( col in 1:ncol(case))
{
  print(sum(is.na(case[,col])))
}
case$`Bid Strategy` ### missing values

case <- select(case, -`Bid Strategy`) ###drop column with NA varibales


#renaming columns for summary table
colnames(case)<- c('pub_id', 'pub', 'keyword_id', 'keyword', 'match_type', 'campaign',
                   'keyword_grop', 'category', 'keyword_type', 'status', 'search_eng_bid', 'clicks',
                   'clicks_charges', 'av_cost_click', 'impressions', 'engine_click',
                   'avg_pos', 'trans_conv', 'total_cost_trans', 'amount', 'total_cost',
                   'booking', 'c')



############GENERAL SUMMARY#########################################################
summary_of_engine <- case %>%
  select(pub, clicks, booking, amount,clicks_charges,av_cost_click, impressions, total_cost,trans_conv) %>%
  group_by(pub) %>%
summarise(clicks_new = sum(clicks), click_charges_new= sum(clicks_charges), av_per_click = mean(av_cost_click), booking = sum(booking), sum_of_total_revenue = sum(amount), impressions_new = sum(impressions),
                            sum_total_cost = sum(total_cost))

summary_of_engine$sum_of_net_revenue<- summary_of_engine$sum_of_total_revenue-summary_of_engine$sum_total_cost
summary_of_engine$roa<- summary_of_engine$sum_of_total_revenue/summary_of_engine$sum_total_cost
summary_of_engine$ctr<- summary_of_engine$clicks_new/summary_of_engine$impressions_new
summary_of_engine$tcr<- summary_of_engine$booking/summary_of_engine$clicks_new
summary_of_engine$prob_of_booking<- summary_of_engine$ctr*summary_of_engine$tcr*100
summary_of_engine$cost_per_booking<- summary_of_engine$click_charges_new/summary_of_engine$booking
summary_of_engine$cost_per_click<- summary_of_engine$click_charges_new/summary_of_engine$clicks_new
library(ggplot2)
summary_of_engine %>% 
  ggplot(aes(x=prob_of_booking,y=cost_per_booking, size=sum_of_net_revenue, color=pub, label=pub)) +
  geom_point(alpha=0.6)+
  geom_text(aes(label = pub), hjust = 0, vjust = 0)+
  scale_size_continuous(range = c(1, 20))+
  labs(x="booking", y="sum_total_cost", size="sum_of_net_revenue", color="pub")





############MATCH TYPE SUMMARY#########################################################

### summary table of match_type
summary_of_match_type <- case %>%
  select(pub, clicks, booking, amount,clicks_charges,av_cost_click, impressions, total_cost,trans_conv, match_type) %>%
  group_by(match_type, pub) %>%
  summarise(clicks_new = sum(clicks), click_charges_new= sum(clicks_charges), av_per_click = mean(av_cost_click), booking = sum(booking), sum_of_total_revenue = sum(amount), impressions_new = sum(impressions),
            sum_total_cost = sum(total_cost))


summary_of_match_type$sum_of_net_revenue<- summary_of_match_type$sum_of_total_revenue-summary_of_match_type$sum_total_cost
summary_of_match_type$roa<- summary_of_match_type$sum_of_total_revenue/summary_of_match_type$sum_total_cost
summary_of_match_type$ctr<- summary_of_match_type$clicks_new/summary_of_match_type$impressions_new
summary_of_match_type$tcr<- summary_of_match_type$booking/summary_of_match_type$clicks_new
summary_of_match_type$prob_of_booking<- summary_of_match_type$ctr*summary_of_match_type$tcr*100
summary_of_match_type$cost_per_booking<- summary_of_match_type$click_charges_new/summary_of_match_type$booking
summary_of_match_type$cost_per_click<- summary_of_match_type$click_charges_new/summary_of_match_type$clicks_new

summary_of_match_type <-arrange(summary_of_match_type, sum_of_net_revenue)

library(ggplot2)
ggplot(summary_of_match_type,aes(x=match_type,y=sum_of_net_revenue,fill=factor(pub)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Gender",
                      breaks=c('Google - Global', 'Google - US', 'Overture - US', 
                               'Yahoo - US', 'Overture - Global',
                               'MSN - Global', 'MSN - US'),
                      labels=c('Google - Global', 'Google - US', 'Overture - US', 
                               'Yahoo - US', 'Overture - Global',
                              'MSN - Global', 'MSN - US'))+
  xlab("Engine")+ylab("Sum_of_net_revenue")




############KEY WORDS SUMMARY #########################################################
summary_of_words <- case %>%
  select(keyword_grop, pub, clicks, booking, amount,clicks_charges,av_cost_click, impressions, total_cost,trans_conv) %>%
  group_by(keyword_grop) %>%
  summarise(clicks_new = sum(clicks), click_charges_new= sum(clicks_charges), av_per_click = mean(av_cost_click), booking = sum(booking), sum_of_total_revenue = sum(amount), impressions_new = sum(impressions),
            sum_total_cost = sum(total_cost))

summary_of_words$sum_of_net_revenue<- summary_of_words$sum_of_total_revenue-summary_of_words$sum_total_cost
summary_of_words$roa<- summary_of_words$sum_of_total_revenue/summary_of_words$sum_total_cost
summary_of_words$ctr<- summary_of_words$clicks_new/summary_of_words$impressions_new
summary_of_words$tcr<- summary_of_words$booking/summary_of_words$clicks_new
summary_of_words$prob_of_booking<- summary_of_words$ctr*summary_of_words$tcr*100
summary_of_words$cost_per_booking<- summary_of_words$click_charges_new/summary_of_words$booking
summary_of_words$cost_per_click<- summary_of_words$click_charges_new/summary_of_words$clicks_new

summary_of_words <-arrange(summary_of_words, sum_of_net_revenue)

summary_of_words$sum_of_net_revenue<- as.numeric(summary_of_words$sum_of_net_revenue)


ggplot(summary_of_words, 
       aes(x=sum_of_net_revenue, 
           y=reorder(keyword_grop, sum_of_net_revenue))) +
  geom_point(color="brown", 
             size = 2) +
  geom_segment(aes(x = -3208723, 
                   xend = sum_of_net_revenue, 
                   y = reorder(keyword_grop, sum_of_net_revenue), 
                   yend = reorder(keyword_grop, sum_of_net_revenue)),
               color = "lightgrey") +
  labs (x = "sum_of_net_revenue",
        y = "",
        title = "sum_of_net_revenue",
        subtitle = "sum_of_net_revenue$key word category") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


####
high_rate_keyword<- summary_of_words[ which(summary_of_words$sum_of_net_revenue >0), ]


ggplot(high_rate_keyword, aes(x=keyword_grop, y=roa, fill = keyword_grop)) + geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral")
### consider do not to use this keyword groups as it is not lead to booking


summary_of_words$sum_of_net_revenue<- as.numeric(summary_of_words$sum_of_net_revenue)
ggplot(summary_of_words, 
       aes(x=prob_of_booking, 
           y=reorder(keyword_grop, prob_of_booking))) +
  geom_point(color="blue", 
             size = 2) +
  geom_segment(aes(x = 5, 
                   xend = prob_of_booking, 
                   y = reorder(keyword_grop, prob_of_booking), 
                   yend = reorder(keyword_grop, prob_of_booking)),
               color = "lightgrey") +
  labs (x = "prob_of_booking",
        y = "",
        title = "Booking",
        subtitle = "prob_of_booking$key word category") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




#######################OPTIMISATION OPTION 1##############################################
##########################################################################################
###1 step = evaluation bookings which are = 0
###2 step = CTR evaluation = if less than 20 = we can remove from our dataset
### 3 step = calculating our savings

purpose_subset_key <- case[ which(case$booking == "0"), ]
count(purpose_subset_key) ### 4142 observations  - no bookings

purpose_subset_key$ctr <- purpose_subset_key$clicks/ purpose_subset_key$impressions*100

purpose_subset_key_to_remove <- case[ which(purpose_subset_key$ctr <20), ]
count(purpose_subset_key_to_remove) ### 3530 observations do not show high CTR
##(number of searches (impression) converted to traffic(clicks) rate)


case_removed <- case
case_removed$ctr <- case_removed$clicks/ case_removed$impressions*100
case_removed <- case_removed[- which(case_removed$booking == "0" & case_removed$ctr<20), ]


summary_of_case_removed <- case_removed %>%
  select(pub, clicks, booking, amount,clicks_charges,av_cost_click, impressions, total_cost,trans_conv) %>%
  group_by(pub) %>%
  summarise(clicks_new = sum(clicks), click_charges_new= sum(clicks_charges), av_per_click = mean(av_cost_click), booking = sum(booking), sum_of_total_revenue = sum(amount), impressions_new = sum(impressions),
            sum_total_cost = sum(total_cost))
summary_of_case_removed$sum_of_net_revenue<- summary_of_case_removed$sum_of_total_revenue-summary_of_case_removed$sum_total_cost

savings_option_1 <- sum(summary_of_engine$sum_of_net_revenue) - sum(summary_of_case_removed$sum_of_net_revenue)
savings_option_1 ###amount of savings if we go to option 1 




#######################OPTIMISATION OPTION 2##############################################
##########################################################################################

foo <- function(x){ 
x <- as.character(x) 
sapply(strsplit(x, " "), function(s) sum(nchar(s)))
} 

case$c<-foo(case$keyword)

 ### based on this we can conclude as well that the most expensive length is between 7 and 20

ggplot(case, aes(x=c, y=booking, colour=c)) + geom_point() + 
  scale_colour_gradientn(colours=rainbow(4))

ggplot(case, aes(x=c, y=total_cost, colour=c)) + geom_point() + 
  scale_colour_gradientn(colours=rainbow(4))
### we can conclude that it is better to keep length count between  7 - 20 letters

############LENGTH OF KEY WORD SUMMARY#########################################################
summary_of_c <- case %>%
  select(c, keyword_grop, pub, clicks, booking, amount,clicks_charges,av_cost_click, impressions, total_cost,trans_conv) %>%
  group_by(c) %>%
  summarise(clicks_new = sum(clicks), click_charges_new= sum(clicks_charges), av_per_click = mean(av_cost_click), booking = sum(booking), sum_of_total_revenue = sum(amount), impressions_new = sum(impressions),
            sum_total_cost = sum(total_cost))

summary_of_c$sum_of_net_revenue<- summary_of_c$sum_of_total_revenue-summary_of_c$sum_total_cost
summary_of_c$roa<- summary_of_c$sum_of_total_revenue/summary_of_c$sum_total_cost
summary_of_c$ctr<- summary_of_c$clicks_new/summary_of_c$impressions_new
summary_of_c$tcr<- summary_of_c$booking/summary_of_c$clicks_new
summary_of_c$prob_of_booking<- summary_of_c$ctr*summary_of_c$tcr*100
summary_of_c$cost_per_booking<- summary_of_c$click_charges_new/summary_of_c$booking
summary_of_c$cost_per_click<- summary_of_c$click_charges_new/summary_of_c$clicks_new

summary_of_c 


kayak<- c('kayak', 2839, '0', '0', '208',233694 , '0', 3568, 230127, '0', '0', 0,0,0,0,0)

summary_of_engine<- rbind(summary_of_engine, kayak)

str(summary_of_engine)



install.packages("RColorBrewer")
library(RColorBrewer)
ggplot(summary_of_engine, aes(x=pub, y=roa, fill = pub)) + geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral")

ggplot(summary_of_engine, aes(x=pub, y=prob_of_booking, fill = pub)) + geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set3")

ggplot(summary_of_engine, aes(x=pub, y=sum_of_net_revenue, fill = pub)) + geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set3")
