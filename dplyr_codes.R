#SOURCE: http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

##Important Functions
#select()	select columns
#filter()	filter rows
#arrange()	re-order or arrange rows
#mutate()	create new columns
#summarise()	summarise values
#group_by()	allows for group operations in the “split-apply-combine” concept

filter(msleep, sleep_total >= 16)

filter(msleep, sleep_total >= 16, bodywt >= 1)

filter(msleep, order %in% c("Perissodactyla", "Primates"))

select(msleep, name, sleep_total)

msleep %>% 
    select(name, sleep_total) %>% 
    head
    
    
msleep %>% arrange(order) %>% head


msleep %>% 
    mutate(rem_proportion = sleep_rem / sleep_total) %>%
    head
    
    
msleep %>% 
    summarise(avg_sleep = mean(sleep_total))
    
msleep %>% 
    group_by(order) %>%
    summarise(avg_sleep = mean(sleep_total), 
              min_sleep = min(sleep_total), 
              max_sleep = max(sleep_total),
              total = n())
              
