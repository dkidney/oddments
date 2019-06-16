\dontrun{

library(smbinning)
library(magrittr)
library(dplyr)

# data for analysis
data(chileancredit)
chileancredit %<>%  
    tbl_df %>% 
    select(FlagGB, IncomeLevel, Bal01, MaxDqBin01, FlagSample) %>%
    mutate(FlagGB = as.integer(1 - FlagGB)) %>%
    filter(complete.cases(.)) %T>% 
    print

# method 1: woe define and apply functions ----

# define the transformation on the training set
y.train = chileancredit %>% filter(FlagSample == 1) %>% extract2('FlagGB')
x.train = chileancredit %>% filter(FlagSample == 1) %>% select(-FlagGB, -FlagSample)
definition = x_transform_woe_define(x = x.train, y = y.train)
definition$lookup

# apply the transformation to the test set
x.test = chileancredit %>% filter(FlagSample == 0) %>% select(-FlagGB, -FlagSample) %T>% print
x.test.trans = x_transform_woe_apply(x = x.test, lookup = definition$lookup)
x.test.trans %>% head

# method 2: generic define and apply functions ----

# transformation instructions
x.transform = x_transform_woe(y = chileancredit$FlagGB)

# define the transformation on the training set
x = chileancredit %>% select(-FlagGB, -FlagSample)
train.index = chileancredit$FlagSample == 1
definition = x_transform_define(x, x.transform, train.index)
definition$lookup

# apply the transformation to the test set
test.index = chileancredit$FlagSample == 0
x.test.trans = x_transform_apply(x, definition, test.index)
x.test.trans %>% head
}









