\dontrun{

suppressPackageStartupMessages({
    library(CImodelling)
    library(ggplot2)
    library(dplyr)
})
    
# example data for analysis
data(kaggle)
kaggle %<>% 
    filter(partition == 'build') %>% 
    filter(complete.cases(.)) %>% 
    select(SeriousDlqin2yrs, MonthlyIncome, age) %>% 
    filter(!is.na(SeriousDlqin2yrs)) %>% 
    mutate(age = cut_number(age, 5, dig.lab = 10)) %>%
    mutate(MonthlyIncome = cut_number(MonthlyIncome, 5, dig.lab = 10))

# x as data.frame
x = kaggle %>% select(-SeriousDlqin2yrs)
y = kaggle[['SeriousDlqin2yrs']]

# characteristic table
characteristic_table(y, x)
characteristic_table(y, x, min.total = 19000)
characteristic_table(y, x, min.bads = 1000)
characteristic_table(y, x, woe.min.range = 1)
characteristic_table(y, x, woe.min.level = 0.02)
characteristic_table(y, x, woe.ignore.level = "[0,3000]")

# shorthand functions
woe(y, x)
iv(y, x)

# x as vector 
x = kaggle[['MonthlyIncome']]
y = kaggle[['SeriousDlqin2yrs']]

characteristic_table(y, x)
woe(y, x)
iv(y, x)

# compare with another package
# - this function sets undefined woe to zero rather than using an adjustment term 
library(Information)
create_infotables(kaggle, y = "SeriousDlqin2yrs")[['Tables']]
}
