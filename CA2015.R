################################
## Project: CA Population Pyramid for 2015
##
##
##  Notes:
##  Census site for variables/tables/ details: https://www.census.gov/data/developers/data-sets.html
##
##
################################

library(acs)
library(reshape2)
library(ggplot2)


# Get the Data
acs.lookup(2010,keyword = "age")
sex.age <- acs.lookup(table.name="Age", endyear=2015)
sex.age <- sex.age[2:49]
ca.pop <- acs.fetch(geography = geo.make(state = "CA"), endyear = 2015, variable = sex.age, col.names = "pretty")

acs.lookup(table.number = "B01001", endyear = 2015)
acs.lookup(table.number = "B01001", endyear = 2015, keyword = "Male")
sex.age.m <- acs.lookup(table.number = "B01001", endyear = 2015, keyword = "Male")[-1]
sex.age.f <- acs.lookup(table.number = "B01001", endyear = 2015, keyword = "Female")[-1]


ca.pop.m <- acs.fetch(geography = geo.make(state = "CA"), endyear = 2015, variable = sex.age.m, col.names = "pretty" )
ca.pop.f <- acs.fetch(geography = geo.make(state = "CA"), endyear = 2015, variable = sex.age.f, col.names = "pretty" )


#Make Dataframe
#ca.pop.df <- rbind(estimate(ca.pop.m),estimate(ca.pop.f))
#dimnames(ca.pop.df)[[2]] <- gsub("Sex by Age: Male: ","", dimnames(ca.pop.df)[[2]])
#dimnames(ca.pop.df)[[1]] <- c("Male","Female")
#as.data.frame(ca.pop.df)
#ca.pop.df$Sex <- c("Male","Female")


ca.f.df <- data.frame(estimate(ca.pop.f))
ca.m.df <- data.frame(estimate(ca.pop.m))
names(ca.f.df) <- gsub("Sex.by.Age..Female..","",names(ca.f.df))
names(ca.m.df) <- gsub("Sex.by.Age..Male..","",names(ca.m.df))
ca.pop.df <- rbind(ca.m.df,ca.f.df)
ca.pop.df[1,] <- -1*ca.pop.df[1,]
ca.pop.df$Sex <- c("Male","Female")

ca.long <- melt(ca.pop.df, id.vars = "Sex", variable.name = "Age", value.name = "Population")



ggplot(ca.long, aes(x = Age, y = Population, fill= Sex)) +
  geom_bar(subset=.(Sex="Female"), stat = "identity") +
  geom_bar(subset =.(Sex="Male"), stat = "identity") +
  coord_flip()

