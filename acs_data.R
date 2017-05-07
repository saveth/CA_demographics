


######################
# Extract ACS DATA
######################

# CA State only
sacs <- function(year){
    sacs_single <- function(year_sing){
      sex.age.m <- acs.lookup(table.number = "B01001", endyear = year_sing, keyword = "Male")[-1]
      sex.age.f <- acs.lookup(table.number = "B01001", endyear = year_sing, keyword = "Female")[-1]
      ca.pop.m <- acs.fetch(geography = geo.make(state = "CA"), endyear = year_sing, variable = sex.age.m, col.names = "pretty" )
      ca.pop.f <- acs.fetch(geography = geo.make(state = "CA"), endyear = year_sing, variable = sex.age.f, col.names = "pretty" )
      
      ca.f.df <- data.frame(estimate(ca.pop.f))
      ca.m.df <- data.frame(estimate(ca.pop.m))
      names(ca.f.df) <- gsub("Sex.by.Age..Female..","",names(ca.f.df))
      names(ca.m.df) <- gsub("Sex.by.Age..Male..","",names(ca.m.df))
      ca.pop.df <- rbind(ca.m.df,ca.f.df)
      ca.pop.df$Sex <- c("Male","Female")
      ca.pop.df$Year <- year_sing
      
      ca.long <- melt(ca.pop.df, id.vars = c("Year","Sex"), variable.name = "Age", value.name = "Population")
      ca.long <- ca.long %>%
        mutate(Age = revalue(Age, c("15.to.17.years" = "15.to.19.years",
                                    "18.and.19.years" = "15.to.19.years",
                                    "20.years"="20.to.24.years",
                                    "21.years"="20.to.24.years",
                                    "22.to.24.years" ="20.to.24.years", 
                                    "60.and.61.years"="60.to.64.years", 
                                    "62.to.64.years"="60.to.64.years",
                                    "65.and.66.years"="65.to.69.years",
                                    "67.to.69.years"="65.to.69.years"))) %>%
        group_by(Year,Sex, Age) %>%
        summarise(Population =sum(Population))
      
      ca.long$Age <- gsub(".years","",ca.long$Age)
      ca.long$Age <- gsub(".to.","-",ca.long$Age)
      ca.long$Age[ca.long$Age=="Under.5"] <- "0-4"
      ca.long$Age[ca.long$Age=="85.and.over"] <- "85+"
      
      return(ca.long)
    }
    
  sacslist <- lapply(year, function(x) sacs_single(year_sing=x))
  acs.df <- do.call(rbind.data.frame, sacslist)
  return(acs.df)
}


#CA ALL County level data
cacs <- function(year){
  cacs_single <- function(year_sing){
    sex.age.m <- acs.lookup(table.number = "B01001", endyear = year_sing, keyword = "Male")[-1]
    sex.age.f <- acs.lookup(table.number = "B01001", endyear = year_sing, keyword = "Female")[-1]
    ca.pop.m <- acs.fetch(geography = geo.make(state = "CA", county = "*"), endyear = year_sing, variable = sex.age.m, col.names = "pretty" )
    ca.pop.f <- acs.fetch(geography = geo.make(state = "CA", county = "*"), endyear = year_sing, variable = sex.age.f, col.names = "pretty" )
    
    ca.f.df <- data.frame(estimate(ca.pop.f))
    ca.m.df <- data.frame(estimate(ca.pop.m))
    names(ca.f.df) <- gsub("Sex.by.Age..Female..","",names(ca.f.df))
    names(ca.m.df) <- gsub("Sex.by.Age..Male..","",names(ca.m.df))
    ca.f.df$County <- gsub(" County, California","",row.names(ca.f.df))
    ca.m.df$County <- gsub(" County, California","",row.names(ca.m.df))
    ca.f.df$Sex <- "Female"
    ca.m.df$Sex <- "Male"
    ca.pop.df <- rbind(ca.m.df,ca.f.df)
    ca.pop.df$Year <- year_sing
    
    ca.long <- melt(ca.pop.df, id.vars = c("Year","County","Sex"), variable.name = "Age", value.name = "Population")
    ca.long <- ca.long %>%
      mutate(Age = revalue(Age, c("15.to.17.years" = "15.to.19.years",
                                  "18.and.19.years" = "15.to.19.years",
                                  "20.years"="20.to.24.years",
                                  "21.years"="20.to.24.years",
                                  "22.to.24.years" ="20.to.24.years", 
                                  "60.and.61.years"="60.to.64.years", 
                                  "62.to.64.years"="60.to.64.years",
                                  "65.and.66.years"="65.to.69.years",
                                  "67.to.69.years"="65.to.69.years"))) %>%
      group_by(Year,County,Sex, Age) %>%
      summarise(Population =sum(Population))
    
    ca.long$Age <- gsub(".years","",ca.long$Age)
    ca.long$Age <- gsub(".to.","-",ca.long$Age)
    ca.long$Age[ca.long$Age=="Under.5"] <- "0-4"
    ca.long$Age[ca.long$Age=="85.and.over"] <- "85+"
    
    return(ca.long)
  }
  
  cacslist <- lapply(year, function(x) cacs_single(year_sing=x))
  acs.df <- do.call(rbind.data.frame, cacslist)
  return(acs.df)
}



# Add in proportion and Sex Ratio into Dataframe : State
sdata <- function(df){
  df <- df %>%
    group_by(Year, Sex) %>%
    mutate(Proportion = Population/sum(Population)) %>%
    group_by(Year, Age) %>%
    mutate(SexRatio = Population[Sex=="Male"]/Population[Sex=="Female"])
}


# Add in proportion and Sex Ratio into Dataframe : county
sdata <- function(df){
  df <- df %>%
    group_by(Year, Sex) %>%
    mutate(Proportion = Population/sum(Population)) %>%
    group_by(Year, Age) %>%
    mutate(SexRatio = Population[Sex=="Male"]/Population[Sex=="Female"])
}

