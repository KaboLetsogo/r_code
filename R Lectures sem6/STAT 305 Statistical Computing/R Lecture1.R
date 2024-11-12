install.packages('tidyverse')
library(dplyr)
data(starwars)
head(starwars,5)
str(starwars)
starwars$eye_color<-as.factor(starwars$eye_color)
starwars$skin_color<-as.factor(starwars$skin_color)
starwars$hair_color<-as.factor(starwars$skin_color)
str(starwars)

# dplyr pipe takes data from its left and does operation on the right , which can be
# anything  eg filtering or assignment as shown below
starwars%>%filter(gender=='feminine')->star_fem
head(starwars)
starwars
starwars%>%filter(sex=='male',skin_color=='light')

#It was said summarize function preffered for aggregations
starwars%>%filter(eye_color%in%c('blue','brown'))%>%
  arrange(desc(height))%>%summarize(mean_height=mean(height,na.rm = TRUE))

#Mutate Select 
head(starwars,10)
tail(starwars,17)

starwars%>% filter(height>150)
starwars%>% filter(height%in%c(148:170))
select(starwars,num_range(height,100:150))

starwars %>% select(height,eye_color)

starwars %>%
  filter(height >= 100 & height <= 150) 

starwars%>%mutate(bb=eye_color%in%c('blue','brown'))->bb_kk

####### QUIZ
starwars%>%select(name,height,skin_color,eye_color,mass)->df1
head(df1,13)
df1%>%mutate(mh=mass/height)
starwars%>%filter(species=="Droid")
######

####SELECT NOTES
#Selecting columns from name to gender
starwars%>%select(name:gender)
#Selecting columns that end with _color
starwars%>%select(ends_with("_color"))
#Selecting columns except species
starwars%>%select(-species)
#Selecting columns except a list of columns
starwars%>%select(-c(species,mass))
#Renaming columns
starwars%>%rename("skin_type"="skin_color")
#Selecting a list of columns that end with a list of suffixes
starwars%>%select(ends_with(c("_color","_year")))
#Renaming species variable to origin
starwars%>%rename("origin"="species")
#Transmute combines columns together according to ...
starwars%>%transmute(SEY=paste(skin_color,eye_color))
#Bard Gemini says it basically is mutate but only outputs newly created columns

#CREATING DFs to join them later
D1 = data.frame(ID=c(1,2,3,4),
                Name=c("A","B","C","D"),
                Age=c(25,30,35,40))
D2 = data.frame(ID=c(1,2,3,4),
                Occupation=c("Eng","Teach","Doctor","Lawyer"),
                Salary=c(5000,4000,6000,9000))

#Bind rows appends D2 under D1
bind_rows(D1,D2) -> DR
#Bind columns appends the colums
bind_cols(D1,D2) -> DC

head(DR,8)
head(DC)

#union and intersection as in set theory
#inner join looks for values that are the same by observation
D1%>%inner_join(D2,by="ID")
#left_join matches dataframes to look for obs in right that are same as those in left. 
#left is the criterion of matching
D1%>%left_join(D2,by="ID")
#simillarly for the right_join
#Full join is taking everything

df1
starwars%>%select()

