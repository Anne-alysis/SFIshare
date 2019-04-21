#heroplaying

herostats_sai<-herostats %>% select(HERO:INT.) %>%
        rename(Strength=STR,Agility=AGI,Intelligence=INT,
               StrengthPlus=STR.,AgilityPlus=AGI.,IntelligencePlus=INT.)

herostats_sai_melt<-gather(herostats_sai,stat,value,-HERO,-A)
herostats_sai_melt<-herostats_sai_melt %>% mutate(Type=ifelse(grepl("Plus",stat),"Level Up","Base"),
                stat=gsub("Plus","",stat))

g<-ggplot(herostats_sai_melt,aes(x=value))+geom_histogram(aes(fill=A),col="black")+
        theme_light()+facet_grid(stat~Type,scales="free")+labs(x="Value",y="Frequency",
        title="Distribution of Attributes for DOTA 2 Heroes")+
        guides(fill=guide_legend("Main Attribute"))
g



x<-filter(herostats_sai_melt,Type=="Base")
ind<-dplyr::summarize(group_by(x,HERO),value=max(value))
x<-left_join(ind,x,by=c("HERO","value"))
pdf("HeroStats.pdf")
g
dev.off()


heroteams_join<-left_join(heroteams,herostats,by="lowerhero")
heroteams_join<-heroteams_join %>% select(team,lowerhero,A:INT.) %>% arrange(team)
