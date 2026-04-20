library(extrafont)
library(tidyverse)
library(ggtext)

data <- read.csv('talls.csv',header=T)
data$long <- -data$long

lab <- c('Great Pyramid of Giza','Anu Ziggurat Uruk','Tower of Jericho','Gobekli Tepe')
pos <- c(50,-50,-50,-50)
yea <- as.factor(c(-2560,-4000,-8000,-9500))

d2 <- tibble(lab,pos,yea)

labb <- c('← 3811 years','← 1350 years','← 4000 years','← 1500 years')
poss <- c(-300,-300,-300,-300)
yeaa <- as.factor(c(-2560,-4000,-8000,-9500))

d22 <- tibble(labb,poss,yeaa)

g1 <- ggplot(data) +
  labs(title='**Touching the sky: what record heights tell about us**',
       subtitle='Record time and height of freestanding structures. Source: Wikipedia, 2026') +
  
  geom_vline(xintercept=0,color='grey20',linetype='dotted') +
  geom_segment(aes(x=long,xend=ht,y=as.factor(year),yend=as.factor(year),color=cont),
              linewidth=4.5,alpha=0.2) +
  geom_point(aes(x=long,y=as.factor(year),color=cont),size=4) +
  geom_point(aes(x=ht,y=as.factor(year),color=cont),size=4,show.legend=FALSE) +
  
  geom_text(data=d2,aes(label = lab,x=pos, y =yea),
            color = "#4a4e4d",size = 3,family = "Segoe UI Semibold") +
  geom_text(aes(label = build, x=(long+ht)/2, y = as.factor(year)),
            color = "#4a4e4d",size = 3,family = "Segoe UI Semibold") +
  geom_text(data=d22,aes(label = labb,x=poss, y =yeaa ),
            color = "#4a4e4d",size = 3,hjust='left',family = "Segoe UI Semibold") +
  geom_text(aes(label = paste(-long, 'years'),x=long-10, y =as.factor(year) ),
            color = "#4a4e4d",size = 3,hjust='right',family = "Segoe UI Semibold") +
  geom_text(aes(label = paste(ht, 'm'),x=ht+10, y =as.factor(year) ),
            color = "#4a4e4d",size = 3,hjust='left',family = "Segoe UI Semibold") +
  
  coord_cartesian(xlim=c(-250,850)) +
  scale_x_continuous(breaks = c(-300,0,300,600,900),labels = c('300','0','300','600','900')) +
  ylab('Construction year') +
  xlab('                               Record holding time (years) ←      → Height (m)') +
  
  theme_bw() +
  theme(
    legend.position='top',
    legend.title=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(linewidth=0.5),
    axis.title.x = element_text(hjust = 0),
    text=element_text(family="Segoe UI Semibold",size=12),
    plot.title = element_markdown(size=20),
    plot.subtitle = element_text(size=10)
    )

ggsave('talls.png',dpi=300,width=1536/96,height=818/96)