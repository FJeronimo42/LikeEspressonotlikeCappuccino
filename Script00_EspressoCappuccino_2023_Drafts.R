# M1
FIG.M1 <- ggplot(data=data.PLOT1, 
                 aes(x=CVRG, y=PROD))+
  geom_point(size=3, 
             alpha=0.75, 
             color='#91091E')+
  geom_smooth(method='glm', 
              method.args=list(family=Gamma(link=inverse)),
              linetype=2, 
              color='#493323', 
              alpha=0.125,
              fill='#493323')+
  labs(x='Forest cover percentage', 
       y='Coffee production (kg ha^-1)')+
  ylim(0, 4750)+
  xlim(0,50)+
  theme(axis.title=element_text(size=12, 
                                face='bold', 
                                color='#493323'),
        axis.text=element_text(size=10, 
                               color='#493323'),
        axis.line=element_line(color='#493323'),
        plot.background=element_rect(fill='#F4EEED', 
                                     color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', 
                                      color='#F4EEED'))
FIG.M1

png(filename='Figures/FIG.M1.tiff', width=15, height=7.5, units='cm', res=600)
plot(FIG.M1)
dev.off() 