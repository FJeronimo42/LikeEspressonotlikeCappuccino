### JERONIMO & VARASSIN, 2023 ###

#### Script02 Espresso Cappuccino 2023 Data Visualization ####

#### Library ####
pacman::p_load(cowplot, ggcorrplot, gridExtra, tidyverse)

#### MANTEL PLOT FARM LEVEL ####

# Mantel plot production data
data.MTL01 <- read.table('Data/data.MTL01.csv')

View(data.MTL01)

# Mantel plot production
FIG.MTL01 <- ggplot(data=data.MTL01, aes(x=pcoo.ecl, y=ppkg.ecl))+
  geom_point(size=2, alpha=0.75, color='#91091E')+
  labs(x='Coordinates', y='Coffee production')+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'), 
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate('text', x=-0.80, y=4, label='A', size=5, color='#493323', fontface=2)+
  coord_cartesian(ylim=c(0, 4), xlim=c(0,5), clip='off')

FIG.MTL01

png(filename='Figures/FIG.MTL01.tiff', width=16, height=9, units='cm', res=600)
plot(FIG.MTL01)
dev.off()

# Mantel plot landscape metrics
data.MTL02 <- read.table('Data/data.MTL02.csv')

FIG.MTL02 <- ggplot(data=data.MTL02, aes(x=pcoo.ecl, y=pmtr.ecl))+
  geom_point(size=2, alpha=0.75, color='#91091E')+
  labs(x='Coordinates', y='Landscape metrics')+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'), 
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate('text', x=-0.80, y=8, label='B', size=5, color='#493323', fontface=2)+
  coord_cartesian(ylim=c(0, 8), xlim=c(0,5), clip='off')

FIG.MTL02

png(filename='Figures/FIG.MTL02.tiff', width=16, height=9, units='cm', res=600)
plot(FIG.MTL02)
dev.off()

FIG.BOARD1 <- grid.arrange(FIG.MTL01, FIG.MTL02, ncol=2, nrow=1, 
                           widths=c(15, 15), 
                           heights=c(7.5))

png(filename='Figures/FIG.BOARD1.tiff', width=15, height=7.5, units='cm', res=600)
plot(FIG.BOARD1)
dev.off()



#### PEARSON CORRELATION MATRIX - LANDSCAPE METRICS ####

FIG.COR01 <- 
  ggcorrplot(test.COR01, 
             hc.order=T, 
             lab=T, 
             method='square',
             outline.col='white', 
             type='lower',
             ggtheme=ggplot2::theme(axis.title=element_text(size=12, 
                                                            face='bold', 
                                                            color='#2D2424'),
                                    axis.text=element_text(size=10, 
                                                           color='#2D2424'), 
                                    axis.line=element_line(colour='#2D2424'),
                                    plot.background=element_rect(fill='#F4EEED', 
                                                                 color='#F4EEED'),
                                    legend.background=element_rect(fill='#F4EEED',
                                                                   linewidth=0.5),
                                    panel.grid=element_blank(),
                                    legend.title=element_blank(),
                                    panel.background=element_rect(fill='#F4EEED', 
                                                                  color='#F4EEED')), 
             colors=c('#91091E', 'white', '#493323'))
FIG.COR01

png(filename = 'Figures/FIG.COR01.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.COR01)
dev.off()

FIG.COR02 <- 
  ggcorrplot(test.COR02, 
             hc.order=T, 
             lab=T,
             method='square',
             outline.col='white', 
             type='lower',
             ggtheme=ggplot2::theme(axis.title=element_text(size=12, 
                                                            face='bold', 
                                                            color='#2D2424'),
                                    axis.text=element_text(size=10, 
                                                           color='#2D2424'), 
                                    axis.line=element_line(colour='#2D2424'),
                                    plot.background=element_rect(fill='#F4EEED', 
                                                                 color='#F4EEED'),
                                    legend.background=element_rect(fill='#F4EEED',
                                                                   linewidth=0.5),
                                    panel.grid=element_blank(),
                                    legend.title=element_blank(),
                                    panel.background=element_rect(fill='#F4EEED', 
                                                                  color='#F4EEED')), 
             colors=c('#91091E', 'white', '#493323'))
FIG.COR02

png(filename='Figures/FIG.COR02.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.COR02)
dev.off()



#### GLM FIGURES ####
### MODELS PLOTS
# Univariate models

## Remove outliers
data.PLOT1 <- data.GLM01

View(data.PLOT1)

data.PLOT2 <- data.GLM01 %>% 
  slice(-14,-17)

View(data.PLOT2)

write.table(data.PLOT2, 'Data/DataPlotGLMFarm.csv')

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

# M2
summary(M2)
FIG.M2 <- ggplot(data=data.PLOT1, aes(x=DNST, y=PROD))+
  geom_point(size=3, alpha=0.75, color='#91091E')+
  geom_smooth(method=lm, linetype=1, color='#493323', alpha=0.125,
              fill='#493323')+
  labs(x='Forest patch density', y='Coffee production (kg ha^-1)')+
  ylim(0, 3500)+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate(size=2.5, 'text', x=2.133, y=3250, label='Y = 914.80 + 338.90 * X | t = 2.935, p = 0.007 **',
           color='#493323')

FIG.M2

png(filename='Figures/FIG.M2.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.M2)
dev.off()

#M3
summary(M3)
FIG.M3 <- ggplot(data=data.PLOT1, aes(x=ISLT, y=PROD))+
  geom_point(size=3, alpha=0.75, color='#91091E')+
  geom_smooth(method=lm, linetype=1, color='#493323', alpha=0.125,
              fill='#493323')+
  labs(x='Forest patch isolation', y='Coffee production (kg ha^-1)')+
  ylim(0, 3500)+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate(size=2.5, 'text', x=238.629, y=3250, label='Y = 2575.062 - 5.078 * X | t = -3.886, p < 0.001 ***',
           color='#493323')

FIG.M3

png(filename='Figures/FIG.M3.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.M3)
dev.off()

#M4
summary(M4)
FIG.M4 <- ggplot(data=data.PLOT1, aes(x=PRXM, y=PROD))+
  geom_point(size=3, alpha=0.75, color='#91091E')+
  labs(x='Forest proximity index', y='Coffee production (kg ha^-1)')+
  ylim(0, 3500)+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))

FIG.M4

png(filename='Figures/FIG.M4.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.M4)
dev.off()

#M5
summary(M5)
FIG.M5 <- ggplot(data=data.PLOT1, aes(x=CNNC, y=PROD))+
  geom_point(size=3, alpha=0.75, color='#91091E')+
  geom_smooth(method=lm, linetype=2, color='#493323', alpha=0.125,
              fill='#493323')+
  labs(x='Forest patch connectance', y='Coffee production (kg ha^-1)')+
  ylim(0, 3500)+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate(size=2.5, 'text', x=25.544, y=3250, label='Y = -424.48 + 88.53 * X | t = 1.956, p = 0.063 .',
           color='#493323')

FIG.M5

png(filename='Figures/FIG.M5.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.M5)
dev.off()

#M6
summary(M6)
FIG.M6 <- ggplot(data=data.PLOT1, aes(x=SHEI, y=PROD))+
  geom_point(size=3, alpha=0.75, color='#91091E')+
  geom_smooth(method=lm, linetype=1, color='#493323', alpha=0.125,
              fill='#493323')+
  labs(x='Landscape diversity', y='Coffee production (kg ha^-1)')+
  ylim(0, 3500)+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate(size=2.5, 'text', x=0.5959, y=3250, label='Y = 642.1 + 1718.3 * X | t = 2.629, p = 0.015 *',
           color='#493323')

FIG.M6

png(filename='Figures/FIG.M6.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.M6)
dev.off()

#M7
summary(M7)
FIG.M7 <- ggplot(data=data.PLOT2, aes(x=IIND, y=PROD))+
  geom_point(size=3, alpha=0.75, color='#91091E')+
  geom_smooth(method=lm, linetype=1, color='#493323', alpha=0.125,
              fill='#493323')+
  labs(x='Landscape intensity index', y='Coffee production (kg ha^-1)')+
  ylim(0, 3500)+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  annotate(size=2.5, 'text', x=7.946, y=3250, label='Y = 1946.904 - 14.876 * X | t = -3.913, p < 0.001 ***',
           color='#493323')

FIG.M7

png(filename='Figures/FIG.M7.tiff', width=32, height=18, units='cm', res=600)
plot(FIG.M7)
dev.off()

# Delta figure for landscape intensity
data.delta <- data.prop %>%
  mutate(delta=ProdArea_kg-1932) %>%
  mutate(variation=cut(delta, breaks=c(-Inf, 0, Inf), 
                       labels=c('lower','higher'))) %>% 
  slice(-14,-17)

View(data.delta)

FIG.Delta <- ggplot(data=data.delta, aes(x=II_2KM, y=delta, group=variation))+
  geom_point(size=3, alpha=0.75, aes(shape=variation, fill=variation))+
  scale_shape_manual(values=c(25,24))+
  geom_hline(yintercept=417.6798)+
  geom_vline(xintercept=9.783090909)+
  labs(x='Landscape intensity index', y='Coffee production (\u0394)')+
  scale_color_manual(values=c('#91091E', '#493323'))+
  scale_fill_manual(values=c('#91091E', '#493323'))+
  theme(legend.position='none',
        axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))


FIG.Delta

png(filename='FIG.Delta.tiff', width=16, height=9, units='cm', res=600)
plot(FIG.Delta)
dev.off() 

median(data.delta$II_2KM)

# FIGBOARD PAPPER
FIG.BOARD.2 <- plot_grid(FIG.M7, FIG.Delta, FIG.M2, FIG.M6,
                         ncol=2, nrow=2,
                         rel_widths = 1,
                         rel_heights = 1,
                         labels = c('A', 'B', 'C', 'D'),
                         label_colour = '#493323',
                         label_size = 12)

FIG.BOARD.2

png(filename='Figures/FIG.BOARD.2.tiff', width=16, height=16, units='cm', res=300)
plot(FIG.BOARD.2)
dev.off()














# Board 2
Fig.Board2 <- grid.arrange(FIG.M7, FIG.Delta, FIG.M2, FIG.M6, 
                           ncol=2, nrow=2, widths=c(15, 15), heights=c(7.5, 7.5))

png(filename='Figures/FIG.BOARD2.tiff', width=15, height=15, units='cm', res=600)
plot(Fig.Board2)
dev.off()

Fig.Board3 <- grid.arrange(FIG.M3, FIG.M1, FIG.M5, FIG.M4,
                           ncol=2, nrow=2, widths=c(15, 15), heights=c(7.5, 7.5))

png(filename='Figures/FIG.BOARD3.tiff', width=15, height=15, units='cm', res=600)
plot(Fig.Board3)
dev.off()


ttest.data <- read.csv('Data/TtestCP.csv') 

FIG.TTS <- ggplot(ttest.data %>% 
                  mutate(Level = factor(Level)) %>% 
                  mutate(Level = fct_relevel(Level, c('Farms', 
                                                      'Municipalities',
                                                      'Brazil'))) %>%
                  arrange(Level), aes(y=mean, x=Level))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.25, size=1, color='#91091E')+
  geom_point(size=3, color = '#493323')+
  theme(axis.title=element_text(size=12, face='bold', color='#493323'),
        axis.text=element_text(size=10, color='#493323'),
        axis.line=element_line(colour='#493323'),
        plot.background=element_rect(fill='#F4EEED', color='#F4EEED'),
        panel.grid=element_blank(),
        panel.background=element_rect(fill='#F4EEED', color='#F4EEED'))+
  labs(x='Level', y='Coffee production (kg ha^-1)')

FIG.TTS

png(filename='Figures/FIGTTS.tiff', width=9, height=9, units='cm', res=600)
plot(FIG.TTS)
dev.off()
