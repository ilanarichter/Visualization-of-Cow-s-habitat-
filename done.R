
library(reshape)

library(ggplot2)

Cp1<-read.csv("C:\\Users\\ilana\\Desktop\\vizILA\\allCows.csv")
LONG <- Cp1


LONG$catFood <- as.factor(LONG$catFood)
LONG$cow <- as.factor(LONG$cow)
# create grid of all cows
t <-ggplot(LONG, aes(x = LONG$x , y = LONG$y, color = catFood, size = LONG$logDuration )) + geom_point(alpha = 0.3, position = "jitter", xlim=c(0,30)) + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + facet_wrap( ~ cow, ncol=8)+ scale_fill_continuous(guide = guide_legend(title = "V")) + scale_color_brewer( name = "Amount of food/Day",labels = c("20-28 kg","28-36 kg", "36-44 kg", "44-52 kg","52-60 kg") , palette="RdYlBu")+ guides(size=FALSE, color = guide_legend(reverse=TRUE) )
t

do1 <- LONG [ LONG$cow == 2927, ]
pCow1 <- ggplot(do1 , aes(x = do1$x , y = do1$y, size = do1$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30), colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do2 <- LONG [ LONG$cow == 3010, ]
pCow2 <- ggplot(do2 , aes(x = do2$x , y = do2$y, size = do2$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do3 <- LONG [ LONG$cow == 3024, ]
pCow3 <- ggplot(do3 , aes(x = do3$x , y = do3$y, size = do3$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do4 <- LONG [ LONG$cow == 3049, ]
pCow4 <- ggplot(do4 , aes(x = do4$x , y = do4$y, size = do4$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do5 <- LONG [ LONG$cow == 3077, ]
pCow5 <- ggplot(do5 , aes(x = do5$x , y = do5$y, size = do5$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do6 <- LONG [ LONG$cow == 3107, ]
pCow6 <- ggplot(do6 , aes(x = do6$x , y = do6$y, size = do6$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do7 <- LONG [ LONG$cow == 3188, ]
pCow7 <- ggplot(do7 , aes(x = do7$x , y = do7$y, size = do7$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do8 <- LONG [ LONG$cow == 3194, ]
pCow8 <- ggplot(do8 , aes(x = do8$x , y = do8$y, size = do8$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do9 <- LONG [ LONG$cow == 3203, ]
pCow9 <- ggplot(do9 , aes(x = do9$x , y = do9$y, size = do9$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do10 <- LONG [ LONG$cow == 3227, ]
pCow10 <- ggplot(do10 , aes(x = do10$x , y = do10$y, size = do10$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do11 <- LONG [ LONG$cow == 3242, ]
pCow11 <- ggplot(do11 , aes(x = do11$x , y = do11$y, size = do11$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do12 <- LONG [ LONG$cow == 3253, ]
pCow12 <- ggplot(do12 , aes(x = do12$x , y = do12$y, size = do12$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do13 <- LONG [ LONG$cow == 3297, ]
pCow13 <- ggplot(do13 , aes(x = do13$x , y = do13$y, size = do13$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do14 <- LONG [ LONG$cow == 3300, ]
pCow14 <- ggplot(do14 , aes(x = do14$x , y = do14$y, size = do14$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do15 <- LONG [ LONG$cow == 3302, ]
pCow15 <- ggplot(do15 , aes(x = do15$x , y = do15$y, size = do15$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do16 <- LONG [ LONG$cow == 3307, ]
pCow16 <- ggplot(do16 , aes(x = do16$x , y = do16$y, size = do16$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do17 <- LONG [ LONG$cow == 3341, ]
pCow17 <- ggplot(do17 , aes(x = do17$x , y = do17$y, size = do17$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do18 <- LONG [ LONG$cow == 3354, ]
pCow18 <- ggplot(do18 , aes(x = do18$x , y = do18$y, size = do18$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do19 <- LONG [ LONG$cow == 3388, ]
pCow19 <- ggplot(do19 , aes(x = do19$x , y = do19$y, size = do19$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do20 <- LONG [ LONG$cow == 3397, ]
pCow20 <- ggplot(do20 , aes(x = do20$x , y = do20$y, size = do20$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do21 <- LONG [ LONG$cow == 3404, ]
pCow21 <- ggplot(do21 , aes(x = do21$x , y = do21$y, size = do21$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do22 <- LONG [ LONG$cow == 3405, ]
pCow22 <- ggplot(do22 , aes(x = do22$x , y = do22$y, size = do22$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do23 <- LONG [ LONG$cow == 3406, ]
pCow23 <- ggplot(do23 , aes(x = do23$x , y = do23$y, size = do23$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do24 <- LONG [ LONG$cow == 3418, ]
pCow24 <- ggplot(do24 , aes(x = do24$x , y = do24$y, size = do24$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do25 <- LONG [ LONG$cow == 3427, ]
pCow25 <- ggplot(do25 , aes(x = do25$x , y = do25$y, size = do25$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do26 <- LONG [ LONG$cow == 3431, ]
pCow26 <- ggplot(do26 , aes(x = do26$x , y = do26$y, size = do26$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do27 <- LONG [ LONG$cow == 3436, ]
pCow27 <- ggplot(do27 , aes(x = do27$x , y = do27$y, size = do27$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do28 <- LONG [ LONG$cow == 3437, ]
pCow28 <- ggplot(do28 , aes(x = do28$x , y = do28$y, size = do28$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do29 <- LONG [ LONG$cow == 3440, ]
pCow29 <- ggplot(do29 , aes(x = do29$x , y = do29$y, size = do29$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do30 <- LONG [ LONG$cow == 3441, ]
pCow30 <- ggplot(do30 , aes(x = do30$x , y = do30$y, size = do30$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do31 <- LONG [ LONG$cow == 3452, ]
pCow31 <- ggplot(do31 , aes(x = do31$x , y = do31$y, size = do31$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))


do32 <- LONG [ LONG$cow == 3463, ]
pCow32 <- ggplot(do32 , aes(x = do32$x , y = do32$y, size = do32$logDuration )) + geom_point( alpha = 0.3, position = "jitter", xlim=c(0,30),colour="#2c7bb6") + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + geom_density2d(colour="#4d4d4d") + guides(size=guide_legend(title= "Duration"))




#################################

fat1 <- LONG [ LONG$catFood == 1, ]
Pfat1 <- ggplot(fat1, aes(x = fat1$x , y = fat1$y  , size = fat1$logDuration )) + geom_point( colour = '#d7191c',alpha = 0.3, position = "jitter", xlim=c(0,30)) + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + facet_wrap( ~ cow, ncol=2)  + guides(size=guide_legend(title= "Duration"))


fat2 <- LONG [ LONG$catFood == 2, ]
Pfat2 <- ggplot(fat2, aes(x = fat2$x , y = fat2$y, size = fat2$logDuration )) + geom_point( colour = '#fdae61',alpha = 0.3, position = "jitter", xlim=c(0,30)) + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + facet_wrap( ~ cow, ncol=3) + guides(size=guide_legend(title= "Duration"))


fat3 <- LONG [ LONG$catFood == 3, ]
Pfat3 <- ggplot(fat3, aes(x = fat3$x , y = fat3$y, size = fat3$logDuration )) + geom_point( colour = '#ffffbf', alpha = 0.3, position = "jitter", xlim=c(0,30)) + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + facet_wrap( ~ cow, ncol=4) + guides(size=guide_legend(title= "Duration"))


fat4 <- LONG [ LONG$catFood == 4, ]
Pfat4 <- ggplot(fat4, aes(x = fat4$x , y = fat4$y, size = fat4$logDuration )) + geom_point( colour = '#abd9e9', alpha = 0.3, position = "jitter", xlim=c(0,30)) + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + facet_wrap( ~ cow, ncol=2) + guides(size=guide_legend(title= "Duration"))


fat5 <- LONG [ LONG$catFood == 5, ]
Pfat5 <- ggplot(fat5, aes(x = fat5$x , y = fat5$y, size = fat5$logDuration )) + geom_point( colour = '#2c7bb6',alpha = 0.3, position = "jitter", xlim=c(0,30)) + labs( x = "X" , y = "Y") + scale_x_continuous(limits=c(0, 30)) + facet_wrap( ~ cow, ncol=2) + guides(size=guide_legend(title= "Duration"))

