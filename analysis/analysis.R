#!/usr/bin/Rscript --vanilla

library(ggplot2)
library(lme4)

data <- read.csv("../data/small_hits_grouped.csv")
data$rank <- data$pos + (data$page-1)*10
data$ordering_factor <- data$rank
data$rank <- factor(data$rank)
data$rank <- with(data, reorder(rank,ordering_factor, mean))

data$T0 <- unclass(as.POSIXct(strptime(data$t0, "%Y-%m-%d %H:%M:%S")))
data$T1 <- unclass(as.POSIXct(strptime(data$t1, "%Y-%m-%d %H:%M:%S")))
data$deltat <- with(data, T1-T0)
data <- subset(data, deltat < 100 & deltat > 0)
data$y <- 0
#data$y[data$deltaY < 0] <-  1/(data$deltat[data$deltaY < 0]/60)
data$y[data$deltaY < 0] <- 1
data$temp.t <- strptime(data$t0, "%Y-%m-%d %H:%M:%S")
data$temp.t2 <- as.POSIXct(data$temp.t)
l = length(data$temp.ts)

x <- strftime(data$temp.t2, "%H")

data$H <- strftime(as.POSIXct(strptime(data$t0, "%Y-%m-%d %H:%M:%S")), "%H")


#################################################
# Rank Based - Multilevel Model 
#################################################

df.rank <- function(category, label){
  m <- lmer(y ~ factor(rank) - 1 + (1|id) + (1|H), data = subset(data, cat == category))
  r.output <- data.frame(
                       reg = names(fixef(m)), 
                       coef = fixef(m),
                       rank = 1:length(fixef(m)),
                       se = sqrt(diag(vcov(m)))
                       )
  r.output$model <- label
  r.output$page <- 3
  r.output$page[r.output$rank < 21] <- 2
  r.output$page[r.output$rank < 11] <- 1
  r.output$avg <- mean(r.output$coef)
  r.output
}

views = c("AssignmentDurationInSeconds1", "LastUpdatedTime1", "LatestExpiration1","NumHITs1", "Reward1","Title1")

r.com <- rbind(
               df.rank(views[4],"# of HITs (most)"),
               df.rank(views[2],"Time (newest)"),
               df.rank(views[5],"Reward (highest)"),
               df.rank(views[6],"Title (a-z)")
               )
#r.com$reg <- factor(r.com$reg, levels(r.com$reg)[c(2,10:3,1)])

r.com$reg <- with(r.com, reorder(reg, -1*rank, mean))

limits <- aes(xmax = coef + se, xmin=coef - se, height=.1,colour=factor(page)) 
g <- ggplot(r.com, aes(y = I(-1*rank), x = coef))  +  geom_point()
g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=avg), col="black")
g <- g + facet_wrap(~model, ncol=1)
g <- g + labs(x="Expected HIT-Disappearing Event Per Scrape Iteration", y="Rank", title="")

print(g)
pdf("../writeup/plots/FIGA_coef_re.pdf")
print(g)
dev.off()

################################################
# Linear Model 
###############################################

df.pos.lm <- function(category,label){
  m <- lmer(y ~ factor(rank) - 1 + (1|H), data = subset(data, cat==category))
  r.output <- data.frame(
                       reg = names(fixef(m)), 
                       coef = fixef(m),
                       rank = 1:length(fixef(m)),
                       se = sqrt(diag(vcov(m)))
                       )
  r.output$model <- label
  r.output$page <- 3
  r.output$page[r.output$rank < 21] <- 2
  r.output$page[r.output$rank < 11] <- 1
  r.output$avg <- mean(r.output$coef)
  r.output
}

r.com <- rbind(
               df.pos.lm(views[4],"# of HITs (most)"),
               df.pos.lm(views[2],"Time (newest)"),
               df.pos.lm(views[5],"Reward (highest)"),
               df.pos.lm(views[6],"Title (a-z)")
               )

r.com$reg <- with(r.com, reorder(reg, -1*rank, mean))

limits <- aes(xmax = coef + se, xmin=coef - se, height=.1,colour=factor(page)) 
g <- ggplot(r.com, aes(y = I(-1*rank), x = coef))  +  geom_point()
g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=avg), col="black")
g <- g + facet_wrap(~model, ncol=1)
g <- g + labs(x="Expected HIT-Disappearing Event Per Scrape Iteration", y="Rank", title="")


pdf("../writeup/plots/FIGB_coef_pooled.pdf")
print(g)
dev.off()



#
#m <- lmer(y ~ factor(H)-1 + (1|id), data = data)
#x = fixef(m) 
#qplot(names(x),x) + coord_flip()
#
#m <- lm(y ~ factor(H)-1, data = data)
#x = coef(m) 
#tmp <- data.frame(coef = coef(m), names=names(coef(m)),
#                  se = sqrt(diag(vcov(m))))

#limits <- aes(ymax = coef + se, ymin=coef - se, height=.1)
#g <- ggplot(aes(x = names, y=coef), data = tmp) + geom_point()
#g <- g + coord_flip()
#g <- g + geom_errorbar(limits)
#print(g)
#qplot(names,coef,data=tmp) + geom_errorbar(limits) + coord_flip()

# Position Effects - *1 
#
# df.pos <- function(category,label,page_listing){
#  m <- lmer(y ~ pos - 1 + (1|id) + (1|H), data = subset(data, cat==category & page==page_listing))
#  r.output <- data.frame(
#                       reg = names(fixef(m)), 
#                       coef = fixef(m), 
#                       se = sqrt(diag(vcov(m)))
#                       )
#  r.output$model <- label
#  r.output$page <- page_listing
#  r.output$avg <- mean(r.output$coef)
#  r.output
#}
#
#r.com <- rbind(
#               df.pos(views[4],views[4],1),
#               df.pos(views[4],views[4],2),
#               df.pos(views[4],views[4],3),
#               df.pos(views[2],views[2],1),
#               df.pos(views[2],views[2],2),
#               df.pos(views[2],views[2],3),
#               df.pos(views[5],views[5],1),
#               df.pos(views[5],views[5],2),
#               df.pos(views[5],views[5],3),
#               df.pos(views[6],views[6],1),
#               df.pos(views[6],views[6],2),
#               df.pos(views[6],views[6],3)
#               )
#r.com$reg <- factor(r.com$reg, levels(r.com$reg)[c(2,10:3,1)])

#limits <- aes(xmax = coef + se, xmin=coef - se, height=.1) 
#g <- ggplot(r.com, aes(y = reg, x = coef))  + geom_point()
#g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=avg), col="red")
#g <- g + facet_grid(page~model)
#g <- g + labs(x="Coefficients", y="", title="")

#pdf("../../search_tex_version/images/FIGA_coef_re.pdf")
#print(g)
#dev.off()
#print(g)

# Position effects - no group effects

# df.pos.lm <- function(category,label,page_listing){
#  m <- lm(y ~ pos - 1, data = subset(data, cat==category & page==page_listing))
#  r.output <- data.frame(
#                       reg = names(coef(m)), 
#                       coef = coef(m), 
#                       se = sqrt(diag(vcov(m)))
#                       )
#  r.output$model <- label
#  r.output$page <- page_listing
#  r.output$avg <- mean(r.output$coef)
#  r.output
#}

#r.com <- rbind(
#               df.pos.lm(views[4],views[4],1),
#               df.pos.lm(views[4],views[4],2),
#               df.pos.lm(views[4],views[4],3),
#               df.pos.lm(views[2],views[2],1),
#               df.pos.lm(views[2],views[2],2),
#               df.pos.lm(views[2],views[2],3),
#               df.pos.lm(views[5],views[5],1),
#               df.pos.lm(views[5],views[5],2),
#               df.pos.lm(views[5],views[5],3),
 #              df.pos.lm(views[6],views[6],1),
 #              df.pos.lm(views[6],views[6],2),
 #              df.pos.lm(views[6],views[6],3)
#               )
#r.com$reg <- factor(r.com$reg, levels(r.com$reg)[c(2,10:3,1)])
#
#limits <- aes(xmax = coef + se, xmin=coef - se, height=.1) 
#g <- ggplot(r.com, aes(y = reg, x = coef))  + geom_point()
#g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=avg), col="red")
#g <- g + facet_grid(page~model)
#g <- g + labs(x="Coefficients", y="", title="")
#print(g)

#pdf("../../search_tex_version/images/FIGB_coef_pooled.pdf")
#print(g)
#dev.off()


# Position Effects - *0 
#views = c("AssignmentDurationInSeconds0", "LastUpdatedTime0", "LatestExpiration0","NumHITs0", "Reward0","Title0") 
#
#df.pos <- function(category,label,page_listing){
#  m <- lmer(y ~ pos - 1 + (1|id), data = subset(data, cat==category & page==page_listing))
#  r.output <- data.frame(
#                       reg = names(fixef(m)), 
#                       coef = fixef(m), 
#                       se = sqrt(diag(vcov(m)))
#                       )
#  r.output$model <- label
#  r.output$page <- page_listing 
#  r.output
#}##


#r.com <- rbind(
#               df.pos(views[2],views[2],1),
#               df.pos(views[2],views[2],2),
#               df.pos(views[2],views[2],3)
#               )#



#r.com <- rbind(
#               df.pos(views[2],views[2],1),
#               df.pos(views[2],views[2],2),
#               df.pos(views[2],views[2],3),
#               df.pos(views[5],views[5],1),
#               df.pos(views[5],views[5],2),
#               df.pos(views[5],views[5],3)
#             )
#               
#               df.pos(views[2],views[2],1),
#               df.pos(views[2],views[2],2),
#               df.pos(views[2],views[2],3),
#               
#               df.pos(views[6],views[6],1),
#               df.pos(views[6],views[6],2),
#               df.pos(views[6],views[6],3)
#               )
#r.com$reg <- factor(r.com$reg, levels(r.com$reg)[c(2,10:3,1)])
#levels(r.com$reg) <- 10:1

#limits <- aes(xmax = coef + se, xmin=coef - se, height=.1) 
#g <- ggplot(r.com, aes(y = reg, x = coef))  + geom_point()
#g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=0), col="red")
#g <- g + facet_grid(page~model)
#g <- g + labs(x="Coefficients", y="", title="")
#print(g)



# in the other data
#data.all <- read.csv("/home/john/ungrouped.csv")
#qplot(positionOnPage, facets=pageNumber~sortCategory, data = data.all)


# explaining the "top spot" effects

#groups holding the top spot 
top_spot <- with(data, id[pos==1 & page==1 & cat=="LastUpdatedTime1"])
x = as.numeric(table(top_spot))

tmp.df <- as.data.frame(table(top_spot))
qplot(tmp.df$Freq)
bad.ids <- as.character(subset(tmp.df, Freq > 75)$top_spot)

# drop.levels <- function(dat){dat[] <- lapply(dat, function(x) x[,drop=TRUE])return(dat)}


# Position of Requestors at the Top 
tmp <- subset(data, cat=="LastUpdatedTime1" & id %in% bad.ids)
tmp$rank <- -1*((as.numeric(tmp$page)-1)*10 + as.numeric(tmp$pos))

tmp$p <- -1*as.numeric(as.character(tmp$pos))
tmp$time <- unclass(as.POSIXct(strptime(tmp$t1, "%Y-%m-%d %H:%M:%S")))
tmp$time <- (tmp$time - min(tmp$time))/(3600*24)

tmp <- droplevels(tmp)

#levels(tmp$id) <- 1:10
theme_set(theme_bw())
g <- qplot(time,rank,geom="line", data = tmp) + geom_point(aes(colour=factor(page)),alpha=I(.5)) + facet_wrap(~id,ncol=1)
print(g)

pdf("../writeup/plots//FIGC.pdf", width=9, heigh=7)
print(g)
dev.off()


#############
## RCM Portion
##############


library(ggplot2)

data <- read.csv("../data/best_case.csv")

data$category <- with(data, as.factor(category))
data$search <- with(data, factor(search))


g <- ggplot(data, aes(x=count, y=search)) +
  geom_segment(aes(yend=search,xend=0,colour=case), size=I(5)) +
  facet_grid(category ~ case, scale="free")  +
    theme(legend.position="none",
          strip.text.y = element_text()) +
    ylab("")

print(g)

pdf("../writeup/plots/rcm_combined.pdf")
 print(g)
dev.off()

time.data <- read.csv("../data/timing.csv")

#qplot(time,count,colour=case, data = time.data)
#today <- as.POSIXlt("2010-05-06 00:00:00")
#time.data$t <- as.numeric(strptime(time.data$time,"%H:%M:%S") - today)
#time.data$hour <- time.data$t/6600

g <- ggplot(time.data, aes(x=time,y=count)) +
    geom_point(aes(colour=case,shape=case),size=I(3)) +
    geom_line(aes(linetype=case)) +
    xlab("time in hours") + ylab("number of workers") + 
    geom_text(aes(x=5, y=72, label="BEST")) +
    geom_text(aes(x=22, y=37, label="A-Z")) +
    geom_text(aes(x=22.5, y=28, label="NEWEST")) +
    geom_text(aes(x=23, y=15, label="WORST")) +
    theme(legend.position="none")

#print(g)

pdf("../writeup/plots/rcm_timing.pdf")
theme_set(theme_bw())
print(g)
dev.off()


# Make Histogram Plot
pages.data <- read.csv("../data/pages.csv")
pages.data$page <- with(pages.data, reorder(page,-order,mean))

## g <- qplot(page, count, geom="bar",
##            data = pages.data) +
##     ylab("number of workers") +
##     xlab("result page number") + 
##     coord_flip()

g <- ggplot(data = pages.data, aes(x = page, y = count)) + geom_point() +
    xlab("result page number") +
    ylab("number of workers") +
    coord_flip()

print(g)

pdf("../writeup/plots/rcm_pages.pdf")
 print(g)
dev.off()


# 7 day plot
day7.data <- read.csv("../data/timing_7day.csv")

g <- ggplot(day7.data, aes(x=time,y=count)) +
  xlab("time in days") + ylab("number of workers") + 
  geom_text(aes(x=.2, y=71.55, label="BEST")) +
  geom_text(aes(x=2.3, y=71.5, label="A-Z")) +
  geom_text(aes(x=4.3, y=60, label="NEWEST")) +
    geom_text(aes(x=5.2, y=60, label="WORST")) +
    theme(legend.position="none") +
  geom_point(aes(colour=case,shape=case),size=I(3)) +
  geom_line(aes(linetype=case)) 

pdf("../writeup/plots/rcm_timing_7day.pdf")
 print(g)
dev.off()
