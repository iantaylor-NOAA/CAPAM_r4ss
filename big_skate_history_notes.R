if(FALSE){
  # getting info on directories associated with big skate assessment
  mydir <- 'c:/SS/skates/models/'
  bs.mods <- grep("/bigskate", dir(mydir, full.names = TRUE), value = TRUE)

  # all files in directory
  allfiles <- dir(mydir, full.names = TRUE, recursive = TRUE)
  # all report files
  allreps <- grep("/Report", allfiles, value = TRUE)
  allpng <- grep(".png$", allfiles, value = TRUE)
  # info on all report files
  repinfo <- file.info(allreps)
  repinfo$date <- as.Date(repinfo$mtime)

  days <- seq(as.Date("2019-04-24"),
              as.Date("2019-08-21"),
              by = 1)
  repinfo$type <- "model"
  repinfo$type[grep("sensitivity.bigskate", rownames(repinfo))] <- "sens"
  repinfo$type[grep("profile", rownames(repinfo))] <- "profile"
  repinfo$type[grep("retro", rownames(repinfo))] <- "retro"
  repinfo$type[grep("forecasts", rownames(repinfo))] <- "fore"
  repinfo$type[grep("forecast_", rownames(repinfo))] <- "fore"
  repinfo$type[grep("jitter", rownames(repinfo))] <- "jitter"

}
  # cummulative counts of models
  model.counts <- data.frame(date = days,
                             model = NA,
                             profile = NA,
                             sens = NA,
                             retro = NA,
                             jitter = NA,
                             fore = NA)
  for(irow in 1:nrow(model.counts)){
    day <- model.counts$date[irow]
    model.counts$model[irow]   <- sum(repinfo$date[repinfo$type == "model"] <= day)
    model.counts$profile[irow] <- sum(repinfo$date[repinfo$type == "profile"] <= day)
    model.counts$retro[irow]   <- sum(repinfo$date[repinfo$type == "retro"] <= day)
    model.counts$sens[irow]    <- sum(repinfo$date[repinfo$type == "sens"] <= day)
    model.counts$fore[irow]    <- sum(repinfo$date[repinfo$type == "fore"] <= day)
    model.counts$jitter[irow]  <- sum(repinfo$date[repinfo$type == "jitter"] <= day)
  }
  model.counts$dummy <-  0

  # remove later dates
  model.counts <- model.counts[model.counts$date <= as.Date("2019-06-19"),]


# plot showing totals
mydir <- 'c:/Users/Ian.Taylor/Documents/talks/CAPAM2019_NZ_next-gen/CAPAM_r4ss'

colvec <- rev(r4ss::rich.colors.short(7)[-1])
coltext <- adjustcolor(colvec, offset = c(-0.3, -0.3, -0.3, 0))
# open PNG
png(file.path(mydir, 'models_run_v2.png'), width = 7, height = 5,
    res = 300, units = 'in')
par(mar = c(4,4,1,1))
# make empty plot
plot(model.counts$date, apply(model.counts[,-1], 1, sum),
     xlim = as.Date(c("2019-04-24", "2019-07-15"))+c(0,0.4),
     xaxs = 'i', yaxs = 'i',
     ylim = c(0,900),
     type = 'n', xlab = "Date", ylab = "Number of models",
     axes = FALSE)
# add grid
yticks <- seq(0,350,50)
yticks <- c(seq(0,800,100), 847)
abline(h = yticks, lty = 3, col = gray(0, alpha = 0.5))
rect(xleft = as.Date("2019-06-19"),
     xright = as.Date("2019-07-20"),
     ybottom = 0,
     ytop = 1000,
     col = 'white', border = NA)
# add bars
for(icol in 1:6){
  points(model.counts$date, apply(model.counts[,c(1 + 1:(7 - icol), 8)], 1, sum),
         type='h', lwd = 8,
         col = colvec[icol],
         lend = 3)
}
# add axes
xticks <- as.Date(c("2019-04-24", "2019-05-01", "2019-05-15", "2019-06-01", "2019-06-19"))
axis(1, at = xticks, labels = c("24 April","1 May", "15 May", "1 June", "19 June"))
axis(2, las = 1, at = yticks)
# add text
x.text <- as.Date("2019-06-19")+2
text(x = x.text, y =  50, pos = 4, col = coltext[6], "General models")
text(x = x.text, y = 250, pos = 4, col = coltext[5], "Likelihood profiles")
text(x = x.text, y = 500, pos = 4, col = coltext[4], "Sensitivities")
text(x = x.text, y = 630, pos = 4, col = coltext[3], "Retrospectives")
text(x = x.text, y = 720, pos = 4, col = coltext[2], "Jitters")
text(x = x.text, y = 840, pos = 4, col = coltext[1], "Forecasts")
dev.off()

# how many models run during STAR panel
model.counts$total <- apply(model.counts[,-1], 1, sum)
model.counts[40:45,]
##          date model profile retro sens fore dummy total
## 41 2019-06-03   105      22    18   64    4     0   213
## 42 2019-06-04   105      22    18   65    4     0   214
## 43 2019-06-05   106      22    18   73    4     0   223
## 44 2019-06-06   112      33    18  136    4     0   303
## 45 2019-06-07   112      33    18  136    7     0   306
model.counts[45,] - model.counts[40,]
##      date model profile sens retro jitter fore dummy total
## 45 5 days     7     108   72     0      0    3     0   190
