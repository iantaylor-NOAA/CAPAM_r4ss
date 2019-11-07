if(FALSE){

  # parse git history for SS
  # failed package: #remotes::install_github("lorenzwalthert/gitsum")

  mydir <- 'c:/Users/Ian.Taylor/Documents/talks/CAPAM2019_NZ_next-gen/CAPAM_r4ss'
  git1 <- readLines(file.path(mydir, 'git_history_lines.txt'))

  # extract lines indicating change
  which.date <- grep("^Date:", git1)
  which.changed <- grep("files changed, ", git1)
  lines.date <- git1[which.date]
  lines.changed <- git1[which.changed]
  # figure out which dates were associated with files changing
  which.date.changed <- rep(NA, length(which.changed))
  for(i in 1:length(which.changed)){
    which.date.changed[i] <- max(which.date[which.date < which.changed[i]])
  }
  # new lines.date of same length as lines.change
  lines.date <- git1[which.date.changed]
  # date format
  #Date:   Tue Oct 28 17:50:17 2008 +0000
  dates <- as.Date(substring(lines.date, 13, 32), format = "%b %d %H:%M:%S %Y") 

  insertions.list <- unlist(lapply(strsplit(lines.changed, ", "), function(x){x[2]}))
  insertions <- as.numeric(unlist(lapply(strsplit(insertions.list, " "), function(x){x[1]})))
  deletions.list <- unlist(lapply(strsplit(lines.changed, ", "), function(x){x[3]}))
  deletions <- as.numeric(unlist(lapply(strsplit(deletions.list, " "), function(x){x[1]})))
  insertions[is.na(insertions)] <- 0
  deletions[is.na(deletions)] <- 0

  #deletions.list <- strsplit(lines.changed, "[[:blank:]]+,")
  hedeletions.list <- strsplit(lines.changed, ", ")

  insertions <- insertions[order(dates)]
  deletions <- deletions[order(dates)]
  dates <- dates[order(dates)]
  total <- cumsum(insertions) - cumsum(deletions)
  dates <- c(dates[1]-1, dates)
  total <- c(0, total)

  # commits by month
  commit.date <- as.numeric(format(dates, format = "%Y")) +
    as.numeric(format(dates, format = "%m"))/12
  commit.date <- round(commit.date, 3)
  date.table <- expand.grid(1:12, 2008:2019)
  all.dates.date <- paste(date.table$Var2,
                                  substring(100 + date.table$Var1, 2),
                          "01", sep = "-")
  all.dates.date <- as.Date(all.dates.date)
  all.dates.num <- round(seq(2008, 2019+11/12, 1/12), 3)
  commit.counts <- rep(NA, length(all.dates.num))
  for(i in 1:length(all.dates.num)){
    commit.counts[i] <- sum(commit.date == all.dates.num[i])
  }

  # get 
  CRAN <- read.table(file.path(mydir, 'CRAN_archive_info.txt'))
  dates.CRAN <- as.Date(CRAN$V2)
}

if(FALSE){
  # read files in local directory to double-check word count
  allfiles <- dir('c:/github/r4ss', recursive = TRUE, full.names = TRUE)
  allfiles <- allfiles[-grep('revdep', allfiles)]
  allfiles <- allfiles[-grep('ignored', allfiles)]
  allfiles.info <- data.frame(file = allfiles, type = 'other',
                              stringsAsFactors = FALSE)
  allfiles.info$type[grep(".R$", allfiles.info$file)] <- 'R'
  allfiles.info$type[grep("extdata/simple", allfiles.info$file)] <- 'SS'
  allfiles.info$type[grep("/man/", allfiles.info$file)] <- 'man'
  allfiles.info$lines <- NA

  for(irow in 1:nrow(allfiles.info)){
    allfiles.info$lines[irow] <- length(readLines(allfiles.info$file[irow]))
  }

  sum(allfiles.info$lines)
  aggregate(allfiles.info$lines, by = list(allfiles.info$type), FUN = sum)

    ## lines.total
  ## # [1] 389523
  ## Rlines.total
  ## # [1] 36410

}


# make plot
png(file.path(mydir, 'git_history_v2.png'),
    width = 10, height = 6, res = 300, units = 'in')
par(mar = c(4,1,1,7))
plot(dates, total, type='n', las = 1, axes = FALSE,
     xlim = as.Date(c('2005-01-01','2020-01-01')),
     ylim = c(0, 450000),
     xlab = '', ylab = '', lwd=2, xaxs = 'i', yaxs = 'i')

scale <- 2000
points(all.dates.date,
       scale*commit.counts,
       type = 'h', lend = 3,
       lwd = 4, col = 'grey80')

x.ticks <- as.Date(paste0(2005:2020, "-01-01"))
x.labs <- as.Date(paste0(2005:2020, "-07-01"))
labs <- 2005:2020
axis(1, at = x.ticks, labels = rep("", length(x.ticks)))
axis(1, at = x.labs, labels = labs, tick = FALSE)
yvec <- seq(0, 500000, by = 100000)
yvec2 <- c(10,20,30)
axis(4, at = scale*yvec2, labels = yvec2, las = 1,
     col = 'grey60', col.axis = 'grey60', col.ticks = 'grey60')
axis(4, las = 1, at = yvec, labels = rep("", 6))
axis(4, las = 1, at = yvec, format(yvec, scientific=FALSE, big.mark = ","))
mtext("Commits\nper month", side = 4, adj = 0.03, line = 3, col = 'grey60')
mtext("Total lines (insertions - deletions)", side = 4, line = 5)
for(i in 1:nrow(CRAN)){
  points(dates.CRAN[i], total[abs(dates - dates.CRAN[i]) ==
                              min(abs(dates - dates.CRAN[i]))][1],
         col = rgb(1,0,0,1),
         cex = 1.5, pch=16)
}
lines(dates, total, lwd = 3, col = rgb(0,0,1,0.8))
dev.off()
