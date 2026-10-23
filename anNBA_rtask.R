rm(list = ls(all = TRUE))

library(XML)
library(RMySQL)
library(dplyr)
library(tidyr)
library(data.table)
library(jsonlite)
library(lubridate)
library(purrr)
library(stringr)
library(httr)
library(urltools)

select<-dplyr::select

# mode function #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

loc<-"/Users/boooeee/Dropbox/sportsdatabase/nba/"
ssn<-2026

dt<-as.character(Sys.Date())
dt<-paste(substr(dt,1,4),substr(dt,6,7),substr(dt,9,10),sep="")

dt1<-as.character(Sys.Date()-1)
dt1<-paste(substr(dt1,1,4),substr(dt1,6,7),substr(dt1,9,10),sep="")

dt2<-as.character(Sys.Date())

# create date range #
dts<-as_date("2026-10-20")
dte<-Sys.Date()+3
dtl<-as.character(as_date(dts:dte))

adtl<-vector('list')
for (adt in dtl) {
  dtc<-gsub("-","",adt)
  print(dtc)
  flush.console()
  # action network nba prices #
  sburl<-paste0("https://api.actionnetwork.com/web/v1/scoreboard/nba?bookIds=15,30,68,75,69,76,71,79,247,123&date=",dtc) # game lines #
  
  h1<-handle('')
  urlsb<-RETRY("GET",times=10,url=sburl,handle=h1,user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"))
  urlsb<-paste("%",urlsb$content,sep="")
  urlsb<-paste(urlsb,collapse="")
  urlsb<-url_decode(urlsb)
  
  sb<-jsonlite::fromJSON(urlsb,simplifyVector = F)
  
  wp<-sb$games
  # extract betting infomation #
  wpf<-function(x) {
    
    htm<-x$home_team_id
    atm<-x$away_team_id
    tms<-x$teams
    tms<-lapply(tms,function(x) {x$standings<-NULL
                                return(x)})
    tms<-bind_rows(tms)
    odds<-x$odds
    odds<-lapply(odds,function(x) {x$meta<-x$line_status<-NULL
                                  return(x)})
    
    if (length(odds)>0) {
      odds<-odds %>%
        bind_rows(odds) %>%
        filter(type=="game") %>%
        distinct() 
    }
    
    # calculate average lines and totals #
    line <- median(odds$spread_home,na.rm=T)
    if (is.null(line)) {return(NULL)}
    
    tot <- median(odds$total,na.rm=T)
    
    team<-tms$display_name[tms$id==htm]
    to.team<-tms$display_name[tms$id==atm]
    
    tm<-tms$abbr[tms$id==htm]
    to.tm<-tms$abbr[tms$id==atm]
    
    out<-tibble(
      dtc=dtc,
      team=team,
      to.team=to.team,
      tm=tm,
      to.tm=to.tm,
      line=line,
      total=tot
    )
    return(out)
  }
  wpl<-lapply(wp,wpf)
  wpd<-bind_rows(wpl)
  adtl[[dtc]]<-wpd
}
mkd<-bind_rows(adtl)
mkd<-mkd %>%
  mutate(total=ifelse(total<100,NA,total))

# fix team names #
mkd <- mkd %>%
  mutate(tm=ifelse(tm=="WSH","WAS",tm),to.tm=ifelse(tm=="WSH","WAS",to.tm))


library(httr)
library(jsonlite)

fetch_nba_schedule <- function() {
  resp <- RETRY(
    "GET", "https://cdn.nba.com/static/json/staticData/scheduleLeagueV2.json",
    add_headers(
      Accept            = "application/json, text/plain, */*",
      `Accept-Language` = "en-US,en;q=0.9",
      Referer           = "https://www.nba.com/"
    ),
    user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15"),
    times = 10, pause_base = 1, pause_cap = 30
  )
  stop_for_status(resp)
  
  sb <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  # Flatten to a single list of games; each game with homeTeam/awayTeam as 1-row data frames
  sch <- list()
  for (d in sb$leagueSchedule$gameDates) {
    if (length(d$games) == 0) next
    for (g in d$games) {
      # Convert homeTeam/awayTeam from nested lists into 1-row data frames.
      # Drop sub-lists (like `players`) that would break cbind.
      flatten_team <- function(tm) {
        tm <- tm[!vapply(tm, is.list, logical(1))]            # drop nested list fields
        tm <- lapply(tm, function(v) if (is.null(v)) NA else v)
        as.data.frame(tm, stringsAsFactors = FALSE)
      }
      g$homeTeam <- flatten_team(g$homeTeam)
      g$awayTeam <- flatten_team(g$awayTeam)
      sch[[length(sch) + 1]] <- g
    }
  }
  sch
}

sch <- fetch_nba_schedule()
sf<-function(x) {
  gid<-x$gameId
  dt<-x$gameDateEst
  hm<-x$homeTeam
  colnames(hm)<-paste("home",colnames(hm),sep="_")
  aw<-x$awayTeam
  colnames(aw)<-paste("away",colnames(hm),sep="_")
  
  out<-cbind(gid,dt,hm,aw)
  
  return(out)
}

scd<-lapply(sch,sf)
scd<-bind_rows(scd)

ssd<-scd %>%
  filter(substr(gid,1,3) %in% c("002","004","005")) %>%
  mutate(dtc=gsub("-","",substr(as.character(dt),1,10)),
         playoffs=ifelse(substr(gid,1,3)=="004",1,0),
         site="home",
         date=as_date(dt)) %>%
  select(gid,dtc,date,playoffs,site,
         team=home_teamName,to.team=away_home_teamName,
         tm=home_teamTricode,to.tm=away_home_teamTricode,
         points=home_score,to.points=away_home_score)

# merge in odds #
ssd<-ssd %>%
  left_join(mkd,by=c("dtc","team","to.team","tm","to.tm"))

# flip for away teams #
awy<-ssd %>%
  mutate(site="away",line= -line) %>%
  rename(to.team=team,team=to.team,to.tm=tm,tm=to.tm,to.points=points,points=to.points)

ssd<-bind_rows(ssd,awy)

# load old file for fields and confdiv map #
load("~/Dropbox/sportsdatabase/nba/archive/sdbarc_20210601.RData")
cd<-nba %>%
  distinct(team,conference,division)

cd$team[cd$team=="Trailblazers"]<-"Trail Blazers"
cd$team[cd$team=="Seventysixers"]<-"76ers"

ssd <- ssd %>%
  left_join(cd,by=c("team")) %>%
  left_join(select(cd,to.team=team,to.conference=conference,to.division=division),by=c("to.team")) %>%
  mutate(season=ssn)

nba<-nba %>%
  filter(season==ssn) %>%
  mutate(date=as_date(date))

# merge in to get all the old fields #
pnba<-ssd %>%
  left_join(nba,suffix=c("",".y")) 

nba<-pnba

save(nba,file=paste(loc,"ancur.RData",sep=""),compress=TRUE)
save(nba,file=paste(loc,"archive/anarc_",dt,".RData",sep=""),compress=TRUE)

quit(save="no")
