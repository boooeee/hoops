rm(list = ls(all = TRUE))

library(XML)
library(RJSONIO)
library(RMySQL)
library(segmented)
library(httr2)
library(urltools)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ── HTTP helpers ──────────────────────────────────────────────────────────────
# cdn.nba.com is Akamai in front of the NBA's own S3 bucket, which serves the
# same schedule, play-by-play and box score files. Akamai answers 403 with or
# without browser headers, so each file is read from the bucket first, with
# cdn.nba.com as the backup. The browser headers are only sent to
# stats.nba.com, which wants them.
# Responses are still parsed with RJSONIO, so every list downstream keeps the
# shape the parsing code was written against.
nba_sources <- c(
  "S3"  = "https://nba-prod-us-east-1-mediaops-stats.s3.amazonaws.com/NBA/",
  "CDN" = "https://cdn.nba.com/static/json/")

get_json <- function(url) {
  resp <- request(url) |>
    req_timeout(60) |>
    req_retry(max_tries = 3, backoff = \(i) 5 * i) |>
    req_perform()
  RJSONIO::fromJSON(resp_body_string(resp))
}

# path is relative to static/json/, e.g. "liveData/boxscore/boxscore_<gid>.json".
# NULL instead of an error, so one missing game doesn't stop the run.
nba_file <- function(path) {
  for (src in names(nba_sources)) {
    out <- tryCatch(get_json(paste0(nba_sources[[src]], path)), error = function(e) {
      message(src, " failed (", basename(path), "): ", conditionMessage(e))
      NULL
    })
    if (!is.null(out)) return(out)
  }
  NULL
}

nba_get <- function(url, query = list()) {
  resp <- request(url) |>
    req_headers(
      "User-Agent"         = paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                                    "AppleWebKit/537.36 (KHTML, like Gecko) ",
                                    "Chrome/131.0.0.0 Safari/537.36"),
      "Accept"             = "application/json, text/plain, */*",
      "Accept-Encoding"    = "gzip, deflate, br",
      "Accept-Language"    = "en-US,en;q=0.9",
      "Referer"            = "https://www.nba.com/",
      "Origin"             = "https://www.nba.com",
      "x-nba-stats-origin" = "stats",
      "x-nba-stats-token"  = "true",
      "Sec-Fetch-Dest"     = "empty",
      "Sec-Fetch-Mode"     = "cors",
      "Sec-Fetch-Site"     = "same-site",
      "sec-ch-ua"          = '"Google Chrome";v="131", "Chromium";v="131", "Not_A Brand";v="24"',
      "sec-ch-ua-mobile"   = "?0",
      "sec-ch-ua-platform" = '"macOS"',
      "Connection"         = "keep-alive"
    ) |>
    req_url_query(!!!query) |>
    req_timeout(60) |>
    req_retry(max_tries = 4, backoff = \(i) 10 * i) |>
    req_perform()
  RJSONIO::fromJSON(resp_body_string(resp))
}

pbp_path <- function(gid) paste0("liveData/playbyplay/playbyplay_", gid, ".json")
box_path <- function(gid) paste0("liveData/boxscore/boxscore_", gid, ".json")

# Try each copy of the league schedule, then stats.nba.com as a last resort
# (same leagueSchedule layout, but slower and sometimes blocked).
fetch_schedule <- function(ssnf) {
  for (f in c("scheduleLeagueV2.json", "scheduleLeagueV2_2.json", "scheduleLeagueV2_1.json")) {
    sched <- nba_file(paste0("staticData/", f))
    if (!is.null(sched)) {
      message("Schedule from ", f)
      return(sched)
    }
  }
  message("All schedule files failed; trying stats.nba.com (may time out)...")
  nba_get("https://stats.nba.com/stats/scheduleleaguev2",
          list(Season = ssnf, LeagueID = "00"))
}

# ── Database helpers ──────────────────────────────────────────────────────────
# Logins come from ~/.Renviron. Each NBA_ variable falls back to its WNBA_
# twin, so the .Renviron set up for the WNBA scripts works here as is.
# Re-read here so an edit to ~/.Renviron is picked up without restarting R.
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

db_local <- function() {
  dbConnect(RMySQL::MySQL(),
    username = Sys.getenv("NBA_DB_USER", Sys.getenv("WNBA_DB_USER", "root")),
    password = Sys.getenv("NBA_DB_PASS", Sys.getenv("WNBA_DB_PASS")),
    dbname   = "nba",
    host     = "localhost",
    port     = 3306L)
}

db_remote_new <- function() {
  dbConnect(RMySQL::MySQL(),
    username = Sys.getenv("NBA_DB_REMOTE_USER", Sys.getenv("WNBA_DB_REMOTE_USER", "boooeee_rupdate")),
    password = Sys.getenv("NBA_DB_REMOTE_PASS_NEW", Sys.getenv("WNBA_DB_REMOTE_PASS_NEW")),
    dbname   = "boooeee_nba",
    host     = Sys.getenv("NBA_DB_IP_NEW", Sys.getenv("WNBA_DB_IP_NEW")),
    port     = 3306L)
}

wrap_function<- function() {
  fromJSON<-RJSONIO::fromJSON
  select<-dplyr::select
  
  slp<-1
  slp2<-5
  
  locsdb<-"/Users/boooeee/Dropbox/sportsdatabase/nba/"
  loc<-"/Users/boooeee/Dropbox/Nba/playbyplay/nba/json/"
  locn<-"/Users/boooeee/Dropbox/Nba/playbyplay/nba/"
  locwp<-"/Users/boooeee/Dropbox/Nba/playbyplay/inpredictable/"
  locl<-"/Users/boooeee/Dropbox/Nba/"
  # latest season #
  lssn<-2026
  ssn<-2026
  ssnf<-paste(ssn,substr(ssn+1,3,4),sep="-")
  mxgms<-250
  
  po<-0
  
  # select TRUE if updating with real data #
  update<-TRUE 
  # select TRUE if replacing existing data
  replace<-T
  
  dt.s<-as.character(Sys.Date()-2)
  if (hour(Sys.time())>=24) {
    dt.s<-as.character(Sys.Date()-3)
  }
  dt.e<-as.character(Sys.Date()-0)
  
  #dt.s<-"2025-10-17"
  #dt.e<-"2025-10-17"
  
  # convert to dates #
  dt.sd<-as.Date(dt.s)
  dt.ed<-as.Date(dt.e)
  
  dline<- -3.25 # if i can't find the line in sdb #
  dtotal<- 200.52 # if i can't find the total in sdb #
  # can use the above values to flag missing values #
  
  # team names #
  tm.lng<-c("Hornets","Bobcats","Bucks","Bulls","Cavaliers","Celtics","Clippers","Grizzlies",    
            "Hawks","Heat","Hornets","Jazz","Kings","Knicks","Lakers",
            "Magic","Mavericks","Nets","Nuggets","Pacers","Pistons","Raptors",      
            "Rockets","76ers","Spurs","Suns","Thunder","Timberwolves","Trailblazers", 
            "Warriors","Wizards","Pelicans","Supersonics","Eastern","Western","Nets","Hornets","Grizzlies","Hornets")  
  tm.sht<-c("CHA","CHA","MIL","CHI","CLE","BOS","LAC","MEM",    
            "ATL","MIA","NOH","UTA","SAC","NYK","LAL",
            "ORL","DAL","BKN","DEN","IND","DET","TOR",      
            "HOU","PHI","SAS","PHX","OKC","MIN","POR", 
            "GSW","WAS","NOP","SEA","EST","WST","NJN",
            "CHH","VAN","NOK")  
  
  sqlf<-function(x) {
    con <- db_local()
    out<-dbGetQuery(con,x)
    dbDisconnect(con)
    return(out)
  }
  
  gmdg<-sqlf("SELECT ssn, gms.gid, po, htm, atm, dt, hscr, ascr, hspk, aspk FROM gms")
  
  gmdg$scr<-paste(gmdg$atm," ",gmdg$ascr,", ",gmdg$htm," ",gmdg$hscr,sep="")
  
  hl<-strsplit(gmdg$hspk,",")
  names(hl)<-gmdg$gid
  
  fx<-function(x) {
    df<-mean((as.numeric(p)-as.numeric(x))^2)
    names(df)<-names(x)
    return(df)
  }
  
  # set parameters #
  # rebound percentages - see rebPct R script #
  fgreb <- 0.360
  ftreb <- 0.186
  # free throw percent - see ftPct R script #
  ftpct <- 0.7563
  # field goal percentages - see fgPct script #
  fg2pct <- 0.483
  fg3pct <- 0.358
  
  # create list of eWPA values #
  eWPA<-vector('list')
  eWPA[['to']]<- -0.02
  eWPA[['msfg']]<- -0.015
  eWPA[['msft']]<- -0.016
  eWPA[['mkft']]<- 0.005
  eWPA[['ft1']]<- 0.016
  eWPA[['ft2']]<- 0.013
  eWPA[['ft3']]<- 0.033
  eWPA[['mkfg']]<- 0.022
  eWPA[['mkf3']]<- 0.046
  eWPA[['dreb']]<-0.008
  eWPA[['oreb']]<-0.015
  
  # create list of eWPA values #
  eWPA<-vector('list')
  eWPA[['TO']]<- -0.02
  eWPA[['MS2']]<- -0.015
  eWPA[['MS1']]<- -0.016
  eWPA[['MK1']]<- 0.005
  eWPA[['DF1']]<- 0.016
  eWPA[['DF2']]<- 0.013
  eWPA[['DF3']]<- 0.033
  eWPA[['MK2']]<- 0.022
  eWPA[['MK3']]<- 0.046
  eWPA[['DRB']]<-0.008
  eWPA[['ORB']]<-0.015
  eWPA[['MS3']]<- eWPA[['MS2']]
  
  # load eWPA percentiles #
  load(file=paste(locwp,"eWPA.RData",sep=""))
  load(file=paste(locwp,"eWPA25.RData",sep=""))
  load(file=paste(locwp,"eWPA75.RData",sep=""))
  
  #if (po==0) {
  
  po.lst<-vector('list')
  pbppo.lst<-vector('list')
  boxsmpo.lst<-vector('list')
  boxpo.lst<-vector('list')
  shpo.lst<-vector('list')
  
  rg.lst<-vector('list')
  pbprg.lst<-vector('list')
  boxsm.lst<-vector('list')
  boxrg.lst<-vector('list')
  shrg.lst<-vector('list')
  
  # pbp unlist function #
  ulf<-function(x) {
    x<-t(matrix(unlist(as.character(x))))
  }
  if (update) {
    # Pull from new NBA static JSON feed - single call gets all games
    sb <- fetch_schedule(ssnf)

    # Extract game dates array
    game_dates <- sb$leagueSchedule$gameDates
    
    # Convert date range to Date objects for comparison
    dt_start <- as.Date(dt.sd, origin="1970-01-01")
    dt_end <- as.Date(dt.ed, origin="1970-01-01")
    
    # Loop through each date in the response
    for (i in seq_along(game_dates)) {
      # Parse the game date
      dtc <- as.Date(substr(game_dates[[i]]$gameDate, 1, 10), format="%m/%d/%Y")
      
      # Skip if outside our date range
      if (dtc < dt_start || dtc > dt_end) {next}
      
      # Get games for this date
      games <- game_dates[[i]]$games
      
      if (length(games) == 0) {next}
      
      # Convert games list to dataframe format similar to old structure
      sbd <- data.frame()
      for (j in seq_along(games)) {
        game <- games[[j]]
        game_row <- data.frame(
          GAME_ID = game$gameId,
          GAME_DATE = game$gameDateEst,
          GAME_STATUS_ID = game$gameStatus,  # 1=scheduled, 2=in progress, 3=final
          GAME_STATUS_TEXT = game$gameStatusText,
          HOME_TEAM_ID = game$homeTeam$teamId,
          HOME_TEAM_NAME = game$homeTeam$teamName,
          HOME_TEAM_CITY = game$homeTeam$teamCity,
          HOME_TEAM_TRICODE = game$homeTeam$teamTricode,
          HOME_TEAM_SCORE = game$homeTeam$score,
          AWAY_TEAM_ID = game$awayTeam$teamId,
          AWAY_TEAM_NAME = game$awayTeam$teamName,
          AWAY_TEAM_CITY = game$awayTeam$teamCity,
          AWAY_TEAM_TRICODE = game$awayTeam$teamTricode,
          AWAY_TEAM_SCORE = game$awayTeam$score,
          stringsAsFactors = FALSE
        )
        sbd <- rbind(sbd, game_row)
      }
      
      # Check for currently active games
      active_games <- sbd %>%
        as_tibble() %>%
        filter(GAME_STATUS_ID == 2)
      
      if (nrow(active_games) == 0) {
        print("NO ACTIVE GAMES")
        flush.console()
      }
      
      # Store in list using string date as key
      dtc_str <- as.character(dtc)
      if (po == 0) {
        rg.lst[[dtc_str]] <- sbd
      } else {
        po.lst[[dtc_str]] <- sbd
      }
      
      print(dtc_str)
      flush.console()
    }
    
    # Save the data
    if (po == 0) {
      #save(rg.lst, file=paste(loc, "rg", ssn, ".RData", sep=""), compress=TRUE)
    } else {
      #save(po.lst, file=paste(loc, "rg", ssn, ".RData", sep=""), compress=TRUE)
    }
  }
  
  # pull play by play #
  if (update) {
    if (po==0) {
      rg.df<-do.call('rbind',rg.lst[names(rg.lst)>=dt.s & names(rg.lst)<=dt.e])
    } else {
      rg.df<-do.call('rbind',po.lst[names(po.lst)>=dt.s & names(po.lst)<=dt.e])
    }
  } else {
    if (po==0) {
      rg.df<-do.call('rbind',rg.lst)
    } else {
      rg.df<-do.call('rbind',po.lst)
    }
  }
  rg.df<-data.frame(rg.df)
  rg.df<-rg.df[rg.df$GAME_STATUS_ID=="3",]
  # remove all star games #
  rg.df<-rg.df[substr(rg.df$GAME_ID,1,3)!="003",]
  
  ids<-rg.df$GAME_ID
  
  if (!replace) {
    # eliminate games that are already in database #
    con <- db_local()
    curid<-dbGetQuery(con,paste("select DISTINCT gid from gms WHERE ssn=",ssn,sep=""))
    dbDisconnect(con)
    
    ids<-ids[!(ids %in% curid$gid)]
    rg.df<-rg.df[rg.df$GAME_ID %in% ids,]
  }
  
  # cutoff #
  mxgms<-min(mxgms,nrow(rg.df))
  rg.df<-rg.df[1:mxgms,]
  
  boxsmrg.lst<-vector('list')
  boxsmpo.lst<-vector('list')
  if (update) {
    # load new play by play data #
    for (i in 1:nrow(rg.df)) {
      gid <- rg.df$GAME_ID[i]
      gid_chr <- as.character(gid)

      # Play-by-play
      pbp <- nba_file(pbp_path(gid))
      if (po == 0) {
        pbprg.lst[[gid_chr]] <- pbp
      } else {
        pbppo.lst[[gid_chr]] <- pbp
      }
      
      # Boxscore
      box <- nba_file(box_path(gid))
      if (is.null(box)) next
      if (po == 0) {
        boxrg.lst[[gid_chr]] <- box
      } else {
        boxpo.lst[[gid_chr]] <- box
      }
      
      # Boxscore summary — currently fetching the same URL as box above.
      # If you want the actual summary, use boxscoresummaryv3 from stats.nba.com (see note below).
      boxsm <- box  # placeholder — same payload as box, unless you swap the endpoint
      if (is.null(boxsm)) next
      if (po == 0) {
        boxsmrg.lst[[gid_chr]] <- boxsm
      } else {
        boxsmpo.lst[[gid_chr]] <- boxsm
      }
      
      print(paste0(i, " of ", nrow(rg.df)))
      flush.console()
      
      Sys.sleep(0.3)  # be polite, avoid rate-limit pattern detection
    }
    if (length(if (po==0) boxrg.lst else boxpo.lst)==0) {
      stop("no box scores downloaded - see the failures above")
    }
    if (po==0) {
      #save(boxsmrg.lst,file=paste(loc,"boxsmrg",ssn,".RData",sep=""),compress=TRUE)
      #save(boxrg.lst,file=paste(loc,"boxrg",ssn,".RData",sep=""),compress=TRUE)
      #save(pbprg.lst,file=paste(loc,"pbprgv2",ssn,".RData",sep=""),compress=TRUE)
    } else {
      #save(boxsmpo.lst,file=paste(loc,"boxsmpo",ssn,".RData",sep=""),compress=TRUE)
      #save(boxpo.lst,file=paste(loc,"boxpo",ssn,".RData",sep=""),compress=TRUE)
      #save(pbppo.lst,file=paste(loc,"pbppov2",ssn,".RData",sep=""),compress=TRUE)
    }
  } 
  
  # restrict to current games #
  if (po==1) {
    boxsmrg.lst<-boxsmpo.lst
    boxrg.lst<-boxpo.lst
    pbprg.lst<-pbppo.lst
    shrg.lst<-shpo.lst
  }
  boxsmrg.lst<-boxsmrg.lst[names(boxrg.lst) %in% ids]
  boxrg.lst<-boxrg.lst[names(boxrg.lst) %in% ids]
  pbprg.lst<-pbprg.lst[names(pbprg.lst) %in% ids]
  shrg.lst<-shrg.lst[names(shrg.lst) %in% ids]
  
  # load odds data #
  if (ssn==lssn) {
    load(file=paste(locsdb,"ancur.RData",sep=""))
    nba$team[nba$team=="Trail Blazers"]<-"Trailblazers"
    nba$to.team[nba$to.team=="Trail Blazers"]<-"Trailblazers"
  } else {
    load(file=paste(locsdb,"sdbpst.RData",sep=""))
    nba<-sdb.lst[[as.character(ssn)]]
  }
  
  # load player file #
  load(file=paste(locn,"player.RData",sep=""))
  
  # create data frame of games #
  gm<-rg.df
  gm$tm1<-gm$AWAY_TEAM_TRICODE
  gm$tm2<-gm$HOME_TEAM_TRICODE
  #gm$tm1<-substr(gm$GAMECODE,10,12)
  #gm$tm2<-substr(gm$GAMECODE,13,15)
  
  # convert pbp list to data frame #
  ltd<-function(y,n=33) {
    m<-matrix(nrow=1,ncol=n)
    for (i in 1:n) {
      if (!is.null(y[[i]][1])) {
        m[1,i]<-y[[i]][1]
      }
    }
    return(m)
  }
  
  # on off function #
  oof<-function(x) {
    x<-merge(x,pa,by=c("pid","ri"),all.x=TRUE)
    x$b[is.na(x$b)]<-0
    if (nrow(sq)>0) {
      sq$ri<-ugt[findInterval(sq$r,ugt)]
      sq$s<-1
      x<-merge(x,sq[c("ri","pid1","s")],by.x=c("pid","ri"),by.y=c("pid1","ri"),all.x=TRUE)
      x$out<-ifelse(is.na(x$s),0,1)
      x$s<-NULL
      x<-merge(x,sq[c("ri","pid2","s")],by.x=c("pid","ri"),by.y=c("pid2","ri"),all.x=TRUE)
      x$ins<-ifelse(is.na(x$s),0,1)
      x$s<-NULL
    } else {
      x$ins<-0
      x$out<-0
    }
    # offset ins and outs #
    io<-x$ins+x$out
    x$ins<-ifelse(io==2,0,x$ins)
    x$out<-ifelse(io==2,0,x$out)
    # populate starters if first quarter #
    if (q==1) {
      x$ins<-ifelse(x$pid %in% str & x$ri==0,1,x$ins)
    }
    # seed #
    x$oof<-0
    x$oof<-ifelse(x$b==1,1,x$oof)
    x$oof<-ifelse(x$ins==1,1,x$oof)
    pout<-c(x$out[2:nrow(x)],0)
    if (nrow(sq)==0) {
      pout<-x$out
    }
    pins<-c(x$ins[2:nrow(x)],0)
    x$oof<-ifelse(pout==1,1,x$oof)
    chk<-x$oof
    if (nrow(sq)>0) {
      # forward pass #
      for (i in 1:20) {
        poof<-c(x$oof[1],x$oof[1:(nrow(x)-1)])
        x$oof<-ifelse(poof==1 & x$out==0,1,x$oof)
        if (sum(!(x$oof==chk))==0) {break}
        chk<-x$oof
      }
      chk<-x$oof
      # backwards pass #
      for (i in 1:20) {
        poof<-c(x$oof[2:(nrow(x))],x$oof[nrow(x)])
        x$oof<-ifelse(poof==1 & pins==0,poof,x$oof)
        if (sum(!(x$oof==chk))==0) {break}
        chk<-x$oof
      }
    }
    return(x)
  }
  
  # Helper functions
  
  # Convert NBA Live API clock format (PT12M00.00S) to minutes:seconds format
  parseClock <- function(clock_str) {
    if (is.null(clock_str) || is.na(clock_str) || clock_str == "") {
      return("0:00")
    }
    
    # Remove PT at the start and S at the end
    cleaned <- gsub("^PT|S$", "", clock_str)
    
    # Split by M to get minutes and seconds
    parts <- strsplit(cleaned, "M")[[1]]
    if (length(parts) == 2) {
      min <- as.numeric(parts[1])
      sec <- as.numeric(parts[2])
      return(sprintf("%d:%02d", min, floor(sec)))
    } else {
      # If we can't parse it properly, return the original string
      return(clock_str)
    }
  }
  
  # Map NBA Live API action types to event types (emt) used in the original function
  mapActionTypeToEmt <- function(action) {
    if (is.null(action$actionType)) {
      return(NA)
    }
    
    action_type <- action$actionType
    
    # Map action types to event types used in original function
    mapping <- list(
      "2pt" = "1",          # Made FG (will filter for makes/misses)
      "3pt" = "1",          # Made FG (will filter for makes/misses)
      "rebound" = "4",
      "turnover" = "5",
      "foul" = "6",
      "violation" = "7",
      "substitution" = "8",
      "timeout" = "9",
      "jumpball" = "10",
      "period" = function(a) if(a$subType == "start") "12" else "13",  # Start/End of period
      "freethrow" = "3",
      "block" = "7",        # Blocks are considered as events
      "steal" = "7"         # Steals are considered as events
    )
    
    # Process the mapping
    if (is.function(mapping[[action_type]])) {
      return(mapping[[action_type]](action))
    } else if (!is.null(mapping[[action_type]])) {
      # Special handling for field goals (makes/misses)
      if (action_type %in% c("2pt", "3pt")) {
        return(if (!is.null(action$shotResult) && action$shotResult == "Missed") "2" else "1")
      }
      return(mapping[[action_type]])
    }
    
    return(NA)
  }
  
  # Map NBA Live API sub-types to event type details (emat)
  mapSubTypeToEmat <- function(action) {
    if (is.null(action$actionType) || is.null(action$subType)) {
      return(NA)
    }
    
    # Extract action type and subtype
    action_type <- action$actionType
    sub_type <- action$subType
    descriptor <- action$descriptor
    qualifiers <- action$qualifiers
    
    # Free throw mappings
    if (action_type == "freethrow") {
      # Based on the "X of Y" format
      if (sub_type == "1 of 1") return("10")  # 1 of 1
      if (sub_type == "1 of 2") return("11")  # 1 of 2
      if (sub_type == "2 of 2") return("12")  # 2 of 2
      if (sub_type == "1 of 3") return("13")  # 1 of 3
      if (sub_type == "2 of 3") return("14")  # 2 of 3
      if (sub_type == "3 of 3") return("15")  # 3 of 3
      # Technical free throws
      if (grepl("technical", tolower(sub_type))) return("16")  # Technical
      # Clear path
      if (!is.null(qualifiers) && any(grepl("clear path", tolower(qualifiers)))) return("17")
    }
    
    # Foul mappings
    if (action_type == "foul") {
      # Personal fouls
      if (sub_type == "personal") return("1")
      # Shooting fouls - distinguish by free throw info in descriptor
      if (sub_type == "shooting") {
        if (!is.null(descriptor) && grepl("2 FT", descriptor)) return("2") 
        if (!is.null(descriptor) && grepl("3 FT", descriptor)) return("3")
        return("2")  # Default to 2 FT if not specified
      }
      # Offensive fouls
      if (sub_type == "offensive") return("4")
      # Loose ball fouls
      if (sub_type == "loose ball") return("5")
      # Away from play
      if (sub_type == "away from play") return("6")
      # Inbound foul
      if (sub_type == "inbound") return("7")
      # Clear path
      if (sub_type == "clear path") return("9")
      # Flagrant fouls
      if (sub_type == "flagrant" && grepl("1", descriptor)) return("15")
      if (sub_type == "flagrant" && grepl("2", descriptor)) return("16")
      # Technical fouls
      if (sub_type == "technical") return("11")
    }
    
    # Violation mappings
    if (action_type == "violation") {
      # Defensive 3 seconds
      if (sub_type == "defensive 3 second") return("1")
      # Delay of game
      if (sub_type == "delay of game") return("5")
      # Lane violation
      if (sub_type == "lane") return("3")
      # Kicked ball
      if (sub_type == "kicked ball") return("4")
      # Goaltending
      if (sub_type == "goaltending") return("2")
    }
    
    # Turnover mappings - just mapping lost ball and bad pass for live ball turnovers
    if (action_type == "turnover") {
      # bad pass
      if (sub_type == "bad pass") return("1")
      # bad pass
      if (sub_type == "badpass") return("1")
      # lost ball
      if (sub_type == "lost ball") return("2")
      # lost ball
      if (sub_type == "lostball") return("2")
    }
    
    # Default return value if no specific mapping
    return(NA)
  }
  
  # Function to handle player information from the NBA Live API format
  processPlayerInfo <- function(action) {
    if (is.null(action)) {
      return(list(
        pt1 = "", pid1 = NA, pnm1 = NA, tid1 = NA, cty1 = NA, nn1 = NA, ts1 = NA,
        pt2 = "", pid2 = NA, pnm2 = NA, tid2 = NA, cty2 = NA, nn2 = NA, ts2 = NA,
        pt3 = "", pid3 = NA, pnm3 = NA, tid3 = NA, cty3 = NA, nn3 = NA, ts3 = NA
      ))
    }
    
    # Initialize with empty/NA values
    result <- list(
      pt1 = "", pid1 = action$personId %||% NA, pnm1 = action$playerName %||% NA, 
      tid1 = action$teamId %||% NA, cty1 = action$teamTricode %||% NA, nn1 = NA, ts1 = NA,
      
      pt2 = "", 
      pid2 = action$assistPersonId %||% action$blockPersonId %||% action$stealPersonId %||% 
        action$foulDrawnPersonId %||% action$jumpBallWonPersonId %||% NA, 
      pnm2 = action$assistPlayerNameInitial %||% action$blockPlayerName %||% action$stealPlayerName %||% 
        action$foulDrawnPlayerName %||% action$jumpBallWonPlayerName %||% NA,
      tid2 = NA, cty2 = NA, nn2 = NA, ts2 = NA,
      
      pt3 = "", pid3 = NA, pnm3 = NA, tid3 = NA, cty3 = NA, nn3 = NA, ts3 = NA
    )
    
    return(result)
  }
  # Simplified version of the ltd function assumed to be used in the original pf function
  # This is used to process each row in the NBA Live API structure
  ltd <- function(row) {
    # Create a standardized row representation from the NBA Live API action
    # This is a simplified version assuming row is already processed
    return(row)
  }
  
  # Function to extract detailed play information from NBA Live API
  extractPlayDetails <- function(action) {
    result <- list()
    
    # Action type and sub-type
    result$actionType <- action$actionType
    result$subType <- action$subType
    
    # Shot details
    if (action$actionType %in% c("2pt", "3pt")) {
      result$shotType <- action$subType
      result$shotResult <- action$shotResult
      result$shotDistance <- action$shotDistance
      result$area <- action$area
      
      # Check for assists, blocks
      result$assisted <- !is.null(action$assistPersonId)
      result$blocked <- !is.null(action$blockPersonId)
    }
    
    # Rebound details
    if (action$actionType == "rebound") {
      result$reboundType <- action$subType
      result$teamRebound <- is.null(action$personId) || action$personId == 0
    }
    
    # Turnover details
    if (action$actionType == "turnover") {
      result$turnoverType <- action$subType
      result$stolen <- !is.null(action$stealPersonId)
    }
    
    # Foul details
    if (action$actionType == "foul") {
      result$foulType <- action$subType
      result$foulDrawn <- !is.null(action$foulDrawnPersonId)
    }
    
    return(result)
  }
  
  # extract play by play data #
  pf <- function(x, sub = FALSE, gmtm = FALSE) {
    # Extract the game ID and actions
    game_id <- x$game$gameId
    actions <- x$game$actions
    
    # If there are no actions, return NULL
    if (length(actions) == 0) {
      return(NULL)
    }
    
    # Print the game ID for debugging
    if (!is.null(game_id)) {
      print(game_id)
      flush.console()
    }
    
    # Determine home and away team IDs and tricodes from the data - safely
    # Extract team IDs safely
    teams <- c()
    for (a in actions) {
      if (is.list(a) && !is.null(a$teamId)) {
        teams <- c(teams, a$teamId)
      }
    }
    teams <- unique(teams)
    
    # Extract team tricodes safely
    team_tricodes <- c()
    for (a in actions) {
      if (is.list(a) && !is.null(a$teamTricode)) {
        team_tricodes <- c(team_tricodes, a$teamTricode)
      }
    }
    team_tricodes <- unique(team_tricodes)
    
    # We'll assume the first two unique team IDs correspond to home and away
    # Generally, in NBA data, home team is listed second
    if (length(teams) >= 2) {
      away_team <- teams[1]
      home_team <- teams[2]
    } else {
      away_team <- NA
      home_team <- NA
    }
    
    # FIXED: Use AWAY_TEAM_ID instead of VISITOR_TEAM_ID (new API uses AWAY_TEAM_ID)
    away_team <- rg.df$AWAY_TEAM_ID[rg.df$GAME_ID==game_id]
    home_team <- rg.df$HOME_TEAM_ID[rg.df$GAME_ID==game_id]
    
    # Handle cases where team lookup fails
    if (length(away_team) == 0) away_team <- NA
    if (length(home_team) == 0) home_team <- NA
    
    # Same for tricodes
    if (length(team_tricodes) >= 2) {
      away_tricode <- team_tricodes[1]
      home_tricode <- team_tricodes[2]
    } else {
      away_tricode <- NA
      home_tricode <- NA
    }
    
    # Process each action to extract relevant information safely
    processed_actions <- list()
    
    for (i in 1:length(actions)) {
      action <- actions[[i]]
      
      # Skip if not a properly structured action
      if (!is.list(action) || is.null(action$actionNumber)) {
        next
      }
      
      # Extract values safely
      action_number <- action$actionNumber %||% NA
      period <- action$period %||% NA
      time_actual <- action$timeActual %||% NA
      clock <- action$clock %||% NA
      score_away <- action$scoreAway %||% "0"
      score_home <- action$scoreHome %||% "0"
      description <- action$description %||% NA
      team_id <- action$teamId %||% NA
      subType <- action$subType %||% NA
      descriptor <- action$descriptor %||% NA
      
      # Calculate game state values
      score_str <- paste(score_away, score_home, sep = "-")
      margin <- as.numeric(score_home) - as.numeric(score_away)
      
      # Basic properties
      result <- list(
        gid = game_id,
        rid = action_number,
        emt = mapActionTypeToEmt(action),
        emat = mapSubTypeToEmat(action),
        q = period,
        ts = time_actual,
        gtc = parseClock(clock),
        scr = score_str,
        mgn = margin,
        subType = subType,
        descriptor = descriptor
      )
      
      # Team-specific properties with safer NA handling
      is_home <- !is.null(team_id) && !is.na(team_id) && !is.na(home_team) && team_id == home_team
      is_away <- !is.null(team_id) && !is.na(team_id) && !is.na(away_team) && team_id == away_team
      
      # Description fields based on team - use isTRUE() for safer logical checks
      result$d0 <- if (is.null(team_id) || is.na(team_id)) description else NA
      result$d1 <- if (isTRUE(is_home)) description else NA
      result$d2 <- if (isTRUE(is_away)) description else NA
      result$d <- description
      
      # Player information
      player_info <- processPlayerInfo(action)
      result <- c(result, player_info)
      
      processed_actions[[length(processed_actions) + 1]] <- result
    }
    
    # If no actions were processed, return NULL
    if (length(processed_actions) == 0) {
      return(NULL)
    }
    
    # Create a data frame from the processed actions - safely
    # First, ensure all lists have the same structure by adding NA for missing elements
    all_names <- unique(unlist(lapply(processed_actions, names)))
    
    processed_actions <- lapply(processed_actions, function(action) {
      missing_names <- setdiff(all_names, names(action))
      if (length(missing_names) > 0) {
        for (name in missing_names) {
          action[[name]] <- NA
        }
      }
      return(action)
    })
    
    # Now try to create a data frame
    p <- tryCatch({
      do.call(rbind.data.frame, lapply(processed_actions, function(action) {
        # Safely convert to a row vector
        values <- lapply(all_names, function(name) {
          val <- action[[name]]
          if (is.null(val)) NA else val
        })
        names(values) <- all_names
        as.data.frame(values, stringsAsFactors = FALSE)
      }))
    }, error = function(e) {
      # If there's an error, create manually with basic structure
      df <- data.frame(
        gid = rep(game_id, length(processed_actions)),
        rid = sapply(processed_actions, function(a) a$rid %||% NA),
        emt = sapply(processed_actions, function(a) a$emt %||% NA),
        emat = sapply(processed_actions, function(a) a$emat %||% NA),
        stringsAsFactors = FALSE
      )
      return(df)
    })
    
    # Make sure we have the essential columns
    if (!"pt1" %in% names(p)) p$pt1 <- ""
    if (!"pt2" %in% names(p)) p$pt2 <- ""
    
    # Convert data types
    p$emt <- as.character(p$emt)
    p$emat <- as.character(p$emat)
    
    # Create a new row index
    p$r <- 1:nrow(p)
    
    # Convert variable types
    p$rid <- as.numeric(as.character(p$rid))
    p$emt <- as.numeric(as.character(p$emt))
    p$emat <- as.numeric(as.character(p$emat))
    p$q <- as.numeric(as.character(p$q))
    p$pid1 <- as.numeric(as.character(p$pid1))
    p$pid2 <- as.numeric(as.character(p$pid2))
    p$pid3 <- as.numeric(as.character(p$pid3))
    p$d1 <- as.character(p$d1)
    p$d2 <- as.character(p$d2)
    p$d0 <- as.character(p$d0)
    p$d <- as.character(p$d)
    
    # Create game time
    p$min <- as.numeric(as.character(substr(p$gtc, 1, regexpr(":", p$gtc) - 1)))
    p$sec <- as.numeric(as.character(substr(p$gtc, regexpr(":", p$gtc) + 1, 100)))
    p$gt <- ifelse(p$q >= 5, 
                   4 * 12 * 60 + (p$q - 5) * 5 * 60 + (4 - p$min) * 60 + (60 - p$sec), 
                   (p$q - 1) * 12 * 60 + (11 - p$min) * 60 + (60 - p$sec))
    
    # Calculate timestamp
    ts1 <- substr(p$ts, 1, regexpr("T", p$ts) - 1)
    ts2 <- substr(p$ts, regexpr("T", p$ts) + 1, regexpr("\\.", p$ts) - 1)
    tsh <- as.numeric(substr(ts2, 1, regexpr(":", ts2) - 1))
    tsm <- as.numeric(substr(ts2, regexpr(":", ts2) + 1, regexpr(":", ts2, fixed = TRUE) + 2))
    gmst <- ts1[1]
    p$tsv <- ifelse(tsh >= 12, tsh - 12, tsh) * 60 + tsm + ifelse(gmst != ts1, 60 * 12, 0)
    p$tsv <- ifelse(p$tsv < p$tsv[1], p$tsv + 720, p$tsv)
    
    # Calculate real time down to seconds - simple method
    bck <- aggregate(rep(1, nrow(p)), by = list(p$tsv), FUN = sum)
    p$rep <- ave(c(1:nrow(p)), p$tsv, FUN = rank)
    if (nrow(bck) == 0) {
      p$nmb <- NA
    } else {
      p$nmb <- bck$x[match(p$tsv, bck$Group.1)]
    }
    p$frc <- p$rep / (p$nmb + 1)
    # Check for next minute
    ntsv <- c(p$tsv[2:nrow(p)], p$tsv[nrow(p)])
    p$nxm <- ntsv - p$tsv
    p$tsv <- p$tsv + round(p$frc, 2)
    ntsv <- c(p$tsv[2:nrow(p)], p$tsv[nrow(p)])
    p$nxs <- ntsv - p$tsv
    p$rep <- p$frc <- p$nmb <- NULL
    
    # Calculate score
    scr <- p[c("r", "scr")]
    scr$a <- as.numeric(substr(scr$scr, 1, regexpr("-", scr$scr) - 1))
    scr$h <- as.numeric(substr(scr$scr, regexpr("-", scr$scr) + 1, 100))
    scr <- scr[!is.na(scr$a), ]
    scr <- scr[c("r", "a", "h")]
    scr <- rbind(c(0, 0, 0), scr)
    p$a <- scr$a[findInterval(p$r, scr$r)]
    p$h <- scr$h[findInterval(p$r, scr$r)]
    p$m <- p$h - p$a
    p$mc <- p$m - c(0, p$m[1:(nrow(p) - 1)])
    # Get finals scores
    af <- p$a[nrow(p)]
    hf <- p$h[nrow(p)]
    
    # Output game time file
    if (gmtm) {
      pt <- p[c("gid", "r", "rid", "q", "gtc", "min", "sec", "gt", "ts", "tsv", "nxm", "nxs", "a", "h", "m", "mc", "emt", "emat", "d1", "d0", "d2")]
      pt$d <- ifelse(!is.na(pt$d1), pt$d1, ifelse(!is.na(pt$d2), pt$d2, pt$d0))
      pt$di <- ifelse(!is.na(pt$d1) & regexpr("BLOCK", pt$d1) < 0 & regexpr("STEAL", pt$d1) < 0, "h", 
                      ifelse(!is.na(pt$d2) & regexpr("BLOCK", pt$d2) < 0 & regexpr("STEAL", pt$d2) < 0, "a", "n"))
      pt$d1 <- pt$d0 <- pt$d2 <- NULL
      # Summarize by minute
      mnt.lst <- vector('list')
      if (all(is.na(pt$tsv))) {
        return(NULL)
      }
      for (sq in 1:max(pt$q)) {
        pq <- pt[pt$q == sq, ]
        if (nrow(pq) == 0) next
        if (length(unique(pq$gt))==1) next
        tfun <- approxfun(pq$gt, pq$tsv)
        gtv <- seq(0, 720, 60) + (sq - 1) * 720
        if (sq > 4) {
          gtv <- seq(0, 300, 60) + (sq - 5) * 300 + 2880
        }
        tmp <- tfun(gtv)
        tdf <- tmp[2:length(tmp)] - tmp[1:(length(tmp) - 1)]
        ml <- (gtv / 60)[2:length(gtv)]
        tdf <- cbind(sq, ml, tdf)
        mnt.lst[[sq]] <- tdf
      }
      if(length(mnt.lst) == 0) return(NULL)
      tdf <- do.call('rbind', mnt.lst)
      tdf <- data.frame(tdf)
      # Get quarter lengths
      ql <- aggregate(tdf$tdf, by = list(tdf$sq), FUN = sum)
      colnames(ql) <- c("q", "qt")
      # Get quarter breaks - assign sq=0
      qb <- pt[pt$emt == 13 & pt$q < max(pt$q), c("q", "nxs")]
      gid <- pt$gid[1]
      tdf <- cbind(gid, tdf)
      spc <- pt[pt$emt %in% c(9, 18), c("gid", "r", "q", "gtc", "gt", "nxs", "emt", "emat", "di")]
      ptl <- vector('list')
      ptl[["pt"]] <- pt
      ptl[["tdf"]] <- tdf
      ptl[["spc"]] <- spc
      ptl[["ql"]] <- ql
      ptl[["qb"]] <- qb
      return(ptl)
    }
    
    # Filter out unneeded rows
    maxq <- max(p$q)
    if (sub == TRUE) {
      sa <- p[p$emt == 8 & !is.na(p$d2), c("gid", "r", "q", "gt", "d2", "pid1", "pnm1", "pid2", "pnm2")]
      sh <- p[p$emt == 8 & !is.na(p$d1), c("gid", "r", "q", "gt", "d1", "pid1", "pnm1", "pid2", "pnm2")]
      colnames(sa) <- colnames(sh) <- c("gid", "r", "q", "gt", "d", "pid1", "pnm1", "pid2", "pnm2")
      slst <- vector('list')
      slst[["a"]] <- sa
      slst[["h"]] <- sh
      return(slst)
    }
    
    # Filter out unneeded events
    p <- p[p$emt != 8, ] # substitution
    p <- p[p$emt != 9, ] # timeout
    #p <- p[!(p$emt == 13 & p$q < maxq), ] # end of period
    p <- p[p$emt != 18, ] # instant replay
    # delete goaltending, lane violation and jump ball violation
    p <- p[!(p$emt == 7 & p$emat == 2), ]
    p <- p[!(p$emt == 7 & p$emat == 3), ]
    p <- p[!(p$emt == 7 & p$emat == 4), ]
    # delete offensive foul since it is always accompanied by a turnover
    p <- p[!(p$emt == 6 & p$emat == 4), ]
    p <- p[!(p$emt == 6 & p$emat == 26), ]
    
    # Play description
    p$d <- ifelse(!is.na(p$d1) & regexpr("BLOCK", p$d1) < 0 & regexpr("STEAL", p$d1) < 0, 
                  as.character(p$d1), 
                  ifelse(!is.na(p$d2) & regexpr("BLOCK", p$d2) < 0 & regexpr("STEAL", p$d2) < 0, 
                         as.character(p$d2), 
                         as.character(p$d0)))
    
    p$di <- ifelse(!is.na(p$d1) & regexpr("BLOCK", p$d1) < 0 & regexpr("STEAL", p$d1) < 0, 
                   "h", 
                   ifelse(!is.na(p$d2) & regexpr("BLOCK", p$d2) < 0 & regexpr("STEAL", p$d2) < 0, 
                          "a", 
                          "n"))
    
    p$da <- ifelse(regexpr("BLOCK", p$d1) >= 0 | regexpr("STEAL", p$d1) >= 0, 
                   as.character(p$d1), 
                   ifelse(regexpr("BLOCK", p$d2) >= 0 | regexpr("STEAL", p$d2) >= 0, 
                          as.character(p$d2), 
                          ""))
    
    p$dai <- ifelse(regexpr("BLOCK", p$d1) >= 0 | regexpr("STEAL", p$d1) >= 0, 
                    "h", 
                    ifelse(regexpr("BLOCK", p$d2) >= 0 | regexpr("STEAL", p$d2) >= 0, 
                           "a", 
                           ""))
    
    # Define play types
    p$pt1 <- ""
    p$pt2 <- ""
    
    # Prior di indicator
    dip <- c("", p$di[1:(nrow(p) - 1)])
    
    # Next emt
    nemt <- c(p$emt[2:nrow(p)], "")
    
    # Next emat
    nemat <- c(p$emat[2:nrow(p)], "")
    
    # Set play types based on event types
    # Jump ball
    p$pt1[p$emt == 10] <- "JB"
    
    # Beginning and end of quarters
    p$pt1 <- ifelse(p$emt == 12, paste("Q", p$q, sep = ""), p$pt1)
    p$pt1 <- ifelse(p$emt == 13, "EG", p$pt1)
    
    # Made field goals
    p$pt1[p$emt == 1 & regexpr("3PT", p$d) < 0] <- "MK2"
    p$pt1[p$emt == 1 & regexpr("3PT", p$d) >= 0] <- "MK3"
    
    # Missed field goals
    p$pt1[p$emt == 2 & regexpr("3PT", p$d) < 0] <- "MS2"
    p$pt1[p$emt == 2 & regexpr("3PT", p$d) >= 0] <- "MS3"
    
    # Turnover
    p$pt1[p$emt == 5] <- "TO"
    
    # Rebounds
    p$pt1[p$emt == 4 & p$di != dip] <- "DRB"
    p$pt1[p$emt == 4 & p$di == dip] <- "ORB"
    if (!is.null(p$subType)) {
      p$pt1[p$emt == 4 & p$subType == "defensive"] <- "DRB"
      p$pt1[p$emt == 4 & p$subType == "offensive"] <- "ORB"
    }
    
    # Free throws
    p$pt1[p$emt == 3 & p$emat == 10 & regexpr("MISS", p$d) < 0] <- "MK11"
    p$pt1[p$emt == 3 & p$emat == 10 & regexpr("MISS", p$d) >= 0] <- "MS11"
    p$pt1[p$emt == 3 & p$emat == 11 & regexpr("MISS", p$d) < 0] <- "MK12"
    p$pt1[p$emt == 3 & p$emat == 11 & regexpr("MISS", p$d) >= 0] <- "MS12"
    p$pt1[p$emt == 3 & p$emat == 12 & regexpr("MISS", p$d) < 0] <- "MK22"
    p$pt1[p$emt == 3 & p$emat == 12 & regexpr("MISS", p$d) >= 0] <- "MS22"
    p$pt1[p$emt == 3 & p$emat == 13 & regexpr("MISS", p$d) < 0] <- "MK13"
    p$pt1[p$emt == 3 & p$emat == 13 & regexpr("MISS", p$d) >= 0] <- "MS13"
    p$pt1[p$emt == 3 & p$emat == 14 & regexpr("MISS", p$d) < 0] <- "MK23"
    p$pt1[p$emt == 3 & p$emat == 14 & regexpr("MISS", p$d) >= 0] <- "MS23"
    p$pt1[p$emt == 3 & p$emat == 15 & regexpr("MISS", p$d) < 0] <- "MK33"
    p$pt1[p$emt == 3 & p$emat == 15 & regexpr("MISS", p$d) >= 0] <- "MS33"
    p$pt1[p$emt == 3 & p$emat == 16 & regexpr("MISS", p$d) < 0] <- "MKTF"
    p$pt1[p$emt == 3 & p$emat == 16 & regexpr("MISS", p$d) >= 0] <- "MSTF"
    p$pt1[p$emt == 3 & p$emat == 17 & regexpr("MISS", p$d) < 0] <- "MKCP"
    p$pt1[p$emt == 3 & p$emat == 17 & regexpr("MISS", p$d) >= 0] <- "MSCP"
    
    # Fouls
    p$pt1[p$emt == 6 & nemt != 3] <- "DF"
    p$pt1[p$emt == 6 & nemt == 3 & nemat == 10] <- "DF1"
    p$pt1[p$emt == 6 & nemt == 3 & nemat == 11] <- "DF2"
    p$pt1[p$emt == 6 & nemt == 3 & nemat == 13] <- "DF3"
    p$pt1[p$emt == 6 & p$emat == 9] <- "CPF" # clear path
    p$pt1[p$emt == 6 & p$emat == 14] <- "CPF" # clear path
    p$pt1[p$emt == 6 & p$emat == 16] <- "FLG" # flagrant foul
    
    # Technical fouls
    p$pt1[p$emt == 6 & p$emat == 11] <- "TF"
    p$pt1[p$emt == 6 & p$emat == 13] <- "TF"
    p$pt1[p$emt == 6 & p$emat == 17] <- "TF"
    p$pt1[p$emt == 6 & p$emat == 19] <- "TF"
    
    # Violations
    p$pt1[p$emt == 7] <- "VIO"
    
    # Add assist indicator
    p$pt2[(p$pt1 == "MK2" | p$pt1 == "MK3") & regexpr("AST", p$d) > 0] <- "AST"
    
    # Add block indicator
    p$pt2[regexpr("blocked", p$d) >= 0] <- "BLK"
    
    # Add steal indicator
    p$pt2[p$pt1=="TO" & !is.na(p$pid2)] <- "STL"
    
    # Delete rebounds from non-terminal and technical free throws
    ftd <- p$pt1 %in% c("MS12", "MS13", "MS23", "MSTF", "MSCP", "MSF12", "MSF22", "MSC12", "MSC22")
    nftd <- c(FALSE, ftd[1:(length(ftd) - 1)])
    p <- p[!(nftd & p$pt1 %in% c("ORB", "DRB")), ]
    
    # Define game state
    p$gst1 <- ""
    p$gst2 <- ""
    p$gst1 <- ifelse(p$emt == 12, paste("Q", p$q, sep = ""), p$gst1)
    p$gst1 <- ifelse(p$emt == 13, paste("EQ", p$q, sep = ""), p$gst1)
    # get game status #
    game_status <- rg.df$GAME_STATUS_ID[rg.df$GAME_ID==game_id]
    p$gst1 <- ifelse(p$emt == 13 & p$q == maxq & game_status=="3", "EG", p$gst1)
    # add this to capture end game situations - currently defaulting to OT game state
    p$gst1 <- ifelse(p$emt == 13 & p$q >= 4 & p$m != 0, "EG", p$gst1)
    
    # Determine who possessed jump ball
    pdi <- c(p$di[2:nrow(p)], "")
    p$gst1[p$pt1 == "JB" & pdi == "a"] <- "A"
    p$gst1[p$pt1 == "JB" & pdi == "h"] <- "H"
    p$gst2[p$pt1 == "JB"] <- "P"
    
    # Game state after made shot
    p$gst1[(p$pt1 == "MK2" | p$pt1 == "MK3") & p$di == "a"] <- "H"
    p$gst1[(p$pt1 == "MK2" | p$pt1 == "MK3") & p$di == "h"] <- "A"
    p$gst2[(p$pt1 == "MK2" | p$pt1 == "MK3")] <- "P"
    
    # Game state after missed shot
    p$gst1[(p$pt1 == "MS2" | p$pt1 == "MS3") & p$di == "a"] <- "A"
    p$gst1[(p$pt1 == "MS2" | p$pt1 == "MS3") & p$di == "h"] <- "H"
    p$gst2[(p$pt1 == "MS2" | p$pt1 == "MS3")] <- "MFG"
    
    # Game state after rebound
    p$gst1[(p$pt1 == "DRB" | p$pt1 == "ORB") & p$di == "a"] <- "A"
    p$gst1[(p$pt1 == "DRB" | p$pt1 == "ORB") & p$di == "h"] <- "H"
    p$gst2[(p$pt1 == "DRB" | p$pt1 == "ORB")] <- "P"
    
    # Game state after turnover
    p$gst1[p$pt1 == "TO" & p$di == "h"] <- "A"
    p$gst1[p$pt1 == "TO" & p$di == "a"] <- "H"
    p$gst2[p$pt1 == "TO"] <- "P"
    
    # Game state after drawn foul with turnovers
    p$gst1[(p$pt1 == "DF1" | p$pt1 == "DF2" | p$pt1 == "DF3" | p$pt1 == "CPF" | p$pt1 == "FLG") & p$di == "h"] <- "A"
    p$gst1[(p$pt1 == "DF1" | p$pt1 == "DF2" | p$pt1 == "DF3" | p$pt1 == "CPF" | p$pt1 == "FLG") & p$di == "a"] <- "H"
    p$gst2[p$pt1 == "DF1"] <- "FT1"
    p$gst2[p$pt1 == "DF2"] <- "FT2"
    p$gst2[p$pt1 == "DF3"] <- "FT3"
    p$gst2[p$pt1 == "CPF"] <- "FT2R"
    p$gst2[p$pt1 == "FLG"] <- "FT2R"
    
    # Free throw game states
    p$gst1[(p$pt1 == "MK11" | p$pt1 == "MK22" | p$pt1 == "MK33") & p$di == "h"] <- "A"
    p$gst1[(p$pt1 == "MK11" | p$pt1 == "MK22" | p$pt1 == "MK33") & p$di == "a"] <- "H"
    p$gst2[(p$pt1 == "MK11" | p$pt1 == "MK22" | p$pt1 == "MK33")] <- "P"
    p$gst1[(p$pt1 == "MS11" | p$pt1 == "MS22" | p$pt1 == "MS33") & p$di == "a"] <- "A"
    p$gst1[(p$pt1 == "MS11" | p$pt1 == "MS22" | p$pt1 == "MS33") & p$di == "h"] <- "H"
    p$gst2[(p$pt1 == "MS11" | p$pt1 == "MS22" | p$pt1 == "MS33")] <- "MFT"
    p$gst1[(p$pt1 == "MK12" | p$pt1 == "MS12" | p$pt1 == "MK23" | p$pt1 == "MS23") & p$di == "a"] <- "A"
    p$gst1[(p$pt1 == "MK12" | p$pt1 == "MS12" | p$pt1 == "MK23" | p$pt1 == "MS23") & p$di == "h"] <- "H"
    p$gst2[(p$pt1 == "MK12" | p$pt1 == "MS12" | p$pt1 == "MK23" | p$pt1 == "MS23")] <- "FT1"
    p$gst1[(p$pt1 == "MK13" | p$pt1 == "MS13") & p$di == "a"] <- "A"
    p$gst1[(p$pt1 == "MK13" | p$pt1 == "MS13") & p$di == "h"] <- "H"
    p$gst2[(p$pt1 == "MK13" | p$pt1 == "MS13")] <- "FT2"
    
    # Non-shooting foul game states
    p$gst1[p$pt1 == "DF" & p$di == "h"] <- "A"
    p$gst1[p$pt1 == "DF" & p$di == "a"] <- "H"
    p$gst2[p$pt1 == "DF"] <- "P"
    
    # Violations
    p$gst1[p$pt1 == "VIO" & p$di == "h"] <- "A"
    p$gst1[p$pt1 == "VIO" & p$di == "a"] <- "H"
    p$gst2[p$pt1 == "VIO"] <- "P"
    
    # Technical foul handling
    p$tech <- 0 # negative indicates away team has tech foul to shoot
    p$tech[p$pt1 == "TF" & p$di == "h"] <- 1
    p$tech[p$pt1 == "TF" & p$di == "a"] <- -1
    
    # Create output file
    pwp <- p[c("gid", "rid", "r", "ts", "tsv", "nxm", "nxs", "q", "min", "sec", "gt", "a", "h", "m", "d", "di", "da", "dai", "subType", "descriptor",
               "emt", "emat", "gst1", "gst2", "pt1", "pt2", "tech", "pid1", "pnm1", "pid2", "pnm2", "pid3", "pnm3")]
    
    # Add betting line and totals if available
    if (!is.null(gm) && !is.null(tm.lng) && !is.null(tm.sht) && !is.null(nba)) {
      aw <- gm$tm1[as.character(gm$GAME_ID) == as.character(pwp$gid[1])]
      hm <- gm$tm2[as.character(gm$GAME_ID) == as.character(pwp$gid[1])]
      away <- tm.lng[match(aw, tm.sht)]
      home <- tm.lng[match(hm, tm.sht)]
      date <- gm$GAME_DATE[as.character(gm$GAME_ID) == as.character(pwp$gid[1])]
      date <- substr(date, 1, 10)
      date <- gsub("-", "", date)
      line <- nba$line[nba$dtc == date & nba$team == home & nba$to.team == away]
      total <- nba$total[nba$dtc == date & nba$team == home & nba$to.team == away]
      if (length(line) == 0 || (length(line) > 0 && is.na(line))) {
        line <- dline
      }
      if (length(total) == 0 || (length(total) > 0 && is.na(total))) {
        total <- dtotal
      }
    } else {
      line <- dline
      total <- dtotal
    }
    
    pwp$line <- line
    pwp$total <- total
    
    # Add results
    pwp$fm <- pwp$h[nrow(pwp)] - pwp$a[nrow(pwp)]
    pwp$ft <- pwp$h[nrow(pwp)] + pwp$a[nrow(pwp)]
    
    return(pwp)
  }
  # create cleaned pbp files #
  pbp.lst<-lapply(pbprg.lst,pf)
  # create cleaned sub files #
  sub.lst<-lapply(pbprg.lst,pf,sub=TRUE)
  # create game time files #
  gmt.lst<-lapply(pbprg.lst,pf,gmtm=TRUE)
  # extract box score data #
  bf <- function(x) {
    # Extract game ID
    gid <- x$game$gameId
    
    # Extract away team player stats
    away_players <- x$game$awayTeam$players
    if (is.null(away_players) || length(away_players) == 0) {
      return(NULL)
    }
    
    # Get away team info
    tid_away <- x$game$awayTeam$teamId
    tm_away <- x$game$awayTeam$teamTricode
    city_away <- x$game$awayTeam$teamCity
    
    # Process away team players
    away_list <- list()
    for (i in seq_along(away_players)) {
      player <- away_players[[i]]
      
      # Skip if player didn't play
      if (is.null(player$played) || player$played == "0") {
        next
      }
      
      # Extract statistics safely
      stats <- player$statistics
      
      # Parse minutes from ISO 8601 duration format (PT28M26.10S)
      minutes_str <- stats$minutes %||% "PT00M00.00S"
      # Extract minutes and seconds
      min_match <- regmatches(minutes_str, regexpr("[0-9]+M", minutes_str))
      sec_match <- regmatches(minutes_str, regexpr("[0-9.]+S", minutes_str))
      
      min_val <- if (length(min_match) > 0) as.numeric(gsub("M", "", min_match)) else 0
      sec_val <- if (length(sec_match) > 0) as.numeric(gsub("S", "", sec_match)) else 0
      
      player_row <- data.frame(
        gid = gid,
        tid = tid_away,
        tm = tm_away,
        city = city_away,
        tmst = "away",
        pid = player$personId %||% NA,
        pnm = player$name %||% NA,
        spos = player$position %||% "",
        min = min_val,
        sec = sec_val,
        fgm = stats$fieldGoalsMade %||% 0,
        fga = stats$fieldGoalsAttempted %||% 0,
        fg3m = stats$threePointersMade %||% 0,
        fg3a = stats$threePointersAttempted %||% 0,
        ftm = stats$freeThrowsMade %||% 0,
        fta = stats$freeThrowsAttempted %||% 0,
        orb = stats$reboundsOffensive %||% 0,
        drb = stats$reboundsDefensive %||% 0,
        reb = stats$reboundsTotal %||% 0,
        ast = stats$assists %||% 0,
        stl = stats$steals %||% 0,
        blk = stats$blocks %||% 0,
        tovr = stats$turnovers %||% 0,
        pf = stats$foulsPersonal %||% 0,
        pts = stats$points %||% 0,
        pm = stats$plusMinusPoints %||% 0,
        stringsAsFactors = FALSE
      )
      
      away_list[[length(away_list) + 1]] <- player_row
    }
    
    # Extract home team player stats
    home_players <- x$game$homeTeam$players
    if (is.null(home_players) || length(home_players) == 0) {
      return(NULL)
    }
    
    # Get home team info
    tid_home <- x$game$homeTeam$teamId
    tm_home <- x$game$homeTeam$teamTricode
    city_home <- x$game$homeTeam$teamCity
    
    # Process home team players
    home_list <- list()
    for (i in seq_along(home_players)) {
      player <- home_players[[i]]
      
      # Skip if player didn't play
      if (is.null(player$played) || player$played == "0") {
        next
      }
      
      # Extract statistics safely
      stats <- player$statistics
      
      # Parse minutes from ISO 8601 duration format (PT28M26.10S)
      minutes_str <- stats$minutes %||% "PT00M00.00S"
      # Extract minutes and seconds
      min_match <- regmatches(minutes_str, regexpr("[0-9]+M", minutes_str))
      sec_match <- regmatches(minutes_str, regexpr("[0-9.]+S", minutes_str))
      
      min_val <- if (length(min_match) > 0) as.numeric(gsub("M", "", min_match)) else 0
      sec_val <- if (length(sec_match) > 0) as.numeric(gsub("S", "", sec_match)) else 0
      
      player_row <- data.frame(
        gid = gid,
        tid = tid_home,
        tm = tm_home,
        city = city_home,
        tmst = "home",
        pid = player$personId %||% NA,
        pnm = player$name %||% NA,
        spos = player$position %||% "",
        min = min_val,
        sec = sec_val,
        fgm = stats$fieldGoalsMade %||% 0,
        fga = stats$fieldGoalsAttempted %||% 0,
        fg3m = stats$threePointersMade %||% 0,
        fg3a = stats$threePointersAttempted %||% 0,
        ftm = stats$freeThrowsMade %||% 0,
        fta = stats$freeThrowsAttempted %||% 0,
        orb = stats$reboundsOffensive %||% 0,
        drb = stats$reboundsDefensive %||% 0,
        reb = stats$reboundsTotal %||% 0,
        ast = stats$assists %||% 0,
        stl = stats$steals %||% 0,
        blk = stats$blocks %||% 0,
        tovr = stats$turnovers %||% 0,
        pf = stats$foulsPersonal %||% 0,
        pts = stats$points %||% 0,
        pm = stats$plusMinusPoints %||% 0,
        stringsAsFactors = FALSE
      )
      
      home_list[[length(home_list) + 1]] <- player_row
    }
    
    # Combine away and home players
    if (length(away_list) == 0 && length(home_list) == 0) {
      return(NULL)
    }
    
    b <- rbind(
      if (length(away_list) > 0) do.call(rbind, away_list) else NULL,
      if (length(home_list) > 0) do.call(rbind, home_list) else NULL
    )
    
    # Ensure numeric columns
    numvar <- c("pid", "min", "sec", "fgm", "fga", "fg3m", "fg3a", "ftm", "fta", 
                "orb", "drb", "reb", "ast", "stl", "blk", "tovr", "pf", "pts", "pm")
    b[numvar] <- lapply(b[numvar], function(x) as.numeric(as.character(x)))
    b[is.na(b)] <- 0
    
    return(b)
  }
  # create cleaned box score files #
  box.lst<-lapply(boxrg.lst,bf)
  # null result function #
  nf<-function(x,df="",n=FALSE) {
    if (is.null(x)) {return(df)}
    if (n) {return(as.numeric(x))}
    return(x)
  }
  # extract game info file #
  gmf <- function(x) {
    g <- data.frame(matrix(nrow=1))
    colnames(g) <- "ssn"
    g$ssn <- ssn
    
    # NEW: Access game data from x$game instead of x$boxScoreSummary
    g$gid <- x$game$gameId
    g$dt <- substr(x$game$gameEt, 1, 10)
    g$status <- x$game$gameStatus
    g$per <- x$game$period
    
    gameClock <- x$game$gameClock
    # Parse ISO 8601 duration format (PT04M23.00S)
    min_match <- regmatches(gameClock, regexpr("[0-9]+M", gameClock))
    sec_match <- regmatches(gameClock, regexpr("[0-9]+\\.?[0-9]*S", gameClock))
    
    min_val <- if (length(min_match) > 0) gsub("M", "", min_match) else "00"
    sec_val <- if (length(sec_match) > 0) {
      sec_full <- gsub("S", "", sec_match)
      # Take integer part only
      floor(as.numeric(sec_full))
    } else {
      0
    }
    
    g$minsec <- sprintf("%02d:%02d", as.numeric(min_val), sec_val)
    
    # NEW: Broadcasters structure may be different - using safe pluck
    g$ntwk <- pluck(x, "game", "broadcasters", "nationalBroadcasters", 1, "broadcasterDisplay", .default=NA)
    
    # NEW: Team IDs are directly under homeTeam/awayTeam objects
    g$hid <- nf(x$game$homeTeam$teamId)
    g$aid <- nf(x$game$awayTeam$teamId)
    
    g$htm <- nf(x$game$homeTeam$teamTricode)
    g$hcty <- nf(x$game$homeTeam$teamCity)
    g$hnm <- nf(x$game$homeTeam$teamName)
    g$hscr <- nf(x$game$homeTeam$score)
    
    g$atm <- nf(x$game$awayTeam$teamTricode)
    g$acty <- nf(x$game$awayTeam$teamCity)
    g$anm <- nf(x$game$awayTeam$teamName)
    g$ascr <- nf(x$game$awayTeam$score)
    
    g$fm <- g$hscr - g$ascr
    g$hwin <- ifelse(g$hscr > g$ascr, 1, ifelse(g$hscr < g$ascr, 0, 0.5))
    g$att <- nf(x$game$attendance)
    g$gmtm <- nf(x$game$duration)
    
    g$hwl <- NA
    g$awl <- NA
    
    # NEW: Access periods from x$game$homeTeam$periods instead of x$boxScoreSummary$homeTeam$periods
    g$hq1 <- pluck(x, "game", "homeTeam", "periods", 1, "score", .default=NA)
    g$hq2 <- pluck(x, "game", "homeTeam", "periods", 2, "score", .default=NA)
    g$hq3 <- pluck(x, "game", "homeTeam", "periods", 3, "score", .default=NA)
    g$hq4 <- pluck(x, "game", "homeTeam", "periods", 4, "score", .default=NA)
    g$hot1 <- pluck(x, "game", "homeTeam", "periods", 5, "score", .default=NA)
    g$hot2 <- pluck(x, "game", "homeTeam", "periods", 6, "score", .default=NA)
    g$hot3 <- pluck(x, "game", "homeTeam", "periods", 7, "score", .default=NA)
    g$hot4 <- pluck(x, "game", "homeTeam", "periods", 8, "score", .default=NA)
    g$hot5 <- pluck(x, "game", "homeTeam", "periods", 9, "score", .default=NA)
    g$hot6 <- pluck(x, "game", "homeTeam", "periods", 10, "score", .default=NA)
    g$hot7 <- pluck(x, "game", "homeTeam", "periods", 11, "score", .default=NA)
    g$hot8 <- pluck(x, "game", "homeTeam", "periods", 12, "score", .default=NA)
    g$hot9 <- pluck(x, "game", "homeTeam", "periods", 13, "score", .default=NA)
    g$hot10 <- pluck(x, "game", "homeTeam", "periods", 14, "score", .default=NA)
    
    g$aq1 <- pluck(x, "game", "awayTeam", "periods", 1, "score", .default=NA)
    g$aq2 <- pluck(x, "game", "awayTeam", "periods", 2, "score", .default=NA)
    g$aq3 <- pluck(x, "game", "awayTeam", "periods", 3, "score", .default=NA)
    g$aq4 <- pluck(x, "game", "awayTeam", "periods", 4, "score", .default=NA)
    g$aot1 <- pluck(x, "game", "awayTeam", "periods", 5, "score", .default=NA)
    g$aot2 <- pluck(x, "game", "awayTeam", "periods", 6, "score", .default=NA)
    g$aot3 <- pluck(x, "game", "awayTeam", "periods", 7, "score", .default=NA)
    g$aot4 <- pluck(x, "game", "awayTeam", "periods", 8, "score", .default=NA)
    g$aot5 <- pluck(x, "game", "awayTeam", "periods", 9, "score", .default=NA)
    g$aot6 <- pluck(x, "game", "awayTeam", "periods", 10, "score", .default=NA)
    g$aot7 <- pluck(x, "game", "awayTeam", "periods", 11, "score", .default=NA)
    g$aot8 <- pluck(x, "game", "awayTeam", "periods", 12, "score", .default=NA)
    g$aot9 <- pluck(x, "game", "awayTeam", "periods", 13, "score", .default=NA)
    g$aot10 <- pluck(x, "game", "awayTeam", "periods", 14, "score", .default=NA)
    
    #print(g$gid)
    #flush.console()
    return(g)
  }
  # remove all star game #
  boxsmrg.lst<-boxsmrg.lst[substr(names(boxsmrg.lst),1,3)!='003']
  # create game files #
  boxsmrg.lst<-boxsmrg.lst[!is.na(names(boxsmrg.lst))]
  gm.lst<-lapply(boxsmrg.lst,gmf)
  
  # extract official information #
  off<-function(x) {
    ol<-x[["resultSets"]][[3]][["rowSet"]]
    if (length(ol)==0) {return(NULL)}
    ol<-lapply(ol,FUN=unlist)
    o<-do.call('rbind',ol)
    if (ncol(o)==3) {
      o<-cbind(o,"")
    }
    colnames(o)<-c("oid","ofnm","olnm","ojrs")
    gid<-nf(x[["parameters"]][["GameID"]])
    o<-cbind(gid,o)
    o<-data.frame(o)
    o$oid<-as.numeric(as.character(o$oid))
    return(o)
  }
  # create official files #
  off.lst<-lapply(boxsmrg.lst,off)
  
  # extract shot location files #
  shf<-function(x) {
    sh2<-x[["home"]][["resultSets"]][[1]][["rowSet"]]
    sh1<-x[["away"]][["resultSets"]][[1]][["rowSet"]]
    sh2<-lapply(sh2,unlist)
    sh1<-lapply(sh1,unlist)
    sh2<-data.frame(do.call('rbind',sh2))
    sh1<-data.frame(do.call('rbind',sh1))
    sh<-rbind(sh2,sh1)
    if (nrow(sh)==0) {return(NULL)}
    colnames(sh)<-c("grt","gid","rid","pid","pnm","tid","tnm","q","min","sec","etp","atp","shtp","szb","sza","szr","dst","lcx","lcy","sha","shm")
    sh$rid<-as.numeric(as.character(sh$rid))
    sh$q<-as.numeric(as.character(sh$q))
    sh$min<-as.numeric(as.character(sh$min))
    sh$sec<-as.numeric(as.character(sh$sec))
    sh$dst<-as.numeric(as.character(sh$dst))
    sh$lcx<-as.numeric(as.character(sh$lcx))
    sh$lcy<-as.numeric(as.character(sh$lcy))
    sh$sha<-as.numeric(as.character(sh$sha))
    sh$shm<-as.numeric(as.character(sh$shm))
    sh$gt<-ifelse(sh$q>=5,4*12*60+(sh$q-5)*5*60+(4-sh$min)*60+(60-sh$sec),(sh$q-1)*12*60+(11-sh$min)*60+(60-sh$sec))
    return(sh)
  }
  # create cleaned shot location files #
  if (po==1) {
    sh.lst<-lapply(shpo.lst,shf)
  } else {
    sh.lst<-lapply(shrg.lst,shf)
  }
  
  # assign win probability #
  load(file=paste(locn,"nbaWinProb.RData",sep=""))
  load(file=paste(locn,"qPrbLst.RData",sep=""))
  # derive distinct lines #
  dln<-unique(plst[[4]]$line)
  
  # load leverage files #
  load(file=paste(locl,"leverage/lvg.RData",sep=""))
  load(file=paste(locl,"leverage/clbck.RData",sep=""))
  load(file=paste(locl,"leverage/lvgmn.RData",sep=""))
  load(file=paste(locl,"leverage/lvgpct.RData",sep=""))
  
  # win probability functions #
  prbf<-function(x,ff=FALSE) {
    mgnc<-max(-30,min(30,x$m))
    line<-x$line[1]
    q<-x$q
    maxq<-min(4,q)
    if (ff) {line<-0}
    if (substr(x$gst1,1,1)=='Q') {
      q<-as.numeric(substr(x$gst1,2,2))
      q<-min(4,q)
      qprb<-qprb.lst[[q]]
      if (q==1 | q>=5) {
        qprb$qm<-0
        mgnc<-0}
      wp<-round(qprb$prd[qprb$qm==mgnc & qprb$line==line],4)
      return(wp)
    } else if (substr(x$gst1,1,2)=='EQ') {
      q<-as.numeric(substr(x$gst1,3,3))
      q<-q+1
      q<-min(4,q)
      qprb<-qprb.lst[[q]]
      if (q==1 | q>=5) {qprb$qm<-0}
      wp<-round(qprb$prd[qprb$qm==mgnc & qprb$line==line],4)
      return(wp)
    } else
      if (x$gst1=='EG') {
        wp<-ifelse(x$m==0,0.5,sign(x$m)/2 + 0.5)
        return(wp)
      }
    wpq<-pplst[[maxq]]
    # create alternate gt's for overtime #
    x$gta<-ifelse(as.numeric(as.character(x$q))<=4,x$gt,x$gt-(as.numeric(as.character(x$q))-4)*60*5)
    # subtract a half second if not in the last minute #
    #x$gta<-ifelse(x$min>0,x$gta-0.5,x$gta)
    # determine which wp column to select #
    wpc<-paste(x$gst1,x$gst2,sep="_")
    # get boundaries of square for interpolation #
    gtmin<-max(wpq$gt[wpq$gt<=x$gta])
    gtmax<-ifelse(x$gta==max(wpq$gt),x$gta,min(wpq$gt[wpq$gt>x$gta]))
    mgmin<-max(wpq$m[wpq$m<mgnc])
    mgmax<-min(wpq$m[wpq$m>=mgnc])
    lnmin<-max(wpq$line[wpq$line<line])
    lnmax<-min(wpq$line[wpq$line>=line])
    wp000<-wpq[wpq$gt==gtmin & wpq$m==mgmin & wpq$line==lnmin,wpc]
    wp001<-wpq[wpq$gt==gtmin & wpq$m==mgmin & wpq$line==lnmax,wpc]
    wp010<-wpq[wpq$gt==gtmin & wpq$m==mgmax & wpq$line==lnmin,wpc]
    wp011<-wpq[wpq$gt==gtmin & wpq$m==mgmax & wpq$line==lnmax,wpc]
    wp100<-wpq[wpq$gt==gtmax & wpq$m==mgmin & wpq$line==lnmin,wpc]
    wp101<-wpq[wpq$gt==gtmax & wpq$m==mgmin & wpq$line==lnmax,wpc]
    wp110<-wpq[wpq$gt==gtmax & wpq$m==mgmax & wpq$line==lnmin,wpc]
    wp111<-wpq[wpq$gt==gtmax & wpq$m==mgmax & wpq$line==lnmax,wpc]
    # interpolate #
    wp00<-(lnmax-line)/(lnmax-lnmin)*wp000 + (line- lnmin)/(lnmax-lnmin)*wp001
    wp01<-(lnmax-line)/(lnmax-lnmin)*wp010 + (line- lnmin)/(lnmax-lnmin)*wp011
    wp10<-(lnmax-line)/(lnmax-lnmin)*wp100 + (line- lnmin)/(lnmax-lnmin)*wp101
    wp11<-(lnmax-line)/(lnmax-lnmin)*wp110 + (line- lnmin)/(lnmax-lnmin)*wp111
    wp0<-(mgmax-mgnc)/(mgmax-mgmin)*wp00 + (mgnc- mgmin)/(mgmax-mgmin)*wp01
    wp1<-(mgmax-mgnc)/(mgmax-mgmin)*wp10 + (mgnc- mgmin)/(mgmax-mgmin)*wp11
    wp<-ifelse(gtmin==gtmax,wp0,(gtmax-x$gta)/(gtmax-gtmin)*wp0 + (x$gta-gtmin)/(gtmax-gtmin)*wp1)
    return(wp)
  }
  
  # no line #
  pnlf<-function(x,ff=FALSE) {
    mgnc<-max(-30,min(30,x$m))
    line<-0
    q<-x$q
    maxq<-min(4,q)
    if (substr(x$gst1,1,1)=='Q') {
      q<-as.numeric(substr(x$gst1,2,2))
      q<-min(4,q)
      qprb<-qprb.lst[[q]]
      if (q==1 | q>=5) {
        qprb$qm<-0
        mgnc<-0}
      wp<-round(qprb$prd[qprb$qm==mgnc & qprb$line==line],4)
      return(wp)
    } else if (substr(x$gst1,1,2)=='EQ') {
      q<-as.numeric(substr(x$gst1,3,3))
      q<-q+1
      q<-min(4,q)
      qprb<-qprb.lst[[q]]
      if (q==1 | q>=5) {qprb$qm<-0}
      wp<-round(qprb$prd[qprb$qm==mgnc & qprb$line==line],4)
      return(wp)
    } else
      if (x$gst1=='EG') {
        wp<-ifelse(x$m==0,0.5,sign(x$m)/2 + 0.5)
        return(wp)
      }
    wpq<-pplst[[maxq]]
    # create alternate gt's for overtime #
    x$gta<-ifelse(as.numeric(as.character(x$q))<=4,x$gt,x$gt-(as.numeric(as.character(x$q))-4)*60*5)
    # subtract a half second if not in the last minute #
    #x$gta<-ifelse(x$min>0,x$gta-0.5,x$gta)
    # determine which wp column to select #
    wpc<-paste(x$gst1,x$gst2,sep="_")
    # get boundaries of square for interpolation #
    gtmin<-max(wpq$gt[wpq$gt<=x$gta])
    gtmax<-ifelse(x$gta==max(wpq$gt),x$gta,min(wpq$gt[wpq$gt>x$gta]))
    mgmin<-max(wpq$m[wpq$m<mgnc])
    mgmax<-min(wpq$m[wpq$m>=mgnc])
    wp00<-wpq[wpq$gt==gtmin & wpq$m==mgmin & wpq$line==0,wpc]
    wp01<-wpq[wpq$gt==gtmin & wpq$m==mgmax & wpq$line==0,wpc]
    wp10<-wpq[wpq$gt==gtmax & wpq$m==mgmin & wpq$line==0,wpc]
    wp11<-wpq[wpq$gt==gtmax & wpq$m==mgmax & wpq$line==0,wpc]
    # interpolate #
    wp0<-(mgmax-mgnc)/(mgmax-mgmin)*wp00 + (mgnc- mgmin)/(mgmax-mgmin)*wp01
    wp1<-(mgmax-mgnc)/(mgmax-mgmin)*wp10 + (mgnc- mgmin)/(mgmax-mgmin)*wp11
    wp<-ifelse(gtmin==gtmax,wp0,(gtmax-x$gta)/(gtmax-gtmin)*wp0 + (x$gta-gtmin)/(gtmax-gtmin)*wp1)
    return(wp)
  }
  
  prbf2<-function(x) {
    
    if (x$tech==0) {
      now<-Sys.time()
      x$wp<-round(pnlf(x),4)
      later<-Sys.time()
      later-now
      x$wpl<-round(prbf(x,ff=FALSE),4)
      return(x)
    } else
      if (abs(x$tech)==1) {
        y<-x
        y$m<-y$m + 1*sign(x$tech)
        x$wp<-round((1-ftpct)*pnlf(x) + ftpct*pnlf(y),4)
        x$wpl<-round((1-ftpct)*prbf(x,ff=FALSE) + ftpct*prbf(y,ff=FALSE),4)
        return(x)
      } else
        if (abs(x$tech)==2) {
          y<-x
          y$m<-y$m+1*sign(x$tech)
          z<-x
          z$m<-z$m+2*sign(x$tech)
          x$wp<-round((1-ftpct)*(1-ftpct)*pnlf(x) + 2*(1-ftpct)*ftpct*pnlf(y) + ftpct*ftpct*pnlf(z),4)
          x$wpl<-round((1-ftpct)*(1-ftpct)*prbf(x,ff=FALSE) + 2*(1-ftpct)*ftpct*prbf(y,ff=FALSE) + ftpct*ftpct*prbf(z,ff=FALSE),4)
          return(x)
        } else
          if (abs(x$tech)>=3) {
            y<-x
            y$m<-y$m+1*sign(x$tech)
            z<-x
            z$m<-z$m+2
            aa<-x
            aa$m<-aa$m+3*sign(x$tech)
            x$wp<-round((1-ftpct)*(1-ftpct)*(1-ftpct)*pnlf(x) + 3*(1-ftpct)*(1-ftpct)*ftpct*pnlf(y) + 3*(1-ftpct)*ftpct*ftpct*pnlf(z) + ftpct*ftpct*ftpct*pnlf(aa),4)
            x$wpl<-round((1-ftpct)*(1-ftpct)*(1-ftpct)*prbf(x,ff=FALSE) + 3*(1-ftpct)*(1-ftpct)*ftpct*prbf(y,ff=FALSE) + 3*(1-ftpct)*ftpct*ftpct*prbf(z,ff=FALSE) + ftpct*ftpct*ftpct*prbf(aa,ff=FALSE),4)
            return(x)
          } else {
            x$wp<-0.5
            x$wpl<-0.5
            return(x)
          }
  }
  
  # aggregate player missed shot rebound rate #
  rbf<-function(x) {
    x$shzb<-psh$shzb[match(x$r,psh$r)]
    x$ppt1<-c(x$pt1[2:nrow(x)],NA)
    x<-x[substr(x$pt1,1,2)=="MS",]
    x<-x[x$ppt1 %in% c("ORB","DRB"),]
    
    x$cnt<-ifelse(!(x$min==0 & x$sec==0),1,0)
    x$orb<-ifelse(x$ppt1=="ORB" & !(x$min=0 & x$sec==0),1,0)
    
    x$ms2c<-ifelse(x$pt1=="MS2" & !(x$min==0 & x$sec==0),1,0)
    x$ms2r<-ifelse(x$pt1=="MS2" & !(x$min==0 & x$sec==0),x$orb,0)
    
    x$ms3c<-ifelse(x$pt1=="MS3" & !(x$min==0 & x$sec==0),1,0)
    x$ms3r<-ifelse(x$pt1=="MS3" & !(x$min==0 & x$sec==0),x$orb,0)
    
    x$msftc<-ifelse(!(x$pt1 %in% c("MS2","MS3")) & !(x$min==0 & x$sec==0),1,0)
    x$msftr<-ifelse(!(x$pt1 %in% c("MS2","MS3")) & !(x$min=0 & x$sec==0),x$orb,0)
    
    x$msrac<-ifelse(x$shzb=="RA" & !(x$min==0 & x$sec==0),1,0)
    x$msrar<-ifelse(x$shzb=="RA" & !(x$min==0 & x$sec==0),x$orb,0)
    
    x$msptc<-ifelse(x$shzb=="PNT" & !(x$min==0 & x$sec==0),1,0)
    x$msptr<-ifelse(x$shzb=="PNT" & !(x$min==0 & x$sec==0),x$orb,0)
    
    x$msmrc<-ifelse(x$shzb=="MR" & !(x$min==0 & x$sec==0),1,0)
    x$msmrr<-ifelse(x$shzb=="MR" & !(x$min==0 & x$sec==0),x$orb,0)
    
    x$msab3c<-ifelse(x$shzb=="AB3" & !(x$min==0 & x$sec==0),1,0)
    x$msab3r<-ifelse(x$shzb=="AB3" & !(x$min==0 & x$sec==0),x$orb,0)
    
    x$mscnrc<-ifelse(x$shzb %in% c("LC3","RC3") & !(x$min==0 & x$sec==0),1,0)
    x$mscnrr<-ifelse(x$shzb %in% c("LC3","RC3") & !(x$min==0 & x$sec==0),x$orb,0)
    
    xa<-aggregate(x[c("cnt","orb","ms2c","ms2r","ms3c","ms3r","msftc","msftr","msrac","msrar","msptc","msptr","msmrr","msmrc","msab3c","msab3r","mscnrc","mscnrr")],by=list(x$pid1),FUN=sum,na.rm=T)
    colnames(xa)[1:3]<-c("pid","msc","msr")
    return(xa)
  }
  
  # remove all star game #
  pbp.lst<-pbp.lst[substr(names(pbp.lst),1,3)!='003']
  # win probability added function #
  wpa.lst<-vector('list')
  wpg.lst<-vector('list')
  shl.lst<-vector('list')
  pos.lst<-vector('list')
  scl.lst<-vector('list')
  dwp.lst<-vector('list')
  owp.lst<-vector('list')
  rwp.lst<-vector('list')
  gmd.lst<-vector('list')
  pm.lst<-vector('list')
  sps.lst<-vector('list')
  clp.lst<-vector('list')
  pmst.lst<-vector('list')
  psst.lst<-vector('list')
  jwl.lst<-vector('list')
  # calculate win probability charts ----
  now<-Sys.time()
  for (i in 1:length(pbp.lst)) {
    #for (i in 2:length(pbp.lst)) {
    pbp<-pbp.lst[[i]]
    pbp<-unique(pbp)
    gid<-as.character(pbp$gid[1])
    if (length(gid)==0) {next}
    box<-box.lst[[gid]]
    sh<-sh.lst[[gid]]
    gmd<-gm.lst[[gid]]
    sb<-sub.lst[[gid]]
    spc<-gmt.lst[[gid]][["spc"]]
    qb<-gmt.lst[[gid]][["qb"]]
    ql<-gmt.lst[[gid]][["ql"]]
    
    htm<-gmd$htm[1]
    atm<-gmd$atm[1]
    
    tms<-unique(box$tid)
    b1<-box[box$tid==tms[1],]
    b2<-box[box$tid==tms[2],]
    
    pbp$line<-round(pbp$line*2,0)/2
    line<-pbp$line[1]
    lnmin<-max(dln[dln<line])
    lnmax<-min(dln[dln>=line])
    total<-pbp$total[1]
    
    pplst<-lapply(plst,function(x) {x[x$line==lnmin | x$line==lnmax | x$line==0,]})
    
    pbpl<-split(pbp,pbp$rid,drop=TRUE)
    pbplf<-lapply(pbpl,prbf2)
    pbpd<-do.call('rbind',pbplf)
    pbpd<-pbpd[order(pbpd$r),]
    # excise NA values for win prob #
    wpna<-pbpd[is.na(pbpd$wp),]
    pbpd<-pbpd[!is.na(pbpd$wp),]
    prwp<-c(pbpd$wp[1], pbpd$wp[1:(nrow(pbpd)-1)])
    pbpd<-cbind(pbpd,prwp)
    pbpd$wpa<-pbpd$wp - pbpd$prwp
    pbpd$wrnk<-rank(-abs(pbpd$wpa))
    prwpl<-c(pbpd$wpl[1], pbpd$wpl[1:(nrow(pbpd)-1)])
    pbpd<-cbind(pbpd,prwpl)
    pbpd$wpla<-pbpd$wpl - pbpd$prwpl
    pbpd$wlrnk<-rank(-abs(pbpd$wpla))
    # prior play #
    ppt1<-c("",pbpd$pt1[1:(nrow(pbpd)-1)])
    pbpd<-cbind(pbpd,ppt1)
    # margin change #
    prmg<-c(0, pbpd$m[1:(nrow(pbpd)-1)])
    pbpd<-cbind(pbpd,prmg)
    pbpd$ma<-pbpd$m - pbpd$prmg
    # assign home and away teams #
    home<-gm$htm[match(pbpd$gid[1],gm$gid)]
    away<-gm$atm[match(pbpd$gid[1],gm$GAME_ID)]
    pbpd$home<-gm$tm2[match(pbpd$gid,gm$GAME_ID)]
    pbpd$away<-gm$tm1[match(pbpd$gid,gm$GAME_ID)]
    
    # calculate alternate win probability #
    apbp<-pbp[pbp$pt1 %in% c("JB","MS2","MS3","MS12","MS13","MS23","MSTF","MSF12","MSC12","MS11","MS22","MS33","MSF22","MSC22","MK2","MK3","MK12","MK13","MK23","MKF12","MKC12","MKTF","MKCP","MK11","MK22","MK33","MKF22","MKC22","ORB","DRB"),]
    # jump ball #
    apbp$gst1<-ifelse(apbp$pt1=="JB",ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    # missed shots #
    apbp$m<-ifelse(apbp$pt1=="MS2",ifelse(apbp$gst1=="H",apbp$m+2,apbp$m-2),apbp$m)
    apbp$gst1<-ifelse(apbp$pt1=="MS2",ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    apbp$gst2<-ifelse(apbp$pt1=="MS2","P",apbp$gst2)
    apbp$m<-ifelse(apbp$pt1=="MS3",ifelse(apbp$gst1=="H",apbp$m+3,apbp$m-3),apbp$m)
    apbp$gst1<-ifelse(apbp$pt1=="MS3",ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    apbp$gst2<-ifelse(apbp$pt1=="MS3","P",apbp$gst2)
    # missed free throws #
    apbp$m<-ifelse(apbp$pt1 %in% c("MS12","MS13","MS23","MSTF","MSF12","MSC12","MS11","MS22","MS33","MSF22","MSC22"),ifelse(apbp$gst1=="H",apbp$m+1,apbp$m-1),apbp$m)
    apbp$gst1<-ifelse(apbp$pt1 %in% c("MS11","MS22","MS33"),ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    apbp$gst2<-ifelse(apbp$pt1 %in% c("MS11","MS22","MS33"),"P",apbp$gst2)
    # made shots #
    apbp$m<-ifelse(apbp$pt1=="MK2",ifelse(apbp$gst1=="A",apbp$m-2,apbp$m+2),apbp$m)
    apbp$m<-ifelse(apbp$pt1=="MK3",ifelse(apbp$gst1=="A",apbp$m-3,apbp$m+3),apbp$m)
    apbp$gst1<-ifelse(apbp$pt1 %in% c("MK2","MK3"),ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    apbp$gst2<-ifelse(apbp$pt1 %in% c("MK2","MK3"),"MFG",apbp$gst2)
    # made free throws #
    apbp$m<-ifelse(apbp$pt1 %in% c("MK12","MK13","MK23","MKTF","MKF12","MKC12","MK11","MK22","MK33","MKF12","MKC12","MKF22","MKC22"),ifelse(apbp$di=="h",apbp$m-1,apbp$m+1),apbp$m)
    apbp$gst1<-ifelse(apbp$pt1 %in% c("MK11","MK22","MK33"),ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    apbp$gst2<-ifelse(apbp$pt1 %in% c("MK11","MK22","MK33"),"MFT",apbp$gst2)
    # rebounds #
    apbp$gst1<-ifelse(apbp$pt1 %in% c('ORB','DRB'),ifelse(apbp$gst1=="H","A","H"),apbp$gst1)
    apbp<-apbp[c("r","line","q","gt","m","gst1","gst2","tech","pt1")]
    # calculate alternate win probabilities #
    apbpl<-split(apbp,apbp$r,drop=TRUE)
    apbplf<-lapply(apbpl,prbf2)
    apbpd<-do.call('rbind',apbplf)
    apbpd<-apbpd[order(apbpd$r),]
    #apbpd$owp<-pbpd$wp[match(apbpd$r,pbpd$r)]
    #apbpd$swg<-abs(apbpd$owp-apbpd$wp)
    #plot(apbpd$swg[apbpd$pt1=="MK12"])
    # add field back to pbpd file #
    pbpd$awp<-apbpd$wp[match(pbpd$r,apbpd$r)]
    pbpd$awp[is.na(pbpd$awp)]<- -1
    pbpd$awpl<-apbpd$wpl[match(pbpd$r,apbpd$r)]
    pbpd$awpl[is.na(pbpd$awpl)]<- -1
    # calculate leverage #
    pbpd$lprb<-0
    pbpd$lprb<-ifelse(pbpd$awp<0,0,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1=="MS2",1-fg2pct,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1=="MS3",1-fg3pct,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1=="MK2",fg2pct,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1=="MK3",fg3pct,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1 %in% c("MK11","MK12","MK22","MK13","MK23","MK33","MKTF","MKCP"),ftpct,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1 %in% c("MS11","MS12","MS22","MS13","MS23","MS33","MSTF","MSCP"),1-ftpct,pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1=="ORB",ifelse(pbpd$ppt1 %in% c("MS2","MS3"),fgreb,ftreb),pbpd$lprb)
    pbpd$lprb<-ifelse(pbpd$pt1=="DRB",ifelse(pbpd$ppt1 %in% c("MS2","MS3"),1-fgreb,1-ftreb),pbpd$lprb)
    pbpd$lvg<-ifelse(pbpd$lprb==0,NA,(pbpd$lprb*(pbpd$wp-pbpd$prwp)^2 + (1-pbpd$lprb)*(pbpd$awp-pbpd$prwp)^2)^0.5)
    pbpd$shlvg<-ifelse(pbpd$pt1 %in% c("MK2","MK3","MS2","MS3"),pbpd$lvg,NA)
    pbpd$ftlvg<-ifelse(pbpd$pt1 %in% c("MK11","MK12","MK22","MK13","MK23","MK33","MKTF","MKF12","MKC12","MKF22","MKC22","MS11","MS12","MS22","MS13","MS23","MS33","MSTF","MSF12","MSC12","MSF22","MSC22"),pbpd$lvg,NA)
    pbpd$rblvg<-ifelse(pbpd$pt1 %in% c("ORB","DRB"),pbpd$lvg,NA)
    pbpd$swg<-ifelse(pbpd$lprb==0,NA,1)*abs(pbpd$awp-pbpd$wp)
    #plot(pbpd$shlvg[!is.na(pbpd$shlvg) & pbpd$pt1 %in% c("MK2","MS2")],type="l")
    #plot(pbpd$ftlvg[!is.na(pbpd$ftlvg)],type="l")
    # create possession ids #
    pbpd$psf<-0
    pbpd$psf<-ifelse(pbpd$pt1=='JB',1,pbpd$psf)
    pbpd$psf<-ifelse(pbpd$pt1 %in% c('MK2','MK3','DRB','TO'),1,pbpd$psf)
    pbpd$psf<-ifelse(pbpd$pt1 %in% c("MK11","MK22","MK33"),1,pbpd$psf)
    pbpd$psf<-ifelse(pbpd$gst1 %in% c('Q2','Q3','Q4'),1,pbpd$psf)
    # if an and-one don't terminate possession #
    and1<-pbpd$gt[pbpd$pt1=="DF1"]
    pbpd$psf<-ifelse(pbpd$gt %in% and1 & pbpd$pt1 %in% c('MK2','MK3'),0,pbpd$psf)
    pbpd$posid<-cumsum(pbpd$psf)
    # shift margin up so that it sums to proper possession #
    pbpd$posma<-c(pbpd$ma[2:nrow(pbpd)],0)
    # create shot clock ids #
    pbpd$scf<-pbpd$psf
    pbpd$scf<-ifelse(pbpd$pt1=="ORB",1,pbpd$scf)
    pbpd$scf<-ifelse(pbpd$pt1 %in% c("MS2","MS3"),1,pbpd$scf)
    #pbpd$scf<-ifelse(pbpd$fr=="RP",1,pbpd$scf)
    pbpd$scf<-ifelse(pbpd$pt1 %in% c("DF","DF2","DF3"),1,pbpd$scf)
    pbpd$sclid<-ave(pbpd$scf,pbpd$posid,FUN=cumsum)
    # add clutch bucket #
    pbpd$clb<-0
    
    pbpd$clb[pbpd$pt1 %in% c("MK2","MS2") & pbpd$swg<0.02]<-1
    pbpd$clb[pbpd$pt1 %in% c("MK2","MS2") & pbpd$swg>=0.02 & pbpd$swg<0.07]<-2
    pbpd$clb[pbpd$pt1 %in% c("MK2","MS2") & pbpd$swg>=0.07 & pbpd$swg<0.18]<-3
    pbpd$clb[pbpd$pt1 %in% c("MK2","MS2") & pbpd$swg>=0.18]<-4
    
    pbpd$clb[pbpd$pt1 %in% c("MK3","MS3") & pbpd$swg<0.03]<-1
    pbpd$clb[pbpd$pt1 %in% c("MK3","MS3") & pbpd$swg>=0.03 & pbpd$swg<0.11]<-2
    pbpd$clb[pbpd$pt1 %in% c("MK3","MS3") & pbpd$swg>=0.11 & pbpd$swg<0.32]<-3
    pbpd$clb[pbpd$pt1 %in% c("MK3","MS3") & pbpd$swg>=0.32]<-4
    
    pbpd$clb[pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23') & pbpd$swg<0.005]<-1
    pbpd$clb[pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23') & pbpd$swg>=0.005 & pbpd$swg<0.045]<-2
    pbpd$clb[pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23') & pbpd$swg>=0.045 & pbpd$swg<0.11]<-3
    pbpd$clb[pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23') & pbpd$swg>=0.11]<-4
    
    pbpd$clb[pbpd$pt1 %in% c('MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$swg<0.005]<-1
    pbpd$clb[pbpd$pt1 %in% c('MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$swg>=0.005 & pbpd$swg<0.04]<-2
    pbpd$clb[pbpd$pt1 %in% c('MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$swg>=0.04 & pbpd$swg<0.095]<-3
    pbpd$clb[pbpd$pt1 %in% c('MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$swg>=0.095]<-4
    
    # create files for possession and shot clock - exclude technical fouls #
    pos<-pbpd[!(pbpd$pt1 %in% c('TF','MKTF','MSTF')),c('gid','r','gt','di','gst1','psf','posid','scf','sclid','posma','home','away','q','m')]
    pos<-pos[order(pos$r),]
    pos1<-pos[pos$psf==1,]
    # fix quarter beginnings #
    pos1$pgst1<-c(pos1$gst1[2:nrow(pos1)],"")
    pos1$gst1<-ifelse(pos1$gst1 %in% c('Q2','Q3','Q4'),ifelse(pos1$pgst1=='H','A','H'),pos1$gst1)
    # set offense and defense #
    pos1$off<-ifelse(pos1$gst1=='A',pos1$away,pos1$home)
    pos1$def<-ifelse(pos1$gst1=='A',pos1$home,pos1$away)
    # get start of possession #
    pos1$spt1<-pbpd$pt1[match(pos1$r,pbpd$r)]
    pos1$semt<-pbpd$emt[match(pos1$r,pbpd$r)]
    pos1$semat<-pbpd$emat[match(pos1$r,pbpd$r)]
    pos1<-pos1[c('gid','posid','r','spt1','semt','semat','gt','off','def','q','m','home')]
    # next time #
    pos1$gtnx<-c(pos1$gt[2:nrow(pos1)],ifelse(pos1$gt[nrow(pos1)]<=2880,2880,ifelse(pos1$gt[nrow(pos1)]<=3180,3180,ifelse(pos1$gt[nrow(pos1)]<=3480,3480,ifelse(pos1$gt[nrow(pos1)]<=3780,3780,ifelse(pos1$gt[nrow(pos1)]<=4080,4080))))))
    # sum points #
    psm<-aggregate(abs(pos$posma),by=list(pos$posid),FUN=sum)
    colnames(psm)<-c('posid','pts')
    pos1<-merge(pos1,psm,by=c('posid'))
    pos1$time<-pos1$gtnx-pos1$gt
    pos1<-pos1[order(pos1$r),]
    # merge in leverage #
    pos1<-merge(pos1,lf,by.x=c("q","gt","m"),by.y=c("q","gt","m"),all.x=T)
    pos1$li<-pos1$pwp/mn
    pos1$pct<-names(pct)[findInterval(pos1$pwp,pct)]
    pos1$pct<-gsub("%","",pos1$pct)
    pos1$pct<-as.numeric(pos1$pct)/100
    
    # add buckets #
    pos1$bck<-NA
    pos1$bck[pos1$pwp<cb[1]]<-1
    pos1$bck[pos1$pwp>=cb[1] & pos1$pwp<cb[2]]<-2
    pos1$bck[pos1$pwp>=cb[2] & pos1$pwp<cb[3]]<-3
    pos1$bck[pos1$pwp>=cb[3]]<-4
    pos1$cnt<-1
    pos1$hm<-ifelse(pos1$off==pos1$home,1,0)
    
    # aggregate clutch efficiency #
    apos<-aggregate(pos1[c("cnt","pts","time")],by=list(pos1$gid,pos1$bck,pos1$hm),FUN=sum)
    colnames(apos)[1:3]<-c("gid","bck","hm")
    # home team stats #
    aps1<-apos[apos$bck==1 & apos$hm==1,c("gid","cnt","pts","time")]
    colnames(aps1)<-c("gid","chpos1","chpts1","chtop1")
    aps2<-apos[apos$bck==2 & apos$hm==1,c("gid","cnt","pts","time")]
    colnames(aps2)<-c("gid","chpos2","chpts2","chtop2")
    aps3<-apos[apos$bck==3 & apos$hm==1,c("gid","cnt","pts","time")]
    colnames(aps3)<-c("gid","chpos3","chpts3","chtop3")
    aps4<-apos[apos$bck==4 & apos$hm==1,c("gid","cnt","pts","time")]
    colnames(aps4)<-c("gid","chpos4","chpts4","chtop4")
    apm<-merge(aps1,aps2,by="gid",all.x=T,all.y=T)
    apm<-merge(apm,aps3,by="gid",all.x=T,all.y=T)
    apm<-merge(apm,aps4,by="gid",all.x=T,all.y=T)
    
    # away team stats #
    aps1<-apos[apos$bck==1 & apos$hm==0,c("gid","cnt","pts","time")]
    colnames(aps1)<-c("gid","capos1","capts1","catop1")
    aps2<-apos[apos$bck==2 & apos$hm==0,c("gid","cnt","pts","time")]
    colnames(aps2)<-c("gid","capos2","capts2","catop2")
    aps3<-apos[apos$bck==3 & apos$hm==0,c("gid","cnt","pts","time")]
    colnames(aps3)<-c("gid","capos3","capts3","catop3")
    aps4<-apos[apos$bck==4 & apos$hm==0,c("gid","cnt","pts","time")]
    colnames(aps4)<-c("gid","capos4","capts4","catop4")
    apm<-merge(apm,aps1,by="gid",all.x=T,all.y=T)
    apm<-merge(apm,aps2,by="gid",all.x=T,all.y=T)
    apm<-merge(apm,aps3,by="gid",all.x=T,all.y=T)
    apm<-merge(apm,aps4,by="gid",all.x=T,all.y=T)
    apm[is.na(apm)]<-0
    
    pos1$hm<-pos1$q<-pos1$m<-NULL
    pos.lst[[gid]]<-pos1
    # shot clock #
    scl<-pos[pos$scf==1,]
    # fix quarter beginnings #
    scl$pgst1<-c(scl$gst1[2:nrow(scl)],"")
    scl$gst1<-ifelse(scl$gst1 %in% c('Q2','Q3','Q4'),ifelse(scl$pgst1=='H','A','H'),scl$gst1)
    # set offense and defense #
    scl$off<-ifelse(scl$gst1=='A',scl$away,scl$home)
    scl$def<-ifelse(scl$gst1=='A',scl$home,scl$away)
    scl<-scl[c('gid','posid','sclid','r','gt','off','def')]
    # next time #
    scl$gtnx<-c(scl$gt[2:nrow(scl)],ifelse(scl$gt[nrow(scl)]<=2880,2880,ifelse(scl$gt[nrow(scl)]<=3180,3180,ifelse(scl$gt[nrow(pos1)]<=3480,3480,ifelse(scl$gt[nrow(scl)]<=3780,3780,ifelse(scl$gt[nrow(scl)]<=4080,4080))))))
    # next rid #
    scl$rdnx<-c(scl$r[2:nrow(scl)],scl$r[nrow(scl)])
    scl$spt1<-pbpd$pt1[match(scl$r,pbpd$r)]
    scl$semt<-pbpd$emt[match(scl$r,pbpd$r)]
    scl$semat<-pbpd$emat[match(scl$r,pbpd$r)]
    scl$ept1<-pbpd$pt1[match(scl$rdnx,pbpd$r)]
    # sum points #
    ssm<-aggregate(abs(pos$posma),by=list(pos$posid,pos$sclid),FUN=sum)
    colnames(ssm)<-c('posid','sclid','pts')
    # merge in time and pts #
    scl<-merge(scl,ssm,by=c('posid','sclid'))
    scl$time<-scl$gtnx-scl$gt
    scl<-scl[order(scl$r),]
    scl.lst[[gid]]<-scl
    
    # attribute player to shots #
    psh<-pbpd[pbpd$pt1 %in% c("MK2","MK3","MS2","MS3"),c("r","rid","di","d","pt1","pt2","pid1","pnm1")]
    # assign player id from shot location file #
    psh$pid<-psh$pid1
    # assign shot location, distance, and type #
    psh$lcx<-sh$lcx[match(psh$rid,sh$rid)]
    psh$lcy<-sh$lcy[match(psh$rid,sh$rid)]
    psh$dst<-sh$dst[match(psh$rid,sh$rid)]
    if (is.null(psh$lcx)) {psh$lcx<-""} 
    if (is.null(psh$lcy)) {psh$lcy<-""} 
    if (is.null(psh$dst)) {psh$dst<-""} 
    # map shot type to shorter names #
    shtpl<-c('Jump Shot','Layup Shot','Hook Shot','Alley Oop Dunk Shot','Alley Oop Layup shot','Dunk Shot','Driving Layup Shot','Reverse Dunk Shot','Running Dunk Shot','Turnaround Jump Shot','Running Jump Shot','Slam Dunk Shot','Fadeaway Jump Shot','Driving Dunk Shot','Tip Shot',
             'Jump Bank Shot','Step Back Jump shot','Putback Layup Shot','Pullup Jump shot','Putback Dunk Shot','Floating Jump shot',
             'Driving Finger Roll Layup Shot','Reverse Layup Shot','Finger Roll Layup Shot','Putback Slam Dunk Shot','Turnaround Hook Shot',
             'Driving Jump shot','Jump Hook Shot','Driving Slam Dunk Shot','Running Reverse Layup Shot','Driving Reverse Layup Shot','Fadeaway Bank shot',
             'Turnaround Fadeaway shot','Driving Bank shot','Running Hook Shot',
             'Jump Bank Hook Shot','Running Bank shot','Running Finger Roll Layup Shot','Turnaround Bank shot','Driving Hook Shot','Turnaround Bank Hook Shot','Running Layup Shot','Hook Bank Shot','Running Tip Shot','Pullup Bank shot','Running Slam Dunk Shot','Reverse Slam Dunk Shot')
    shtps<-c('JMP','LUP','HK','AOD','AOL','DNK','DLU','RVD','RND','TAJ','RJ','SLM','FDE','DRD','TIP',
             'JBS','SBJ','PLS','PUJ','PDS','FJS','DFR','RLS','FRL','PSD','THS','DJS','JHS','DSD','RRL','DRL','FBS',
             'TFS','DBS','RHS',
             'JBH','RBS','RFR','TAB','DHS','TBH','RLS','HBS','RTS','PUB','RSD','RSD')
    psh$shtp<-sh$atp[match(psh$rid,sh$rid)]
    if (is.null(psh$shtp)) {psh$shtp<-""} 
    psh$shtp<-ifelse(is.na(match(as.character(psh$shtp),shtpl)),as.character(psh$shtp),shtps[match(as.character(psh$shtp),shtpl)])
    # map shot zone a to shorter names #
    shzal<-c('Right Side(R)','Center(C)','Left Side Center(LC)','Left Side(L)','Right Side Center(RC)')
    shzas<-c('R','C','LC','L','RC')
    psh$shza<-sh$sza[match(psh$rid,sh$rid)]
    if (is.null(psh$shza)) {psh$shza<-""} 
    psh$shza<-ifelse(is.na(match(as.character(psh$shza),shzal)),psh$shza,shzas[match(as.character(psh$shza),shzal)])
    # map shot zone b to shorter names #
    shzbl<-c('Mid-Range','Restricted Area','In The Paint (Non-RA)','Above the Break 3','Left Corner 3','Right Corner 3')
    shzbs<-c('MR','RA','PNT','AB3','LC3','RC3')
    psh$shzb<-sh$szb[match(psh$rid,sh$rid)]
    if (is.null(psh$shzb)) {psh$shzb<-""} 
    psh$shzb<-ifelse(is.na(match(as.character(psh$shzb),shzbl)),psh$shzb,shzbs[match(as.character(psh$shzb),shzbl)])
    
    # attribute free throws to player #
    ft<-pbpd[pbpd$pt1 %in% c("MK11","MS11","MK12","MS12","MK22","MS22","MK13","MS13","MK23","MS23","MK33","MS33","MKTF","MSTF","MKC12","MKC22","MSC12","MSC22","MKF12","MSF12","MKF22","MSF22"),c("r","pid1")]
    colnames(ft)<-c("r","pid")
    # attribute drawn fouls to player #
    df<-pbpd[pbpd$pt1 %in% c("DF1","DF2","DF3","CPF","FLG"),c("r","pid2")]
    colnames(df)<-c("r","pid")
    # attribute turnover to player #
    to<-pbpd[pbpd$pt1 %in% c("TO"),c("r","pid1","pnm1")]
    colnames(to)<-c("r","pid","pnm")
    # delete team turnovers #
    to<-to[!is.na(to$pnm),]
    # attribute assists to player #
    as<-pbpd[pbpd$pt2 %in% c("AST"),c("r","pid2")]
    colnames(as)<-c("r","pid")
    # attribute steals to player #
    st<-pbpd[pbpd$pt2 %in% c("STL"),c("r","pid2")]
    colnames(st)<-c("r","pid")
    # attribute blocks to player #
    bl<-pbpd[pbpd$pt2 %in% c("BLK"),c("r","pid2")]
    colnames(bl)<-c("r","pid")
    # attribute rebounds to player #
    rb<-pbpd[pbpd$pt1 %in% c("DRB","ORB"),c("r","pid1","pnm1")]
    # delete team rebounds #
    rb<-rb[!is.na(rb$pnm),]
    # create detailed win probability file #
    pwpa<-rbind(psh[c("r","pid")],ft[c("r","pid")],df[c("r","pid")],to[c("r","pid")])
    pwpa<-pwpa[order(pwpa$r),]
    pwpa$gt<-pbpd$gt[match(pwpa$r,pbpd$r)]
    pwpa$gid<-pbpd$gid[match(pwpa$r,pbpd$r)]
    pwpa$pt1<-pbpd$pt1[match(pwpa$r,pbpd$r)]
    pwpa$pt2<-pbpd$pt2[match(pwpa$r,pbpd$r)]
    pwpa$d<-pbpd$d[match(pwpa$r,pbpd$r)]
    pwpa$di<-pbpd$di[match(pwpa$r,pbpd$r)]
    pwpa$m<-pbpd$m[match(pwpa$r,pbpd$r)]
    pwpa$wp<-pbpd$wp[match(pwpa$r,pbpd$r)]
    pwpa$wpa<-pbpd$wpa[match(pwpa$r,pbpd$r)]
    pwpa$home<-pbpd$home[match(pwpa$r,pbpd$r)]
    pwpa$away<-pbpd$away[match(pwpa$r,pbpd$r)]
    pwpa$tm<-box$tm[match(pwpa$pid,box$pid)]
    # create multiplier #
    pwpa$mult<-ifelse(pwpa$tm==pwpa$home,1,-1)
    pwpa$WPA<-pwpa$mult * pwpa$wpa
    pwpa$plsWPA<-ifelse(pwpa$WPA>=0,pwpa$WPA,0)
    pwpa$minWPA<-ifelse(pwpa$WPA<0,pwpa$WPA,0)
    # flip win probability for away team #
    pwpa$WP<-ifelse(pwpa$tm==pwpa$away,1-pwpa$wp,pwpa$wp)# get WPA percentiles #
    # create points and attempts field #
    pwpa$pts<-0
    pwpa$pts[pwpa$pt1 == "MK2"]<-2
    pwpa$pts[pwpa$pt1 == "MK3"]<-3
    pwpa$pts[pwpa$pt1 %in% c("MK12","MK13","MK23","MKTF","MKC12","MKC22","MKF12","MKF22")]<-1
    pwpa$att<-0
    pwpa$att[pwpa$pt1 %in% c("MK2","MK3","MK12","MK13","MK23","MKTF","MKC12","MKC22","MKF12","MKF22")]<-1
    pwpa$att[pwpa$pt1 %in% c("MS2","MS3","MS12","MS13","MS23","MSTF","MSC12","MSC22","MSF12","MSF22")]<-1
    # get WPA percentiles #
    veWPA<-unlist(eWPA)
    veWPA25<-unlist(eWPA25)
    veWPA75<-unlist(eWPA75)
    # create alternate play type flag for pattern matching #
    pwpa$apt1<-ifelse(pwpa$pt1 %in% c("MK11","MK22","MK33"),"MKT",pwpa$pt1)
    pwpa$apt1<-ifelse(pwpa$pt1 %in% c("MS11","MS22","MS33"),"MST",pwpa$apt1)
    pwpa$apt1<-ifelse(pwpa$pt1 %in% c("MK12","MK13","MK23","MKTF","MKC12","MKC22","MKF12","MKF22"),"MKN",pwpa$apt1)
    pwpa$apt1<-ifelse(pwpa$pt1 %in% c("MS12","MS13","MS23","MSTF","MSC12","MSC22","MSF12","MSF22"),"MSN",pwpa$apt1)
    pwpa$apt1<-ifelse(pwpa$pt1 %in% c("CPF","FLG"),"DF2",pwpa$apt1)
    pwpa$eWPA<-veWPA[match(pwpa$apt1,names(veWPA))]
    pwpa$eWPA25<-veWPA25[match(pwpa$apt1,names(veWPA25))]
    pwpa$eWPA75<-veWPA75[match(pwpa$apt1,names(veWPA75))]
    # set clutch and garbage flags #
    pwpa$cl<-ifelse(pwpa$eWPA>0,ifelse(pwpa$WPA>=pwpa$eWPA,1,0),ifelse(pwpa$WPA<=pwpa$eWPA,1,0))
    pwpa$gb<-ifelse(pwpa$eWPA>0,ifelse(pwpa$WPA<pwpa$eWPA,1,0),ifelse(pwpa$WPA>pwpa$eWPA,1,0))
    # set double clutch and double garbage flags #
    pwpa$cc<-ifelse(abs(pwpa$WPA)>=abs(pwpa$eWPA75),1,0)
    pwpa$gg<-ifelse(abs(pwpa$WPA)<abs(pwpa$eWPA25),1,0)
    # set clutch and garbage WPA #
    pwpa$clWPA<-pwpa$cl * (pwpa$WPA-pwpa$eWPA)
    pwpa$gbWPA<- -pwpa$gb * (pwpa$WPA-pwpa$eWPA)
    pwpa$clplsWPA<-ifelse(pwpa$clWPA>=0,pwpa$clWPA,0)
    pwpa$clminWPA<-ifelse(pwpa$clWPA<0,pwpa$clWPA,0)
    pwpa$gbplsWPA<-ifelse(pwpa$gbWPA>=0,pwpa$gbWPA,0)
    pwpa$gbminWPA<-ifelse(pwpa$gbWPA<0,pwpa$gbWPA,0)
    # set normal, double clutch and double garbage WPA #
    pwpa$nrWPA<-0
    pwpa$ccWPA<-ifelse(pwpa$eWPA>0,pwpa$cc*(pwpa$WPA-pwpa$eWPA75),pwpa$cc*(pwpa$WPA-pwpa$eWPA75))
    pwpa$ggWPA<-ifelse(pwpa$eWPA>0,pwpa$gg*(pwpa$eWPA25-pwpa$WPA),pwpa$gg*(pwpa$eWPA25-pwpa$WPA))
    pwpa$nrWPA<-pwpa$WPA - pwpa$ccWPA + pwpa$ggWPA
    pwpa$ccplsWPA<-ifelse(pwpa$ccWPA>=0,pwpa$ccWPA,0)
    pwpa$ccminWPA<-ifelse(pwpa$ccWPA<0,pwpa$ccWPA,0)
    pwpa$ggplsWPA<-ifelse(pwpa$ggWPA>=0,pwpa$ggWPA,0)
    pwpa$ggminWPA<-ifelse(pwpa$ggWPA<0,pwpa$ggWPA,0)
    # set clutch points and attempts #
    pwpa$clPts<-pwpa$cl*pwpa$pts
    pwpa$gbPts<-pwpa$gb*pwpa$pts
    pwpa$ccPts<-pwpa$cc*pwpa$pts
    pwpa$ggPts<-pwpa$gg*pwpa$pts
    pwpa$clAtt<-pwpa$cl*pwpa$att
    pwpa$gbAtt<-pwpa$gb*pwpa$att
    pwpa$ccAtt<-pwpa$cc*pwpa$att
    pwpa$ggAtt<-pwpa$gg*pwpa$att
    # create sparklines #
    m1<-seq(from=0,to=max(pbpd$gt),by=60)
    pwpa$cwpa<-ave(pwpa$WPA,pwpa$pid,FUN=cumsum)
    pspf<-function(x) {
      cwpa<-c(0,x$cwpa)
      y<-round(c(0,cwpa[findInterval(m1,x$gt)+1]),2)
      y<-paste(y,collapse=",")
      return(y)
    }
    spkl<-split(pwpa,pwpa$pid)
    spkl2<-lapply(spkl,pspf)
    pspk<-do.call('rbind',spkl2)
    # output wpa file #
    dwpa<-pwpa[pwpa$pid!="",c('gid','r','pt1','pid','tm','wp','WPA','plsWPA','minWPA','eWPA','clWPA','gbWPA','clplsWPA','clminWPA','gbplsWPA','gbminWPA','cl','gb',
                              'nrWPA','ccWPA','ggWPA','ccplsWPA','ccminWPA','ggplsWPA','ggminWPA','cc','gg','cwpa')]
    dwp.lst[[gid]]<-dwpa
    # aggregate #
    wpa<-aggregate(dwpa[c('WPA','plsWPA','minWPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(dwpa$pid),FUN=sum)
    colnames(wpa)[1]<-c('pid')
    wpa$pspk<-pspk[match(as.character(wpa$pid),rownames(pspk)),1]
    shwpa<-aggregate(dwpa[dwpa$pt1 %in% c('MK2','MK3','MS2','MS3'),c('WPA','eWPA','clWPA','gbWPA','clplsWPA','clminWPA','gbplsWPA','gbminWPA','cl','gb','nrWPA','ccWPA','ggWPA','ccplsWPA','ccminWPA','ggplsWPA','ggminWPA','cc','gg')],by=list(dwpa$pid[dwpa$pt1 %in% c('MK2','MK3','MS2','MS3')]),FUN=sum)
    colnames(shwpa)[1]<-c('pid')
    colnames(shwpa)[2:ncol(shwpa)]<-paste("sh",colnames(shwpa)[2:ncol(shwpa)],sep="")
    if (nrow(dwpa[dwpa$pt1 %in% c('DF1','DF2','DF3','CPF','FLG'),])>0) {
      dfwpa<-aggregate(dwpa[dwpa$pt1 %in% c('DF1','DF2','DF3','CPF','FLG'),c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(dwpa$pid[dwpa$pt1 %in% c('DF1','DF2','DF3','CPF','FLG')]),FUN=sum)
    } else {
      dfwpa<-data.frame(matrix(nrow=1,ncol=12))
    }
    colnames(dfwpa)<-c('pid','WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')
    colnames(dfwpa)[2:ncol(dfwpa)]<-paste("df",colnames(dfwpa)[2:ncol(dfwpa)],sep="")
    ftwpa<-aggregate(dwpa[dwpa$pt1 %in% c('MK11','MS11','MK12','MS12','MK22','MS22','MK13','MS13','MK23','MS23','MK33','MS33','MKTF','MSTF','MKC12','MKC22','MKF12','MKF22','MSC12','MSC22','MSF12','MSF22'),c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(dwpa$pid[dwpa$pt1 %in% c('MK11','MS11','MK12','MS12','MK22','MS22','MK13','MS13','MK23','MS23','MK33','MS33','MKTF','MSTF','MKC12','MKC22','MKF12','MKF22','MSC12','MSC22','MSF12','MSF22')]),FUN=sum)
    colnames(ftwpa)[1]<-c('pid')
    colnames(ftwpa)[2:ncol(ftwpa)]<-paste("ft",colnames(ftwpa)[2:ncol(ftwpa)],sep="")
    towpa<-aggregate(dwpa[dwpa$pt1 %in% c('TO'),c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(dwpa$pid[dwpa$pt1 %in% c('TO')]),FUN=sum)
    colnames(towpa)[1]<-c('pid')
    colnames(towpa)[2:ncol(towpa)]<-paste("to",colnames(towpa)[2:ncol(towpa)],sep="")
    # aggregate shot splits #
    if (nrow(dwpa[dwpa$pt1 %in% c('MS11','MS22','MS33'),])>0) {
      mst<-aggregate(dwpa[dwpa$pt1 %in% c('MS11','MS22','MS33'),c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1 %in% c('MS11','MS22','MS33')]),FUN=sum)
    } else {
      mst<-data.frame(matrix(nrow=1,ncol=5))
    }
    colnames(mst)<-c('pid','clMST','gbMST','ccMST','ggMST')
    if (nrow(dwpa[dwpa$pt1 %in% c('MK11','MK22','MK33'),])>0) {
      mkt<-aggregate(dwpa[dwpa$pt1 %in% c('MK11','MK22','MK33'),c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1 %in% c('MK11','MK22','MK33')]),FUN=sum)
    } else {
      mkt<-data.frame(matrix(nrow=1,ncol=5))
    }
    colnames(mkt)<-c('pid','clMKT','gbMKT','ccMKT','ggMKT')
    if (nrow(dwpa[dwpa$pt1 %in% c('MS12','MS13','MS23','MSTF','MSC12','MSC22','MSF12','MSF22'),])>0) {
      msn<-aggregate(dwpa[dwpa$pt1 %in% c('MS12','MS13','MS23','MSTF','MSC12','MSC22','MSF12','MSF22'),c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1 %in% c('MS12','MS13','MS23','MSTF','MSC12','MSC22','MSF12','MSF22')]),FUN=sum)
    } else {
      msn<-data.frame(matrix(nrow=1,ncol=5))
    }
    colnames(msn)<-c('pid','clMSN','gbMSN','ccMSN','ggMSN')
    mkn<-aggregate(dwpa[dwpa$pt1 %in% c('MK12','MK13','MK23','MKTF','MKC12','MKC22','MKF12','MKF22'),c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1 %in% c('MK12','MK13','MK23','MKTF','MKTF','MKC12','MKC22','MKF12','MKF22')]),FUN=sum)
    colnames(mkn)<-c('pid','clMKN','gbMKN','ccMKN','ggMKN')
    ms2<-aggregate(dwpa[dwpa$pt1=="MS2",c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1=="MS2"]),FUN=sum)
    colnames(ms2)<-c('pid','clMS2','gbMS2','ccMS2','ggMS2')
    mk2<-aggregate(dwpa[dwpa$pt1=="MK2",c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1=="MK2"]),FUN=sum)
    colnames(mk2)<-c('pid','clMK2','gbMK2','ccMK2','ggMK2')
    ms3<-aggregate(dwpa[dwpa$pt1=="MS3",c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1=="MS3"]),FUN=sum)
    colnames(ms3)<-c('pid','clMS3','gbMS3','ccMS3','ggMS3')
    if (nrow(dwpa[dwpa$pt1 =="MK3",])>0) {
      mk3<-aggregate(dwpa[dwpa$pt1 =="MK3",c("cl","gb","cc","gg")],by=list(dwpa$pid[dwpa$pt1 == "MK3"]),FUN=sum)
    } else {
      mk3<-data.frame(matrix(nrow=1,ncol=5))
    }
    colnames(mk3)<-c('pid','clMK3','gbMK3','ccMK3','ggMK3')
    # create file for assists, blocks, steals #
    pwpa<-rbind(bl[c("r","pid")],st[c("r","pid")],as[c("r","pid")])
    pwpa<-pwpa[order(pwpa$r),]
    pwpa$gid<-pbpd$gid[match(pwpa$r,pbpd$r)]
    pwpa$pt1<-pbpd$pt1[match(pwpa$r,pbpd$r)]
    pwpa$pt2<-pbpd$pt2[match(pwpa$r,pbpd$r)]
    pwpa$d<-pbpd$d[match(pwpa$r,pbpd$r)]
    pwpa$di<-pbpd$di[match(pwpa$r,pbpd$r)]
    pwpa$m<-pbpd$m[match(pwpa$r,pbpd$r)]
    pwpa$wp<-pbpd$wp[match(pwpa$r,pbpd$r)]
    pwpa$wpa<-pbpd$wpa[match(pwpa$r,pbpd$r)]
    pwpa$home<-pbpd$home[match(pwpa$r,pbpd$r)]
    pwpa$away<-pbpd$away[match(pwpa$r,pbpd$r)]
    pwpa$tm<-box$tm[match(pwpa$pid,box$pid)]
    # create multiplier #
    pwpa$mult<-ifelse(pwpa$tm==pwpa$home,1,-1)
    pwpa$WPA<-pwpa$mult * pwpa$wpa
    # flip win probability for away team #
    pwpa$WP<-ifelse(pwpa$tm==pwpa$away,1-pwpa$wp,pwpa$wp)# get WPA percentiles #
    # get WPA percentiles #
    veWPA<-unlist(eWPA)
    veWPA25<-unlist(eWPA25)
    veWPA75<-unlist(eWPA75)
    pwpa$eWPA<-veWPA[match(pwpa$pt1,names(veWPA))]*ifelse(pwpa$pt2=="AST",1,-1)
    pwpa$eWPA25<-veWPA25[match(pwpa$pt1,names(veWPA25))]*ifelse(pwpa$pt2=="AST",1,-1)
    pwpa$eWPA75<-veWPA75[match(pwpa$pt1,names(veWPA75))]*ifelse(pwpa$pt2=="AST",1,-1)
    # set clutch and garbage flags #
    pwpa$cl<-ifelse(pwpa$eWPA>0,ifelse(pwpa$WPA>=pwpa$eWPA,1,0),ifelse(pwpa$WPA<=pwpa$eWPA,1,0))
    pwpa$gb<-ifelse(pwpa$eWPA>0,ifelse(pwpa$WPA<pwpa$eWPA,1,0),ifelse(pwpa$WPA>pwpa$eWPA,1,0))
    # set double clutch and double garbage flags #
    pwpa$cc<-ifelse(abs(pwpa$WPA)>=abs(pwpa$eWPA75),1,0)
    pwpa$gg<-ifelse(abs(pwpa$WPA)<abs(pwpa$eWPA25),1,0)
    # set clutch and garbage WPA #
    pwpa$clWPA<-pwpa$cl * (pwpa$WPA-pwpa$eWPA)
    pwpa$gbWPA<- -pwpa$gb * (pwpa$WPA-pwpa$eWPA)
    # set normal, double clutch and double garbage WPA #
    pwpa$nrWPA<-0
    pwpa$ccWPA<-ifelse(pwpa$eWPA>0,pwpa$cc*(pwpa$WPA-pwpa$eWPA75),pwpa$cc*(pwpa$WPA-pwpa$eWPA75))
    pwpa$ggWPA<-ifelse(pwpa$eWPA>0,pwpa$gg*(pwpa$eWPA25-pwpa$WPA),pwpa$gg*(pwpa$eWPA25-pwpa$WPA))
    pwpa$nrWPA<-pwpa$WPA - pwpa$ccWPA + pwpa$ggWPA
    # output owpa file #
    owpa<-pwpa[pwpa$pid!="",c('gid','r','pt2','pt1','pid','tm','wp','WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')]
    owp.lst[[gid]]<-owpa
    # aggregate #
    aswpa<-aggregate(owpa[owpa$pt2 %in% c('AST'),c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(owpa$pid[owpa$pt2 %in% c('AST')]),FUN=sum)
    colnames(aswpa)[1]<-c('pid')
    colnames(aswpa)[2:ncol(aswpa)]<-paste("as",colnames(aswpa)[2:ncol(aswpa)],sep="")
    if (nrow(owpa[owpa$pt2=="BLK",])>0) {
      blwpa<-aggregate(owpa[owpa$pt2 %in% c('BLK'),c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(owpa$pid[owpa$pt2 %in% c('BLK')]),FUN=sum)
    } else {
      blwpa<-data.frame(matrix(nrow=1,ncol=12))
    }
    colnames(blwpa)[1]<-c('pid')
    colnames(blwpa)[2:ncol(blwpa)]<-paste("bl",c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg'),sep="")
    
    stwpa<-aggregate(owpa[owpa$pt2 %in% c('STL'),c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(owpa$pid[owpa$pt2 %in% c('STL')]),FUN=sum)
    colnames(stwpa)[1]<-c('pid')
    colnames(stwpa)[2:ncol(stwpa)]<-paste("st",colnames(stwpa)[2:ncol(stwpa)],sep="")
    # create rebound win probability file #
    pwpa<-rb
    pwpa$pid<-pwpa$pid1
    pwpa<-pwpa[order(pwpa$r),]
    pwpa$gid<-pbpd$gid[match(pwpa$r,pbpd$r)]
    pwpa$pt1<-pbpd$pt1[match(pwpa$r,pbpd$r)]
    pwpa$pt2<-pbpd$pt2[match(pwpa$r,pbpd$r)]
    pwpa$d<-pbpd$d[match(pwpa$r,pbpd$r)]
    pwpa$di<-pbpd$di[match(pwpa$r,pbpd$r)]
    pwpa$m<-pbpd$m[match(pwpa$r,pbpd$r)]
    pwpa$wp<-pbpd$wp[match(pwpa$r,pbpd$r)]
    pwpa$wpa<-pbpd$wpa[match(pwpa$r,pbpd$r)]
    pwpa$home<-pbpd$home[match(pwpa$r,pbpd$r)]
    pwpa$away<-pbpd$away[match(pwpa$r,pbpd$r)]
    pwpa$tm<-box$tm[match(pwpa$pid,box$pid)]
    # create multiplier #
    pwpa$mult<-ifelse(pwpa$tm==pwpa$home,1,-1)
    pwpa$WPA<-pwpa$mult * pwpa$wpa
    # flip win probability for away team #
    pwpa$WP<-ifelse(pwpa$tm==pwpa$away,1-pwpa$wp,pwpa$wp)# get WPA percentiles #
    # get WPA percentiles #
    veWPA<-unlist(eWPA)
    veWPA25<-unlist(eWPA25)
    veWPA75<-unlist(eWPA75)
    pwpa$eWPA<-veWPA[match(pwpa$pt1,names(veWPA))]
    pwpa$eWPA25<-veWPA25[match(pwpa$pt1,names(veWPA25))]
    pwpa$eWPA75<-veWPA75[match(pwpa$pt1,names(veWPA75))]
    # set clutch and garbage flags #
    pwpa$cl<-ifelse(pwpa$eWPA>0,ifelse(pwpa$WPA>=pwpa$eWPA,1,0),ifelse(pwpa$WPA<=pwpa$eWPA,1,0))
    pwpa$gb<-ifelse(pwpa$eWPA>0,ifelse(pwpa$WPA<pwpa$eWPA,1,0),ifelse(pwpa$WPA>pwpa$eWPA,1,0))
    # set double clutch and double garbage flags #
    pwpa$cc<-ifelse(abs(pwpa$WPA)>=abs(pwpa$eWPA75),1,0)
    pwpa$gg<-ifelse(abs(pwpa$WPA)<abs(pwpa$eWPA25),1,0)
    # set clutch and garbage WPA #
    pwpa$clWPA<-pwpa$cl * (pwpa$WPA-pwpa$eWPA)
    pwpa$gbWPA<- -pwpa$gb * (pwpa$WPA-pwpa$eWPA)
    # set normal, double clutch and double garbage WPA #
    pwpa$nrWPA<-0
    pwpa$ccWPA<-ifelse(pwpa$eWPA>0,pwpa$cc*(pwpa$WPA-pwpa$eWPA75),pwpa$cc*(pwpa$WPA-pwpa$eWPA75))
    pwpa$ggWPA<-ifelse(pwpa$eWPA>0,pwpa$gg*(pwpa$eWPA25-pwpa$WPA),pwpa$gg*(pwpa$eWPA25-pwpa$WPA))
    pwpa$nrWPA<-pwpa$WPA - pwpa$ccWPA + pwpa$ggWPA
    # get shot id for each rebound #
    nxt<-pbpd[2:nrow(pbpd),c('r','pt1')]
    prv<-pbpd[1:(nrow(pbpd)-1),c('r','pt1')]
    colnames(nxt)<-c('r.n','pt1.n')
    colnames(prv)<-c('r.p','pt1.p')
    np<-cbind(prv,nxt)
    np<-np[np$pt1.p %in% c('MS2','MS3','MS11','MS22','MS33') & np$pt1.n %in% c('ORB','DRB'),]
    np<-np[,c('r.n','r.p')]
    colnames(np)<-c('r','shr')
    pwpa<-merge(pwpa,np,by=c('r'),all.x=TRUE)
    # output wpa file #
    rwpa<-pwpa[,c('gid','r','pt1','pid','tm','wp','WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg','shr')]
    rwp.lst[[gid]]<-rwpa
    # aggregate #
    orwpa<-rwpa[rwpa$pt1 %in% c('ORB'),]
    orwpa<-aggregate(orwpa[c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(orwpa[,c('pid'),drop=TRUE]),FUN=sum)
    colnames(orwpa)[1]<-c('pid')
    colnames(orwpa)[2:ncol(orwpa)]<-paste("or",colnames(orwpa)[2:ncol(orwpa)],sep="")
    drwpa<-rwpa[rwpa$pt1 %in% c('DRB'),]
    drwpa<-aggregate(drwpa[c('WPA','eWPA','clWPA','gbWPA','cl','gb','nrWPA','ccWPA','ggWPA','cc','gg')],by=list(drwpa$pid),FUN=sum)
    colnames(drwpa)[1]<-c('pid')
    colnames(drwpa)[2:ncol(drwpa)]<-paste("dr",colnames(drwpa)[2:ncol(drwpa)],sep="")
    # summarize clutch shots #
    pbpd$two1<-ifelse(pbpd$pt1 %in% c("MK2","MS2") & pbpd$clb==1,1,0)
    pbpd$two2<-ifelse(pbpd$pt1 %in% c("MK2","MS2") & pbpd$clb==2,1,0)
    pbpd$two3<-ifelse(pbpd$pt1 %in% c("MK2","MS2") & pbpd$clb==3,1,0)
    pbpd$two4<-ifelse(pbpd$pt1 %in% c("MK2","MS2") & pbpd$clb==4,1,0)
    
    pbpd$twom1<-ifelse(pbpd$pt1 %in% c("MK2") & pbpd$clb==1,1,0)
    pbpd$twom2<-ifelse(pbpd$pt1 %in% c("MK2") & pbpd$clb==2,1,0)
    pbpd$twom3<-ifelse(pbpd$pt1 %in% c("MK2") & pbpd$clb==3,1,0)
    pbpd$twom4<-ifelse(pbpd$pt1 %in% c("MK2") & pbpd$clb==4,1,0)
    
    pbpd$thr1<-ifelse(pbpd$pt1 %in% c("MK3","MS3") & pbpd$clb==1,1,0)
    pbpd$thr2<-ifelse(pbpd$pt1 %in% c("MK3","MS3") & pbpd$clb==2,1,0)
    pbpd$thr3<-ifelse(pbpd$pt1 %in% c("MK3","MS3") & pbpd$clb==3,1,0)
    pbpd$thr4<-ifelse(pbpd$pt1 %in% c("MK3","MS3") & pbpd$clb==4,1,0)
    
    pbpd$thrm1<-ifelse(pbpd$pt1 %in% c("MK3") & pbpd$clb==1,1,0)
    pbpd$thrm2<-ifelse(pbpd$pt1 %in% c("MK3") & pbpd$clb==2,1,0)
    pbpd$thrm3<-ifelse(pbpd$pt1 %in% c("MK3") & pbpd$clb==3,1,0)
    pbpd$thrm4<-ifelse(pbpd$pt1 %in% c("MK3") & pbpd$clb==4,1,0)
    
    pbpd$ft1<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23','MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$clb==1,1,0)
    pbpd$ft2<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23','MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$clb==2,1,0)
    pbpd$ft3<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23','MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$clb==3,1,0)
    pbpd$ft4<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MS12','MS13','MS23','MK11','MK22','MK33','MS11','MS22','MS33') & pbpd$clb==4,1,0)
    
    pbpd$ftm1<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MK11','MK22','MK33') & pbpd$clb==1,1,0)
    pbpd$ftm2<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MK11','MK22','MK33') & pbpd$clb==2,1,0)
    pbpd$ftm3<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MK11','MK22','MK33') & pbpd$clb==3,1,0)
    pbpd$ftm4<-ifelse(pbpd$pt1 %in% c('MK12','MK13','MK23','MK11','MK22','MK33') & pbpd$clb==4,1,0)
    
    cla<-aggregate(pbpd[c("two1","two2","two3","two4","twom1","twom2","twom3","twom4","thr1","thr2","thr3","thr4","thrm1","thrm2","thrm3","thrm4","ft1","ft2","ft3","ft4","ftm1","ftm2","ftm3","ftm4")],by=list(pbpd$pid1),FUN=sum)
    colnames(cla)[1]<-c("pid")
    # aggregate by team #
    clt<-cla
    clt$tmst<-box$tmst[match(clt$pid,box$pid)]
    clt<-clt[!is.na(clt$tmst),]
    clt<-aggregate(clt[c("two1","two2","two3","two4","twom1","twom2","twom3","twom4","thr1","thr2","thr3","thr4","thrm1","thrm2","thrm3","thrm4","ft1","ft2","ft3","ft4","ftm1","ftm2","ftm3","ftm4")],by=list(clt$tmst),FUN=sum)
    colnames(clt)[1]<-c("tmst")
    # get rebound rate for player missed shots #
    rma<-rbf(pbpd)
    
    # merge in with the box score data #
    boxd<-box[c('gid','tm','tmst','pid','pnm','spos','min','sec','fgm','fga','fg3m','fg3a','ftm','fta','orb','drb','reb','ast','stl','blk','tovr','pf','pts','pm')]
    
    boxd<-merge(boxd,wpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,shwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,dfwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,ftwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,towpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,mst,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,mkt,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,msn,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,mkn,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,ms2,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,mk2,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,ms3,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,mk3,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,aswpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,blwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,stwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,orwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,drwpa,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,cla,by=c('pid'),all.x=TRUE)
    boxd<-merge(boxd,rma,by=c('pid'),all.x=TRUE)
    boxd[is.na(boxd)]<-0
    wpa.lst[[gid]]<-boxd
    # create shot location file #
    xy<-psh[c('r','di','pt1','pt2','pid','lcx','lcy','dst','shtp','shza','shzb')]
    xy<-xy[order(xy$r),]
    xy$gid<-pbpd$gid[match(xy$r,pbpd$r)]
    xy$pt1<-pbpd$pt1[match(xy$r,pbpd$r)]
    xy$pt2<-pbpd$pt2[match(xy$r,pbpd$r)]
    xy$d<-pbpd$d[match(xy$r,pbpd$r)]
    xy$di<-pbpd$di[match(xy$r,pbpd$r)]
    xy$m<-pbpd$m[match(xy$r,pbpd$r)]
    xy$wp<-pbpd$wp[match(xy$r,pbpd$r)]
    xy$wpa<-pbpd$wpa[match(xy$r,pbpd$r)]
    xy$lvg<-round(pbpd$lvg[match(xy$r,pbpd$r)],4)
    xy$swg<-pbpd$swg[match(xy$r,pbpd$r)]
    xy$home<-pbpd$home[match(xy$r,pbpd$r)]
    xy$away<-pbpd$away[match(xy$r,pbpd$r)]
    xy$off<-box$tm[match(xy$pid,box$pid)]
    xy$def<-ifelse(xy$off==xy$home,xy$away,ifelse(xy$off==xy$away,xy$home,""))
    # create multiplier #
    xy$mult<-ifelse(xy$off==xy$home,1,-1)
    xy$WPA<-xy$mult * xy$wpa
    # switch probability #
    xy$WP<-ifelse(xy$off==xy$away,1-xy$wp,xy$wp)
    # assign assist and block flags #
    xy$as<-ifelse(xy$pt2=="AST",1,0)
    xy$bl<-ifelse(xy$pt2=="BLK",1,0)
    # assign player responsible for block and/or assist #
    xy$aspid<-as$pid[match(xy$r,as$r)]
    xy$blpid<-bl$pid[match(xy$r,bl$r)]
    xy$aspid[is.na(xy$aspid)]<- -1
    xy$blpid[is.na(xy$blpid)]<- -1
    # output shot location file #
    shloc<-xy[c('gid','r','off','def','pid','pt1','lcx','lcy','dst','shtp','shza','shzb','WPA','WP','lvg','swg','as','bl')]
    shl.lst[[gid]]<-shloc
    # create final win probability game file #
    pbpo<-pbpd[c('gid','r','rid','emt','emat','ts','tsv','gt','q','min','sec','d','di','wp','wpl','wpa','wpla','wrnk','wlrnk','m','ma','a','h','gst1','gst2','pt1','pt2','lvg','swg','shlvg','rblvg','ftlvg','awp','clb','posid')]
    pbpo$lvg<-round(pbpo$lvg,4)
    pbpo$shlvg<-round(pbpo$shlvg,4)
    pbpo$rblvg<-round(pbpo$rblvg,4)
    pbpo$ftlvg<-round(pbpo$ftlvg,4)
    # add four factor play type #
    pbpo$ff<-""
    pbpo$ff[pbpo$pt1 %in% c("MK2","MK3","MS2","MS3") & pbpo$di=="h"]<-"HFG"
    pbpo$ff[pbpo$pt1 %in% c("MK2","MK3","MS2","MS3") & pbpo$di=="a"]<-"AFG"
    pbpo$ff[pbpo$pt1 %in% c("DF1","DF2","DF3","CPF","FLG") & pbpo$gst1=="H"]<-"HFT"
    pbpo$ff[pbpo$pt1 %in% c("DF1","DF2","DF3","CPF","FLG") & pbpo$gst1=="A"]<-"AFT"
    pbpo$ff[pbpo$pt1 %in% c('MK11','MS11','MK12','MS12','MK22','MS22','MK13','MS13','MK23','MS23','MK33','MS33','MKTF','MSTF','MKC12','MKC22','MKF12','MKF22','MSC12','MSC22','MSF12','MSF22') & pbpo$di=="h"]<-"HFT"
    pbpo$ff[pbpo$pt1 %in% c('MK11','MS11','MK12','MS12','MK22','MS22','MK13','MS13','MK23','MS23','MK33','MS33','MKTF','MSTF','MKC12','MKC22','MKF12','MKF22','MSC12','MSC22','MSF12','MSF22') & pbpo$di=="a"]<-"AFT"
    pbpo$ff[pbpo$pt1 %in% c("ORB") & pbpo$di=="h"]<-"HRB"
    pbpo$ff[pbpo$pt1 %in% c("ORB") & pbpo$di=="a"]<-"ARB"
    pbpo$ff[pbpo$pt1 %in% c("DRB") & pbpo$di=="a"]<-"HRB"
    pbpo$ff[pbpo$pt1 %in% c("DRB") & pbpo$di=="h"]<-"ARB"
    pbpo$ff[pbpo$pt1 %in% c("TO") & pbpo$di=="h"]<-"HTO"
    pbpo$ff[pbpo$pt1 %in% c("TO") & pbpo$di=="a"]<-"ATO"
    #ffa<-aggregate(pbpo$wpa,by=list(pbpo$ff),FUN=sum)
    #sum(ffa$x)
    # add player id for main win probability assignment #
    pbpo$pid1<-dwpa$pid[match(pbpo$r,dwpa$r)]
    pbpo$pid1[is.na(pbpo$pid1)]<- -1
    bl_pid2<-bl$pid[match(pbpo$r,bl$r)]
    st_pid2<-st$pid[match(pbpo$r,st$r)]
    as_pid2<-as$pid[match(pbpo$r,as$r)]
    rb_pid2<-rb$pid[match(pbpo$r,rb$r)]
    pbpo <- pbpo %>%
      mutate(pid2=ifelse(!is.na(bl_pid2),bl_pid2,ifelse(!is.na(st_pid2),st_pid2,ifelse(!is.na(as_pid2),as_pid2,ifelse(!is.na(rb_pid2),rb_pid2,-1)))))
    
    # add annotation for marking the quarter #
    pbpo$ann<-ifelse(pbpo$emt=='12',ifelse(pbpo$q<=4,paste("Q",pbpo$q,sep=""),paste("OT",pbpo$q-4,sep="")),ifelse(pbpo$emt=='13'," ",""))
    # add Q1 annotation for 1st row - not sure if that worked #
    pbpo$ann[1]<-"Q1"
    # game turning points #
    tp<-pbpd[,c("r","gt","wp")]
    var<-var(tp$wp)
    mgt<-max(tp$gt)
    lmt<-lm(wp~gt,data=tp)
    tvar<-var
    for (tps in 1:10) {
      gtc<-seq(from=mgt/(tps+1),to=mgt*tps/(1+tps),length.out=tps)
      seg<-tryCatch(segmented(lmt,seg.Z=~gt,psi=gtc),error=function(e) NULL)
      if (is.null(seg)) {next}
      sprd<-predict(seg)
      #matplot(tp$gt,cbind(tp$wp,sprd),type="l")
      nvar<-var(sprd-tp$wp)
      imp<-nvar/var-1
      if (imp>0) {next}
      #print(tps)
      #print(nvar)
      #print(round(imp,2))
      #flush.console()
      var<-nvar
      sseg<-seg
    }
    sprd<-predict(sseg)
    tp<-cbind(tp,sprd)
    # retrieve split points #
    psi<-c(0,sseg$psi[,2])
    # retrieve intercept and slopes #
    ict<-intercept(sseg)[[1]]
    slp<-slope(sseg)[[1]]
    psi<-psi[order(psi)]
    tp$ivl<-findInterval(tp$gt,psi)
    tp$psi<-psi[tp$ivl]
    tp$ict<-ict[tp$ivl]
    tp$slp<-slp[tp$ivl]
    # add to play by play file #
    pbpo$phs<-tp$sprd[match(pbpo$r,tp$r)]
    # create split point summary #
    sps<-cbind(psi[2:length(psi)],ict[1:(length(ict)-1)],slp[1:(nrow(slp)-1),1],ict[2:length(ict)],slp[2:nrow(slp),1])
    colnames(sps)<-c("gt","lict","lslp","rict","rslp")
    sps<-data.frame(sps)
    sps$chg<-sps$rslp-sps$lslp
    sps.lst[[gid]]<-sps
    #tp$chk<-tp$ict+tp$gt*tp$slp
    #matplot(tp$gt,cbind(tp$wp,sprd),type="l")
    #matplot(tp$gt,tp[c("sprd","chk")])
    
    # create double clutch file #
    clp<-pbpo[pbpo$clb==4,c("pid1","gid","rid","r","di","q","min","sec","pt1","wp","swg","d","h","a")]
    colnames(clp)[1]<-"pid"
    if (nrow(clp)==0) {
      clp<-data.frame(matrix(nrow=0,ncol=21))
      colnames(clp)<-c("pid1","gid","rid","r","di","q","min","sec","pt1","wp","swg","d","h","a","ssn","po","dtc","htm","atm","hscr","ascr")
    } else {
      clp$ssn<-ssn
      clp$po<-po
      clp$dt<-gmd$dt[1]
      clp$htm<-gmd$htm[1]
      clp$atm<-gmd$atm[1]
      clp$hscr<-gmd$hscr[1]
      clp$ascr<-gmd$ascr[1]
    }
    clp.lst[[gid]]<-clp
    
    
    # create game summary statistics #
    gmd<-data.frame(gmd)
    gmd$line<-line
    gmd$total<-total
    # game time #
    q1<-ql$qt[ql$q==1]
    q2<-ql$qt[ql$q==2]
    q3<-ql$qt[ql$q==3]
    q4<-ql$qt[ql$q==4]
    q5<-ql$qt[ql$q==5]
    q6<-ql$qt[ql$q==6]
    q7<-ql$qt[ql$q==7]
    q8<-ql$qt[ql$q==8]
    gmd$q1t<-ifelse(length(q1)==0,0,q1)
    gmd$q2t<-ifelse(length(q2)==0,0,q2)
    gmd$q3t<-ifelse(length(q3)==0,0,q3)
    gmd$q4t<-ifelse(length(q4)==0,0,q4)
    gmd$q5t<-ifelse(length(q5)==0,0,q5)
    gmd$q6t<-ifelse(length(q6)==0,0,q6)
    gmd$q7t<-ifelse(length(q7)==0,0,q7)
    gmd$q8t<-ifelse(length(q8)==0,0,q8)
    q12<-qb$nxs[qb$q==1]
    q23<-qb$nxs[qb$q==2]
    q34<-qb$nxs[qb$q==3]
    q45<-qb$nxs[qb$q==4]
    q56<-qb$nxs[qb$q==5]
    q67<-qb$nxs[qb$q==6]
    q78<-qb$nxs[qb$q==7]
    gmd$q12<-ifelse(length(q12)==0,0,q12)
    gmd$q23<-ifelse(length(q23)==0,0,q23)
    gmd$q34<-ifelse(length(q34)==0,0,q34)
    gmd$q45<-ifelse(length(q45)==0,0,q45)
    gmd$q56<-ifelse(length(q56)==0,0,q56)
    gmd$q67<-ifelse(length(q67)==0,0,q67)
    gmd$q78<-ifelse(length(q78)==0,0,q78)
    gmd$htof<-0
    gmd$htoft<-0
    gmd$htos<-0
    gmd$htost<-0
    gmd$atof<-0
    gmd$atoft<-0
    gmd$atos<-0
    gmd$atost<-0
    gmd$oto<-0
    gmd$otot<-0
    # timeouts taken #
    hto<-spc[spc$di=="h" & spc$emt=="9",]
    if (!is.null(hto)) {
      if (nrow(hto)>0) {
        gmd$htof<-nrow(hto[hto$emat=="1",])
        gmd$htoft<-sum(hto$nxs[hto$emat=="1"])
        gmd$htos<-nrow(hto[hto$emat=="2",])
        gmd$htost<-sum(hto$nxs[hto$emat=="2"])
      }
    }
    ato<-spc[spc$di=="a" & spc$emt=="9",]
    if (!is.null(ato)) {
      if (nrow(ato)>0) {
        gmd$atof<-nrow(ato[ato$emat=="1",])
        gmd$atoft<-sum(ato$nxs[ato$emat=="1"])
        gmd$atos<-nrow(ato[ato$emat=="2",])
        gmd$atost<-sum(ato$nxs[ato$emat=="2"])
      }
    }
    oto<-spc[spc$emat=="4" & spc$emt=="9",]
    if (!is.null(oto)) {
      if (nrow(oto)>0) {
        gmd$oto<-nrow(oto)
        gmd$otot<-sum(oto$nxs)
      }
    }
    # official reviews #
    gmd$ofr<-0
    gmd$ofrt<-0
    ofr<-spc[spc$emt=="18",]
    if (!is.null(ofr)) {
      if (nrow(ofr)>0) {
        gmd$ofr<-nrow(ofr)
        gmd$ofrt<-sum(ofr$nxs)
      }
    }
    
    # add missing game scores if needed #
    rper<-pbpd$q[nrow(pbpd)]
    rhscr<-pbpd$h[nrow(pbpd)]
    rascr<-pbpd$a[nrow(pbpd)]
    rminsec<-paste(pbpd$min[nrow(pbpd)],pbpd$sec[nrow(pbpd)],sep=":")
    
    gmd <- gmd %>% mutate(hscr=ifelse(hscr=="",rhscr,hscr))
    gmd <- gmd %>% mutate(ascr=ifelse(ascr=="",rascr,ascr))
    gmd <- gmd %>% mutate(hwin=ifelse(hscr>ascr,1,ifelse(ascr>hscr,0,0.5)))
    gmd <- gmd %>% mutate(fm=hscr-ascr)
    
    # populate quarter scores #
    qtr_scores <- pbpd %>%
      group_by(q) %>%
      slice_tail() %>%
      select(gid,q,h,a) %>%
      ungroup() %>%
      mutate(h=h-lag(h,default=0),
             a=a-lag(a,default=0))
    
    gmd$hq1<-qtr_scores %>% filter(q==1) %>% pull(h)
    gmd$hq2<-qtr_scores %>% filter(q==2) %>% pull(h)
    gmd$hq3<-qtr_scores %>% filter(q==3) %>% pull(h)
    gmd$hq4<-qtr_scores %>% filter(q==4) %>% pull(h)
    gmd$hot1<-qtr_scores$h[match(5,qtr_scores$q)]
    gmd$hot2<-qtr_scores$h[match(6,qtr_scores$q)]
    gmd$hot3<-qtr_scores$h[match(7,qtr_scores$q)]
    gmd$hot4<-qtr_scores$h[match(8,qtr_scores$q)]
    gmd$hot5<-qtr_scores$h[match(9,qtr_scores$q)]
    gmd$hot6<-qtr_scores$h[match(10,qtr_scores$q)]
    gmd$hot7<-qtr_scores$h[match(11,qtr_scores$q)]
    gmd$hot8<-qtr_scores$h[match(12,qtr_scores$q)]
    gmd$hot9<-qtr_scores$h[match(13,qtr_scores$q)]
    gmd$hot10<-qtr_scores$h[match(14,qtr_scores$q)]
    
    gmd$aq1<-qtr_scores %>% filter(q==1) %>% pull(a)
    gmd$aq2<-qtr_scores %>% filter(q==2) %>% pull(a)
    gmd$aq3<-qtr_scores %>% filter(q==3) %>% pull(a)
    gmd$aq4<-qtr_scores %>% filter(q==4) %>% pull(a)
    gmd$aot1<-qtr_scores$a[match(5,qtr_scores$q)]
    gmd$aot2<-qtr_scores$a[match(6,qtr_scores$q)]
    gmd$aot3<-qtr_scores$a[match(7,qtr_scores$q)]
    gmd$aot4<-qtr_scores$a[match(8,qtr_scores$q)]
    gmd$aot5<-qtr_scores$a[match(9,qtr_scores$q)]
    gmd$aot6<-qtr_scores$a[match(10,qtr_scores$q)]
    gmd$aot7<-qtr_scores$a[match(11,qtr_scores$q)]
    gmd$aot8<-qtr_scores$a[match(12,qtr_scores$q)]
    gmd$aot9<-qtr_scores$a[match(13,qtr_scores$q)]
    gmd$aot10<-qtr_scores$a[match(14,qtr_scores$q)]
    
    # replace NA values with zero #
    gmd <- gmd %>%
      mutate(across(c(hot1:hot10,aot1:aot10), ~replace_na(., 0)))
    
    # add kullback-liebler divergence excitement index
    pbpo <- pbpo %>%
      mutate(wpc = ifelse(wp==0,0.000025,ifelse(wp==1,0.999975,wp))) %>%
      mutate(wplc = ifelse(wpl==0,0.000025,ifelse(wpl==1,0.999975,wpl))) %>%
      mutate(kli = ifelse(wpc==0,0,wpc*log2(wpc/lag(wpc))) + ifelse(wpc==1,0,(1-wpc)*log2((1-wpc)/(1-lag(wpc))))) %>%
      mutate(klli = ifelse(wplc==0,0,wplc*log2(wplc/lag(wplc))) + ifelse(wplc==1,0,(1-wplc)*log2((1-wplc)/(1-lag(wplc)))))
    
    gmd$ei<-sum(abs(pbpo$wpa))
    gmd$ei_kl=sum(pbpo$kli,na.rm=T)
    gmd$prb<-ifelse(gmd$hwin==1,min(pbpo$wp),1-max(pbpo$wp))
    gmd$cb<-round((1-gmd$prb)/gmd$prb,2)
    gmd$ti<-round(mean(ifelse(pbpo$wp %in% c(0,1),0,-pbpo$wp*log(pbpo$wp,base=2) -(1-pbpo$wp)*log(1-pbpo$wp,base=2))),4)
    gmd$eil<-sum(abs(pbpo$wpla))
    gmd$eil_kl=sum(pbpo$klli,na.rm=T)
    gmd$prbl<-ifelse(gmd$hwin==1,min(pbpo$wpl),1-max(pbpo$wpl))
    gmd$cbl<-round((1-gmd$prbl)/gmd$prbl,2)
    gmd$hfg<-sum(pbpo$wpa[pbpo$ff=="HFG"])
    gmd$hft<-sum(pbpo$wpa[pbpo$ff=="HFT"])
    gmd$hrb<-sum(pbpo$wpa[pbpo$ff=="HRB"])
    gmd$hto<-sum(pbpo$wpa[pbpo$ff=="HTO"])
    gmd$afg<- -sum(pbpo$wpa[pbpo$ff=="AFG"])
    gmd$aft<- -sum(pbpo$wpa[pbpo$ff=="AFT"])
    gmd$arb<- -sum(pbpo$wpa[pbpo$ff=="ARB"])
    gmd$ato<- -sum(pbpo$wpa[pbpo$ff=="ATO"])
    gmd$ffd<-gmd$hfg+gmd$hft+gmd$hrb+gmd$hto-gmd$afg-gmd$aft-gmd$arb-gmd$ato
    gmd$ffm<-ifelse(gmd$hwin==1,0.5-gmd$ffd,gmd$ffd+0.5)
    gmd$hwpa<-gmd$hfg+gmd$hft+gmd$hrb+gmd$hto
    gmd$awpa<-gmd$afg+gmd$aft+gmd$arb+gmd$ato
    pbpo$kli<-pbpo$klli<-pbpo$wpc<-pbpo$wplc<-NULL
    # possessions #
    gmd$pos<-nrow(pos1)
    gmd$hpos<-sum(ifelse(pos1$off==gmd$htm,1,0))
    gmd$apos<-sum(ifelse(pos1$off==gmd$atm,1,0))
    
    # time of possession #
    gmd$htop<-sum(ifelse(pos1$off==gmd$htm,pos1$time,0))
    gmd$atop<-sum(ifelse(pos1$off==gmd$atm,pos1$time,0))
    # points after turnovers #
    gmd$hpto<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="TO",1,0))
    gmd$htopto<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="TO",pos1$time,0))
    gmd$hptsto<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="TO",pos1$pts,0))
    gmd$apto<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="TO",1,0))
    gmd$atopto<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="TO",pos1$time,0))
    gmd$aptsto<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="TO",pos1$pts,0))
    # points after made shots #
    gmd$hpmk<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),1,0))
    gmd$htopmk<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),pos1$time,0))
    gmd$hptsmk<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),pos1$pts,0))
    gmd$apmk<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),1,0))
    gmd$atopmk<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),pos1$time,0))
    gmd$aptsmk<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),pos1$pts,0))
    # points after defensive rebounds #
    gmd$hpdr<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="DRB",1,0))
    gmd$htopdr<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="DRB",pos1$time,0))
    gmd$hptsdr<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="DRB",pos1$pts,0))
    gmd$apdr<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="DRB",1,0))
    gmd$atopdr<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="DRB",pos1$time,0))
    gmd$aptsdr<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="DRB",pos1$pts,0))
    # points after live turnovers #
    gmd$hptol<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="TO" & pos1$semt==5 & pos1$semat %in% c(1,2),1,0))
    gmd$htoptol<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="TO" & pos1$semt==5 & pos1$semat %in% c(1,2),pos1$time,0))
    gmd$hptstol<-sum(ifelse(pos1$off==gmd$htm & pos1$spt1=="TO" & pos1$semt==5 & pos1$semat %in% c(1,2),pos1$pts,0))
    gmd$aptol<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="TO" & pos1$semt==5 & pos1$semat %in% c(1,2),1,0))
    gmd$atoptol<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="TO" & pos1$semt==5 & pos1$semat %in% c(1,2),pos1$time,0))
    gmd$aptstol<-sum(ifelse(pos1$off==gmd$atm & pos1$spt1=="TO" & pos1$semt==5 & pos1$semat %in% c(1,2),pos1$pts,0))
    # quarterly pace 
    pos1$q<-pbpo$q[match(pos1$r,pbpo$r)]
    gmd$hq1p<-sum(ifelse(pos1$off==gmd$htm & pos1$q==1,1,0))
    gmd$hq1t<-sum(ifelse(pos1$off==gmd$htm & pos1$q==1,pos1$time,0))
    gmd$hq2p<-sum(ifelse(pos1$off==gmd$htm & pos1$q==2,1,0))
    gmd$hq2t<-sum(ifelse(pos1$off==gmd$htm & pos1$q==2,pos1$time,0))
    gmd$hq3p<-sum(ifelse(pos1$off==gmd$htm & pos1$q==3,1,0))
    gmd$hq3t<-sum(ifelse(pos1$off==gmd$htm & pos1$q==3,pos1$time,0))
    gmd$hq4p<-sum(ifelse(pos1$off==gmd$htm & pos1$q==4,1,0))
    gmd$hq4t<-sum(ifelse(pos1$off==gmd$htm & pos1$q==4,pos1$time,0))
    gmd$hq5p<-sum(ifelse(pos1$off==gmd$htm & pos1$q==5,1,0))
    gmd$hq5t<-sum(ifelse(pos1$off==gmd$htm & pos1$q==5,pos1$time,0))
    gmd$aq1p<-sum(ifelse(pos1$off==gmd$atm & pos1$q==1,1,0))
    gmd$aq1t<-sum(ifelse(pos1$off==gmd$atm & pos1$q==1,pos1$time,0))
    gmd$aq2p<-sum(ifelse(pos1$off==gmd$atm & pos1$q==2,1,0))
    gmd$aq2t<-sum(ifelse(pos1$off==gmd$atm & pos1$q==2,pos1$time,0))
    gmd$aq3p<-sum(ifelse(pos1$off==gmd$atm & pos1$q==3,1,0))
    gmd$aq3t<-sum(ifelse(pos1$off==gmd$atm & pos1$q==3,pos1$time,0))
    gmd$aq4p<-sum(ifelse(pos1$off==gmd$atm & pos1$q==4,1,0))
    gmd$aq4t<-sum(ifelse(pos1$off==gmd$atm & pos1$q==4,pos1$time,0))
    gmd$aq5p<-sum(ifelse(pos1$off==gmd$atm & pos1$q==5,1,0))
    gmd$aq5t<-sum(ifelse(pos1$off==gmd$atm & pos1$q==5,pos1$time,0))
    # time to shot
    gmd$htts<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3"),scl$time,0))
    gmd$httsc<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3"),1,0))
    gmd$httsp<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3"),scl$pts,0))
    gmd$atts<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3"),scl$time,0))
    gmd$attsc<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3"),1,0))
    gmd$attsp<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3"),scl$pts,0))
    # time to shot after turnover
    gmd$httsto<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO"),scl$time,0))
    gmd$httscto<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO"),1,0))
    gmd$httspto<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO"),scl$pts,0))
    gmd$attsto<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO"),scl$time,0))
    gmd$attscto<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO"),1,0))
    gmd$attspto<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO"),scl$pts,0))
    # time to shot after made shots
    gmd$httsmk<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),scl$time,0))
    gmd$httscmk<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),1,0))
    gmd$httspmk<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),scl$pts,0))
    gmd$attsmk<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),scl$time,0))
    gmd$attscmk<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),1,0))
    gmd$attspmk<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("MK2","MK3","MK11","MK22","MK33"),scl$pts,0))
    # time to shot after offensive rebound
    gmd$httsor<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("ORB"),scl$time,0))
    gmd$httscor<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("ORB"),1,0))
    gmd$httspor<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("ORB"),scl$pts,0))
    gmd$attsor<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("ORB"),scl$time,0))
    gmd$attscor<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("ORB"),1,0))
    gmd$attspor<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("ORB"),scl$pts,0))
    # time to shot after defensive rebound
    gmd$httsdr<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DRB"),scl$time,0))
    gmd$httscdr<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DRB"),1,0))
    gmd$httspdr<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DRB"),scl$pts,0))
    gmd$attsdr<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DRB"),scl$time,0))
    gmd$attscdr<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DRB"),1,0))
    gmd$attspdr<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DRB"),scl$pts,0))
    # time to shot after drawn foul
    gmd$httsdf<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DF"),scl$time,0))
    gmd$httscdf<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DF"),1,0))
    gmd$httspdf<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DF"),scl$pts,0))
    gmd$attsdf<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DF"),scl$time,0))
    gmd$attscdf<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DF"),1,0))
    gmd$attspdf<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("DF"),scl$pts,0))
    # time to shot after live turnover
    gmd$httstol<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO") & scl$semt==5 & scl$semat %in% c(1,2),scl$time,0))
    gmd$httsctol<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO") & scl$semt==5 & scl$semat %in% c(1,2),1,0))
    gmd$httsptol<-sum(ifelse(scl$off==gmd$htm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO") & scl$semt==5 & scl$semat %in% c(1,2),scl$pts,0))
    gmd$attstol<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO") & scl$semt==5 & scl$semat %in% c(1,2),scl$time,0))
    gmd$attsctol<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO") & scl$semt==5 & scl$semat %in% c(1,2),1,0))
    gmd$attsptol<-sum(ifelse(scl$off==gmd$atm & scl$ept1 %in% c("MK2","MK3","MS2","MS3") & scl$spt1 %in% c("TO") & scl$semt==5 & scl$semat %in% c(1,2),scl$pts,0))
    if (1==1) {
      # clutch bucket summaries #
      gmd$atwo1<-clt$two1[clt$tmst=="away"]
      gmd$atwo2<-clt$two2[clt$tmst=="away"]
      gmd$atwo3<-clt$two3[clt$tmst=="away"]
      gmd$atwo4<-clt$two4[clt$tmst=="away"]
      gmd$atwom1<-clt$twom1[clt$tmst=="away"]
      gmd$atwom2<-clt$twom2[clt$tmst=="away"]
      gmd$atwom3<-clt$twom3[clt$tmst=="away"]
      gmd$atwom4<-clt$twom4[clt$tmst=="away"]
      gmd$athr1<-clt$thr1[clt$tmst=="away"]
      gmd$athr2<-clt$thr2[clt$tmst=="away"]
      gmd$athr3<-clt$thr3[clt$tmst=="away"]
      gmd$athr4<-clt$thr4[clt$tmst=="away"]
      gmd$athrm1<-clt$thrm1[clt$tmst=="away"]
      gmd$athrm2<-clt$thrm2[clt$tmst=="away"]
      gmd$athrm3<-clt$thrm3[clt$tmst=="away"]
      gmd$athrm4<-clt$thrm4[clt$tmst=="away"]
      gmd$aft1<-clt$ft1[clt$tmst=="away"]
      gmd$aft2<-clt$ft2[clt$tmst=="away"]
      gmd$aft3<-clt$ft3[clt$tmst=="away"]
      gmd$aft4<-clt$ft4[clt$tmst=="away"]
      gmd$aftm1<-clt$ftm1[clt$tmst=="away"]
      gmd$aftm2<-clt$ftm2[clt$tmst=="away"]
      gmd$aftm3<-clt$ftm3[clt$tmst=="away"]
      gmd$aftm4<-clt$ftm4[clt$tmst=="away"]
      gmd$htwo1<-clt$two1[clt$tmst=="home"]
      gmd$htwo2<-clt$two2[clt$tmst=="home"]
      gmd$htwo3<-clt$two3[clt$tmst=="home"]
      gmd$htwo4<-clt$two4[clt$tmst=="home"]
      gmd$htwom1<-clt$twom1[clt$tmst=="home"]
      gmd$htwom2<-clt$twom2[clt$tmst=="home"]
      gmd$htwom3<-clt$twom3[clt$tmst=="home"]
      gmd$htwom4<-clt$twom4[clt$tmst=="home"]
      gmd$hthr1<-clt$thr1[clt$tmst=="home"]
      gmd$hthr2<-clt$thr2[clt$tmst=="home"]
      gmd$hthr3<-clt$thr3[clt$tmst=="home"]
      gmd$hthr4<-clt$thr4[clt$tmst=="home"]
      gmd$hthrm1<-clt$thrm1[clt$tmst=="home"]
      gmd$hthrm2<-clt$thrm2[clt$tmst=="home"]
      gmd$hthrm3<-clt$thrm3[clt$tmst=="home"]
      gmd$hthrm4<-clt$thrm4[clt$tmst=="home"]
      gmd$hft1<-clt$ft1[clt$tmst=="home"]
      gmd$hft2<-clt$ft2[clt$tmst=="home"]
      gmd$hft3<-clt$ft3[clt$tmst=="home"]
      gmd$hft4<-clt$ft4[clt$tmst=="home"]
      gmd$hftm1<-clt$ftm1[clt$tmst=="home"]
      gmd$hftm2<-clt$ftm2[clt$tmst=="home"]
      gmd$hftm3<-clt$ftm3[clt$tmst=="home"]
      gmd$hftm4<-clt$ftm4[clt$tmst=="home"]
    }
    # game scripts #
    gmd$gsp<-round(ifelse(gmd$hwin==1,mean(pbpo$wp),mean(1-pbpo$wp)),3)
    gmd$gsm<-round(ifelse(gmd$hwin==1,gmd$fm-mean(pbpo$m),-gmd$fm+mean(pbpo$m)),2)
    # tension - shannon entropy or distance from zero margin #
    gmd$tnp<-round(mean(ifelse(pbpo$wp %in% c(0,1),0,-pbpo$wp*log(pbpo$wp,base=2) -(1-pbpo$wp)*log(1-pbpo$wp,base=2))),4)
    gmd$tnm<-round(mean(abs(pbpo$m)),2)
    # top players #
    boxd<-boxd[order(-boxd$WPA),]
    MVP<-boxd$pid[1]
    boxd<-boxd[order(boxd$WPA),]
    LVP<-boxd$pid[1]
    boxd<-boxd[order(-boxd$clWPA),]
    Hero<-boxd$pid[1]
    boxd<-boxd[order(boxd$clWPA),]
    Goat<-boxd$pid[1]
    boxd<-boxd[order(-boxd$gbWPA),]
    MGP<-boxd$pid[1]
    boxd<-boxd[order(boxd$gbWPA),]
    LGP<-boxd$pid[1]
    gmd$mvpId<-MVP
    gmd$mvpNm<-boxd$pnm[match(MVP,boxd$pid)]
    gmd$mvpTm<-boxd$tm[match(MVP,boxd$pid)]
    gmd$mvpWPA<-boxd$WPA[match(MVP,boxd$pid)]
    gmd$lvpId<-LVP
    gmd$lvpNm<-boxd$pnm[match(LVP,boxd$pid)]
    gmd$lvpTm<-boxd$tm[match(LVP,boxd$pid)]
    gmd$lvpWPA<-boxd$WPA[match(LVP,boxd$pid)]
    gmd$heroId<-Hero
    gmd$heroNm<-boxd$pnm[match(Hero,boxd$pid)]
    gmd$heroTm<-boxd$tm[match(Hero,boxd$pid)]
    gmd$heroWPA<-boxd$clWPA[match(Hero,boxd$pid)]
    gmd$goatId<-Goat
    gmd$goatNm<-boxd$pnm[match(Goat,boxd$pid)]
    gmd$goatTm<-boxd$tm[match(Goat,boxd$pid)]
    gmd$goatWPA<-boxd$clWPA[match(Goat,boxd$pid)]
    gmd$mgpId<-MGP
    gmd$mgpNm<-boxd$pnm[match(MGP,boxd$pid)]
    gmd$mgpTm<-boxd$tm[match(MGP,boxd$pid)]
    gmd$mgpWPA<-boxd$gbWPA[match(MGP,boxd$pid)]
    gmd$lgpId<-LGP
    gmd$lgpNm<-boxd$pnm[match(LGP,boxd$pid)]
    gmd$lgpTm<-boxd$tm[match(LGP,boxd$pid)]
    gmd$lgpWPA<-boxd$gbWPA[match(LGP,boxd$pid)]
    # comeback Hero and Scapegoat #
    # find comeback point #
    cbf<-pbpd[,c("rid","wp")]
    won<-cbf$wp[nrow(cbf)]
    cbtm<-gmd$htm
    if (won==0) {
      cbf$wp<-1-cbf$wp
      cbtm<-gmd$atm
    }
    cbf<-cbf[order(cbf$wp),]
    cbr<-cbf$r[1]
    cbw<-dwpa[dwpa$r>cbr & dwpa$tm==cbtm,]
    cbw<-cbw[!is.na(cbw$gid),]
    if (nrow(cbw)>0) {
      cbw<-aggregate(cbw[c("WPA")],by=list(cbw$pid),FUN=sum)
      cbw<-cbw[order(-cbw$WPA),]
      cbHero<-cbw$Group.1[1]
      gmd$cbHeroId<-cbHero
      gmd$cbHeroNm<-boxd$pnm[match(cbHero,boxd$pid)]
      gmd$cbHeroTm<-boxd$tm[match(cbHero,boxd$pid)]
      gmd$cbHeroWPA<-cbw$WPA[1]	
    } else {
      gmd$cbHeroId<- -1
      gmd$cbHeroNm<-""
      gmd$cbHeroTm<-""
      gmd$cbHeroWPA<-0
    }
    cbw<-dwpa[dwpa$r<=cbr & dwpa$tm==cbtm & dwpa$pid>0,]
    if (nrow(cbw)>0) {
      cbw<-aggregate(cbw[c("WPA")],by=list(cbw$pid),FUN=sum)
      cbw<-cbw[order(cbw$WPA),]
      cbGoat<-cbw$Group.1[1]
      gmd$cbGoatId<-cbGoat
      gmd$cbGoatNm<-boxd$pnm[match(cbGoat,boxd$pid)]
      gmd$cbGoatTm<-boxd$tm[match(cbGoat,boxd$pid)]
      gmd$cbGoatWPA<-cbw$WPA[1]
    } else {
      gmd$cbGoatId<- -1
      gmd$cbGoatNm<-""
      gmd$cbGoatTm<-""
      gmd$cbGoatWPA<-0
    }
    
    # create wp sparkline #
    m1<-seq(from=0,to=max(pbpo$gt),by=60)
    m2<-round(c(0.5,pbpo$wp[findInterval(m1,pbpo$gt)]),2)
    gmd$hspk<-paste(m2,collapse=",")
    gmd$aspk<-paste(1-m2,collapse=",")
    gmd$po<-po
    # assign playoffs #
    gmd$po<-ifelse(substr(gmd$gid,1,3)=="004",1,0)
    gmd$po<-ifelse(substr(gmd$gid,1,3)=="006",2,gmd$po) # set NBA cup to po=2 #
    
    # create jerry west highlight reel ----
    jwhr<-dwpa %>%
      select(gid,r,pid,pt=pt1,clWPA) %>%
      bind_rows(select(owpa,gid,r,pid,pt=pt2,pta=pt1,clWPA)) %>%
      bind_rows(select(rwpa,gid,r,pid,pt=pt1,clWPA)) %>%
      # 0.015 is roughly the 90th percentile of clutch impact #
      filter(abs(clWPA)>=0.015) %>%
      inner_join(select(pbpd,gid,r,rid,d,di,q,min,sec,h,a),join_by(gid,r)) %>%
      inner_join(select(box,gid,pid,tmst),join_by(gid,pid)) %>%
      # exclude minute 11 due to weird win probability behavior #
      filter(min<11) %>%
      # calculate score prior to play #
      mutate(a_pre=ifelse(di=="h",a,
                          ifelse(substr(pt,1,2)=="MK",
                                 ifelse(pt=="MK2",a-2,ifelse(pt=="MK3",a-3,a-1)),
                                 ifelse(pt=="AST",ifelse(pta=="MK2",a-2,a-3),a)))) %>%
      mutate(h_pre=ifelse(di=="a",h,ifelse(substr(pt,1,2)=="MK",ifelse(pt=="MK2",h-2,ifelse(pt=="MK3",h-3,h-1)),
                                           ifelse(pt=="AST",ifelse(pta=="MK2",h-2,h-3),h)))) %>%
      inner_join(select(gmd,gid,ssn,po,dt,htm,atm,hscr,ascr),join_by(gid))
    
    jwl.lst[[gid]]<-jwhr
    
    
    # find dopplegamers #
    p<-strsplit(gmd$hspk,",")[[1]]
    rl<-lapply(hl,fx)
    rld<-data.frame(cbind(names(rl),unlist(rl)),stringsAsFactors = F)
    colnames(rld)<-c("gid","df")
    rld<-rld[rld$gid != gid,]
    rld$df<-as.numeric(rld$df)^0.5
    rld<-rld[order(rld$df),]
    rld$scr<-gmdg$scr[match(rld$gid,gmdg$gid)]
    rld$spk<-gmdg$hspk[match(rld$gid,gmdg$gid)]
    gmd$g1<-rld$gid[1]
    gmd$s1<-rld$df[1]
    gmd$spk1<-rld$spk[1]
    gmd$scr1<-rld$scr[1]
    gmd$g2<-rld$gid[2]
    gmd$s2<-rld$df[2]
    gmd$spk2<-rld$spk[2]
    gmd$scr2<-rld$scr[2]
    gmd$g3<-rld$gid[3]
    gmd$s3<-rld$df[3]
    gmd$spk3<-rld$spk[3]
    gmd$scr3<-rld$scr[3]
    
    # ad clutch efficiency #
    gmd<-merge(gmd,apm,by=c("gid"))
    gmd.lst[[gid]]<-gmd
    
    # create plus/minus #
    sub<-sb[["a"]]
    mq<-max(pbpo$q)
    sql<-vector('list')
    for (q in 1:mq) {
      sq<-sub[sub$q==q,] %>%
        filter(!is.na(gid))
      ugt<-min(pbpo$r[pbpo$q==q])
      if (nrow(sq)>0) {
        agt<-aggregate(sq$r,by=list(sq$gt),FUN=min)$x
        ugt<-c(ugt,agt)
        ugt<-ugt[order(ugt)]
      }
      gts<-sq$gt[match(ugt,sq$r)]
      gts[1]<-ifelse(q<=5,(q-1)*720,720*4+(q-1)*5*60)
      gte<-c(gts[2:length(gts)],NA)
      gte[length(gte)]<-ifelse(q<=4,q*720,4*720+(q-4)*5*60)
      sec<-gte-gts
      p1<-pbpo[pbpo$q==q & pbpo$pid1>0,c("r","gt","pid1")]
      p2<-pbpo[pbpo$q==q & pbpo$pid2>0,c("r","gt","pid2")]
      colnames(p1)<-colnames(p2)<-c("r","gt","pid")
      p<-rbind(p1,p2)
      p$ri<-ugt[findInterval(p$r,ugt)]
      pa<-unique(p[c("ri","pid")])
      pa$b<-1
      pl<-box$pid[box$tmst=="away" & box$min*60 + box$sec>0]
      pl<-data.frame(merge(pl,ugt))
      colnames(pl)<-c("pid","ri")
      pl$sec<-sec[match(pl$ri,ugt)]
      if (nrow(sq)==0) {
        pl$sec<-ifelse(q<=4,720,300)
      }
      pll<-split(pl,pl$pid)
      str<-box$pid[box$tmst=="away" & box$spos!=""]
      pll<-lapply(pll,oof)
      pld<-do.call('rbind',pll)
      pld<-cbind(q,pld)
      sql[[q]]<-pld
    }
    pma<-do.call('rbind',sql)
    sqla<-aggregate(pma$oof*pma$sec,by=list(pma$pid),FUN=sum)
    colnames(sqla)<-c("pid","dsec")
    sqla$sec<-box$sec[match(sqla$pid,box$pid)]
    
    sub<-sb[["h"]]
    mq<-max(pbpo$q)
    sql<-vector('list')
    for (q in 1:mq) {
      sq<-sub[sub$q==q,] %>%
        filter(!is.na(gid))
      ugt<-min(pbpo$r[pbpo$q==q])
      if (nrow(sq)>0) {
        agt<-aggregate(sq$r,by=list(sq$gt),FUN=min)$x
        ugt<-c(ugt,agt)
        ugt<-ugt[order(ugt)]
      }
      gts<-sq$gt[match(ugt,sq$r)]
      gts[1]<-ifelse(q<=5,(q-1)*720,720*4+(q-1)*5*60)
      gte<-c(gts[2:length(gts)],NA)
      gte[length(gte)]<-ifelse(q<=4,q*720,4*720+(q-4)*5*60)
      #gte<-gte[!is.na(gte)]
      sec<-gte-gts
      p1<-pbpo[pbpo$q==q & pbpo$pid1>0,c("r","gt","pid1")]
      p2<-pbpo[pbpo$q==q & pbpo$pid2>0,c("r","gt","pid2")]
      colnames(p1)<-colnames(p2)<-c("r","gt","pid")
      p<-rbind(p1,p2)
      p$ri<-ugt[findInterval(p$r,ugt)]
      pa<-unique(p[c("ri","pid")])
      pa$b<-1
      pl<-box$pid[box$tmst=="home" & box$min*60 + box$sec>0]
      pl<-data.frame(merge(pl,ugt))
      colnames(pl)<-c("pid","ri")
      pl$sec<-sec[match(pl$ri,ugt)]
      if (nrow(sq)==0) {
        pl$sec<-ifelse(q<=4,720,300)
      }
      pll<-split(pl,pl$pid)
      str<-box$pid[box$tmst=="home" & box$spos!=""]
      pll<-tryCatch(lapply(pll,oof),error=function(e) NULL)
      if (is.null(pll)) {next}
      pld<-do.call('rbind',pll)
      pld<-cbind(q,pld)
      sql[[q]]<-pld
    }
    pmh<-do.call('rbind',sql)
    sqlh<-aggregate(pmh$oof* pmh$sec,by=list(pmh$pid),FUN=sum)
    colnames(sqlh)<-c("pid","dsec")
    sqlh$sec<-box$sec[match(sqlh$pid,box$pid)]
    
    pm<-rbind(pma,pmh)
    pm<-cbind(gid,pm)
    pm.lst[[gid]]<-pm
    
    # create derived plus minus stats #
    # create stat counts from win probability file #
    wpr<-pbpo %>%
      mutate(lr1=lag(r),lpt1=lag(pt1),lr2=lag(r,n=2),lpt2=lag(pt1,n=2),lr3=lag(r,n=3),lpt3=lag(pt1,n=3)) %>% # back date 1st/2nd/3rd free throws #
      mutate(ar=ifelse(pt1 %in% c("MK11","MS11","MK12","MS12","MK22","MS22","MK13","MS13","MK23","MS23","MK33","MS33"),
                       ifelse(lpt1 %in% c('DF','DF1','DF2','DF3'),lr1,
                              ifelse(lpt2 %in% c('DF','DF1','DF2','DF3'),lr2,
                                     ifelse(lpt3 %in% c('DF','DF1','DF2','DF3'),lr3,r))),r)) %>%
      mutate(hfga2=ifelse(di=="h" & pt1 %in% c("MK2","MS2"),1,0)) %>%
      mutate(afga2=ifelse(di=="a" & pt1 %in% c("MK2","MS2"),1,0)) %>%
      mutate(hfgm2=ifelse(di=="h" & pt1 %in% c("MK2"),1,0)) %>%
      mutate(afgm2=ifelse(di=="a" & pt1 %in% c("MK2"),1,0)) %>%
      mutate(hfga3=ifelse(di=="h" & pt1 %in% c("MK3","MS3"),1,0)) %>%
      mutate(afga3=ifelse(di=="a" & pt1 %in% c("MK3","MS3"),1,0)) %>%
      mutate(hfgm3=ifelse(di=="h" & pt1 %in% c("MK3"),1,0)) %>%
      mutate(afgm3=ifelse(di=="a" & pt1 %in% c("MK3"),1,0)) %>%
      mutate(hfta=ifelse(di=="h" & pt1 %in% c("MK11","MS11","MK12","MS12","MK22","MS22","MK13","MS13","MK23","MS23","MK33","MS33"),1,0)) %>%
      mutate(afta=ifelse(di=="a" & pt1 %in% c("MK11","MS11","MK12","MS12","MK22","MS22","MK13","MS13","MK23","MS23","MK33","MS33"),1,0)) %>%
      mutate(hftm=ifelse(di=="h" & pt1 %in% c("MK11","MK12","MK22","MK13","MK23","MK33"),1,0)) %>%
      mutate(aftm=ifelse(di=="a" & pt1 %in% c("MK11","MK12","MK22","MK13","MK23","MK33"),1,0)) %>%
      mutate(hdreb=ifelse(di=="h" & pt1 %in% c("DRB"),1,0)) %>%
      mutate(adreb=ifelse(di=="a" & pt1 %in% c("DRB"),1,0)) %>%
      mutate(horeb=ifelse(di=="h" & pt1 %in% c("ORB"),1,0)) %>%
      mutate(aoreb=ifelse(di=="a" & pt1 %in% c("ORB"),1,0)) %>%
      mutate(hto=ifelse(di=="h" & pt1 %in% c("TO"),1,0)) %>%
      mutate(ato=ifelse(di=="a" & pt1 %in% c("TO"),1,0)) %>%
      mutate(tsva=tsv-lag(tsv)) %>%
      mutate(gta=gt-lag(gt,default=0))
    
    # simple cartesian merge for play by play file #
    pma<-pm %>%
      arrange(pid,ri) %>%
      group_by(pid) %>%
      mutate(lri=lead(ri,default=100000,order_by=pid)) %>%
      inner_join(dplyr::select(wpr,gid,r,ar,di,wp,wpa,m,ma,hfga2:gta),by="gid") %>%
      inner_join(dplyr::select(box,pid,tmst,pnm),by="pid") %>%
      filter(ar>=ri,ar<lri) %>%
      dplyr::select(gid,tmst,pid,pnm,q,r,di,oof,wp,wpa,m,ma,hfga2:gta) %>%
      # calculate wpa #
      mutate(wpa=ifelse(tmst=="home",wpa,-wpa)) %>%
      # calculate offense stats #
      mutate(om=ifelse(tmst=="home",ifelse(ma>0,ma,0),ifelse(ma<0,-ma,0))) %>%
      mutate(ofga2=ifelse(tmst=="home",hfga2,afga2)) %>%
      mutate(ofgm2=ifelse(tmst=="home",hfgm2,afgm2)) %>%
      mutate(ofga3=ifelse(tmst=="home",hfga3,afga3)) %>%
      mutate(ofgm3=ifelse(tmst=="home",hfgm3,afgm3)) %>%
      mutate(ofta=ifelse(tmst=="home",hfta,afta)) %>%
      mutate(oftm=ifelse(tmst=="home",hftm,aftm)) %>%
      mutate(oreb=ifelse(tmst=="home",horeb,aoreb)) %>%
      mutate(oto=ifelse(tmst=="home",hto,ato)) %>%
      # calculate defense stats #
      mutate(dm=ifelse(tmst=="away",ifelse(ma>0,ma,0),ifelse(ma<0,-ma,0))) %>%
      mutate(dfga2=ifelse(tmst=="away",hfga2,afga2)) %>%
      mutate(dfgm2=ifelse(tmst=="away",hfgm2,afgm2)) %>%
      mutate(dfga3=ifelse(tmst=="away",hfga3,afga3)) %>%
      mutate(dfgm3=ifelse(tmst=="away",hfgm3,afgm3)) %>%
      mutate(dfta=ifelse(tmst=="away",hfta,afta)) %>%
      mutate(dftm=ifelse(tmst=="away",hftm,aftm)) %>%
      mutate(dreb=ifelse(tmst=="away",horeb,aoreb)) %>%
      mutate(dto=ifelse(tmst=="away",hto,ato))
    
    # calculate simplified stints for win probability file #
    pmst<-pma %>%
      dplyr::select(gid,pid,pnm,r,oof) %>%
      mutate(chg=abs(sign(oof-lag(oof))),chg=ifelse(is.na(chg),lead(chg),chg)) %>%
      mutate(stint=cumsum(chg)+1) %>%
      group_by(gid,pid,stint,oof) %>%
      summarise(mnr=min(r),mxr=max(r))
    
    # summarize on/off stats #
    pmoof <- pma %>%
      group_by(gid,tmst,pid,pnm) %>%
      dplyr::summarise(on_gt=sum(gta*oof),off_gt=sum(gta*(1-oof)),
                       on_wpa=sum(wpa*oof),off_wpa=sum(wpa*(1-oof)),
                       on_awpa=sum(abs(wpa)*oof),off_awpa=sum(abs(wpa)*(1-oof)),
                       # offense stats #
                       on_om=sum(om*oof),off_om=sum(om*(1-oof)),
                       on_ofga2=sum(ofga2*oof),off_ofga2=sum(ofga2*(1-oof)),
                       on_ofgm2=sum(ofgm2*oof),off_ofgm2=sum(ofgm2*(1-oof)),
                       on_ofga3=sum(ofga3*oof),off_ofga3=sum(ofga3*(1-oof)),
                       on_ofgm3=sum(ofgm3*oof),off_ofgm3=sum(ofgm3*(1-oof)),
                       on_ofta=sum(ofta*oof),off_ofta=sum(ofta*(1-oof)),
                       on_oftm=sum(oftm*oof),off_oftm=sum(oftm*(1-oof)),
                       on_oreb=sum(oreb*oof),off_oreb=sum(oreb*(1-oof)),
                       on_oto=sum(oto*oof),off_oto=sum(oto*(1-oof)),
                       # defense stats #
                       on_dm=sum(dm*oof),off_dm=sum(dm*(1-oof)),
                       on_dfga2=sum(dfga2*oof),off_dfga2=sum(dfga2*(1-oof)),
                       on_dfgm2=sum(dfgm2*oof),off_dfgm2=sum(dfgm2*(1-oof)),
                       on_dfga3=sum(dfga3*oof),off_dfga3=sum(dfga3*(1-oof)),
                       on_dfgm3=sum(dfgm3*oof),off_dfgm3=sum(dfgm3*(1-oof)),
                       on_dfta=sum(dfta*oof),off_dfta=sum(dfta*(1-oof)),
                       on_dftm=sum(dftm*oof),off_dftm=sum(dftm*(1-oof)),
                       on_dreb=sum(dreb*oof),off_dreb=sum(dreb*(1-oof)),
                       on_dto=sum(dto*oof),off_dto=sum(dto*(1-oof))) %>%
      mutate(pm1=on_om-on_dm) %>%
      relocate(pm1,.after=pnm) %>%
      ungroup()
    
    # simple cartesian merge for possession file #
    psa<-pm %>%
      arrange(pid,ri) %>%
      group_by(pid) %>%
      mutate(lri=lead(ri,default=100000,order_by=pid)) %>%
      inner_join(dplyr::select(pos1,gid,posid,r,off,pts,time),by="gid") %>%
      inner_join(dplyr::select(box,pid,tmst,pnm),by="pid") %>%
      filter(r>=ri,r<lri) %>%
      dplyr::select(gid,off,tmst,pid,ri,r,pnm,posid,oof,pts,time) %>%
      # add in actual score at each rid #
      left_join(dplyr::select(pbpo,r,m),by=c("r"="r")) %>%
      mutate(offi=(tmst=="home" & off==htm) | (tmst=="away" & off!=htm)) %>%
      mutate(opos=ifelse(offi,1,0),dpos=ifelse(!offi,1,0)) %>%
      mutate(oposp = ifelse(offi,pts,0),dposp = ifelse(!offi,pts,0)) %>%
      mutate(opospc = ifelse(offi,time,0),dpospc = ifelse(!offi,time,0))
    
    # calculate simplified stints for possessions file #
    psst<-psa %>%
      dplyr::select(gid,pid,pnm,posid,oof) %>%
      mutate(chg=abs(sign(oof-lag(oof))),chg=ifelse(is.na(chg),lead(chg),chg)) %>%
      mutate(stint=cumsum(chg)+1) %>%
      group_by(gid,pid,stint,oof) %>%
      dplyr::summarise(mnp=min(posid),mxp=max(posid))
    
    psoof<-psa %>%
      group_by(gid,tmst,pid,pnm) %>%
      dplyr::summarise(on_opos=sum(opos*oof),off_opos=sum(opos*(1-oof)),
                       on_dpos=sum(dpos*oof),off_dpos=sum(dpos*(1-oof)),
                       on_opts=sum(oposp*oof),off_opts=sum(oposp*(1-oof)),
                       on_dpts=sum(dposp*oof),off_dpts=sum(dposp*(1-oof)),
                       on_opce=sum(opospc*oof),off_opce=sum(opospc*(1-oof)),
                       on_dpce=sum(dpospc*oof),off_dpce=sum(dpospc*(1-oof))) %>%
      mutate(pm2=on_opts-on_dpts)  %>%
      ungroup()
    
    # combine file  #
    tot<-pmoof %>%
      dplyr::select(-tmst,-pnm) %>%
      left_join(dplyr::select(psoof,-tmst,-pnm),by=c("gid","pid"))
    
    # merge in with box score file #
    boxd <- boxd %>%
      left_join(tot,by=c("gid","pid"))
    
    wpa.lst[[gid]]<-boxd
    
    pmst.lst[[gid]]<-pmst
    psst.lst[[gid]]<-psst
    
    ugt<-unique(pm$ri)
    ugt<-ugt[order(ugt)]
    
    if (length(ugt)>0) {
      pbpo$sri<-ugt[findInterval(pbpo$r,ugt)]
    } else {
      pbpo$sri<-0
    }
    
    wpg.lst[[gid]]<-pbpo
    
    print(paste(i," of ",length(pbp.lst),sep=""))
    flush.console()
  }
  later<-Sys.time()
  later-now
  
  if (!replace) {
    con <- db_local()
    
    imygm<-dbGetQuery(con,"SELECT DISTINCT gid FROM gms")
    imywpg<-dbGetQuery(con,"SELECT DISTINCT gid FROM wpg")
    imybox<-dbGetQuery(con,"SELECT DISTINCT gid FROM box")
    imyclp<-dbGetQuery(con,"SELECT DISTINCT gid FROM clp")
    imyshl<-dbGetQuery(con,"SELECT DISTINCT gid FROM shl")
    imypos<-dbGetQuery(con,"SELECT DISTINCT gid FROM pos")
    imyscl<-dbGetQuery(con,"SELECT DISTINCT gid FROM scl")
    imypm<-dbGetQuery(con,"SELECT DISTINCT gid FROM pm")
    #imysps<-dbGetQuery(con,"SELECT DISTINCT gid FROM sps")
    imyoff<-dbGetQuery(con,"SELECT DISTINCT gid FROM off")
    imydwp<-dbGetQuery(con,"SELECT DISTINCT gid FROM dwp")
    imyowp<-dbGetQuery(con,"SELECT DISTINCT gid FROM owp")
    imyrwp<-dbGetQuery(con,"SELECT DISTINCT gid FROM rwp")
    imyjwhr<-dbGetQuery(con,"SELECT DISTINCT gid FROM jwhr")
    imytim<-dbGetQuery(con,"SELECT DISTINCT gid FROM tim")
    imysptim<-dbGetQuery(con,"SELECT DISTINCT gid FROM sptim")
    
    dbDisconnect(con)
    
    
    # fix pm.lst #
    pm.lst<-pm.lst[unlist(lapply(pm.lst,class))=="data.frame"]
    
    mygm<-do.call('rbind',gmd.lst[!(names(gmd.lst) %in% imygm[,1])])
    mywpg<-do.call('rbind',wpg.lst[!(names(wpg.lst) %in% imywpg[,1])])
    mybox<-do.call('rbind',wpa.lst[!(names(wpa.lst) %in% imybox[,1])])
    myclp<-do.call('rbind',clp.lst[!(names(clp.lst) %in% imyclp[,1])])
    myshl<-do.call('rbind',shl.lst[!(names(shl.lst) %in% imyshl[,1])])
    mypos<-do.call('rbind',pos.lst[!(names(pos.lst) %in% imypos[,1])])
    myscl<-do.call('rbind',scl.lst[!(names(scl.lst) %in% imyscl[,1])])
    mypm<-do.call('rbind',pm.lst[!(names(pm.lst) %in% imypm[,1])])
    mysps<-do.call('rbind',sps.lst)
    myoff<-do.call('rbind',off.lst[!(names(off.lst) %in% imyoff[,1])])
    mydwp<-do.call('rbind',dwp.lst[!(names(dwp.lst) %in% imydwp[,1])])
    myowp<-do.call('rbind',owp.lst[!(names(owp.lst) %in% imyowp[,1])])
    myowp$pt1<-NULL
    myrwp<-do.call('rbind',rwp.lst[!(names(rwp.lst) %in% imyrwp[,1])])
    myjwhr<-do.call('rbind',jwl.lst[!(names(jwl.lst) %in% imyjwhr[,1])])
    
    # game time files #
    tdf.lst<-lapply(gmt.lst,function(x) {x[["tdf"]]})
    spc.lst<-lapply(gmt.lst,function(x) {x[["spc"]]})
    qb.lst<-lapply(gmt.lst,function(x) {x[["qb"]]})
    mytdf<-do.call('rbind',tdf.lst[!(names(tdf.lst) %in% imytim[,1])])
    myspc<-do.call('rbind',spc.lst[!(names(spc.lst) %in% imysptim[,1])])
  }
  
  if (replace) {
    # fix pm.lst #
    pm.lst<-pm.lst[unlist(lapply(pm.lst,class))=="data.frame"]
    
    mygm<-do.call('rbind',gmd.lst)
    mywpg<-do.call('rbind',wpg.lst)
    mybox<-do.call('rbind',wpa.lst)
    myclp<-do.call('rbind',clp.lst)
    myshl<-do.call('rbind',shl.lst)
    mypos<-do.call('rbind',pos.lst)
    myscl<-do.call('rbind',scl.lst)
    mypm<-do.call('rbind',pm.lst)
    mysps<-do.call('rbind',sps.lst)
    mydwp<-do.call('rbind',dwp.lst)
    myowp<-do.call('rbind',owp.lst)
    myowp$pt1<-NULL
    myrwp<-do.call('rbind',rwp.lst)
    myjwhr<-do.call('rbind',jwl.lst)
    
    # game time files #
    tdf.lst<-lapply(gmt.lst,function(x) {x[["tdf"]]})
    spc.lst<-lapply(gmt.lst,function(x) {x[["spc"]]})
    qb.lst<-lapply(gmt.lst,function(x) {x[["qb"]]})
    mytdf<-do.call('rbind',tdf.lst)
    myspc<-do.call('rbind',spc.lst)
    myoff<-do.call('rbind',off.lst)
  }
  
  if (replace) {
    # create list of game ids
    gi<-names(pbp.lst)
    gi<-paste(gi,collapse="','")
    gi<-paste("('",gi,"')",sep="")
    # delete existing rows
    # set connections #
    con <- db_local()
    
    dbGetQuery(con,paste("DELETE FROM gms WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM wpg WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM box WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM clp WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM shl WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM pos WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM scl WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM pm WHERE gid IN ",gi,sep=""))
    #dbGetQuery(con,paste("DELETE FROM sps WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM off WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM dwp WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM owp WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM rwp WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM jwhr WHERE gid IN ",gi,sep=""))
    
    dbGetQuery(con,paste("DELETE FROM tim WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM sptim WHERE gid IN ",gi,sep=""))
    
    dbDisconnect(con)
  }
  
  mypos$home<-mypos$cnt<-NULL
  mygm$status<-mygm$per<-mygm$minsec<-NULL
  
  # output to mysql #
  # set connections #
  con <- db_local()
  
  dbWriteTable(con,"gms",mygm,row.names=F,append=T)
  dbWriteTable(con,"wpg",mywpg,row.names=F,append=T)
  dbWriteTable(con,"box",mybox,row.names=F,append=T)
  dbWriteTable(con,"clp",myclp,row.names=F,append=T)
  dbWriteTable(con,"shl",myshl,row.names=F,append=T)
  dbWriteTable(con,"pos",mypos,row.names=F,append=T)
  dbWriteTable(con,"scl",myscl,row.names=F,append=T)
  dbWriteTable(con,"pm",mypm,row.names=F,append=T)
  #dbWriteTable(con,"sps",mysps,row.names=F,append=T)
  dbWriteTable(con,"dwp",mydwp,row.names=F,append=T)
  dbWriteTable(con,"owp",myowp,row.names=F,append=T)
  dbWriteTable(con,"rwp",myrwp,row.names=F,append=T)
  dbWriteTable(con,"jwhr",myjwhr,row.names=F,append=T)
  
  dbWriteTable(con,"tim",mytdf,row.names=F,append=T)
  dbWriteTable(con,"sptim",myspc,row.names=F,append=T)
  #dbWriteTable(con,"off",myoff,row.names=F,append=T) # comment out for now until I can find new endpoint
  
  dbDisconnect(con)

  if (replace) {
    # create list of game ids
    gi<-names(pbp.lst)
    gi<-paste(gi,collapse="','")
    gi<-paste("('",gi,"')",sep="")
    # delete existing rows
    con <- db_remote_new()
    
    dbGetQuery(con,paste("DELETE FROM gms WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM wpg WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM box WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM tim WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM clp WHERE gid IN ",gi,sep=""))
    dbGetQuery(con,paste("DELETE FROM jwhr WHERE gid IN ",gi,sep=""))
    
    dbDisconnect(con)
  }
  
  con <- db_remote_new()
  
  dbWriteTable(con,"gms",mygm,row.names=F,append=T)
  dbWriteTable(con,"wpg",mywpg,row.names=F,append=T)
  dbWriteTable(con,"box",mybox,row.names=F,append=T)
  dbWriteTable(con,"tim",mytdf,row.names=F,append=T)
  dbWriteTable(con,"clp",myclp,row.names=F,append=T)
  dbWriteTable(con,"jwhr",myjwhr,row.names=F,append=T)
  
  dbDisconnect(con)
  
  # pull in complete play by play #
  # reuse the play by play pulled above rather than downloading every game twice #
  jslst<-vector('list')
  for (gid in mygm$gid[1:nrow(mygm)]) {
    jslst[[gid]] <- pbprg.lst[[gid]] %||% nba_file(pbp_path(gid))
  }
  
  yl<-vector('list')
  now<-Sys.time()
  for (i in 1:length(jslst)) {
    x<-jslst[[i]]
    
    ch<-x$resultSets[[1]]$headers
    y<-x$game$actions
    if (length(y)==0) {next}
    
    y<-lapply(y,unlist)
    y<-bind_rows(y)
    gid<-x$game$gameId
    y$gid<-gid
    yl[[gid]]<-y
    print(gid)
    flush.console()
  }
  
  xd<-bind_rows(yl)
  
  xd2 <- xd %>%
    mutate_at(vars(actionNumber,period,personId,possession,scoreHome,scoreAway,
                   orderNumber,isFieldGoal,teamId,jumpBallRecoverdPersonId,
                   personIdsFilter,personIdsFilter1,personIdsFilter2,personIdsFilter3,
                   #personIdsFilter,
                   jumpBallWonPersonId,jumpBallLostPersonId,x,y,shotDistance,xLegacy,yLegacy,
                   personIdsFilter,shotActionNumber,reboundTotal,reboundDefensiveTotal,reboundOffensiveTotal,
                   pointsTotal,officialId,foulPersonalTotal,foulTechnicalTotal,foulDrawnPersonId,
                   assistPersonId,assistTotal,turnoverTotal,stealPersonId,blockPersonId,
                   jumpBallRecoverdPersonId),as.numeric)
  
  xd2 <- xd %>%
    mutate_at(vars(actionNumber,period,personId,possession,scoreHome,scoreAway,
                   orderNumber,isFieldGoal,teamId,jumpBallRecoverdPersonId,
                   personIdsFilter,personIdsFilter1,personIdsFilter2,personIdsFilter3,
                   jumpBallWonPersonId,jumpBallLostPersonId,x,y,shotDistance,xLegacy,yLegacy,
                   personIdsFilter,shotActionNumber,reboundTotal,reboundDefensiveTotal,reboundOffensiveTotal,
                   pointsTotal,officialId,foulPersonalTotal,foulTechnicalTotal,foulDrawnPersonId,
                   assistPersonId,assistTotal,turnoverTotal,stealPersonId,blockPersonId,
                   jumpBallRecoverdPersonId),as.numeric)
  
  # remove dupes #
  xd2 <- xd2 %>%
    group_by(gid,actionNumber) %>%
    slice_tail()
  
  gids<-unique(xd2$gid)
  gids<-paste0("'",gids,"'")
  gids<-paste(gids,collapse=",")
  
  # output to mysql #
  # set connections #
  con <- db_local()
  
  dbGetQuery(con,paste("DELETE FROM pbpr2 WHERE gid IN (",gids,")",sep=""))
  dbWriteTable(con,"pbpr2",xd2,row.names=F,append=T)
  
  dbDisconnect(con)
}

# print the error instead of exiting silently, so a 403 or a bad login shows up in the task log #
tryCatch(wrap_function(),error=function(e) message("wpNBA failed: ",conditionMessage(e)))

quit(save="no")
