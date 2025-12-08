create_sessions <- function(env_name,session=NULL,session_n) {
  if(!missing(env_name) && missing(session)) {
    assign(x=env_name,value=new.env(parent=.GlobalEnv),envir=.GlobalEnv)
    env <- get(env_name,envir=.GlobalEnv,inherits=F)
    lapply(seq_len(session_n), \(n) {
      assign(paste0("session",n),
             value=r_session$new(r_session_options(stdout="|",stderr="|")),
             envir=env)
    })
  } else if(!missing(session) && missing(env_name)) {
    assign(x=gsub(".*\\$","",deparse(substitute(session))),
           value=r_session$new(r_session_options(stdout="|",stderr="|")),
           envir=get(gsub("\\$.*","",deparse(substitute(session)))))
  }
}

boots_sessions <- function(sessions,base_dir,
                           args_command=NULL,
                           env=.GlobalEnv,
                           start_url="https://www.google.com/search?q=google&hl=en&gl=us&pws=0",
                           nodriver_name="nodriver",
                           loop_name="loop",
                           await_name="await",
                           nd_name="nd",
                           tab_name="tab",
                           cdp_name="cdp",
                           country_code=country_code,
                           accuracy=100,
                           proxy_url=NULL) {
  
  if(inherits(sessions,"r_session")) {
    sessions <- list(sessions)
  } else if(is.environment(sessions) && !inherits(sessions,"r_session")) {
    # sessions이 담긴 환경 객체인 경우
    ## sessions가 r_session이면 바로 사용 / 환경 변수이면 그 환경의 r_session 객체만 꺼내서 사용
    env0 <- sessions
    sessions <- mget(ls(sessions)[vapply(sessions,\(x) inherits(x,"r_session"),logical(1))],envir=env0)
  }
  
  lapply(sessions,\(session) {
    session$call(function(base_dir,
                          args_command,env,start_url,nodriver_name,loop_name,await_name,nd_name,tab_name,cdp_name,country_code,accuracy,proxy_url) {
      for(i in c('lubridate','dplyr','ggplot2','ggrepel','ggthemes','stringr','data.table',
                 'jsonlite','rvest','purrr','httr2','rrapply','callr','gargle','mirai',
                 'reticulate','ask','collapse','Rcpp','telegram.bot')) {
        tryCatch(suppressPackageStartupMessages(library(i,character.only=TRUE)),
                 error=\(e) { install.packages(i); library(i,character.only=TRUE)} )
      }
      
      mc <- reticulate::miniconda_path()
      py <- file.path(mc, "python.exe")
      
      # 1) PATH 표준화 (세션마다 동일하게 구성)
      prepend <- c(
        mc,
        file.path(mc, "Library", "bin"),
        file.path(mc, "Library", "mingw-w64", "bin"),
        file.path(mc, "Scripts"),
        file.path(mc, "condabin")
      )
      old <- strsplit(Sys.getenv("PATH"), ";", fixed=TRUE)[[1]]
      Sys.setenv(PATH = paste(c(prepend, old), collapse=";"))
      
      # 2) python.exe 고정
      Sys.setenv(RETICULATE_PYTHON = py)
      reticulate::use_python(py, required=TRUE)
      
      assign("asyncio", import("asyncio"), envir=.GlobalEnv)
      assign("nodriver", import("nodriver"), envir=.GlobalEnv)
      assign("base_dir",value=base_dir,envir=.GlobalEnv)
      if(!is.null(start_url)) {
        start_nodriver(args_command=args_command,
                       env=env,
                       start_url=start_url,
                       nodriver_name=nodriver_name,
                       loop_name=loop_name,
                       await_name=await_name,
                       nd_name=nd_name,
                       tab_name=tab_name,
                       cdp_name=cdp_name,
                       country_code=country_code,
                       accuracy=accuracy,
                       proxy_url=proxy_url)
      }
      
    },args=list(base_dir=base_dir,
                args_command=args_command,
                env=env,
                start_url=start_url,
                nodriver_name=nodriver_name,
                loop_name=loop_name,
                await_name=await_name,
                nd_name=nd_name,
                tab_name=tab_name,
                cdp_name=cdp_name,
                country_code=country_code,
                accuracy=accuracy,
                proxy_url=proxy_url))
  })
}

callr_logins <- function(sessions,
                         platform_ids,platform_pws,
                         google_client_jsons,google_accounts,
                         await_name="await",tab_name="tab",env=.GlobalEnv) {
  
  if(inherits(sessions,"r_session")) {
    sessions <- list(sessions)
  } else if(is.environment(sessions)) {
    # sessions이 담긴 환경 객체인 경우
    ## sessions가 r_session이면 바로 사용 / 환경 변수이면 그 환경의 r_session 객체만 꺼내서 사용
    env0 <- sessions
    sessions <- mget(ls(sessions)[vapply(sessions,\(x) inherits(x,"r_session"),logical(1))],envir=env0)
  }
  
  pmap(list(sessions,
            platform_ids,
            platform_pws,
            google_client_jsons,
            google_accounts),
       \(session,platform_id,platform_pw,google_client_json,google_account) {
         
         ## callr
         session$call(\(await_name=await_name,tab_name=tab,env=env,
                        platform_id,platform_pw,google_client_json,google_account) {
           
           auto_login(await_name=await_name,
                      tab_name=tab_name,
                      env=env,
                      platform_id=platform_id,
                      platform_pw=platform_pw,
                      google_client_json=get(google_client_json),
                      google_account=google_account)
         },args=list(await_name=await_name,
                     tab_name=tab_name,
                     env=env,
                     platform_id=platform_id,
                     platform_pw=platform_pw,
                     google_client_json=google_client_json,
                     google_account=google_account))
         ## callr
       })
  lapply(sessions,\(session) session$read())
}

callr_scraps <- function(expr,args,sessions,keywords,raw=NULL,alloc="first") {
  
  alloc <- match.arg(alloc,choices=c("first","onealloc","realloc"))
  
  if(is.environment(sessions) && !inherits(sessions,"r_session")) {
    # sessions이 담긴 환경 객체인 경우
    ## sessions가 r_session이면 바로 사용 / 환경 변수이면 그 환경의 r_session 객체만 꺼내서 사용
    env0 <- sessions
    sessions_names <- ls(env0,all.names=T)
    sessions <- mget(sessions_names[vapply(sessions_names,\(x) inherits(get(x,envir=env0,inherits=F),"r_session"),logical(1))],
                     envir=env0,inherits=F)
    i <- unname(sapply(names(sessions),\(x) as.numeric(gsub("\\D+","",x))))
  } else {
    i <- as.numeric(gsub("\\D+","",deparse(substitute(sessions))))
    sessions <- list(sessions)
  }
  
  n <- length(sessions)
  if(!is.null(raw)) {
    keyword_var <- if("original_keyword" %in% names(raw)) "original_keyword" else "keyword"
  } # Google AI Overview & AI Mode는 original_keyword를 기준으로 해야함
  
  if(!is.null(raw) && alloc=="onealloc") {
    sessions <- list(sessions[[1]])
    original_i <- i[[1]]
    i <- 1L
    n <- 1L
  }
  map2(sessions,i,\(session,i) {
    session$call(func=\(expr,args,keywords,i,save_i,n,raw,keyword_var,alloc) {
      if(is.null(raw)) {
        #raw가 없는 경우(최초 작업)
        keywords <- keywords %>% { .[((seq_along(.)-1)%%n)==(i-1)] }
      } else if(!is.null(raw) && alloc=="realloc") {
        #재작업+재분배
        keywords <- setdiff(keywords,unique(restore_comma(raw[[keyword_var]]))) %>%
          { .[((seq_along(.)-1)%%n)==(i-1)] }
      } else if(!is.null(raw) && alloc=="onealloc") {
        #재작업 분량을 하나에 몰아서
        ##만약 특정 단일 세션 분량만 재시도할거면 raw 값에 필터링한 값을 전달
        keywords <- setdiff(keywords,unique(restore_comma(raw[[keyword_var]])))
      } else if(is.null(raw) && alloc=="first") {
        stop()
      }
      
      if(length(keywords)==0L) return(invisible(NULL))
      args_i <- if(is.null(args)) list() else as.list(args)
      args_i$keywords <- keywords
      args_i$save_file_name <- gsub("\\{i\\}", as.character(save_i), args_i$save_file_name, perl=TRUE)
      do.call(expr,args_i)
    },args=list(expr=expr,
                args=args,
                keywords=keywords,
                i=i,
                n=n,
                save_i=if(exists("original_i")) original_i else i,
                raw=raw,
                keyword_var=if(is.null(raw)) NULL else keyword_var,
                alloc=alloc))
  })
}