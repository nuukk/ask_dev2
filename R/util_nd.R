wait_clickable_nd <- function(await=await,tab=tab,xpath,timeout,check_all=TRUE,interval=0.5) {
  js_is_displayed <- "(e) => {
    const style = window.getComputedStyle(e);
    const rect  = e.getBoundingClientRect();
    return (
      style.visibility !== 'hidden' &&
      style.display    !== 'none'   &&
      rect.width  > 0  &&
      rect.height > 0
    );
  }"
  js_is_enabled  <- "(e) => !e.disabled"
  
  start_time <- Sys.time()
  
  repeat {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    remaining <- timeout-elapsed
    if (remaining <= 0) return(FALSE)
    
    candidates <- tryCatch(
      await(tab$xpath(xpath, timeout = remaining)),
      error = function(e) NULL
    )
    if (is.null(candidates) || length(candidates) == 0L) {
      Sys.sleep(interval)
      next
    }
    
    ##
    statuses <- tryCatch(
      vapply(candidates, function(el) {
        disp <- await(el$apply(js_is_displayed))
        en   <- await(el$apply(js_is_enabled))
        disp && en
      }, logical(1)),
      error = function(e) rep(FALSE, length(candidates))  # 실패 시 전부 FALSE
    )
    
    ## 
    if(check_all) {
      if(all(statuses)) {
        return(candidates) 
      } else {
        return(FALSE)
      }
    } else {
      if(any(statuses)) {
        return(candidates[statuses])
      }
    }
    Sys.sleep(interval)
  }
}


wait_visible_nd <- function(await=await,tab=tab,xpath,timeout=2.5,check_all=FALSE) {
  js_is_displayed <- "(e)=>{          
    const style=window.getComputedStyle(e);
    const rect=e.getBoundingClientRect();
    return (
      style.visibility!=='hidden' &&
      style.display!=='none' &&
      rect.width>0 && rect.height>0
    );
  }"
  
  start_time <- Sys.time()
  repeat {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units='secs'))
    remaining <- timeout - elapsed
    if(remaining<=0) return(FALSE)
    
    candidates <- await(tab$xpath(xpath, timeout=remaining))
    if(length(candidates)>0) {
      vis_flags <- sapply(candidates, \(el) await(el$apply(js_is_displayed)))
      
      if(check_all) {
        if(all(vis_flags)) return(candidates)
      } else {
        if(any(vis_flags)) return(candidates[vis_flags])
      }
    }
    Sys.sleep(0.5)
  }
}


wait_invisible_nd <- function(await=await,tab=tab,xpath,timeout=10,interval=0.5) {
  xp_js <- jsonlite::toJSON(xpath, auto_unbox = TRUE)
  js_expr <- sprintf('
    (function(){
      const xp = %s;
      const snap = document.evaluate(
        xp, document, null,
        XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null
      );
      if (snap.snapshotLength === 0) return true;        

      for (let i = 0; i < snap.snapshotLength; i++) {
        const el = snap.snapshotItem(i);
        const st = getComputedStyle(el);
        const r  = el.getBoundingClientRect();
        if (
          st.display !== "none"        &&
          st.visibility !== "hidden"   &&
          parseFloat(st.opacity || "1") > 0 &&
          r.width  > 0 && r.height > 0
        ) return false;
      }               
      return true;   
    })();
  ', xp_js)
  
  start <- Sys.time()
  repeat {
    invisible_now <- await(
      tab$evaluate(js_expr, return_by_value = TRUE)
    )
    
    if (isTRUE(invisible_now)) return(TRUE) 
    if (difftime(Sys.time(), start, units = "secs") > timeout)
      return(FALSE)
    
    Sys.sleep(interval)
  }
}


save_screenshot_nd <- function(await=await,tab=tab,type,save_dir,save_file_name) {
  save_file_name <- gsub("/","／",save_file_name) %>% gsub("\\?","？",.) %>% gsub(":","：",.)
  type <- match.arg(type,c("Perplexity","ChatGPT","GGAI","Gemini","Copilot","AI_mode"))
  if(type=='Perplexity') {
    scroll_target <- '//h1'
    calc_target <- '//button[@aria-label="복사" or @aria-label="Copy"]'
    shot_target <- '//div[contains(@class,"@container/main")]'
  } else if(type=='ChatGPT') {
    scroll_target <- '//div[@class="whitespace-pre-wrap"]'
    calc_target1 <- '//button[@aria-label="Good response" or @aria-label="좋은 응답"] | //button[@aria-label="Copy" or @aria-label="복사"]'
    calc_target2 <- '//div[@slot="content"]/descendant::a'
    shot_target <- '//div[@id="thread"]'
    shot_target <- '//body'
  } else if(type=='GGAI') {
    scroll_target <- '(//div[@data-st-tgt="mode"] | //div[@role="navigation"])[1]'
    # calc_target <- '//div[@class="main"]'
    calc_target <- '//div[@role="navigation"]'
    shot_target <- '/html/body'
  } else if(type=='Gemini') {
    scroll_target <- '//p[contains(@class,"query-text")]'
    calc_target1 <- '//mat-icon[@fonticon="refresh"]'
    calc_target2 <- '//div[@class="gds-title-l"]/following::inline-source-card'
    # shot_target <- '//div[@class="content-wrapper"]'
    shot_target <- '//chat-window'
  } else if(type=='Copilot') {
    scroll_target <- '//h2[@aria-label="내 메시지"]'
    calc_target <- '//span[@class="p-1"]'
    shot_target <- '//div[@data-content="conversation"]'
  } else if(type=='AI_mode') {
    scroll_target <- '(//span[@aria-level="2" and @role="heading"])'
    calc_target <- '//button[@aria-label="Thumbs up" or @aria-label="Positive feedback" or @aria-label="긍정적인 의견"]'
    shot_target <- '/html/body'
  }
  # await(tab$xpath(scroll_target)) %>%
  #   {.[[1]]$apply("el => el.scrollIntoView({ behavior: 'smooth', block: 'center' })") %>%
  #       await}
  
  await(tab$xpath(scroll_target)) %>% {.[[1]]$scroll_into_view() %>% await}
  original_width <- await(tab$evaluate("window.innerWidth"))
  original_height <- await(tab$evaluate("window.innerHeight"))
  Sys.sleep(0.25)
  
  if(exists("calc_target2")) {
    # CHAT GPT OR Gemini
    if(length(await(tab$xpath(calc_target2,timeout=0.1)))==0) {
      # 출처 링크가 없는 경우
      calc_target <- calc_target1
      ref_node <- await(tab$xpath(calc_target))
      ref_node <- ref_node[[length(ref_node)]]
      
      rect <- await(ref_node$apply("
      el => {
      const r = el.getBoundingClientRect();
      return { x: r.left, y: r.top, width: r.width, height: r.height };
      }"))
      
      modify_height <- rect$y+rect$height
    } else {
      # 출처 링크가 있는 경우
      calc_target_tmp1 <- await(tab$xpath(calc_target1,timeout=1.5))
      calc_target_tmp1 <- calc_target_tmp1[[length(calc_target_tmp1)]]
      rect1 <- await(calc_target_tmp1$apply("
      el => {
      const r = el.getBoundingClientRect();
      return { x: r.left, y: r.top, width: r.width, height: r.height };
      }"))
      calc_target_tmp1 <- rect1$y+rect1$height
      
      calc_target_tmp2 <- await(tab$xpath(calc_target2,timeout=1.5))
      try(await(tab$xpath(scroll_target_tmp2)) %>% {.[[1]]$scroll_into_view() %>% await},silent=TRUE)
      calc_target_tmp2 <- calc_target_tmp2[[length(calc_target_tmp2)]]
      rect2 <- await(calc_target_tmp2$apply("
      el => {
      const r = el.getBoundingClientRect();
      return { x: r.left, y: r.top, width: r.width, height: r.height };
      }"))
      calc_target_tmp2 <- rect2$y+rect2$height
      
      if(calc_target_tmp1>=calc_target_tmp2) {
        modify_height <- calc_target_tmp1
      } else {
        modify_height <- calc_target_tmp2
      }
    }
  } else {
    # PPLX GGAI AIMODE COPILOT
    ref_node <- await(tab$xpath(calc_target))
    ref_node <- ref_node[[length(ref_node)]]
    rect <- await(ref_node$apply("
      el => {
      const r = el.getBoundingClientRect();
      return { x: r.left, y: r.top, width: r.width, height: r.height };
      }"))
    
    modify_height <- rect$y+rect$height
  }
  modify_height <- ceiling(modify_height)
  modify_height <- modify_height*1.2+150
  
  if(modify_height<=400) stop()
  nodriver$cdp$emulation$set_device_metrics_override(
    width=1920,height=as.integer(modify_height),device_scale_factor=1L,mobile=FALSE
  ) %>% { await(tab$send(.)) }
  
  await(tab$xpath(shot_target)) %>% 
    { .[[1]]$save_screenshot(filename=file.path(save_dir,paste0(save_file_name,'.png')),format='png') %>% 
        await }
  
  nodriver$cdp$emulation$clear_device_metrics_override() %>% {await(tab$send(.))}
}

start_nodriver <- function(args_command=NULL,
                           env=.GlobalEnv,
                           start_url="https://www.google.com/search?q=google&hl=en&gl=us&pws=0",
                           nodriver_name="nodriver",
                           loop_name="loop",
                           await_name="await",
                           nd_name="nd",
                           tab_name="tab",
                           cdp_name="cdp",
                           country_code="us",
                           accuracy=100,
                           proxy_url=NULL) {
  args <- c(
    "--disable-background-timer-throttling",
    "--disable-renderer-backgrounding",
    "--disable-backgrounding-occluded-windows",
    "--disable-features=IntensiveWakeUpThrottling,CalculateNativeWinOcclusion"
  )
  
  if(!is.null(proxy_url)) args <- append(args,paste0("--proxy-server=",proxy_url))
  
  if(!is.null(args_command)) args <- c(args_command, args)
  
  if(tolower(start_url) %in% c("ko","kr")) {
    start_url <- "https://www.google.com/search?q=구글&hl=ko&gl=kr&pws=0"
  } else if(!str_starts(start_url,"http")) {
    start_url <- paste0("https://www.google.com/search?q=google&hl=en&gl=",tolower(country_code),"&pws=0")
  }
  
  nodriver_obj <- get(nodriver_name, envir=env)
  assign("args", args, envir=env)
  
  assign(cdp_name, nodriver_obj$cdp, envir=env)
  loop_obj <- nodriver_obj$loop()
  assign(loop_name, loop_obj, envir=env)
  
  await_fun <- loop_obj$run_until_complete
  assign(await_name, await_fun, envir=env)
  
  nd_obj <- await_fun(nodriver_obj$start(browser_args=args))
  assign(nd_name, nd_obj, envir=env)
  
  tab_obj <- await_fun(nd_obj$get("about:blank"))
  assign(tab_name, tab_obj, envir=env)
  
  cdp_obj <- get(cdp_name, envir=env)
  await_fun(tab_obj$send(cdp_obj$network$enable()))
  register_handlers(tab_obj)
  
  ## SET LOCALE ##
  locale <- get_locale(country_code=country_code,accuracy=accuracy)
  locale$accept_language <- gsub("_","-",locale$accept_language)
  locale$locale <- gsub("-","_",locale$locale)
  
  # 1) Accept-Language
  await_fun(tab_obj$send(cdp_obj$network$set_user_agent_override(user_agent=await_fun(tab_obj$evaluate("navigator.userAgent")),
                                                                 accept_language=locale$accept_language)))
  
  # 2) locale
  await_fun(tab_obj$send(cdp_obj$emulation$set_locale_override(locale=locale$locale)))
  
  # 3) timezone
  await_fun(tab_obj$send(cdp_obj$emulation$set_timezone_override(timezone_id=locale$time_zone)))
  
  # 4) geo
  await_fun(tab_obj$send(cdp_obj$emulation$set_geolocation_override(latitude=as.numeric(locale$latitude),
                                                                    longitude=as.numeric(locale$longitude),
                                                                    accuracy=as.numeric(locale$accuracy))))
  
  
  await_fun(tab_obj$get(start_url))
  
  invisible(list(args=args,cdp=nodriver_obj$cdp,loop=loop_obj,await=await_fun,nd=nd_obj,tab=tab_obj))
}

google_captcha_solver <- function(captcha_api_key) {
  target <- await(tab$xpath('//div[@class="g-recaptcha"]',timeout=10))[[1]]
  
  sitekey <- target$attrs["data-sitekey"]
  data_s <- target$attrs["data-s"]
  
  # Cookie 추출
  google_cookies <- lapply(await(nd$cookies$get_all()),\(x) {
    if(str_detect(x$domain,"google.com$")) paste(x$name,x$value, sep="=") else NULL
  }) %>% unlist %>% paste0(collapse=";")
  
  # API Request
  api_request <- ans <- paste0("http://2captcha.com/in.php?key=",captcha_api_key,
                               "&method=userrecaptcha&googlekey=",sitekey,
                               "&data-s=",data_s,
                               "&cookies=",google_cookies,
                               "&useragent=",URLencode(await(tab$evaluate("window,navigator.userAgent"))),
                               # "&useragent=",URLencode(await((await(tab$xpath('//html')))[[1]]$apply('() => navigator.userAgent'))),
                               "&pageurl=",await(tab$evaluate("window.location.href"))) %>% 
    request %>% 
    req_perform()
  
  Sys.sleep(1.5)
  if(!str_starts(resp_body_string(ans),"OK")) {
    stop()
  }
  
  captcha_id <- resp_body_string(ans) %>% gsub("^OK\\|","",.)
  
  # API Response Check
  ela_time <- 0
  
  repeat{
    token <- paste0("http://2captcha.com/res.php?key=",captcha_api_key,"&action=get&id=",captcha_id) %>% 
      request %>% 
      req_perform
    
    if(str_starts(resp_body_string(token),"OK")) break
    Sys.sleep(5)
    ela_time <- ela_time+5
    if(ela_time>=180) break
  }
  
  # Token Extract
  token <- resp_body_string(token)
  
  if(!str_starts(token,"OK")) {
    # ERROR_CAPTCHA_UNSOLVABLE
    stop()
  }
  
  token <- token %>% gsub("^OK\\|","",.)
  
  # Token Injection
  sprintf("document.getElementById('g-recaptcha-response').innerHTML='%s'",token) %>% 
    { await(tab$evaluate(.)) }
  
  
  # Display Hide to Visible
  await(tab$evaluate("document.getElementById('g-recaptcha-response').style.removeProperty('display');"))
  
  # Callback Execute
  callback_js <- "document.querySelector('.g-recaptcha').getAttribute('data-callback')"
  callback <- await(tab$evaluate(callback_js))
  await(tab$evaluate(paste0(callback,"();")))
  
}

google_captcha <- function(await_name="await",nd_name="nd",tab_name="tab",env=.GlobalEnv,
                           start_url="https://www.google.com/search?q=google&hl=en&gl=us&pws=0",
                           chat_id=NULL,chat_token=NULL,ai_mode=TRUE,log_env=log_env,captcha_api_key=NULL,keyword=keyword,country_code="us") {
  
  log_env$captcha_detect_count <- log_env$captcha_detect_count+1L
  
  await <- get(await_name,envir=env)
  nd <- get(nd_name,envir=env)
  tab <- get(tab_name,envir=env)
  
  if(log_env$captcha_detect_count==1 ||
     length(await(tab$xpath('//div[contains(@id,"infoDiv")]/following::text()[contains(.,"≠")]')))>0) {
    # Session Restart/Reset
    await(tab$close())
    nd$stop()
    start_nodriver(await_name=await_name,nd_name=nd_name,tab_name=tab_name,env=env,start_url=start_url,country_code=country_code)
    await <- get(await_name,envir=env)
    nd <- get(nd_name,envir=env)
    tab <- get(tab_name,envir=env)
  } else {
    if(is.null(captcha_api_key)) {
      # Not using API
      stop()
    } else {
      # Using API
      captcha_api_retry_n <- 0
      
      repeat{
        send_msg(chat_id,chat_token,text=paste("[GGAI] Captcha API Call...",captcha_api_retry_n))
        captcha_api_call_res <- try(google_captcha_solver(captcha_api_key=captcha_api_key),silent=T)
        if(!inherits(captcha_api_call_res,"try-error")) break
        captcha_api_retry_n <- captcha_api_retry_n+1L
        if(captcha_api_retry_n>6) {
          log_env$ggai_history <- "ERROR - Captcha API 시도 횟수 초과"
          stop()
        }
      }
    }
  }
  
  # 검색
  await(tab$get(sprintf("https://www.google.com/search?q=%s&hl=%s&gl=%s&pws=0",
                        URLencode(keyword,reserved=T),
                        tolower(language_code),
                        tolower(country_code))))
  
  if(str_starts(await(tab$evaluate('window.location.href')),"https://www.google.com/sorry/") || 
     length(await(tab$xpath('//a[contains(@href,"google.com/sorry/")]',timeout=5)))>0) {
    return(google_captcha(await_name=await_name,tab_name=tab_name,env=env,start_url=start_url,chat_id=chat_id,chat_token=chat_token,ai_mode=ai_mode,log_env=log_env,captcha_api_key=captcha_api_key,keyword=keyword))
  }
  
  if(ai_mode) {
    ai_mode_elem <- wait_clickable_nd(await=await,tab=tab,xpath='//div[@role="listitem"][descendant-or-self::*[normalize-space(.)="AI Mode" or normalize-space(.)="AI 모드"]]',timeout=10)
    if(!isFALSE(ai_mode_elem)) {
      return(TRUE)
    } else {
      return(google_captcha(await_name=await_name,tab_name=tab_name,env=env,start_url=start_url,chat_id=chat_id,chat_token=chat_token,ai_mode=ai_mode,log_env=log_env,captcha_api_key=captcha_api_key,keyword=keyword))
    }
  } else {
    return(TRUE)
  }
}

type_like_human <- function(await_name="await",tab_name="tab",env=.GlobalEnv,
                            element,strings,send=FALSE) {
  await <- get(await_name, envir = env)
  tab <- get(tab_name, envir = env)
  
  await(element$click())
  await(element$focus())
  strings <- stringi::stri_split_boundaries(strings,type="character")[[1]]
  for(i in strings) {
    await(tab$send(nodriver$cdp$input_$dispatch_key_event(type_="keyDown")))
    await(tab$send(nodriver$cdp$input_$dispatch_key_event(type_="char", text=i)))
    await(tab$send(nodriver$cdp$input_$dispatch_key_event(type_="keyUp")))
    Sys.sleep(runif(n=1,min=0.03,max=0.15))
  }
  if(send) {
    await(tab$send(nodriver$cdp$input_$dispatch_key_event(type_="keyDown",key="Enter",code="Enter",windows_virtual_key_code=13)))
    await(tab$send(nodriver$cdp$input_$dispatch_key_event(type_="keyUp",key="Enter",code="Enter",windows_virtual_key_code=13)))
  }
}