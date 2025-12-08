get_verify_code <- function(platform,platform_id,google_client_json,google_account,login_time) {
  
  ## Platform
  platform <- if(tolower(platform) %in% c("pplx","ppx","perplexity")) "perplexity" else if(str_detect(tolower(platform),"gpt")) "chat gpt"
  
  ## Mail ID 추출
  mail_list_try_count <- 0L
  
  ## Get Token
  token <- token_fetch(scopes="https://www.googleapis.com/auth/gmail.readonly",
                       client=google_client_json,email=google_account)$refresh()$credentials$access_token
  
  repeat{
    mail_list <- "https://gmail.googleapis.com/gmail/v1" %>%
      request() %>%
      req_auth_bearer_token(token=token) %>%
      req_url_path_append("users/me/messages") %>%
      req_url_query(q=sprintf('from:%s after:%d in:inbox',
                              '(@openai.com OR @perplexity.ai OR @ascentnet.co.jp)',as.integer(as.POSIXct(login_time))),
                    maxResults=5) %>% 
      req_error(is_error=\(resp) FALSE) %>% 
      req_perform() %>% {
        if(.$status_code==401) {
          "https://gmail.googleapis.com/gmail/v1" %>%
            request() %>%
            req_auth_bearer_token(token=token) %>%
            req_url_path_append("users/me/messages") %>%
            req_url_query(q=sprintf('from:%s after:%d in:inbox',
                                    '(@openai.com OR @perplexity.ai OR @ascentnet.co.jp)',as.integer(as.POSIXct(login_time))),
                          maxResults=5) %>% 
            req_error(is_error=\(resp) FALSE) %>% 
            req_perform()
        } else {
          (.)
        }
      } %>% {
        resp_body_json(.)
      }
    
    if("messages" %in% names(mail_list)) break
    mail_list_try_count <- mail_list_try_count+1L
    token <- token_fetch(scopes="https://www.googleapis.com/auth/gmail.readonly",
                         client=google_client_json,email=google_account)$refresh()$credentials$access_token
    if(mail_list_try_count>15) stop()
    Sys.sleep(3.5)
  }
  
  
  ## Mail 상세 정보
  mail_list_detail <- lapply(mail_list$messages, \(x) {
    paste0("https://gmail.googleapis.com/gmail/v1/users/me/messages/",x$id) %>% 
      request() %>%
      req_auth_bearer_token(token=token) %>%
      req_error(is_error=\(resp) FALSE) %>% 
      req_perform() %>% {
        if(.$status_code==401) {
          paste0("https://gmail.googleapis.com/gmail/v1/users/me/messages/",x$id) %>%
            request() %>%
            req_auth_bearer_token(token=token) %>%
            req_perform()
        } else {
          (.)
        }
      } %>% {
        resp_body_json(.)
      } %>% {
        data.table(id=x$id,
                   to=rrapply(.,
                              condition=\(x, .xname, .xparents) .xname=="value" & "headers" %in% .xparents &
                                str_detect(x,"list") & str_detect(x,"@") & !str_detect(x,"sha"),
                              how="unlist") %>% unname %>% {
                                if(length(.)>0) (.) else NA_character_},
                   to2=.$payload$headers[[1]]$value,
                   body=paste(.$snippet,
                              if(any(sapply(.$payload$headers,\(x) str_detect(x$value,"perplexity")))) "perplexity" 
                              else if(any(sapply(.$payload$headers,\(x) str_detect(tolower(x$value),"gpt|open ai")))) "chat gpt"
                              else NA_character_),
                   send_date_time=.$internalDate %>% gsub("000$","",.) %>% as.numeric %>% as.POSIXct) %>% 
          transmute(id,to=case_when(!is.na(to) ~ to,.default=to2),body,send_date_time) %>% as.data.table
      }
  }) %>% rbindlist(fill=T)
  ## 인증코드 추출
  
  ### 여러 계정이 연결된 경우(메일링 리스트)
  if("to" %chin% names(mail_list_detail) && all(!is.na(mail_list_detail$to))) {
    mail_list_detail <- mail_list_detail[str_detect(to,gsub("@.*","",platform_id))]
  }
  
  res <- mail_list_detail[str_detect(tolower(body),tolower(platform))][order(-send_date_time)] %>% pull(body) %>% str_extract("\\d{6}") %>% {.[[1]]}
  return(res)
}

auto_login <- function(await_name="await",tab_name="tab",env=.GlobalEnv,
                       platform_id,platform_pw,
                       google_client_json,google_account) {
  
  await <- get(await_name,envir=env)
  tab <- get(tab_name,envir=env)
  url <- await(tab$evaluate("window.location.href"))
  platform <- if(str_detect(url,"chatgpt.com|openai.com")) "gpt" else if(str_detect(url,"perplexity.ai")) "pplx"
  set.seed(as.numeric(gsub("\\.","",format(Sys.time(),"%H%M%OS5")))/Sys.getpid())
  
  if(!str_detect(platform_id,"@ascentnet.co.jp")) platform_id <- paste0(platform_id,"@ascentnet.co.jp") else platform_id
  if(!str_detect(google_account,"@ascentnet.co.jp")) google_account <- paste0(google_account,"@ascentnet.co.jp") else google_account
  
  if(platform=="gpt") {
    ## GPT LONIN
    await(wait_clickable_nd(await=await,tab=tab,xpath='//button[@data-testid="login-button"]',timeout=10,check_all=F)[[1]]$click())
    ## GPT의 봇 검열 정책 강화로 직접 페이지 이동은 권장하지 않음
    # await(tab$get("https://auth.openai.com/log-in/password"))
    # await(tab$get("https://auth.openai.com/log-in-or-create-account"))
    id_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@autocomplete="email"]',timeout=15,check_all=F)[[1]]
    if(isFALSE(id_box)) {
      repeat{
        await(tab$reload())
        await(wait_clickable_nd(await=await,tab=tab,xpath='//button[@data-testid="login-button"]',timeout=15,check_all=F)[[1]]$click())
        id_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@autocomplete="email"]',timeout=15,check_all=F)[[1]]
        if(!isFALSE(id_box)) break
      }
    }
    
    Sys.sleep(runif(n=1,min=1.25,max=2.5))
    type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=id_box,strings=platform_id,send=TRUE)
    await(await(tab$xpath('//button[@data-dd-action-name="Continue"] | //button[descendant::div[text()="계속" or text()="Continue"]]'))[[1]]$click())
    while(length(await(tab$xpath('//li[text()="이메일을 적어주세요."]')))>0) {
      await(id_box$click())
      type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=id_box,strings=platform_id,send=TRUE)
      await(await(tab$xpath('//button[@data-dd-action-name="Continue"] | //button[descendant::div[text()="계속" or text()="Continue"]]'))[[1]]$click())
      Sys.sleep(1)
    }
    
    pw_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@autocomplete="new-password" or @autocomplete="current-password"]',timeout=10,check_all=F)[[1]]
    Sys.sleep(runif(n=1,min=0.7,max=1.5))
    type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=pw_box,strings=platform_pw,send=TRUE)
    if(length(await(tab$xpath('//button[@data-dd-action-name="Try again"]',timeout=3.5)))>0) {
      await(await(tab$xpath('//button[@data-dd-action-name="Try again"]',timeout=3.5))[[1]]$click())
    }
    while(length(await(tab$xpath('//li[text()="비밀번호가 필요합니다."]')))>0) {
      await(pw_$click())
      type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=pw_box,strings=platform_pw,send=TRUE)
      await(await(tab$xpath('//button[@data-dd-action-name="Continue"] | //button[descendant::div[text()="계속" or text()="Continue"]]'))[[1]]$click())
      Sys.sleep(1)
    }
    await(await(tab$xpath('//button[@data-dd-action-name="Continue"]'))[[1]]$click())
    login_time <- Sys.time()-minutes(1)
    code_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@maxlength="6"]',timeout=10,check_all=F)[[1]]
    Sys.sleep(3.5)
    verify_code <- get_verify_code(platform=platform,
                                   platform_id=platform_id,
                                   google_client_json=google_client_json,
                                   google_account=google_account,
                                   login_time=login_time)
    Sys.sleep(runif(n=1,min=1.5,max=2.7))
    type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=code_box,strings=verify_code,send=TRUE)
    
    while(length(await(tab$xpath('//li[text()="인증 코드가 필요합니다"]')))>0) {
      code_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@maxlength="6"]',timeout=10,check_all=F)[[1]]
      await(code_box$click())
      await(code_box$send_keys(as.character(verify_code)))
      Sys.sleep(1)
    }
    
    ## 앗, 오류가 발생했습니다!
    if(length(await(tab$xpath('//button[@data-dd-action-name="Try again"]')))>0) {
      await(await(tab$xpath('//button[@data-dd-action-name="Try again"]'))[[1]]$click())
    }
    
    Sys.sleep(runif(n=1,min=2.5,max=3.7))
    await(await(tab$xpath('//button[@data-dd-action-name="Continue"] | //button[descendant::div[text()="계속" or text()="Continue"]]'))[[1]]$click())
    
    ## 이런, 로그인에 문제가 있었습니다
    # if(length(await(tab$xpath('//button[.="돌아가기"]')))>0) {
    #   await(await(tab$xpath('//button[.="돌아가기"]'))[[1]]$click())
    # }
    # 
    
    ## Team Plan Workspace 선택
    workspace_selector <- wait_visible_nd(await=await,tab=tab,xpath='//div[@class="data-testid" or @aria-label="Select a workspace" or @data-testid="fullscreen-workspace-switcher"]',timeout=30)
    if(isFALSE(workspace_selector)) return(invisible(NULL))
    await(await(tab$xpath('//div[text()="ASCENTKOREA"]/ancestor::div[3]/descendant::button',timeout=10))[[1]]$click())
    
    ## 캡챠
    if(length(await(tab$xpath('//a[@class="cf-link"]',timeout=10)))>0) {
      await(tab$verify_cf())
    }
    
    ## 대기
    wait_visible_nd(await=await,tab=tab,xpath='//h2[text()="Projects" or text()="프로젝트"]',timeout=60)
  } else if(platform=="pplx") {
    ## PPLX LOGIN
    # Comet 홍보 요소가 있으면 DOM에서 제거
    # if(length(await(tab$xpath('//div[text()="Comet 다운로드" or text()="Download Comet"]',timeout=10)))>0) {
    #   await(await(tab$xpath('//div[contains(@class,"duration-200 fill-mode-both")]'))[[1]]$remove_from_dom())
    #   await(tab$mouse_click(x=runif(n=1,min=300,max=720),y=runif(n=1,min=420,max=700)))
    # }
    
    if(length(await(tab$xpath('//div[@id="cookie-consent"]',timeout=10)))>0) {
      await(await(tab$xpath('//div[@id="cookie-consent"]/descendant::button[1]'))[[1]]$click())
    }
    
    id_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@type="email"]',timeout=10)[[1]]
    type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=id_box,strings=platform_id,send=TRUE)
    
    await(tab$xpath('//div[text()="이메일로 계속하기" or text()="Continue with email"]'))[[1]]$click() %>% await
    login_time <- Sys.time()-minutes(1)
    
    
    code_box <- wait_clickable_nd(await=await,tab=tab,xpath='//input[@placeholder="코드 입력" or @placeholder="Enter Code"]',timeout=10)[[1]]
    Sys.sleep(3.5)
    verify_code <- get_verify_code(platform=platform,
                                   platform_id=platform_id,
                                   google_client_json=google_client_json,
                                   google_account=google_account,
                                   login_time=login_time)
    type_like_human(await_name=await_name,tab_name=tab_name,env=env,element=code_box,strings=verify_code,send=TRUE)
    
    if(length(await(tab$xpath('//div[text()="Comet 다운로드" or text()="Download Comet"]',timeout=10)))>0) {
      await(await(tab$xpath('//div[contains(@class,"duration-200 fill-mode-both")]'))[[1]]$remove_from_dom())
      await(tab$mouse_click(x=runif(n=1,min=300,max=720),y=runif(n=1,min=420,max=700)))
    }
  }
}
