# PERPLEXITY ========================================================
scrap_ppx_nd <- function(keywords,save_dir,save_file_name,delay_time=5,
                         await_name="await",tab_name="tab",env=.GlobalEnv,
                         save_page=FALSE,capture=TRUE,
                         chat_id=NULL,chat_token=NULL,
                         language_code='EN',country_code='US',
                         temp_chat=FALSE) {
  log_env <- new.env()
  total_steps <- length(keywords)
  if(py_module_available("nodriver")==FALSE) {
    log_env$pplx_history <- "ERROR - nodriver 미설치"
    stop("준비가 되지 않아 실행 불가")
  }
  
  await <- get(await_name,envir=env)
  tab <- get(tab_name,envir=env)
  input <- nodriver$cdp$input_
  if(!dir.exists(file.path(save_dir,"ppx_json_raw"))) dir.create(file.path(save_dir,"ppx_json_raw"))
  iwalk(keywords, \(keyword,i) {
    tryCatch({
      if(str_detect(await(tab$evaluate('window.location.href')), "perplexity.ai") & 
         str_detect(await(tab$evaluate('window.location.href')), "&version=2.18&source=default")) {
        await(tab$evaluate("window.history.back();"))
      }
      if(temp_chat) {
        # 임시 채팅일 때
        if(isFALSE(wait_visible_nd(await=await,tab=tab,xpath='//*[local-name()="path" and @fill-rule="evenodd"]',timeout=10))) {
          wait_clickable_nd(await=await,tab=tab,xpath='//div[text()="계정" or text()="Account"]',timeout=10)[[1]]$click() %>% await
          wait_clickable_nd(await=await,tab=tab,xpath='//button/descendant::div[text()="비공식" or text()="Incognito"]',timeout=10)[[1]]$click() %>% await
          wait_visible_nd(await=await,tab=tab,xpath='//*[local-name()="path" and @fill-rule="evenodd"]',timeout=10)
        }
      }
      wait_visible_nd(await=await,tab=tab,xpath='//button[@data-testid="sidebar-new-thread"]',timeout=3.5)[[1]]$click() %>% await
      log_env$pplx_history <- "새 쓰레드 선택 완료"
      Sys.sleep(0.5)
      if(await(tab$evaluate('window.location.href')) != "https://www.perplexity.ai/") {
        await(await(tab$xpath('//button[@data-testid="sidebar-new-thread"]'))[[1]]$click())
      }
      search_box <- wait_clickable_nd(await=await,tab=tab,'//div[@id="ask-input" and @role="textbox"]',timeout=3.5)[[1]]
      await(search_box$mouse_click())
      
      # Ctrl+A
      await(search_box$apply("
  el => {
    el.focus();
    el.dispatchEvent(new KeyboardEvent('keydown', { key: 'a', code: 'KeyA', ctrlKey: true, bubbles: true }));
    el.dispatchEvent(new KeyboardEvent('keyup',   { key: 'a', code: 'KeyA', ctrlKey: true, bubbles: true }));
  }
"))
      
      # Backspace
      await(search_box$apply("
  el => {
    el.dispatchEvent(new KeyboardEvent('keydown', { key: 'Backspace', code: 'Backspace', bubbles: true }));
    el.dispatchEvent(new KeyboardEvent('keyup',   { key: 'Backspace', code: 'Backspace', bubbles: true }));
  }
"))
      
      await(search_box$send_keys(keyword))
      await(tab$send(input$dispatch_key_event("keyDown",key="Enter",code="Enter",text="\r",windows_virtual_key_code=13)))
      Sys.sleep(1)
      if(await(tab$evaluate('window.location.href'))=="https://www.perplexity.ai/") {
        # 입력이 제대로 이루어지지 않을 경우
        await(await(tab$xpath('//button[@aria-label="Submit"]'))[[1]]$click())
      }
      log_env$pplx_history <- "쿼리 전송 완료"
      Sys.sleep(2.5)
      check_point <- wait_invisible_nd(await=await,tab=tab,timeout=90,interval=1,xpath='//button[@data-testid="stop-generating-response-button"]')
      if(!check_point) {
        log_env$pplx_history <- "ERROR - 제한 시간 초과"
        stop()
      }
      
      Sys.sleep(2)
      ppx_page <- await(tab$evaluate("window.location.href"))
      url_wait_time <- 0
      while(str_detect(ppx_page,"search/new\\?q=pending")) {
        Sys.sleep(0.5)
        url_wait_time <- url_wait_time+0.5
        ppx_page <- await(tab$evaluate("window.location.href"))
        if(url_wait_time>=10) {
          log_env$pplx_history <- "ERROR - URL Pending"
          stop()
        }
      }
      title_wait_time <- 0
      title <- tab$title
      while(title=='Perplexity') {
        Sys.sleep(0.5)
        title <- tab$title
        title_wait_time <- title_wait_time+0.5
        if(title_wait_time>=10) {
          log_env$pplx_history <- "ERROR - Title Freezing"
          stop()
        }
      }
      if(length(await(tab$xpath('//p[normalize-space(text())="Sorry, something went wrong"]')))>0) {
        log_env$pplx_history <- "ERROR - Sorrt, something went wrong"
        stop()
      }
      if(length(await(tab$xpath('//button/descendant::div[text()="Export"]')))>2) {
        log_env$pplx_history <- "ERROR - 2+ Questions in One Page"
        stop()
      }
      
      if(temp_chat) {
        if(length(await(tab$xpath('//*[local-name()="path" and @fill-rule="evenodd"]')))==0) {
          log_env$pplx_history <- "ERROR - 임시 채팅 적용 안됨"
          stop()
        }
      }
      
      # API Capture
      try_n <- 0
      repeat{
        if(try_n>1) { clear_captured(); await(tab$reload()); Sys.sleep(0.5+(try_n+1)*0.5) }
        await(wait_for_network_idle(tab))
        json_raw <- names(py_to_r(nd_response_capture()$captured_responses)) %>% 
          { .[str_detect(.,"thread")] } %>% 
          Filter(f=\(x) str_detect(x,"supported_block_use_cases")) %>%
          Filter(f=\(x) str_detect(x,
                                   gsub(".*ai/search/","",await(tab$evaluate("window.location.href"))) %>% 
                                     gsub("\\?.*","",.))) %>% 
          py_to_r(nd_response_capture()$captured_responses)[.] %>% { .[[1]] } %>% 
          fromJSON %>% try(silent=T)
        try_n <- try_n+1L
        if(!(inherits(json_raw,"try-error")) && is.list(json_raw)) break
        if(try_n>=5L) stop()
      }
      log_env$pplx_history <- "JSON 추출 완료"
      ## JSON 저장
      
      write_json(json_raw,path=file.path(save_dir,"ppx_json_raw",paste0(gsub(".*/","",await(tab$evaluate('window.location.href'))),".json")))
      log_env$pplx_history <- "JSON 저장 완료"
      ## 답변 본문 추출
      answer_text <- rrapply(json_raw,
                             condition=\(x, .xname, .xparents) .xname=="chunks" & "markdown_block" %in% .xparents,
                             classes=c("list"),
                             how="unlist") %>% unname %>% { .[length(.)] }
      log_env$pplx_history <- "답변 본문 추출 완료"
      direct_links_obs <- answer_text %>% str_extract_all("\\[\\d{1,}\\]") %>% { .[[1]] } %>% unique %>%
        gsub("\\D","",.) %>% as.numeric
      
      answer_text <- gsub("\\[\\d{1,}\\]","",answer_text)
      
      ## 링크 데이터 추출
      link_url_text <- rrapply(json_raw,
                               condition=\(x, .xname, .xparents, .xsiblings) .xname %in% c("url","name") &
                                 all(c("web_results","web_result_block") %in% .xparents) &
                                 all(c("url","name","meta_data","timestamp") %in% names(.xsiblings)),
                               classes=c("list","ANY"),
                               how="melt") %>% as.data.table %>% {
                                 if(nrow(.)>0) {
                                   {
                                     data.table(title=.[rowSums(.=="name",na.rm=T)>0,.(title=unlist(value)),by=setdiff(names(.),"value")],
                                                link=.[rowSums(.=="url",na.rm=T)>0,.(link=unlist(value)),by=setdiff(names(.),"value")])
                                   } %>%
                                     setnames(names(.),gsub("^.*\\.","",names(.))) %>%
                                     { .[,.(title,link,rank=seq_len(.N))] } %>%
                                     { .[rank %in% direct_links_obs,direct_indirect_link:="Direct Link"] } %>%
                                     { .[is.na(direct_indirect_link),direct_indirect_link:="Indirect Link"] }
                                 } else {
                                   data.table(title=NA_character_,link=NA_character_,rank=NA_integer_,direct_indirect_link=NA_character_)
                                 }
                               }
      log_env$pplx_history <- "링크 테이블 구성 완료"
      if(save_page) writeLines(await(tab$get_content()),con=file.path(save_dir,paste0(half_to_full(keyword),'.html')))
      if(capture) save_screenshot_nd(await=await,tab=tab,type='Perplexity',save_dir=save_dir,save_file_name=keyword)
      ## 쇼핑
      shopping <- await(tab$xpath('//h1/following::div[text()="Shopping" or text()="쇼핑"]'))
      if(length(shopping)==0) {
        # 쇼핑탭이 없는 경우
        shopping_url_text <- data.table(rank=NA,product=NA,url=NA,shop=NA,price=NA,avg_rating=NA,n_reviews=NA,buy_with_pro=NA)
      } else {
        # 쇼핑탭이 있는 경우
        await(shopping[[1]]$click())
        wait_visible_nd(await=await,tab=tab,xpath='//div[contains(@class,"text-pretty")]',timeout=10,check_all=T)
        scroll_target <- await(tab$xpath('//div[contains(@class,"items-start")]'))
        if(length(scroll_target)>0) {
          # More Products가 있어서 스크롤을 해야 하는 경우
          await(scroll_target[[length(scroll_target)]]$apply('el => el.scrollIntoView({behavior:"smooth", block:"center"})'))
        }
        Sys.sleep(3.5)
        invisible(wait_visible_nd(await=await,tab=tab,xpath='//div[contains(@class,"text-pretty")]',timeout=10,check_all=TRUE))
        page_src_shopping <- await(tab$get_content()) %>% read_html
        if(length(await(tab$xpath('//button[contains(@class,"group/root shadow-subtle")]')))>0) {
          #top3가 있는 경우
          top3 <- lapply(html_elements(page_src_shopping,xpath='//button[contains(@class,"group/root shadow-subtle")]'), \(x) {
            data.table(product=tryCatch(html_elements(x,xpath='./descendant::div[contains(@class,"line-clamp-3 text-pretty")]') %>% html_text2,
                                        error=\(e) {NA}),
                       url=tryCatch(html_elements(x,xpath='./descendant::a') %>% html_attr('href'),
                                    error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       shop=tryCatch(html_text2(html_elements(x,xpath='./descendant::a')),error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       price=html_elements(x,xpath='./descendant::div[not(contains(@class,"line-through")) and contains(text(),"$")]') %>% html_text2 %>% 
                         tryCatch(.,error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       avg_rating=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[1]') %>%
                         html_text2 %>% tryCatch(.,error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       n_reviews=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[2]') %>%
                         html_text2 %>% tryCatch(.,error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       buy_with_pro=html_elements(x,xpath='./descendant::div[normalize-space(text())="Buy now"]') %>% 
                         { if(length(.)==1) {
                           html_text2(.)
                         } else {
                           NA
                         } })
          }) %>% rbindlist
        } else {
          # top3가 없는 경우
          top3 <- NULL
        }
        if(length(await(tab$xpath('//div[text()="More Products" or text()="더 많은 제품"]')))>0) {
          # more가 있는 경우
          mores <- lapply(html_elements(page_src_shopping,xpath='//div[text()="More Products" or text()="더 많은 제품"]/following::button[contains(@class,"group/root")]'), \(x) {
            data.table(product=tryCatch(html_elements(x,xpath='./descendant::div[contains(@class,"line-clamp-2 text-pretty")]') %>% html_text2,
                                        error=\(e) {NA}),
                       url=tryCatch(html_elements(x,xpath='./descendant::a') %>% html_attr('href'),error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       shop=tryCatch(html_text2(html_elements(x,xpath='./descendant::a')),error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       price=html_elements(x,xpath='./descendant::div[not(contains(@class,"line-through")) and contains(text(),"$")]') %>% html_text2 %>% 
                         tryCatch(.,error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       avg_rating=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[1]') %>%
                         html_text2 %>% tryCatch(.,error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       n_reviews=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[2]') %>%
                         html_text2 %>% tryCatch(.,error=\(e) {NA}) %>% 
                         {
                           if(length(.)>0) (.)
                           else NA
                         },
                       buy_with_pro=html_elements(x,xpath='./descendant::div[normalize-space(text())="Buy now"]') %>% 
                         { if(length(.)==1) {
                           html_text2(.)
                         } else {
                           NA
                         } } )
          }) %>% rbindlist
        } else {
          # more가 없는 경우
          mores <- NULL
        }
        shopping_url_text <- rbindlist(list(top3,mores)) %>% mutate(rank=row_number(),.before=everything())
      }
      ans_shoppings <- data.table(tool='Manual',
                                  monitoring_date=Sys.Date(),
                                  keyword=keyword,
                                  platform='Perplexity',
                                  shopping_url_text,
                                  country_code=country_code,
                                  language_code=language_code,
                                  share_link=ppx_page)
      log_env$pplx_history <- "쇼핑 테이블 구성 완료"
      ans_shoppings[,names(ans_shoppings)[sapply(ans_shoppings,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans_shoppings)[sapply(ans_shoppings,\(x) is.character(x))]]
      fwrite(ans_shoppings,file=file.path(save_dir,paste0(save_file_name,'-Shop.csv')),bom=T,append=T)
      log_env$pplx_history <- "쇼핑 데이터 저장 완료"
      setcolorder(link_url_text,c("title","link","direct_indirect_link","rank"))
      ans <- data.table(tool='Manual',
                        monitoring_date=Sys.Date(),
                        keyword=keyword,
                        platform='Perplexity',
                        link_url_text,
                        answer_text,
                        country_code=country_code,
                        language_code=language_code,
                        share_link=ppx_page)
      ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
      fwrite(ans,file=file.path(save_dir,paste0(save_file_name,'-Data.csv')),bom=T,append=T,quote=T)
      log_env$pplx_history <- "본문 데이터 저장 완료"
      if(!is.null(chat_id) & !is.null(chat_token)) {
        send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[PPLX 추출 성공......]",keyword))
      }
      record_log2(country_code=country_code,language_code=language_code,platform='PPLX',keyword=keyword,step=i,total_steps=total_steps,status=TRUE,msg=".",save_dir=save_dir,save_file_name=save_file_name)
      Sys.sleep(delay_time+runif(n=1,min=0,max=15))
    },
    error=\(e) {
      if(!dir.exists(file.path(save_dir,"err_htmls"))) dir.create(file.path(save_dir,"err_htmls"))
      record_log(text1=paste0("[PPLX 추출 실패] ",keyword,"……",log_env$pplx_history),
                 text2=await(tab$evaluate('window.location.href')),
                 save_dir=save_dir,
                 save_file_name=paste0(save_file_name,'_fail_list'),
                 chat_id=chat_id,chat_token=chat_token)
      record_log2(country_code=country_code,language_code=language_code,platform='PPLX',keyword=keyword,step=i,total_steps=total_steps,status=FALSE,msg=log_env$pplx_history,save_dir=save_dir,save_file_name=save_file_name)
      writeLines(await(tab$get_content()),con=file.path(save_dir,"err_htmls",paste0(half_to_full(keyword),".html")))
      await(tab$reload())
      Sys.sleep(delay_time)
    })
  },.progress=TRUE)
}


# CHAT GPT ========================================================
scrap_gpt_nd <- function(keywords,save_dir,save_file_name,delay_time=30,
                         await_name="await",tab_name="tab",env=.GlobalEnv,
                         save_page=FALSE,capture=FALSE,
                         chat_id=NULL,chat_token=NULL,
                         language_code='EN',country_code='US',
                         project_name=NULL,model="ChatGPT 5",temp_chat=FALSE) {
  log_env <- new.env()
  total_steps <- length(keywords)
  if(py_module_available("nodriver")==FALSE) {
    log_env$gpt_history <- "ERROR - nodriver 미설치"
    stop("준비가 되지 않아 실행 불가")
  }
  
  await <- get(await_name,envir=env)
  tab <- get(tab_name,envir=env)
  input <- nodriver$cdp$input_
  if(!dir.exists(file.path(save_dir,"gpt_json_raw"))) dir.create(file.path(save_dir,"gpt_json_raw"))
  iwalk(keywords, \(keyword,i) {
    tryCatch({
      
      if(temp_chat) {
        # 임시채팅 -- 임시채팅에서는 json 캡쳐가 불가능함(2025-09-13)
        await(tab$xpath('//a[@data-testid="create-new-chat-button"]')) %>% {.[[2]]$mouse_click() %>% await}
        wait_clickable_nd(await=await,tab=tab,xpath='//button[@aria-label="Turn on temporary chat" or @aria-label="임시 채팅 켜기"]',timeout=10,check_all=F)[[1]]$click() %>% await
        wait_visible_nd(await=await,tab=tab,xpath='//h1[@data-testid="temporary-chat-label"]',timeout=10)
      } else {
        # 비임시채팅(일반)
        if(is.null(project_name)) {
          await(tab$xpath('//a[@data-testid="create-new-chat-button"]')) %>% {.[[2]]$mouse_click() %>% await}
        } else {
          await(tab$xpath(sprintf('//nav/descendant::a[@tabindex][descendant::div[text()="%s"]]',project_name))) %>% {.[[1]]$click() %>% await}
        }
      }
      log_env$gpt_history <- "새 채팅/프로젝트 선택 완료"
      Sys.sleep(1.5)
      ## Web 검색이 비활성화되어 있으면 클릭
      is_active_web_search <- await(tab$xpath('//button/span[text()="Search" or text()="검색"]'))
      if(length(is_active_web_search)==0) {
        await(tab$xpath('//button[@data-testid="composer-plus-btn"]',timeout=10)) %>% 
          {.[[1]]$mouse_click() %>% await}
        
        wait_clickable_nd(await=await,tab=tab,xpath='//div[text()="Search the web" or text()="Web search" or text()="웹 검색"]',timeout=5) %>% {
          if(isTRUE(.)) {
            #Web Search가 바로 보일 때
            await(.[[1]]$click())
          } else {
            #Web Search가 More에 숨겨져 있을 때
            await(tab$xpath('//div[@role="menuitem"]/descendant::div[text()="More" or text()="더 보기"]',timeout=10)) %>% 
              { .[[1]]$mouse_move() %>% await }
            
            await(tab$xpath('//div[text()="Search the web" or text()="Web search" or text()="웹 검색"]',timeout=10)) %>% 
              {.[[1]]$mouse_click() %>% await}
          }
        }
      }
      log_env$gpt_history <- "Web Search 활성화 완료"
      
      current_model <- await(tab$find_all('//button[contains(@aria-label,"Model selector") or contains(@aria-label,"모델 선택기")]/descendant::div'))[[2]]$text_all
      if(current_model != model) {
        await(tab$find_all('//button[contains(@aria-label,"Model selector") or contains(@aria-label,"모델 선택기")]/descendant::div'))[[2]] %>% 
          {.$mouse_click() %>% await}
        
        if(str_detect(model,"ChatGPT 5")) {
          await(tab$xpath('//div[@role="menuitem"]/descendant::span[text()="Auto"]'))[[1]]$click() %>% await
        } else {
          await(tab$xpath('//div[@data-testid="Legacy models-submenu"] |
                      //div[text()="레거시 모델"]'))[[1]]$click() %>% await
          await(tab$xpath(paste0('//div[@role="menu"]/descendant::span[text()="',gsub("Chat","",model),'"]')))[[1]]$click() %>% await
        }
      }
      log_env$gpt_history <- "모델 선택 완료"
      
      ## 검색
      search_box <- await(tab$xpath('//div[@id="prompt-textarea"]/p'))[[1]]
      await(search_box$mouse_click())
      await(search_box$apply("el => { el.innerText = ''; }")) #Clear Text
      # await(search_box$send_keys(keyword))
      # await(tab$send(input$dispatch_key_event("keyDown",key="Enter",code="Enter",text="\r",windows_virtual_key_code=13)))
      type_like_human(await_name="await",tab_name="tab",element=search_box,strings=keyword,send=T)
      
      if(!temp_chat) {
        check_point <- wait_clickable_nd(await=await,tab=tab,xpath='//button[@aria-label="좋은 응답" or @aria-label="Good response"]',timeout=90)
      } else {
        check_point <- wait_clickable_nd(await=await,tab=tab,xpath='//button[@aria-label="복사" or @aria-label="Copy"]',timeout=90)
      }
      
      if(!is.list(check_point)) {
        log_env$gpt_history <- "ERROR - 제한시간 초과"
        stop()
      }
      
      wait_invisible_nd(await=await,tab,xpath='//button[@data-testid="stop-button"]',timeout=45) %>% {
        if(isFALSE(.)) {
          log_env$gpt_history <- "ERROR - 제한시간 초과"
          stop()
        }
      }
      if(length(await(tab$xpath('//button/div[text()="Retry"]')))>0) {
        log_env$gpt_history <- "ERROR - Retry Button"
        stop()
      }
      if(length(await(tab$xpath('//div[@data-has-thread-error]',timeout=0.1)))>0) {
        log_env$gpt_history <- "ERROR - Regenerate Button"
        stop()
      }
      if(length(await(tab$xpath('//p[contains(text(),"reached our limit of messages per hour. Please try again later")]',timeout=0.1)))>0) {
        log_env$gpt_history <- "ERROR - 한도 초과"
        stop(critical_error())
      }
      if(length(await(tab$xpath('//p[text()="Something went wrong while generating the response. If this issue persists please contact us through our help center at "]')))>0) {
        log_env$gpt_history <- "ERROR - Wrong while generating the response"
        stop()
      }
      if(length(await(tab$xpath('//div[@class="whitespace-pre-wrap"]',timeout=0.1)))>1) {
        log_env$gpt_history <- "ERROR - 한 페이지에 두 개 이상의 질문-답변"
        stop()
      }
      if(length(await(tab$xpath('//button[@data-testid="regenerate-thread-error-button"]',timeout=0.1)))>1) {
        log_env$gpt_history <- "ERROR - Retry Button"
        stop()
      }
      
      if(!temp_chat) {
        # 임시채팅이 아닐 때
        # API Capture
        await(wait_for_network_idle(tab))
        
        try_n <- 0L
        repeat{
          if(try_n>1) { clear_captured();  await(tab$reload()); Sys.sleep(0.5*(try_n+1L)) }
          await(wait_for_network_idle(tab))
          json_raw <- names(py_to_r(nd_response_capture()$captured_responses)) %>%
            { .[str_detect(.,"backend-api/conversation")] } %>%
            Filter(f=\(x) !str_ends(x,"init|textdocs|false")) %>%
            Filter(f=\(x) str_ends(x,gsub(".*/","",await(tab$evaluate("window.location.href"))))) %>%
            py_to_r(nd_response_capture()$captured_responses)[.] %>% { .[[1]] } %>% fromJSON %>% 
            try(silent=T)
          
          try_n <- try_n+1L
          if(!(inherits(json_raw,"try-error")) && is.list(json_raw)) break
          if(try_n>=5L) { log_env$gpt_history <- "ERROR - JSON 추출 실패"; stop()}
        }
        log_env$gpt_history <- "JSON 추출 완료"
        
        ## JSON 저장
        write_json(json_raw,path=file.path(save_dir,"gpt_json_raw",paste0(gsub(".*/","",await(tab$evaluate('window.location.href'))),".json")))
        log_env$gpt_history <- "JSON 저장 완료"
        
        ## 답변 본문 추출
        answer_text <- if(!temp_chat) {
          rrapply(json_raw,
                  condition=\(x, .xname) .xname=="parts",
                  how="unlist") %>% unname %>% { .[[length(.)]] } %>%
            str_split(.,"\n") %>% unlist %>%
            gsub("\\p{Co}cite.*\\d+\\p{Co}","",.,perl=T) %>%
            gsub("\\p{Co}i\\p{Co}turn.*\\d+\\p{Co}","",.,perl=T) %>%
            gsub("\\p{Co}","",.,perl=T) %>%
            Filter(f=\(x) x!="") %>% 
            paste0(collapse="\n")
        } else {
          #임시 채팅
          await(
            (await(tab$xpath('//div[contains(@class,"markdown prose")]')))[[1]]$apply('
    el => {
      const clone = el.cloneNode(true);
      // 제외 대상 제거
      clone.querySelectorAll("a, script, style").forEach(n => n.remove());

      // <br>는 줄바꿈으로
      clone.querySelectorAll("br").forEach(br => br.replaceWith(document.createTextNode("\\n")));

      // 테이블 처리: 각 셀 사이에 탭, 각 행 끝에 줄바꿈
      clone.querySelectorAll("tr").forEach(tr => {
        const cells = tr.querySelectorAll(":scope > th, :scope > td");
        cells.forEach((cell, i) => {
          if (i < cells.length - 1) {
            cell.after(document.createTextNode("\\t"));   // 셀 구분
          }
        });
        tr.appendChild(document.createTextNode("\\n"));   // 행 구분
      });

      return clone.textContent;
    }
  ')
          )
        }
      }
      
      log_env$gpt_history <- "답변 본문 추출 완료"
      
      source <- wait_clickable_nd(await=await,tab=tab,xpath='//*[text()="출처" or text()="Sources"]',timeout=2.5)
      if(length(source)>1 && is.list(source)) {
        log_env$gpt_history <- "ERROR - 한 페이지에 두 개 이상의 질문-답변"
        stop()
      } else if(is.list(source) && length(source)==1) {
        # 링크가 존재하는 경우
        # await(source[[1]]$scroll_into_view())
        await(source[[1]]$apply('el => el.scrollIntoView({behavior:"smooth", block:"center"})'))
        await(source[[1]]$click())
        await(tab$xpath('//div[@slot="content"]/descendant::a',timeout=5))
        page_src0 <- await(tab$get_content())
        page_src <- read_html(page_src0)
        links <- html_elements(page_src,xpath='//div[@slot="content"]/descendant::a')
        link_url_text <- data.table(title_candidate1=html_text2(html_element(links,'.line-clamp-2')),
                                    title_candidate2=html_text2(links),
                                    link=html_attr(links,'href'))
        
        link_url_text <- link_url_text[,.(title=fcase(!is.na(title_candidate1) & title_candidate1!="",title_candidate1,
                                                      default=title_candidate2),link)]
        
        direct_link_n <- html_elements(page_src,xpath='//div[@slot="content"]/descendant::a[following::li[text()="More" or text()="더 보기"]]') %>% length
        link_url_text[,direct_indirect_link:=fcase(.I<=direct_link_n,"Direct Link",
                                                   default='Indirect Link')]
        link_url_text[,rank:=.I]
      } else {
        # 링크가 없는 경우
        if(length(await(tab$xpath('//div[contains(@class,"markdown prose")]/descendant::a')))>0) {
          # 종종 본문 링크는 있는데 출처 보기는 없는 경우가 있음
          link_url_text <- lapply(await(tab$xpath('//div[contains(@class,"markdown prose")]/descendant::a')), \(x) {
            data.table(title=x$text,
                       link=x$attrs["href"],
                       direct_indirect_link="Direct Link")
          }) %>% rbindlist
          link_url_text[,rank:=.I]
        } else {
          link_url_text <- data.table(title=NA_character_,link=NA_character_,direct_indirect_link=NA_character_,rank=NA_integer_)
        }
      }
      log_env$gpt_history <- "링크 데이터 추출 완료"
      
      ans <- data.table(tool='Manual',
                        monitoring_date=Sys.Date(),
                        keyword=keyword,
                        platform='ChatGPT',
                        link_url_text,
                        answer_text,
                        country_code=country_code,
                        language_code=language_code,
                        share_link=await(tab$evaluate("window.location.href")))
      
      if(await(tab$xpath('(//button[contains(@aria-label,"Model selector") or contains(@aria-label,"모델 선택기")])[2]//span'))[[1]]$text != str_extract(model,"\\d.*")) {
        log_env$gpt_history <- "ERROR - 다른 모델이 선택됨"
        stop()
      }
      
      if(save_page) writeLines(await(tab$get_content()),con=file.path(save_dir,paste0(half_to_full(keyword),'.html')))
      if(capture) save_screenshot_nd(await=await,tab=tab,type='ChatGPT',save_dir=save_dir,save_file_name=keyword)
      
      ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
      fwrite(ans,file=file.path(save_dir,paste0(save_file_name,"-Data.csv")),bom=T,append=T,quote=T)
      if(!is.null(chat_id) & !is.null(chat_token)) {
        send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[Chat GPT 추출 성공......]",keyword))
      }
      record_log2(country_code=country_code,language_code=language_code,platform='ChatGPT',keyword=keyword,step=i,total_steps=total_steps,status=TRUE,msg=".",save_dir=save_dir,save_file_name=save_file_name)
      Sys.sleep(delay_time+runif(n=1,min=0.5,max=12.5))
    },
    error=\(e) {
      if(!dir.exists(file.path(save_dir,"err_htmls"))) dir.create(file.path(save_dir,"err_htmls"))
      record_log(text1=paste0("[GPT 추출 실패] ",keyword,"……",log_env$gpt_history),
                 text2=await(tab$evaluate('window.location.href')),
                 save_dir=save_dir,
                 save_file_name=paste0(save_file_name,'_fail_list'),
                 chat_id=chat_id,chat_token=chat_token)
      record_log2(country_code=country_code,language_code=language_code,platform='ChatGPT',keyword=keyword,step=i,total_steps=total_steps,status=FALSE,msg=log_env$gpt_history,save_dir=save_dir,save_file_name=save_file_name)
      writeLines(await(tab$get_content()),con=file.path(save_dir,"err_htmls",paste0(half_to_full(keyword),".html")))
      await(tab$reload())
      Sys.sleep(delay_time)
    })
  },.progress=TRUE)
}


# GEMINI ========================================================
scrap_gemini_nd <- function(keywords,save_dir,save_file_name,delay_time=5,
                            await_name="await",tab_name="tab",env=.GlobalEnv,
                            save_page=FALSE,capture=TRUE,
                            chat_id=NULL,chat_token=NULL,
                            language_code='EN',country_code='US',login=TRUE) {
  log_env <- new.env()
  total_steps <- length(keywords)
  
  if(py_module_available("nodriver")==FALSE) {
    log_env$pplx_history <- "ERROR - nodriver 미설치"
    stop("준비가 되지 않아 실행 불가")
  }
  
  await <- get(await_name,envir=env)
  tab <- get(tab_name,envir=env)
  input <- nodriver$cdp$input_
  
  iwalk(keywords, \(keyword,i) {
    tryCatch({ 
      if(length(await(tab$xpath('//div[@role="tooltip" and text()="메뉴 펼치기" or @role="tooltip" and text()="Expand menu"]',timeout=0.5)))>0) {
        # 사이드바가 닫혀 있는 경우 먼저 사이드바를 활성화
        await(await(tab$xpath('//button[@aria-label="Main menu" and @data-test-id="side-nav-menu-button"]'))[[1]]$click())
      }
      if(length(await(tab$xpath('//p[contains(@class,"query-text-line")]',timeout=0.1)))>0) {
        # 메인 페이지가 아니라 기존 질문 페이지인 경우
        await(await(tab$xpath('//button[@aria-label="New chat" or @aria-label="새 채팅"]'))[[1]]$mouse_click())
      }
      ## 검색
      new_chat <- await(tab$xpath('//span[contains(text(),"New chat") or contains(text(),"새 채팅")]',timeout=3))
      try(await(new_chat[[1]]$click()),silent=TRUE)
      if(!login) {
        ### 비로그인 시작
        await(tab$xpath('//reset-chat-signed-out-dialog/descendant::span[contains(text(),"New chat")]',timeout=10))
        await(await(tab$xpath('//reset-chat-signed-out-dialog/descendant::span[contains(text(),"New chat")]'))[[1]]$click())
        ### 비로그인 끝
      }
      
      search_box <- wait_clickable_nd(await=await,tab=tab,xpath='//div[@aria-label="Enter a prompt here" or @aria-label="여기에 프롬프트 입력"]',timeout=5)
      await(search_box[[1]]$mouse_click())
      await(search_box[[1]]$apply("el => { el.innerText = ''; }"))
      await(search_box[[1]]$send_keys(keyword))
      await(tab$send(input$dispatch_key_event("keyDown",key="Enter",code="Enter",text="\r",windows_virtual_key_code=13)))
      log_env$gemini_history <- "쿼리 전송 완료"
      ## 답변 생성 완료까지 대기
      # check_point1 <- wait_clickable_nd(await=await,tab=tab,xpath='//button[@aria-label="Good response" or @aria-label="대답이 마음에 들어요"]',timeout=120)
      # if(is.logical(check_point1) && !check_point1) {
      #   record_log(text1=keyword,text2="Time Out(Good response)",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
      #   stop()
      # }
      check_point1 <- wait_clickable_nd(await=await,tab=tab,xpath='//mat-icon[@fonticon="refresh"]',timeout=120)
      if(is.logical(check_point1) && !check_point1) {
        record_log(text1=keyword,text2="Time Out(Refresh)",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
        stop()
      }
      check_point2 <- wait_visible_nd(await=await,tab=tab,xpath='//button[@aria-label="Microphone" or @aria-label="마이크"]',timeout=120)
      if(length(check_point2)==0) {
        log_env$gemini_history <- "ERROR - 시간 초과"
        stop()
      }
      Sys.sleep(1)
      if(length(await(tab$xpath('//p[@class="query-text-line ng-star-inserted"]')))>1) {
        log_env$gemini_history <- "한 페이지에 두 개 이상의 질문"
        stop()
      }
      
      ### 링크 추출
      Sys.sleep(0.5)
      if(capture) save_screenshot_nd(await=await,tab=tab,type='Gemini',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword) %>% gsub("/","／",.))
      source_button <- await(tab$xpath('//button[contains(@class,"sources-sidebar")]'))
      page_src0 <- await(tab$get_content())
      page_src <- read_html(page_src0)
      if(save_page) writeLines(await(tab$get_content()),con=file.path(save_dir,paste0(half_to_full(keyword),'.html')))
      
      if(length(source_button)==0) {
        # 없을 때
        link_text <- NA_character_
        link_url <- NA_character_
      } else {
        await(source_button[[1]]$click())
        wait_visible_nd(await=await,tab=tab,xpath='//div[@class="gds-title-l"]/following::inline-source-card',timeout=10,check_all=TRUE)
        page_src0 <- await(tab$get_content())
        page_src <- read_html(page_src0)
        link_url <- html_elements(page_src,xpath='//div[@class="gds-title-l"]/following::inline-source-card/descendant::a') %>% html_attr('href')
        
        link_text <- html_elements(page_src,xpath='//div[contains(@class,"gds-title-l")]/following::inline-source-card') %>% 
          sapply(\(x) {
            if(length(html_elements(x,xpath='./descendant::div[contains(@class,"title gds-title-m")]'))>0) {
              ans <- html_elements(x,xpath='./descendant::div[contains(@class,"title gds-title-m")]') %>% html_text2
            } else {
              ans <- html_elements(x,xpath='./descendant::div[contains(@class,"header")]') %>% html_text2
            }
            return(ans)
          })
        
        if(length(link_url) != length(link_text)) {
          log_env$gemini_history <- "ERROR - 링크 추출 과정에서 오류 발생"
          stop()
        }
        
        if(length(link_text)==0) {
          link_text <- html_elements(page_src,xpath='//div[contains(@class,"gds-title-l")]/following::div[@class="header"]') %>% 
            html_text2 %>% gsub("\n.*","",.)
        }
      }
      log_env$gemini_history <- "링크 추출 및 전처리 완료"
      answer_text <- html_elements(page_src,xpath='//message-content') %>% html_text2 %>% gsub("\n","\r\n",.)
      log_env$gemini_history <- "답변 추출 완료"
      ans <- data.table(tool='Manual',
                        monitoring_date=Sys.Date(),
                        keyword=keyword,
                        platform='Gemini',
                        title=link_text,
                        link=link_url,
                        direct_indirect_link=NA_character_,
                        answer_text,
                        country_code=country_code,
                        language_code=language_code,
                        share_link=await(tab$evaluate("window.location.href"))) %>%
        mutate(rank=row_number(),.after='link') %>% setDT
      ans[,`:=`(title=gsub("^\\d\\.\n","",title),link=URLdecode(link))]
      ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
      if(all(is.na(link_text))) ans[,rank:=NA]
      fwrite(ans,file=file.path(save_dir,paste0(save_file_name,'-Data.csv')),bom=T,append=T)
      log_env$gemini_history <- "데이터 테이블 저장 완료"
      if(!is.null(chat_id) & !is.null(chat_token)) {
        send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[Gemini 추출 성공......]",keyword))
      }
      record_log2(country_code=country_code,language_code=language_code,platform='Gemini',keyword=keyword,step=i,total_steps=total_steps,status=TRUE,msg=".",save_dir=save_dir,save_file_name=save_file_name)
      Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
    },
    error=\(e) {
      if(!dir.exists(file.path(save_dir,"err_htmls"))) dir.create(file.path(save_dir,"err_htmls"))
      record_log(text1=paste0("[Gemini 추출 실패] ",keyword,"……",log_env$gemini_history),
                 text2=await(tab$evaluate('window.location.href')),
                 save_dir=save_dir,
                 save_file_name=paste0(save_file_name,'_fail_list'),
                 chat_id=chat_id,chat_token=chat_token)
      writeLines(await(tab$get_content()),con=file.path(save_dir,"err_htmls",paste0(half_to_full(keyword),".html")))
      record_log2(country_code=country_code,language_code=language_code,platform='Gemini',keyword=keyword,step=i,total_steps=total_steps,status=FALSE,msg=log_env$gemini_history,save_dir=save_dir,save_file_name=save_file_name)
      Sys.sleep(delay_time)
      await(tab$reload())
      wait_visible_nd(await=await,tab=tab,'//mat-action-list',timeout=5)
    })
  },.progress=TRUE)
}


# GOOGLE AI OVERVIEW & AI MODE ========================================================
scrap_ggai_nd <- function(keywords,save_dir,save_file_name,delay_time=5,
                          await_name="await",tab_name="tab",env=.GlobalEnv,
                          save_page=FALSE,capture=TRUE,
                          chat_id=NULL,chat_token=NULL,
                          language_code='EN',country_code='US',
                          ai_overview=TRUE,ai_mode=TRUE,pretty_url=TRUE,captcha_api_key=NULL) {
  log_env <- new.env()
  log_env$captcha_detect_count <- 0L
  total_steps <- length(keywords)
  if(!dir.exists(file.path(save_dir,"ai_mode"))) dir.create(file.path(save_dir,"ai_mode"))
  if(py_module_available("nodriver")==FALSE) {
    log_env$pplx_history <- "ERROR - nodriver 미설치"
    stop("준비가 되지 않아 실행 불가")
  }
  
  if(any(str_detect(keywords,"[가-힣]"))) {
    language_code <- "KO"
    country_code <- "KR"
  }
  
  iwalk(keywords, \(keyword,i) {
    await <- get(await_name,envir=env)
    tab <- get(tab_name,envir=env)
    
    original_keyword <- keyword
    keyword <- gsub("\\s{2}"," ",keyword) # 구글은 공백이 두 개 연속되면 자동으로 하나로 교정해줌
    
    tryCatch({
      Sys.sleep(2)
      if(str_starts(await(tab$evaluate('window.location.href')),"https://www.google.com/sorry/") ||
         length(await(tab$xpath('//a[text()="Why did this happen?" or text()="왜 이런 현상이 발생하는 거죠?"]')))>0) {
        log_env$ggai_history <- "ERROR - Captcha"
        start_url <- paste0("https://www.google.com/search?q=google&hl=",tolower(language_code),"&gl=",tolower(country_code),"&pws=0")
        send_msg(chat_id,chat_token,text=paste("[GGAI] Captcha...",keyword))
        google_captcha(await_name=await_name,tab_name=tab_name,env=env,start_url=start_url,chat_id=chat_id,chat_token=chat_token,log_env=log_env,captcha_api_key=captcha_api_key,keyword=keyword,country_code=country_code)
        await <- get(await_name,envir=env)
        tab <- get(tab_name,envir=env)
      }
      if(await(tab$xpath('//*[@aria-current="page"] | //div[@selected]'))[[1]]$text %chin% c("AI Mode","AI 모드")) await(await(tab$xpath('//*[text()="All" or text()="전체"]'))[[1]]$click())
      
      # 검색
      await(tab$get(sprintf("https://www.google.com/search?q=%s&hl=%s&gl=%s&pws=0",
                            URLencode(keyword,reserved=T),
                            tolower(language_code),
                            tolower(country_code))))
      log_env$ggai_history <- "쿼리 전송 완료"
      
      if(str_starts(await(tab$evaluate('window.location.href')),"https://www.google.com/sorry/") ||
         length(await(tab$xpath('//a[text()="Why did this happen?" or text()="왜 이런 현상이 발생하는 거죠?"]')))>0) {
        log_env$ggai_history <- "ERROR - Captcha"
        start_url <- paste0("https://www.google.com/search?q=google&hl=",tolower(language_code),"&gl=",tolower(country_code),"&pws=0")
        send_msg(chat_id,chat_token,text=paste("[GGAI] Captcha...",keyword))
        google_captcha(await_name=await_name,tab_name=tab_name,env=env,start_url=start_url,chat_id=chat_id,chat_token=chat_token,log_env=log_env,captcha_api_key=captcha_api_key,keyword=keyword,country_code=country_code)
        await <- get(await_name,envir=env)
        tab <- get(tab_name,envir=env)
      }
      
      if(ai_overview) {
        # ai overview 활성화시
        has_aioverview_check_point1 <- wait_visible_nd(await=await,tab=tab,xpath='//strong[text()="AI Overview" or text()="AI 개요"] | //div[text()="AI Overview" or text()="AI 개요"]',timeout=20)
        if(!is.list(has_aioverview_check_point1) && !has_aioverview_check_point1) {
          has_aioverview_check_point2 <- wait_visible_nd(await=await,tab=tab,xpath='//div[text()="Searching"]/child::*[local-name()="svg"]/child::*[local-name()="g"]/child::*[local-name()="circle"]',timeout=10)
        }
        if(is.list(has_aioverview_check_point1) && length(has_aioverview_check_point1)>0) {
          is_real_aio_have <- TRUE
        } else if(is.list(has_aioverview_check_point2) && length(has_aioverview_check_point2)>0) {
          is_real_aio_have <- TRUE
        } else {
          is_real_aio_have <- FALSE
        }
        log_env$ggai_history <- "AI Overview 여부 점검 완료"
        if(is_real_aio_have) {
          #AI Overview가 있을 때
          wait_visible_nd(await=await,tab=tab,xpath='//span[text()="Show more" or text()="Read more" or text()="See full comparison" or text()="더보기"]',timeout=10)
          type_more <- await(tab$xpath('//div[@aria-controls="m-x-content"]/descendant::span[text()="더보기" or text()="Show more" or text()="Read more"]',timeout=10))
          if(length(type_more)==0) {
            type_comparison <- await(tab$xpath('//span[text()="See full comparison"]',timeout=8.5))
          } else {
            type_comparison <- list()
          }
          
          ## Type More의 더보기 / Show More 클릭
          await(await(tab$xpath('(//div[@aria-controls="m-x-content"]/descendant::span[text()="더보기" or text()="Show more" or text()="Read more"] |
                          //span[text()="See full comparison"])/preceding::div[1]'))[[1]]$click())
          
          ### Type More의 더 보기 / Show More가 클릭되지 않은 경우 대비
          wait_clickable_nd(await=await,tab=tab,timeout=5,check_all=F,
                            xpath='(//div[@aria-controls="m-x-content"]/descendant::span[text()="더보기" or text()="Show more" or text()="Read more"] |
                          //span[text()="See full comparison"])/preceding::div[1]') %>% {
                            
                            if(!isFALSE(.)) {
                              wait_clickable_nd(await=await,tab=tab,
                                                xpath='//strong[text()="AI Overview" or text()="AI 개요"]/
                                                following::span[text()="Show more" or text()="더보기"][ancestor-or-self::*[@role="button"]]',
                                                timeout=3,check_all=F)[[1]]$click() %>% await
                            }
                          }
          
          ## Show All Button Click
          await(wait_clickable_nd(await=await,tab=tab,xpath='//*[normalize-space(text())="Show all" or normalize-space(text())="모두 표시"]',timeout=20)[[1]]$click()) %>% 
            try(silent=T)
          
          if(length(wait_clickable_nd(await=await,tab=tab,xpath='//*[normalize-space(text())="Show all" or normalize-space(text())="모두 표시"]',timeout=15)[[1]])>0) {
            await(wait_clickable_nd(await=await,tab=tab,xpath='//*[normalize-space(text())="Show all" or normalize-space(text())="모두 표시"]',timeout=15)[[1]]$mouse_click()) %>% 
              try(silent=T)
          }
          
          Sys.sleep(0.5)
          log_env$ggai_history <- "AI Overview - 클릭 완료"
          # 실제 링크 데이터 추출
          tmp <- wait_clickable_nd(await=await,tab=tab,xpath='//a[@aria-label and @ping][ancestor::ul/li]',timeout=2,check_all=F)
          if(length(tmp)==0 | all(vapply(tmp,\(x) isFALSE(x),logical(1)))) tmp <- wait_clickable_nd(await=await,tab=tab,xpath='//div[@data-subtree="msc"]/descendant::ul/li/a[@aria-label]',timeout=2,check_all=F)
          if(length(tmp)==0 | all(vapply(tmp,\(x) isFALSE(x),logical(1)))) tmp <- wait_clickable_nd(await=await,tab=tab,xpath='//div[@jsname and @role="list" and not(@aria-hidden="true")][
                             following::div[@aria-label="Submit positive feedback" or @aria-label="긍정적인 의견 제출"]]/descendant::a[@aria-label]',timeout=2,check_all=F)
          if(length(tmp)==0 | all(vapply(tmp,\(x) isFALSE(x),logical(1)))) tmp <- wait_visible_nd(await=await,tab=tab,xpath='//a[@aria-labelledby and not(@tabindex)][preceding::*[@aria-label="Previous"]]',timeout=2,check_all=F)
          log_env$ggai_history <- "AI Overview - 링크 추출 성공"
          ## AI Overview 요소가 있는데 링크 데이터를 추출하지 못한 경우 페이지 구조를 제대로 감지하지 못해
          ## 자료가 정상적으로 수집되지 않았을 가능성이 매우 높음
          ## 따라서 정확한 데이터 수집을 위해 절차를 강제 종료함
          if(is.null(tmp) || length(tmp)==0) {
            log_env$ggai_history <- "ERROR - AI Ovewview - 링크 추출 실패"
            stop()
          }
          
          if("aria-label" %in% names(tmp[[1]]$attrs)) {
            title <- sapply(tmp,\(x) x$attrs["aria-label"])
          } else if("data-serialized-params" %in% names(tmp[[1]]$attrs)) {
            title <- sapply(tmp, \(x) fromJSON(x$parent$attrs["data-serialized-params"])[[1]][[1]])
          } else if("aria-labelledby" %in% names(tmp[[1]]$attrs)) {
            title <- sapply(wait_visible_nd(await=await,tab=tab,xpath='//a[@aria-labelledby and not(@tabindex)]/following::div[@aria-hidden="true"][1]',timeout=2,check_all=F),\(x) x$text)
          }
          link <- sapply(tmp,\(x) x$attrs["href"])
          
          if(length(title) != length(link)) {
            log_env$ggai_history <- "ERROR - 링크 title/url 불일치"
            stop()
          }
          
          log_env$ggai_history <- "AI Overview - 링크 데이터 편집 성공"
          # 답변 본문 추출
          answer_text <- tryCatch(await(tab$xpath('//div[@class="LT6XE"]'))[[1]]$apply("el => el.innerText") %>% await %>% 
                                    str_split_1("\n") %>% Filter(f=\(x) x != "Opens in new tab"),
                                  error=\(e) {
                                    log_env$ggai_history <- "ERROR - 답변 본문 추출 실패"
                                    stop()
                                  })
          
          # 소스 코드가 포함되는 경우가 있어 우선
          if(length(answer_text)==0) {
            answer_text <- html_elements(await(tab$get_content()),xpath='//strong[text()="AI Overview" or text()="AI 개요"]/following::div[@data-mcp]') %>% 
              html_text2 %>% gsub("\n","\r\n",.)
          }
          if(length(answer_text)==0) {
            answer_text <- await(tab$xpath('//div[normalize-space(text())="AI responses may include mistakes."]/preceding::div[@data-attrid and @data-mcp]'))[[1]]$apply("el => el.innerText") %>% await %>% 
              gsub("\n","\r\n",.)
          }
          answer_text <- paste0(answer_text,collapse="\n")
          log_env$ggai_history <- "AI Overview - 답변 본문 추출 완료"
        } else {
          ## AI Overview 요소가 없는 경우
          title <- NA_character_
          link <- NA_character_
          answer_text <- NA_character_
        }
        links_ans <- data.table(title,link)
        if(pretty_url) {
          links_ans[,link:=gsub("#:~:text=.*","",link)]
          links_ans <- funique(links_ans,cols="link")
        }
        links_ans[,`:=`(direct_indirect_link=NA_character_,rank=seq_len(.N),answer_text=answer_text)]
        log_env$ggai_history <- "링크 테이블 정제 완료"
        if(nrow(links_ans)==1 && is.na(links_ans$link[1])) links_ans[,rank:=NA_integer_]
        
        # search box가 비워지거나, search box에 입력된 키워드와 파일명이 불일치하는 경우가 발생하는 문제 대응
        if(keyword != await(tab$xpath('//textarea'))[[1]]$attrs["value"]) {
          log_env$ggai_history <- "ERROR - Keyword 불일치"
          stop()
        }
        if(length(await(tab$xpath('//a[@id="fprsl"]')))>0) {
          # Google Search는 문장을 질의어를 자동으로 교정해주고 ai mode에는 자동 교정된 질의어가 들어가게 되어
          # ai overview와 ai mode의 키워드 불일치 발생을 처리하기 위해 keyword를 자동 교정된 텍스트로 대체
          keyword <- await(await(tab$xpath('//a[@id="fprsl"]'))[[1]]$apply("el => el.innerText"))
          log_env$ggai_history <- "쿼리가 자동 교정됨"
        }
        if(save_page) writeLines(await(tab$get_content()),con=file.path(save_dir,paste0(half_to_full(keyword),'.html')))
        if(capture) save_screenshot_nd(await=await,tab=tab,type='GGAI',save_dir=save_dir,save_file_name=keyword)
        ans <- data.table(tool='Manual',
                          monitoring_date=Sys.Date(),
                          keyword=original_keyword,
                          platform='Google',
                          links_ans,
                          country_code=country_code,
                          language_code=language_code)
        ans[,`:=`(share_link=paste0('https://www.google.com/search?q=',gsub(' ','+',keyword) %>% gsub('\\?','+',.)))]
        log_env$ggai_history <- "데이터 테이블 구성 완료"
        ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
        fwrite(ans,file=file.path(save_dir,paste0(save_file_name,"-Data.csv")),bom=T,append=T,quote=T)
        record_log2(country_code=country_code,language_code=language_code,platform='AI Overview',keyword=keyword,step=i,total_steps=total_steps,status=TRUE,msg=".",save_dir=save_dir,save_file_name=save_file_name)
        if(!is.null(chat_id) & !is.null(chat_token)) {
          send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[GGAI 추출 성공......]",keyword))
        }
      }
      
      if(ai_mode) {
        # ai mode 활성화시
        ai_mode <- await(tab$xpath('//div[@role="listitem"][descendant-or-self::*[contains(normalize-space(.), "AI Mode") or contains(normalize-space(.),"AI 모드")]]'))
        if(length(ai_mode)>0) {
          await(ai_mode[[1]]$mouse_click())
          tryCatch({
            ## AI MODE START =================================
            Sys.sleep(3.5)
            wait_clickable_nd(await=await,tab=tab,'//button[@aria-label="Thumbs up" or @aria-label="Positive feedback" or @aria-label="긍정적인 의견"]',timeout=60)
            wait_clickable_nd(await=await,tab=tab,'//a[@href="https://support.google.com/websearch?p=aimode"]',timeout=5)
            log_env$aimode_history <- "AI Mode 생성 확인 완료"
            
            # 실패 분기
            if(length(await(tab$xpath('//div[contains(text(),"Something went wrong and the content was")]')))>0) {
              log_env$aimode_history <- "ERROR - Was Not Generated"
              stop()
            }
            # 본문 추출
            answer_text <- await(tab$xpath('//div[@data-container-id="main-col"]'))[[1]]$apply("el => el.innerText") %>% await
            log_env$aimode_history <- "답변 본문 추출 완료"
            answer_text <- gsub(" AI responses may include mistakes.\\s{1,}Learn more.*$","",answer_text) %>% 
              gsub("Your next question will start a new search.","",.)
            
            # Popup Close
            wait_clickable_nd(await=await,tab=tab,xpath='//button[@aria-label="Close"][following::]',timeout=2,check_all=F) %>% {
              if(!isFALSE(.)) await(.[[1]]$click())
            }
            
            # Show All Button Click
            await(wait_clickable_nd(await=await,tab=tab,xpath='//*[normalize-space(text())="Show all" or normalize-space(text())="모두 표시"]',timeout=20)[[1]]$click()) %>% 
              try(silent=TRUE)
            
            # 링크 추출
            link_url_text <- wait_clickable_nd(await=await,tab=tab,
                                               xpath='//div[@data-type="hovc" and @role="dialog"]/
                                                   descendant::li[not(contains(@class,"jJxJQ"))]/
                                                   descendant::a',
                                               timeout=5) %>%
              lapply(\(x) {
                data.table(title=x$attrs["aria-label"],link=x$attrs["href"],direct_indirect_link=NA_character_)
              }) %>% rbindlist %>% 
              tryCatch(error=\(e) FALSE)
            
            if(isFALSE(link_url_text)) {
              link_url_text <- wait_clickable_nd(await=await,tab=tab,
                                                 xpath='//div/descendant::ul/li/descendant::a[@aria-label]',
                                                 timeout=5,check_all=F) %>%
                lapply(\(x) {
                  data.table(title=x$attrs["aria-label"],link=x$attrs["href"],direct_indirect_link=NA_character_)
                }) %>% rbindlist
            }
            
            log_env$aimode_history <- "링크 추출 완료"
            
            if(pretty_url) {
              link_url_text[,link:=gsub("#:~:text=.*","",link)]
              link_url_text <- funique(link_url_text,cols="link")
            }
            if(nrow(link_url_text)==0 || is.null(link_url_text$title[[1]])) link_url_text <- data.table(title=NA,link=NA,direct_indirect_link=NA)
            link_url_text[,`:=`(rank=.I,answer_text=answer_text)]
            aimode_query <- gsub(" - Google 검색$| - Google Search$","",await(tab$evaluate("document.title")))
            if(keyword != aimode_query) {
              # search box가 비워지거나, search box에 입력된 키워드와 파일명이 불일치하는 경우가 발생하는 문제 대응
              log_env$aimode_history <- "ERROR - 키워드 불일치"
              stop()
            }
            if(save_page) writeLines(await(tab$get_content()),con=file.path(save_dir,"ai_mode",paste0(half_to_full(keyword),'.html')))
            if(capture) save_screenshot_nd(await=await,tab=tab,type='AI_mode',save_dir=file.path(save_dir,"ai_mode"),save_file_name=keyword)
            ans <- data.table(tool='Manual',
                              monitoring_date=Sys.Date(),
                              keyword=original_keyword,
                              platform='Google AI Mode',
                              link_url_text,
                              country_code=country_code,
                              language_code=language_code,
                              share_link=await(tab$evaluate("window.location.href")))
            ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
            fwrite(ans,file=file.path(save_dir,"ai_mode",paste0("AImode",gsub("\\D","",save_file_name),"-Data.csv")),bom=T,append=T)
            if(!is.null(chat_id) & !is.null(chat_token)) {
              send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[AI_Mode 추출 성공......]",keyword))
            }
            record_log2(country_code=country_code,language_code=language_code,platform='AI Mode',keyword=keyword,step=i,total_steps=total_steps,status=TRUE,msg=".",save_dir=save_dir,save_file_name=save_file_name)
            Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
            ## AI MODE END =================================
          },error=\(e) {
            log_env$ggai_history <- log_env$aimode_history
            stop()
          })
        }
      }
      if(await(tab$xpath('//*[@aria-current="page"] | //div[@selected]'))[[1]]$text %chin% c("AI Mode","AI 모드")) await(await(tab$xpath('//*[text()="All" or text()="전체"]'))[[1]]$click())
      Sys.sleep(delay_time+runif(n=1,min=0.5,max=8.5))
    },
    error=\(e) {
      if(!is.null(log_env$aimode_history) && log_env$ggai_history == log_env$aimode_history) {
        zzz <- "[AI Mode"
      } else {
        zzz <- "[AI Overview"
      }
      await <- get(await_name,envir=env)
      tab <- get(tab_name,envir=env)
      if(!dir.exists(file.path(save_dir,"err_htmls"))) dir.create(file.path(save_dir,"err_htmls"))
      record_log(text1=paste0(zzz," 추출 실패] ",keyword,"……",log_env$ggai_history),
                 text2=tryCatch(await(tab$evaluate('window.location.href')),
                                error=\(e) NA_character_),
                 save_dir=save_dir,
                 save_file_name=paste0(save_file_name,'_fail_list'),
                 chat_id=chat_id,chat_token=chat_token)
      writeLines(await(tab$get_content()),con=file.path(save_dir,"err_htmls",paste0(gsub("\\[","",zzz),"_",half_to_full(keyword),".html")))
      await(tab$reload())
      record_log2(country_code=country_code,language_code=language_code,platform=gsub("\\[","",zzz),keyword=keyword,step=i,total_steps=total_steps,status=FALSE,msg=log_env$ggai_history,save_dir=save_dir,save_file_name=save_file_name)
      Sys.sleep(delay_time)
    })
  },.progress=TRUE)
}