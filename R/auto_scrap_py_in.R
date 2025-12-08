scrap_ppx_py_in <- function(keyword,delay_time=1,remote_driver=remDr,country_code='US',language_code='EN',
                            save_dir,save_file_name,save_page=FALSE,capture=FALSE,chat_id=NULL,chat_token=NULL) {
  if(py_module_available("undetected_chromedriver")==FALSE) {
    stop("준비가 되지 않아 실행 불가")
  }
  
  if(str_detect(remote_driver$current_url,"perplexity.ai") &
     str_detect(remote_driver$current_url,"&version=2.18&source=default")) {
    remote_driver$execute_script("window.history.back();")
  }
  Keys <- import("selenium.webdriver.common.keys")$Keys
  By <- webdriver$common$by$By
  webdriver_wait <- webdriver$support$ui$WebDriverWait
  expected_conditions <- webdriver$support$expected_conditions
  Sys.sleep(delay_time+runif(n=1,min=0.3,max=1.7))
  wait <- webdriver_wait(remote_driver,3.5)
  wait$until(expected_conditions$element_to_be_clickable(c('xpath','//button[@aria-label="New Thread" or @aria-label="새로운 쓰레드"]')))$click() #New Thread Click
  Sys.sleep(1.5)
  search_box <- wait$until(expected_conditions$element_to_be_clickable(c('xpath','//div[@id="ask-input" and @role="textbox"]')))
  search_box$click()
  search_box$clear()
  search_box$send_keys(keyword,Keys$ENTER)
  #답변 생성 완료되었는지
  Sys.sleep(2.5)
  wait <- webdriver_wait(remote_driver,10)
  tryCatch(wait$until(expected_conditions$invisibility_of_element_located(c('xpath','//*[text()="Finding sources" or text()="소스 찾는 중"]'))),
           error=function(e) {
             record_log(text1=keyword,text2="Finding sources",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
             stop()
           })
  wait <- webdriver_wait(remote_driver,90)
  tryCatch(wait$until(expected_conditions$invisibility_of_element_located(c('xpath','//button[@aria-label="응답 생성 중지" or @aria-label="Stop generating response"]'))),
           error=function(e) {
             record_log(text1=keyword,text2="Stop generating response",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
             stop()
           })
  Sys.sleep(2)
  ppx_page <- remote_driver$current_url
  url_wait_time <- 0
  while(str_detect(ppx_page,"search/new\\?q=pending")) {
    Sys.sleep(0.5)
    url_wait_time <- url_wait_time+0.5
    ppx_page <- remote_driver$current_url
    if(url_wait_time>=10) {
      record_log(text1=keyword,text2="URL Pending",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
      stop()
    }
  }
  title_wait_time <- 0
  title <- remote_driver$title
  while(title == 'Perplexity') {
    Sys.sleep(0.5)
    title <- remote_driver$title
    title_wait_time <- title_wait_time+0.5
    if(title_wait_time>=7) {
      record_log(text1=keyword,text2="Title",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
      stop()
    }
  }
  ##추출
  page_src <- read_html(remote_driver$page_source)
  wait$until(expected_conditions$visibility_of_element_located(c('xpath','//div[@id="markdown-content-0"]')))
  
  direct_links <- html_elements(page_src,xpath='//div[@id="markdown-content-0"]') %>% 
    html_elements(xpath='./descendant::a[@class="citation ml-xs inline"] | 
    ./descendant::span[@class="citation ml-xs inline"] | ./descendant::span[@class="whitespace-nowrap"]') %>%
    html_text2 %>% as.numeric %>% unique #직접 표출되는 링크 번호
  
  direct_links <- direct_links[!is.na(direct_links)]
  if(length(direct_links)==0) direct_links <- 0
  answer_text <- remote_driver$find_elements('xpath','//div[@id="markdown-content-0"]')[[1]]$text %>% gsub("\n","\r\n",.) %>% gsub("\\d\\.$","\\.",.)
  if(save_page)  writeBin(remote_driver$page_source,con=file.path(save_dir,paste0(gsub("\\?","？",keyword),'.html')))
  if(capture) save_screenshot(type='Perplexity',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword),remote_driver=remote_driver)
  ## 출처 링크-텍스트
  source <- remote_driver$find_elements('xpath','//div[contains(@class,"opacity-100") and (text()="Sources" or text()="출처")]')
  shopping <- remote_driver$find_elements('xpath','//h1/following::div[text()="Shopping" or text()="쇼핑"]')
  if(length(shopping)==0) {
    # 쇼핑탭이 없는 경우
    shopping_url_text <- data.table(rank=NA,product=NA,url=NA,shop=NA,price=NA,avg_rating=NA,n_reviews=NA,buy_with_pro=NA)
  } else {
    # 쇼핑탭이 있는 경우
    shopping[[1]]$click()
    wait <- webdriver_wait(remote_driver,10)
    wait$until(expected_conditions$visibility_of_all_elements_located(c('xpath','//div[contains(@class,"text-pretty")]')))
    scroll_target <- remote_driver$find_elements('xpath','//div[contains(@class,"items-start")]')
    Sys.sleep(1.5)
    if(length(scroll_target)>0) {
      # More Products가 있어서 스크롤을 해야 하는 경우
      scroll_target <- scroll_target[[length(scroll_target)]]
      remote_driver$execute_script("arguments[0].scrollIntoView({behavior: 'smooth', block: 'start'});",scroll_target)
    }
    wait$until(expected_conditions$visibility_of_all_elements_located(c('xpath','//div[contains(@class,"text-pretty")]')))
    
    page_src_shopping <- remote_driver$page_source %>% read_html
    if(length(remote_driver$find_elements('xpath','//button[contains(@class,"group/root shadow-subtle")]'))>0) {
      #top3가 있는 경우
      top3 <- lapply(html_elements(page_src_shopping,xpath='//button[contains(@class,"group/root shadow-subtle")]'), \(x) {
        data.table(product=tryCatch(html_elements(x,xpath='./descendant::div[contains(@class,"line-clamp-3 text-pretty")]') %>% html_text2,
                                    error=function(e) {NA}),
                   url=tryCatch(html_elements(x,xpath='./descendant::a') %>% html_attr('href'),
                                error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   shop=tryCatch(html_text2(html_elements(x,xpath='./descendant::a')),error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   price=html_elements(x,xpath='./descendant::div[not(contains(@class,"line-through")) and contains(text(),"$")]') %>% html_text2 %>% 
                     tryCatch(.,error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   avg_rating=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[1]') %>%
                     html_text2 %>% tryCatch(.,error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   n_reviews=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[2]') %>%
                     html_text2 %>% tryCatch(.,error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   buy_with_pro=html_elements(x,xpath='./descendant::div[normalize-space(text())="Buy with"]') %>% 
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
    if(length(remote_driver$find_elements('xpath','//div[text()="More Products" or text()="더 많은 제품"]'))>0) {
      # more가 있는 경우
      mores <- lapply(html_elements(page_src_shopping,xpath='//div[text()="More Products" or text()="더 많은 제품"]/following::button[contains(@class,"group/root")]'), \(x) {
        data.table(product=tryCatch(html_elements(x,xpath='./descendant::div[contains(@class,"line-clamp-2 text-pretty")]') %>% html_text2,
                                    error=function(e) {NA}),
                   url=tryCatch(html_elements(x,xpath='./descendant::a') %>% html_attr('href'),error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   shop=tryCatch(html_text2(html_elements(x,xpath='./descendant::a')),error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   price=html_elements(x,xpath='./descendant::div[not(contains(@class,"line-through")) and contains(text(),"$")]') %>% html_text2 %>% 
                     tryCatch(.,error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   avg_rating=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[1]') %>%
                     html_text2 %>% tryCatch(.,error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   n_reviews=html_elements(x,xpath='(./descendant::div[contains(@class,"group-hover/link:text-super")])[2]') %>%
                     html_text2 %>% tryCatch(.,error=function(e) {NA}) %>% 
                     {
                       if(length(.)>0) (.)
                       else NA
                     },
                   buy_with_pro=html_elements(x,xpath='./descendant::div[normalize-space(text())="Buy with"]') %>% 
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
  
  if(length(source)==0) {
    link_url_text <- data.table(title=NA_character_,link=NA_character_)
  } else {
    # 소스 링크가 있는 경우
    url <- paste0("https://www.perplexity.ai/rest/thread/",
                  gsub("https://www.perplexity.ai/search/|\\?0=r","",ppx_page),
                  "?with_parent_info=true&with_schematized_response=true&version=2.18&source=default&limit=100&offset=0&from_first=true&supported_block_use_cases=answer_modes&supported_block_use_cases=media_items&supported_block_use_cases=knowledge_cards&supported_block_use_cases=inline_entity_cards&supported_block_use_cases=place_widgets&supported_block_use_cases=finance_widgets&supported_block_use_cases=sports_widgets&supported_block_use_cases=shopping_widgets&supported_block_use_cases=jobs_widgets&supported_block_use_cases=search_result_widgets&supported_block_use_cases=entity_list_answer&supported_block_use_cases=todo_list")
    remote_driver$get(url)
    remote_driver$execute_script("return document.readyState;")
    json_raw <- remote_driver$execute_script("return document.body.innerText;")[[1]] %>% fromJSON
    
    if(length(shopping)==0) {
      # 쇼핑탭이 없는 경우
      links <- json_raw$entries$blocks[[1]]$web_result_block
      links <- links$web_results[which(!is.na(links$progress))][[1]]
      link_url_text <- data.table(title=links$name,link=links$url)
    } else {
      # 쇼핑탭이 있는 경우
      links <- json_raw$entries$blocks[[1]]$sources_mode_block$rows
      links <- links[sapply(links,function(x) is.data.frame(x))][[1]]$web_result
      link_url_text <- data.table(title=links$name,link=links$url)
    }
    # link_url_text <- link_url_text[,.SD[1],by=link] # PPLX 중복 표출에 따라 삭제해야함함
    remote_driver$execute_script("window.history.back();")
  }
  ans_shoppings <- data.table(tool='Manual',
                              monitoring_date=Sys.Date(),
                              keyword=keyword,
                              platform='Perplexity',
                              shopping_url_text,
                              country_code=country_code,
                              language_code=language_code,
                              share_link=ppx_page)
  
  ans_shoppings[,names(ans_shoppings)[sapply(ans_shoppings,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans_shoppings)[sapply(ans_shoppings,\(x) is.character(x))]]
  fwrite(ans_shoppings,file=file.path(save_dir,paste0(save_file_name,'-Shop.csv')),bom=T,append=T)
  link_url_text[,rank:=.I] #링크 번호
  link_url_text[,direct_indirect_link:=map_int(rank, \(x) sum(direct_links==x) )]
  link_url_text[,direct_indirect_link:=fcase(direct_indirect_link==0,"Indirect Link",
                                             default="Direct Link")]
  link_url_text[is.na(link) & is.na(title),`:=`(direct_indirect_link=NA_character_,rank=NA_integer_)] #링크가 없는 경우 조정
  link_url_text <- link_url_text[,.(link,title,direct_indirect_link,rank)]
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
  fwrite(ans,file=file.path(save_dir,paste0(save_file_name,'-Data.csv')),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[PPLX 추출 성공......]",keyword))
  }
  Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
  ans
}


scrap_gpt_py_in <- function(keyword,delay_time,remote_driver=remDr,country_code='US',language_code='EN',
                            save_dir,save_file_name,save_page=FALSE,capture=FALSE,project_name=NULL,
                            chat_id=NULL,chat_token=NULL) {
  if(py_module_available("undetected_chromedriver")==FALSE) {
    stop("준비가 되지 않아 실행 불가")
  }
  Keys <- import("selenium.webdriver.common.keys")$Keys
  By <- webdriver$common$by$By
  webdriver_wait <- webdriver$support$ui$WebDriverWait
  expected_conditions <- webdriver$support$expected_conditions
  Sys.sleep(delay_time+runif(n=1,min=0,max=1))
  
  
  if(!remote_driver$find_element('xpath','//div[text()="New chat" or text()="새 채팅"]')$is_displayed()) {
    # 사이드바가 가려져 있으면 활성화
    remote_driver$find_elements('xpath','//button[@aria-label="Open sidebar" or @aria-label="사이드바 열기"]')[[1]]$click()
  }
  if(is.null(project_name)) {
    remote_driver$find_elements('xpath','//a[@data-testid="create-new-chat-button"]')[[2]]$click()
  } else {
    remote_driver$find_elements('xpath',paste0('//aside/descendant::a/descendant::div[text()="',project_name,'"]'))[[1]]$click()
  }
  Sys.sleep(1.5)
  wait <- webdriver_wait(remote_driver,10)
  ## Web 검색이 비활성화되어 있으면 클릭
  is_active_web_search <- remote_driver$find_elements('xpath','//button/span[text()="Search" or text()="검색"]')
  if(length(is_active_web_search)==0) {
    wait$until(expected_conditions$element_to_be_clickable(c('xpath','//button[@aria-label="도구 선택하기" or @aria-label="Choose tool"]')))
    remote_driver$find_element('xpath','//button[@aria-label="도구 선택하기" or @aria-label="Choose tool"]')$click()
    wait$until(expected_conditions$element_to_be_clickable(c('xpath','//div[text()="웹에서 검색하기" or text()="Search the web"]')))
    remote_driver$find_element('xpath','//div[text()="웹에서 검색하기" or text()="Search the web"]')$click()
  }
  ## check GPT-4o
  is_model_gpt_4o <- remote_driver$find_elements('xpath','//button[contains(@aria-label,"Model selector") or contains(@aria-label,"모델 선택기")]/descendant::div')[[2]]$text
  if(is_model_gpt_4o != "ChatGPT 4o") {
    remote_driver$find_elements('xpath','//button[contains(@aria-label,"Model selector") or contains(@aria-label,"모델 선택기")]/descendant::div')[[2]]$click()
    tryCatch(remote_driver$find_element('xpath','//div[@role and @data-testid="model-switcher-gpt-4o"]')$click(),
             error=function(e) {
               record_log(text1=keyword,text2="GPT-4o Not Usuable",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
               stop(critical_error())
             })
  }
  ##검색
  search_box <- remote_driver$find_elements('xpath','//div[@id="prompt-textarea"]/p[@class="placeholder"]')[[1]]
  Sys.sleep(0.5)
  search_box$clear()
  Sys.sleep(0.5)
  search_box$send_keys(keyword,Keys$ENTER)
  wait <- webdriver_wait(remote_driver,90)
  tryCatch(wait$until(expected_conditions$element_to_be_clickable(c('xpath','//button[@aria-label="좋은 응답" or @aria-label="Good response"]'))),
           error=function(e) {
             record_log(text1=keyword,text2="제한시간 초과",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
             stop()
           })
  if(length(remote_driver$find_elements('xpath','//div[@data-has-thread-error]'))>0) {
    record_log(text1=keyword,text2="Regenerate",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  if(length(remote_driver$find_elements('xpath','//p[contains(text(),"reached our limit of messages per hour. Please try again later")]'))>0) {
    record_log(text1=keyword,text2="Reached Limit",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop(critical_error())
  }
  if(length(remote_driver$find_elements('xpath','//p[text()="Something went wrong while generating the response. If this issue persists please contact us through our help center at "]'))>0) {
    record_log(text1=keyword,text2="Wrong while generating the response",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  if(length(remote_driver$find_elements('xpath','//div[@class="whitespace-pre-wrap"]'))>1) {
    record_log(text1=keyword,text2="2+ Keywords in Page",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  if(length(remote_driver$find_elements('xpath','//button[@data-testid="regenerate-thread-error-button"]'))>0) {
    record_log(text1=keyword,text2="Retry Button",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  wait <- webdriver_wait(remote_driver,2.5)
  try(wait$until(expected_conditions$element_to_be_clickable(c('xpath','//*[text()="출처" or text()="Sources"]'))),silent=TRUE)
  source <- remote_driver$find_elements('xpath','//*[text()="출처" or text()="Sources"]')
  if(length(source)>1) {
    record_log(text1=keyword,text2="한 페이지에 두 개 이상의 질문",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  } else if(length(source)==1) {
    # 링크가 존재하는 경우
    remote_driver$execute_script("arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });",
                                 source[[1]])
    Sys.sleep(1)
    remote_driver$execute_script("arguments[0].click();",source[[1]])
    wait <- webdriver_wait(remote_driver,5)
    wait$until(expected_conditions$visibility_of_all_elements_located(c('xpath','//div[@slot="content"]/descendant::a')))
    Sys.sleep(0.5)
    page_src0 <- remote_driver$page_source
    page_src <- read_html(page_src0)
    links <- html_elements(page_src,xpath='//div[@slot="content"]/descendant::a')
    link_url_text <- data.table(title_candidate1=html_text2(html_element(links,'.line-clamp-2')),
                                title_candidate2=html_text2(links),
                                link=html_attr(links,'href'))
    link_url_text <- link_url_text[,.(title=fcase(!is.na(title_candidate1) & title_candidate1!="",title_candidate1,
                                                  default=title_candidate2),link)]
    direct_link_n <-html_elements(page_src,xpath='//div[@slot="content"]/descendant::a[following::div[text()="More" or text()="더 보기"]]') %>% length
    link_url_text[,direct_indirect_link:=fcase(.I<=direct_link_n,"Direct Link",
                                               default='Indirect Link')]
    link_url_text[,rank:=.I]
  } else {
    # 링크가 존재하지 않는 경우
    page_src0 <- remote_driver$page_source
    page_src <- read_html(page_src0)
    link_url_text <- data.table(title=NA_character_,link=NA_character_,direct_indirect_link=NA_character_,rank=NA_integer_)
  }
  answer_text <- html_elements(page_src,xpath='//div[contains(@class,"markdown prose")]') %>% html_text2 %>% gsub("\n출처$|\nSources","",.)
  ans <- data.table(tool='Manual',
                    monitoring_date=Sys.Date(),
                    keyword=keyword,
                    platform='ChatGPT',
                    link_url_text,
                    answer_text,
                    country_code=country_code,
                    language_code=language_code,
                    share_link=remote_driver$current_url)
  if(remote_driver$find_elements('xpath','(//button[contains(@aria-label,"Model selector") or contains(@aria-label,"모델 선택기")])[2]//span')[[1]]$text != '4o') {
    record_log(text1=keyword,text2="Not ChatGPT 4o",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  if(save_page) writeBin(page_src0,con=file.path(save_dir,paste0(gsub("\\?","？",keyword),'.html')))
  if(capture) Sys.sleep(0.3); save_screenshot(type='ChatGPT',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword),remote_driver=remote_driver)
  ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
  fwrite(ans,file=file.path(save_dir,paste0(save_file_name,"-Data.csv")),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[Chat GPT 추출 성공......]",keyword,""))
  }
  Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
  ans
}

scrap_ggai_py_in <- function(keyword,delay_time,remote_driver=remDr,country_code='US',language_code='EN',
                             save_dir,save_file_name,save_page=FALSE,capture=FALSE,
                             chat_id=NULL,chat_token=NULL) {
  if(py_module_available("undetected_chromedriver")==FALSE) {
    stop("준비가 되지 않아 실행 불가")
  }
  Keys <- import("selenium.webdriver.common.keys")$Keys
  By <- webdriver$common$by$By
  webdriver_wait <- webdriver$support$ui$WebDriverWait
  expected_conditions <- webdriver$support$expected_conditions
  Sys.sleep(delay_time+runif(n=1,min=1.5,max=2.1)*abs(rnorm(n=1,mean=1.5,sd=2.1)))
  wait <- webdriver_wait(remote_driver,5)
  if(remote_driver$find_elements('xpath','//div[@selected]')[[1]]$text=="AI Mode") remote_driver$find_elements('xpath','//div[text()="All"]')[[1]]$click()
  
  tryCatch(wait$until(expected_conditions$invisibility_of_element_located(c('xpath','//div[contains(text(),"비정상")] | //b[text()="페이지 정보"]'))),
           error=function(e) {
             bot_detect <- length(remote_driver$find_elements('xpath','//div[contains(text(),"비정상")] | //b[text()="페이지 정보"]'))
             while(bot_detect >0 ) {
               Sys.sleep(2)
               bot_detect <- length(remote_driver$find_elements('xpath','//div[contains(text(),"비정상")] | //b[text()="페이지 정보"]'))
             }
             Sys.sleep(3)
           })
  wait <- webdriver_wait(remote_driver,8.5)
  search_box <- wait$until(expected_conditions$element_to_be_clickable(c("xpath", '//*[@id="APjFqb"]')))
  search_box$clear()
  search_box$click()
  search_box$send_keys(keyword, Keys$ENTER) #검색(키워드 입력)
  is_real_aio_have <- tryCatch({
    wait$until(expected_conditions$visibility_of_element_located(c("xpath", '//strong[text()="AI Overview" or text()="AI 개요"]')))
    TRUE
  }, error=function(e) {
    tryCatch({
      wait$until(expected_conditions$visibility_of_element_located(c("xpath", '//div[text()="Searching"]/child::*[local-name()="svg"]/child::*[local-name()="g"]/child::*[local-name()="circle"]')))
      TRUE
    },error=function(e) {FALSE})
  })
  if(length(remote_driver$find_elements('xpath','//g-raised-button/descendant::div[text()="Not now"]'))>0) {
    # Use precise location 창이 뜨면 Not now를 선택
    remote_driver$find_element('xpath','//g-raised-button/descendant::div[text()="Not now"]')$click()
  }
  if(is_real_aio_have) {
    #AI Overview가 있을 때
    try(wait$until(expected_conditions$visibility_of_element_located(c("xpath", '//span[text()="Show more" or text()="Read more" or text()="See full comparison" or text()="더보기"]'))),silent=T)
    type_more <- remote_driver$find_elements('xpath','//div[@aria-controls="m-x-content"]/descendant::span[text()="더보기" or text()="Show more" or text()="Read more"]')
    type_comparison <- remote_driver$find_elements('xpath','//span[text()="See full comparison"]')
    remote_driver$find_element('xpath','(//div[@aria-controls="m-x-content"]/descendant::span[text()="더보기" or text()="Show more" or text()="Read more"] | //span[text()="See full comparison"])/preceding::div[1]')$click()
    
    if(length(type_more)>0) {
      # Read more 또는 Show more
      try(wait$until(expected_conditions$element_to_be_clickable(c("xpath", '//span[text()="Show all" or text()="모두 표시"]'))),silent=TRUE)
      clicks <- remote_driver$find_elements('xpath','//span[text()="Show all" or text()="모두 표시"]')
      for(i in seq_along(clicks)) {
        clicks[[i]]$find_elements('xpath','./ancestor::div[@role="button"]')[[1]]$click()
      }
    }
    try(wait$until(expected_conditions$visibility_of_element_located(c("xpath",'//div[@aria-label="Thumbs up" or @aria-label="좋아요"]/preceding::div/ul/li/a'))),silent=T)
    page_src0 <- remote_driver$page_source
    page_src <- read_html(page_src0)
    links <- html_elements(page_src,xpath='//div[@aria-label="Thumbs up" or @aria-label="좋아요"]/preceding::div/ul/li') %>% html_elements(xpath='a[@aria-label]')
    if(length(links)==0) links <- html_elements(page_src,xpath='//div[contains(@jsaction,"closeStickyCorroboration")]/descendant::ul/li/descendant::a[@aria-label]')
    
    links_href <- links %>% html_attr('href') %>% gsub("#:~:text=.*","",.) %>% unique
    links_text <- links %>% html_attr('aria-label') %>% unique
    links_n <- length(links_href)
    answer_text <- html_elements(page_src,xpath='//div[@class="LT6XE"]') %>% html_text2
    if(length(answer_text)==0) {
      answer_text <- html_elements(page_src,xpath='//strong[text()="AI Overview" or text()="AI 개요"]/following::div[@data-mcp]') %>% 
        html_text2 %>% gsub("\n","\r\n",.)
    } # 바뀐 것 같음. LT6XE가 있는 경우도 이 데이터가 있음
    
    links_ans <- data.table(links_text,links_href,answer_text,links_n) %>% mutate(rank=row_number()) %>% setDT
    links_ans[,1:3:=lapply(.SD,replace_comma),.SDcol=1:3]
  } else {
    #AI Overview가 없을 때
    links_ans <- data.table(links_text=NA_character_,
                            links_href=NA_character_,
                            answer_text=NA_character_,
                            links_n=0,
                            rank=NA_integer_)
  }
  if(save_page) writeBin(remote_driver$page_source,con=file.path(save_dir,paste0(gsub("\\?","？",keyword),'.html')))
  if(capture) save_screenshot(type='GGAI',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword),remote_driver=remote_driver)
  ans <- data.table(tool='Manual',
                    monitoring_date=Sys.Date(),
                    keyword=keyword,
                    platform='Google',
                    title=links_ans$links_text,
                    link=links_ans$links_href,
                    direct_indirect_link=NA_character_,
                    rank=links_ans$rank,
                    answer_text=links_ans$answer_text,
                    country_code=country_code,
                    language_code=language_code) %>%
    mutate(share_link=paste0('https://www.google.com/search?q=',gsub(' ','+',keyword) %>% gsub('\\?','+',.))) %>% setDT
  ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
  fwrite(ans,file=file.path(save_dir,paste0(save_file_name,"-Data.csv")),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[GGAI 추출 성공......]",keyword))
  }
  
  ai_mode <- remote_driver$find_elements('xpath','//div[@role="button" and @data-query]/descendant::div[contains(translate(.,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz"),"ai mode")] | 
                                         //div[@role="listitem"]/descendant::span[contains(translate(text(),"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz"),"ai mode")]')
  
  if(length(ai_mode)>0) {
    ai_mode[[1]]$click()
    scrap_aimode_py_in(keyword=keyword,delay_time=delay_time,remote_driver=remote_driver,
                       country_code=country_code,language_code=language_code,
                       save_dir=save_dir,save_file_name='AImode',save_page=save_page,
                       capture=capture,chat_id=chat_id,chat_token=chat_token)
  }
  if(remote_driver$find_elements('xpath','//div[@selected]')[[1]]$text=="AI Mode") remote_driver$find_elements('xpath','//div[text()="All"]')[[1]]$click()
  Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
  ans
}

scrap_gemini_py_in <- function(keyword,delay_time,remote_driver=remDr,country_code='US',language_code='EN',
                               save_dir,save_file_name,save_page=FALSE,capture=FALSE,
                               chat_id=NULL,chat_token=NULL) {
  Keys <- import("selenium.webdriver.common.keys")$Keys
  By <- webdriver$common$by$By
  webdriver_wait <- webdriver$support$ui$WebDriverWait
  expected_conditions <- webdriver$support$expected_conditions
  Sys.sleep(delay_time+runif(n=1,min=0,max=1))
  ## 검색
  wait <- webdriver_wait(remote_driver,1.5)
  if(length(remote_driver$find_elements('xpath','//div[@role="tooltip" and text()="메뉴 펼치기"]'))>0) {
    # 사이드바가 닫혀 있는 경우 먼저 사이드바를 활성화
    remote_driver$find_elements('xpath','//button[@aria-label="Main menu"]')[[1]]$click()
    Sys.sleep(0.5)
  }
  if(length(remote_driver$find_elements('xpath','//p[contains(@class,"query-text-line")]'))>0) {
    # 메인 페이지가 아니라 기존 질문 페이지인 경우
    remote_driver$find_elements('xpath','//side-nav-action-button[@arialabel="New chat"]')[[1]]$click()
  }
  # 한도 초과로 something went wrong이 뜨는 경우를 대비해서 추가해야함
  # tryCatch(remote_driver$find_element('xpath','//span[contains(text(),"New chat") or contains(text(),"새 채팅")]')$click(),
  #          error=function(e) {
  #            record_log(text1=keyword,text2="New Chat Click Failed",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
  #            stop()
  #          })
  wait <- webdriver_wait(remote_driver,5)
  search_box <- wait$until(expected_conditions$element_to_be_clickable(c('xpath','//div[@aria-label="Enter a prompt here" or @aria-label="여기에 프롬프트 입력"]')))
  search_box$click()
  search_box$clear()
  search_box$send_keys(keyword,Keys$ENTER)
  
  ## 답변 생성 완료까지 대기
  wait <- webdriver_wait(remote_driver,120)
  tryCatch(wait$until(expected_conditions$element_to_be_clickable(c('xpath','//button[@aria-label="Good response" or @aria-label="대답이 마음에 들어요"]'))),
           error=function(e) {
             record_log(text1=keyword,text2="Time Out(Good response)",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
             stop()
           })
  tryCatch(wait$until(expected_conditions$visibility_of_element_located(c('xpath','//div[@data-test-lottie-animation-status="completed"]'))),
           error=function(e) {
             record_log(text1=keyword,text2="Time Out(data-test-lottie-animation)",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
             stop()
           })
  Sys.sleep(1.5)
  if(length(remote_driver$find_elements('xpath','//p[@class="query-text-line ng-star-inserted"]'))>1) {
    record_log(text1=keyword,text2="Two+ Questions in One Page",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  ### 링크 추출
  Sys.sleep(0.5)
  remote_driver$execute_script("arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });",
                               remote_driver$find_elements('xpath','//button[@mattooltip="Good response" or @mattooltip="대답이 마음에 들어요"]')[[1]])
  Sys.sleep(1)
  source_button <- remote_driver$find_elements('xpath','//button[contains(@class,"sources-sidebar-button")]')
  page_src0 <- remote_driver$page_source
  page_src <- read_html(page_src0)
  if(length(source_button)==0) {
    # 없을 때
    link_text <- NA_character_
    link_url <- NA_character_
  } else {
    remote_driver$find_elements('xpath','//button[contains(@class,"sources-sidebar-button")]')[[1]]$click()
    wait <- webdriver_wait(remote_driver,10)
    wait$until(expected_conditions$visibility_of_all_elements_located(c('xpath','//div[@class="gds-title-l"]/following::inline-source-card')))
    page_src0 <- remote_driver$page_source
    page_src <- read_html(page_src0)
    link_url <- html_elements(page_src,xpath='//div[@class="gds-title-l"]/following::inline-source-card/descendant::a') %>% html_attr('href')
    link_text <- html_elements(page_src,xpath='//div[contains(@class,"gds-title-l")]/following::div[contains(@class,"title gds-title-m")]') %>% 
      sapply(\(x) {
        ans <- html_text2(x)
        if(length(ans)==0) ans <- html_elements(x,xpath='./preceding::div[@class="header"][1]') %>% html_text2 %>% gsub("\n.*","",.)
        return(ans)
      })
    
    if(length(link_text)==0) {
      link_text <- html_elements(page_src,xpath='//div[contains(@class,"gds-title-l")]/following::div[@class="header"]') %>% 
        html_text2 %>% gsub("\n.*","",.)
    }
  }
  answer_text <- html_elements(page_src,xpath='//message-content') %>% html_text2 %>% gsub("\n","\r\n",.)
  
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
                    share_link=remote_driver$current_url) %>%
    mutate(rank=row_number(),.after='link') %>% setDT
  ans[,`:=`(title=gsub("^\\d\\.\n","",title),
            link=URLdecode(link))]
  ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
  if(all(is.na(link_text))) ans[,rank:=NA]
  fwrite(ans,file=file.path(save_dir,paste0(save_file_name,'-Data.csv')),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[Gemini]",keyword,"......추출 성공"))
  }
  if(capture) save_screenshot(type='Gemini',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword),remote_driver=remote_driver)
  if(save_page)  writeBin(remote_driver$page_source,con=file.path(save_dir,paste0(gsub("\\?","",keyword),'.html')))
  Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
  ans
}

scrap_copilot_py_in <- function(keyword,delay_time,remote_driver=remDr,country_code='US',language_code='EN',
                                save_dir,save_file_name,save_page=FALSE,capture=FALSE,
                                chat_id=NULL,chat_token=NULL) {
  if(py_module_available("undetected_chromedriver")==FALSE) {
    stop("준비가 되지 않아 실행 불가")
  }
  Keys <- import("selenium.webdriver.common.keys")$Keys
  By <- webdriver$common$by$By
  webdriver_wait <- webdriver$support$ui$WebDriverWait
  expected_conditions <- webdriver$support$expected_conditions
  wait <- webdriver_wait(remote_driver,3.5)
  Sys.sleep(delay_time+runif(n=1,min=0,max=1))
  #NEW CHAT
  wait$until(expected_conditions$element_to_be_clickable(c('xpath','//button[@title="새로운 채팅 시작"]')))$click()
  Sys.sleep(delay_time+runif(n=1,min=0,max=1))
  #SEARCH KEYWORD INPUT
  search_box <- wait$until(expected_conditions$element_to_be_clickable(c('xpath','//textarea[@id="userInput"]'))) #Copilot에 메시지
  search_box$click()
  search_box$clear()
  search_box$send_keys(keyword,Keys$ENTER)
  
  
  wait <- webdriver_wait(remote_driver,60)
  wait$until(expected_conditions$visibility_of_all_elements_located(c('xpath','//div[@class="group/ai-message-item space-y-3 break-words"]')))
  wait$until(expected_conditions$visibility_of_all_elements_located(c('xpath','//span[@class="p-1"]')))
  Sys.sleep(1.5)
  
  question_n <- remote_driver$find_elements('xpath','//h2[@aria-label="내 메시지"]') %>% length
  if(question_n>1) {
    record_log(text1=keyword,text2="Two+ Question in Single Page",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  
  #SCRAP
  answer_text <- remote_driver$find_elements('xpath','//div[@class="group/ai-message-item space-y-3 break-words"]')[[1]]$text
  links <- remote_driver$find_elements('xpath','//a[@rel="noopener noreferrer"]')
  if(length(links)==0) {
    link_url_text <- data.table(link=NA,title=NA,direct_indirect_link=NA,rank=NA)
  } else {
    link_url_text <- data.table(link=map_chr(links, \(x) x$get_attribute('href')),
                                title=map_chr(links, \(x) x$get_attribute('title')),
                                direct_indirect_link=NA)
    link_url_text[,rank:=.I]
  }
  copilit_page <- remote_driver$current_url
  ans <- data.table(tool='Manual',
                    monitoring_date=Sys.Date(),
                    keyword=keyword,
                    platform='Copilot',
                    link_url_text,
                    answer_text,
                    country_code=country_code,
                    language_code=language_code,
                    share_link=copilit_page)
  if(length(remote_driver$find_elements('xpath','//div[@data-testid="prompt-sign-in-composer-action"]'))>0) {
    remote_driver$find_elements('xpath','//div[@data-testid="prompt-sign-in-composer-action"]/descendant::button[@data-testid="maybe-later-button"]')[[1]]$click()
  }
  Sys.sleep(0.5)
  if(save_page) writeBin(remote_driver$page_source,con=file.path(save_dir,paste0(gsub("\\?","？",keyword),'.html')))
  if(capture) save_screenshot(type='Copilot',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword),remote_driver=remote_driver)
  ans[,names(ans)[map_lgl(names(ans), \(x) is.character(ans[[x]]))]:=map(.SD,replace_comma),.SDcols=names(ans)[map_lgl(names(ans), \(x) is.character(ans[[x]]))]]
  fwrite(ans,file=file.path(save_dir,paste0(save_file_name,"-Data.csv")),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[Copilot]",keyword,"......추출 성공"))
  }
}

scrap_aimode_py_in <- function(keyword,delay_time,remote_driver=remDr,country_code='US',language_code='EN',
                               save_dir,save_file_name,save_page=FALSE,capture=FALSE,
                               chat_id=NULL,chat_token=NULL) {
  if(!py_module_available("undetected_chromedriver")) {
    stop("준비가 되지 않아 실행 불가")
  }
  if(!(basename(save_dir) %in% c("ai_mode"))) {
    save_dir <- file.path(save_dir,"ai_mode")
    if(!dir.exists(save_dir)) dir.create(save_dir)
  }
  Keys <- import("selenium.webdriver.common.keys")$Keys
  By <- webdriver$common$by$By
  webdriver_wait <- webdriver$support$ui$WebDriverWait
  expected_conditions <- webdriver$support$expected_conditions
  wait <- webdriver_wait(remote_driver,60)
  wait$until(expected_conditions$element_to_be_clickable(c("xpath", '//button[@aria-label="Microphone"]')))
  Sys.sleep(0.5)
  
  # Show All Button Click
  wait <- webdriver_wait(remote_driver,5)
  show_all <- wait$until(expected_conditions$element_to_be_clickable(c("xpath", '//div[contains(translate(.,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz"),"show all") and @role="button"]'))) %>% 
    tryCatch(.,error=function(e) {NULL})
  
  if(length(show_all)>0) show_all$click()
  
  # 실패 분기
  if(length(remote_driver$find_elements('xpath','//div[contains(text(),"Something went wrong and the content was")]'))>0) {
    record_log(text1=keyword,text2="Was Not Generated",save_dir=save_dir,save_file_name=save_file_name,chat_id=chat_id,chat_token=chat_token)
    stop()
  }
  
  # 본문 추출
  answer_text <- remote_driver$find_elements('xpath','//*[@jsaction or @jscontroller]
  [preceding::span[@aria-level="2" and @role="heading"][1]]
  [following::button[@aria-label="Thumbs up"]]') %>%
    map_chr(\(x) x$text) %>% trimws %>% unique %>% Filter(f=function(x) x!="")
  
  answer_text <- map_chr(answer_text, \(x) {
    if(sum(str_starts(answer_text,fixed(x)))<=1) {
      x
    } else {
      NA_character_
    }
  }) %>% .[!is.na(.)] %>% paste0(collapse=" ")
  
  answer_text <- gsub(" AI responses may include mistakes. Learn more$","",answer_text)
  
  # 링크 추출
  link_url_text <- map(remote_driver$find_elements('xpath','//div[@data-type="hovc" and @role="dialog"]/
                                                   descendant::li[not(contains(@class,"jJxJQ"))]/
                                                   descendant::a'), \(x) {
                                                     data.table(title=x$get_attribute('aria-label'),
                                                                link=x$get_attribute('href'),
                                                                direct_indirect_link=NA)
                                                   }) %>% rbindlist
  
  if(nrow(link_url_text)==0) link_url_text <- data.table(title=NA,link=NA,direct_indirect_link=NA)
  link_url_text[,`:=`(rank=.I,answer_text=answer_text)]
  
  if(save_page) writeBin(remote_driver$page_source,con=file.path(save_dir,paste0(gsub("\\?","？",keyword),'.html')))
  if(capture) save_screenshot(type='AI_mode',save_dir=save_dir,save_file_name=gsub("\\?","？",keyword),remote_driver=remote_driver)
  ans <- data.table(tool='Manual',
                    monitoring_date=Sys.Date(),
                    keyword=keyword,
                    platform='Google AI Mode',
                    link_url_text,
                    country_code=country_code,
                    language_code=language_code,
                    share_link=remote_driver$current_url)
  ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,replace_comma),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
  fwrite(ans,file=file.path(save_dir,paste0(save_file_name,"-Data.csv")),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    send_msg(chat_id=chat_id,chat_token=chat_token,text=paste("[AI_Mode]",keyword,"......추출 성공"))
  }
  Sys.sleep(delay_time+runif(n=1,min=1,max=6)+abs(rnorm(n=1,mean=1.5,sd=0.5)))
  try(remote_driver$find_elements('xpath','//div[text()="All"]')[[1]]$click(),silent=T)
  Sys.sleep(delay_time+runif(n=1,min=0.5,max=log(delay_time+1+0.5)))
  ans
}