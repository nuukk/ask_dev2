# children_repeat <- function(x,n=1) {
#   rep_n <- 0L
#   while(rep_n<n) {
#     x <- html_children(x)
#     rep_n <- rep_n+1L
#   }
#   x
# }
# 
# search_node_location <- function(tags) {
#   res <-  map_int(tags, \(x) {
#     tryCatch(length(html_elements(x,xpath="./preceding::*"))+1,
#              error=function(e) {NA})
#   })
#   res
# }

replace_comma <- function(x) {
  return(gsub(",","!@#",x))
}

restore_comma <- function(x) {
  return(gsub("!@#",",",x))
}

quotation_reduce <- function(x) {
  gsub('"{3,}','"',x) 
}

half_to_full <- function(x) {
  return(gsub("/","／",x) %>% gsub("\\?","？",.) %>% gsub(":","：",.))
}


send_msg <- function(chat_id,chat_token,text) {
  bot <- Bot(token=chat_token)
  bot$sendMessage(chat_id=chat_id,text=paste(trunc(Sys.time()),text,"………",Sys.getpid()))
}

send_progress <- function(base_dir,chat_id,chat_token) {
  progress_table <- map(list.dirs(file.path(getwd(),base_dir),recursive=F), \(x) {
    data.table(dir=gsub(".*/","",base_dir),
               cat=gsub(".*/","",x),
               n=length(list.files(x,pattern='png')))
  }) %>% rbindlist %>% mutate(time=trunc(Sys.time())) %>% 
    capture.output %>% paste(collapse="\n") %>% paste0("'''")
  
  send_msg(chat_id=chat_id,chat_token=chat_token,text=progress_table)
}

# send_progress_auto <- function(chat_id,chat_token,base_dir,interval=10,time_limit=600) {
#   time <- 0
#   repeat {
#     Sys.sleep(interval)
#     time <- time+interval
#     
#     try({
#       msgs <- paste0("https://api.telegram.org/bot",chat_token,"/getUpdates") %>% 
#         request %>% req_perform
#       msgs <- resp_body_json(msgs)$result
#       ans <- msgs[length(msgs)][[1]]$message$text
#       if(tolower(ans)=='update') send_progress(base_dir=base_dir,chat_id=chat_id,chat_token=chat_token)
#     },silent=TRUE)
#     
#     if(time>=time_limit) break
#   }
# }

filter_data <- function(csv_path,imgs_path,exclude_geo=NULL) {
  raw <- fread(csv_path,encoding='UTF-8')
  raw[,keyword:=restore_comma(keyword)]
  intersect_keyword <- intersect(raw[,unique(keyword)],
                                 gsub(".png","",imgs_path) %>% gsub("？","?",.) %>% gsub("：",":",.))
  
  raw <- raw[keyword %chin% intersect_keyword]
  if(!is.null(exclude_geo)) {
    wrong_location_keyword <- raw[,.SD[1],by=share_link][str_detect(tolower(answer_text),paste0(exclude_geo,collapse="|"))][,unique(keyword)]
    wrong_location_keyword <- restore_comma(wrong_location_keyword)
    raw <- raw[!(keyword %chin% wrong_location_keyword)]
  }
  raw <- raw[!share_link %chin% c("https://chatgpt.com/","https://chatgpt.com/?model=gpt-4o")]
  raw[,keyword:=replace_comma(keyword)]
  raw[,names(raw)[map_lgl(names(raw), \(x) is.character(raw[[x]]))]:=map(.SD,quotation_reduce),.SDcols=names(raw)[map_lgl(names(raw), \(x) is.character(raw[[x]]))]]
  fwrite(raw,file=csv_path,bom=T)
}

png_loc <- function(base_dir) {
  map(c('gpt','ppx','ggai','ggai/ai_mode','gemini','copilot'), \(x) {
    file_list <- list.files(file.path(getwd(),base_dir,x),pattern='png$',full.names=T)
    if(length(file_list)==0) {
      return("")
    }
    if(!dir.exists(file.path(getwd(),base_dir,paste0("captures_",gsub("-","_",Sys.Date()))))) {
      dir.create(file.path(getwd(),base_dir,paste0("captures_",gsub("-","_",Sys.Date()))))
    }
    
    if(!dir.exists(file.path(getwd(),base_dir,paste0("captures_",gsub("-","_",Sys.Date())),x))) {
      dir.create(file.path(getwd(),base_dir,paste0("captures_",gsub("-","_",Sys.Date())),x))
    }
    map(file_list, \(fl) {
      file.rename(from=fl,
                  to=file.path(getwd(),base_dir,paste0("captures_",gsub("-","_",Sys.Date())),x,
                               basename(fl)))
    })
  })
}

save_screenshot <- function(type,save_dir,save_file_name,remote_driver) {
  type <- match.arg(type,c("Perplexity","ChatGPT","GGAI","Gemini","Copilot","AI_mode"))
  if(type=='Perplexity') {
    scroll_target <- '//h1'
    calc_target <- '//div[contains(@class,"items-center justify-between")]'
    shot_target <- '//div[contains(@class,"@container/main")]'
  } else if(type=='ChatGPT') {
    scroll_target <- '//div[@class="whitespace-pre-wrap"]'
    calc_target1 <- '//button[@aria-label="Good response" or @aria-label="좋은 응답"]'
    calc_target2 <- '//div[@slot="content"]/descendant::a'
    shot_target <- '//body'
  } else if(type=='GGAI') {
    scroll_target <- '(//div[@data-st-tgt="mode"] | //div[@role="navigation"])[1]'
    calc_target <- '//div[@class="main"]'
    shot_target <- '//body'
  } else if(type=='Gemini') {
    scroll_target <- '//p[contains(@class,"query-text")]'
    calc_target1 <- '//mat-icon[@fonticon="thumb_up"]'
    calc_target2 <- '//div[@class="gds-title-l"]/following::inline-source-card'
    shot_target <- '//div[@class="content-wrapper"]'
  } else if(type=='Copilot') {
    scroll_target <- '//h2[@aria-label="내 메시지"]'
    calc_target <- '//span[@class="p-1"]'
    shot_target <- '//div[@data-content="conversation"]'
  } else if(type=='AI_mode') {
    scroll_target <- '(//span[@aria-level="2" and @role="heading"])'
    calc_target <- '//button[@aria-label="Thumbs up"]'
    shot_target <- '//body'
  }
  
  remote_driver$execute_script("arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });",
                               remote_driver$find_element('xpath',scroll_target)) #상단으로 스크롤 이동
  Sys.sleep(1)
  
  if(exists("calc_target2")) {
    # CHAT GPT OR Gemini
    if(length(remote_driver$find_elements('xpath',calc_target2))==0) {
      # 출처 링크가 없는 경우
      calc_target <- calc_target1
      ref_node <- remote_driver$find_elements('xpath',calc_target)
      ref_node <- ref_node[[length(ref_node)]]
      modify_height <- ref_node$location$y+ref_node$size$height
    } else {
      # 출처 링크가 있는 경우
      calc_target_tmp1 <- remote_driver$find_elements('xpath',calc_target1)
      calc_target_tmp1 <- calc_target_tmp1[[length(calc_target_tmp1)]]
      calc_target_tmp1 <- calc_target_tmp1$location$y+calc_target_tmp1$size$height
      
      calc_target_tmp2 <- remote_driver$find_elements('xpath',calc_target2)
      try(remote_driver$execute_script("arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });",
                                       calc_target_tmp2[[1]]),silent=TRUE)
      calc_target_tmp2 <- calc_target_tmp2[[length(calc_target_tmp2)]]
      calc_target_tmp2 <- calc_target_tmp2$location$y+calc_target_tmp2$size$height
      
      if(calc_target_tmp1>calc_target_tmp2) {
        modify_height <- calc_target_tmp1
      } else {
        modify_height <- calc_target_tmp2
      }
    }
  } else {
    ref_node <- remote_driver$find_elements('xpath',calc_target)
    ref_node <- ref_node[[length(ref_node)]]
    modify_height <- ref_node$location$y+ref_node$size$height
  }
  modify_height <- ceiling(modify_height*1.2)+150
  
  remote_driver$execute_cdp_cmd('Emulation.setDeviceMetricsOverride',
                                list(width=1920,
                                     height=modify_height,
                                     deviceScaleFactor=1,
                                     mobile=FALSE))
  
  remote_driver$find_element('xpath',shot_target)$screenshot(file.path(save_dir,paste0(save_file_name,'.png')))
  remote_driver$execute_cdp_cmd("Emulation.clearDeviceMetricsOverride",list('x'=TRUE))
}


scrap_files_merge <- function(path,cond,save_file_name=NULL) {
  ans <- list.files(path,pattern=cond,full.names=T) %>% 
    lapply(\(x) fread(x,encoding="UTF-8",fill=T)[,monitoring_date:=as.Date(monitoring_date)]) %>% rbindlist(use.names=T)
  ans <- ans[share_link %chin% ans[,.SD[which.max(.I)],by=keyword][,share_link]]
  ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,ask::quotation_reduce),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
  ans[,names(ans)[sapply(ans,\(x) is.character(x))]:=lapply(.SD,\(x) gsub('"{3,}','"',x)),.SDcols=names(ans)[sapply(ans,\(x) is.character(x))]]
  if(!is.null(save_file_name)) {
    if(!str_ends(save_file_name,'csv')) save_file_name <- paste0(save_file_name,'.csv')
    if(file.exists(file.path(path,save_file_name))) file.copy(from=file.path(path,save_file_name),
                                                              to=file.path(path,paste0(gsub(".csv","",save_file_name),"_",gsub("-|:","_",round(Sys.time())),'.csv')))
    fwrite(ans,file=file.path(path,save_file_name),bom=T)
  }
  return(ans)
}

get_locale <- function(country_code,accuracy=100) {
  locale_tmp <- "https://ipinfo.io/json" %>% 
    request() %>% 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36") %>% 
    req_headers(Accept="text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7") %>% 
    req_perform %>% 
    resp_body_json
  
  country_code <- tolower(country_code)
  if(country_code=="us") {
    accept_language="en-US,en;q=0.9"
    locale="en_US"
  } else if(country_code %in% c("ko","kr")) {
    accept_language="ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7"
    locale="ko_KR"
  } else if(country_code %in% c("uk","gb")) {
    accept_language="en-GB,en;q=0.9"
    locale="en_GB"
  } else if(country_code=="es") {
    accept_language="es-ES,es;q=0.9"
    locale="en_ES"
  } else if(country_code=="in") {
    accept_language="en-IN,en;q=0.9"
    locale="en_IN"
  } else if(country_code=="au") {
    accept_language="en-AU,en;q=0.9"
    locale="en_AU"
  }
  
  locale <- list(accept_language=accept_language,
                 locale=locale,
                 time_zone=locale_tmp$timezone,
                 latitude=gsub(",.*","",locale_tmp$loc),
                 longitude=gsub(".*,","",locale_tmp$loc),
                 accuracy=accuracy)
  
  return(locale)
}

gpt_entity_parser <-function(text) {
  replace_with_entity <- function(ent) {
    m <- str_match(
      ent,
      'entity\\s*\\[\\s*"[^"]*"\\s*,\\s*"([^"]*)"'
    )
    value <- m[, 2]
    
    if (is.na(value)) {
      return(ent)
    } else {
      return(value)
    }
  }
  
  entity_pattern <- 'entity\\[([^\\[\\]]+|\\[[^\\]]*\\])*\\]'
  ans <- str_replace_all(text,entity_pattern,replace_with_entity)
  ans <- gsub("image_group.*?}", "", ans) %>% gsub("entity_metadata.*?\\}\\]","",.,perl=T)
  return(ans)
}