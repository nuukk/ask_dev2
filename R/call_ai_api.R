chat_gpt_in2 <- function(organization,project,api_key,model='gpt-4o-mini',
                        system_msg=NULL,input_msg,
                        max_tokens=16384L,max_tokens_auto=FALSE,temperature=1,
                        save_dir=NULL,save_file_name=NULL) {
  
  if(max_tokens_auto) {
    if(str_detect(model,'4.1') && str_detect(model,'ft')) max_tokens <- 16384L
    if(str_detect(model,'4.1')) max_tokens <- 32768L
    if(str_detect(model,'4o')) max_tokens <- 16384L
  }
  
  if(str_detect(model,'embed')) {
    url <- 'https://api.openai.com/v1/embeddings'
    payload <- list(input=input_msg,model=model)
  } else {
    url <- 'https://api.openai.com/v1/chat/completions'
    payload <- list(model=model,
                    messages=list(
                      list(role='user',content=input_msg)),
                    temperature=temperature,
                    max_tokens=max_tokens)
    if(!is.null(system_msg)) {
      payload$messages <- append(list(list(role='system', content=system_msg)),payload$messages) 
    } 
  }
  
  ans <- tryCatch(url %>% request() %>% 
                    req_headers(organization=organization,
                                project=project,
                                Authorization=paste0("Bearer ",api_key)) %>% 
                    req_timeout(180) %>% 
                    req_body_json(payload) %>% req_perform,
                  error=function(e) {NA})
  if("status_code" %in% names(ans) && ans$status_code==200) {
    # 호출 성공
    ans <- resp_body_json(ans)
    if(str_detect(model,'embed')) {
      ## embedding model
      res <- data.table(keyword=input_msg,t(ans$data[[1]]$embedding))
      if(!is.null(save_dir) && !is.null(save_file_name)) {
        fwrite(data.table(model=model,keyword=input_msg,tokens=ans$usage$prompt_tokens),
               file=file.path(save_dir,paste0(save_file_name,'_embedding_tokens.csv')),
               bom=T,append=T)
      }
    } else {
      ## non embedding model
      res <- str_split(ans$choices[[1]]$message$content,"\n")[[1]]
      if(jsonlite::validate(res)) {
        res <- data.frame(fromJSON(res))
      } else {
        res <- paste0(res,collapse=" ")
      }
      tokens_input <- ans$usage$prompt_tokens
      tokens_output <- ans$usage$completion_tokens
      res <- data.table(model=model,system_msg=system_msg,question=input_msg,answer=res,tokens_input=tokens_input,tokens_output=tokens_output)
    }
  } else {
    # 호출 실패
    tokens_input <- get_token_count(input_msg,'gpt-4o')
    if(!str_detect(model,'embed')) {
      res <- data.table(model=model,system_msg=system_msg,question=input_msg,answer=NA_character_,tokens_input=tokens_input,tokens_output=0)
    }
  }
  if(!is.null(save_dir) && !is.null(save_file_name)) {
    res_save <- copy(res)
    res_save[,names(res_save)[sapply(res_save,is.character)]:=lapply(.SD,replace_comma),.SDcols=names(res_save)[sapply(res,is.character)]]
    fwrite(res_save,file=file.path(save_dir,paste0(save_file_name,'.csv')),bom=T,append=T)
  }
  res
}

chat_gpts2 <- function(organization,project,api_key,
                      model='gpt-4o-mini',system_msg=NULL,input_msgs,
                      max_tokens=16384,max_tokens_auto=FALSE,temperature=1,
                      save_dir=getwd(),save_file_name=paste0('gpt_api_',gsub(":|-","_",Sys.Date()))) {
  
  tokens_input <- 0L
  tokens_output <- 0L
  tokens_total <- 0L
  requests <- 0L
  res <- vector("list",length(input_msgs))
  
  if(str_detect(model,"embed")) {
    rpm_limit <- 3000
    tpm_limit <- 1000000
  } else if(str_detect(model,"mini|nano")) {
    rpm_limit <- 500
    rpd_limit <- 10000
    tpm_limit <- 2000000
  } else {
    rpm_limit <- 500
    tpm_limit <- 30000
  } # 추론 계열은 제외
  
  start_time <- Sys.time()
  for(i in seq_along(input_msgs)) {
    tmp <- try(chat_gpt_in2(organization=organization,
                           project=project,
                           api_key=api_key,
                           model=model,
                           system_msg=system_msg,
                           input_msg=input_msgs[[i]],
                           max_tokens=max_tokens,
                           max_tokens_auto=max_tokens_auto,
                           save_dir=save_dir,
                           save_file_name=save_file_name,
                           temperature=temperature),silent=T)
    now_time <- Sys.time()
    if(inherits(tmp, "try-error")) {
      message(paste(keywords[[i]],"실패"))
      next
    }
    requests <- requests+1
    if(str_detect(model,'embed')) {
      tokens_total <- tokens_total+get_token_count(input_msgs[[i]],'gpt-4o')
    } else {
      tokens_input <- tokens_input+tmp$tokens_input
      tokens_output <- tokens_output+tmp$tokens_output
      tokens_total <- tokens_input+tokens_output
    }
    if(as.numeric(difftime(now_time,start_time))>=60) {
      start_time <- Sys.time()
      tokens_total <- 0
      tokens_input <- 0
      tokens_output <- 0
      requests <- 0
    }
    if(tokens_total>=tpm_limit) Sys.sleep(60)
    if(requests>=rpm_limit) Sys.sleep(60)
    res[[i]] <- tmp
    cat("\r", i, "번째 작업 완료...(",round(i/length(input_msgs)*100,1),"%)", sep="")
  }
  res
}

# pplx_in <- function(model='r1-1776',api_key,system_msg,input_msg,max_tokens=256L,temperature=0.1) {
#   url <- 'https://api.perplexity.ai/chat/completions'
#   
#   payload <- list(model=model,
#                   messages=list(
#                     list(role='system',content=system_msg),
#                     list(role='user',content=input_msg)
#                   ),
#                   temperature=temperature,
#                   max_tokens=max_tokens)
#   
#   res <- tryCatch(url %>% request %>% 
#                     req_headers(accept="application/json",
#                                 Authorization=paste0("Bearer ",api_key)) %>% 
#                     req_body_json(payload) %>% req_perform,
#                   error=function(e) {NA})
#   
#   if(res$status_code==200) {
#     res <- resp_body_json(res)
#     res <- res$choices[[1]]$message$content %>% gsub("<think>|</think>","",.)
#   } else {
#     res <- NA
#   }
#   res
# }
# 
# 
# pplx <- function(model='sonar',api_key,system_msg,input_msg,max_tokens=256L,temperature=0.1) {
#   res <- imap_chr(input_msg, \(x,i) {
#     if(i%%50==0) Sys.sleep(runif(n=1,min=3.5,max=6.5)+rnorm(n=1,mean=1,sd=1.2))
#     pplx_in(model=model,
#             api_key=api_key,
#             system_msg=system_msg,
#             input_msg=x,
#             max_tokens=256L,temperature=0.1)
#   },.progress=TRUE)
#   res
# }
