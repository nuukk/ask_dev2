record_log <- function(text1,text2,save_dir,save_file_name,chat_id=NULL,chat_token=NULL) {
  if(missing(text2)) text2 <- NA_character_
  log_dt <- data.table(time_stamp=Sys.time()+hours(9),x=text1,y=text2)
  fwrite(log_dt,file=file.path(save_dir,paste0(save_file_name,"_error_log.csv")),bom=T,append=T)
  if(!is.null(chat_id) & !is.null(chat_token)) {
    try(send_msg(chat_id=chat_id,chat_token=chat_token,text=paste(text1,"/",text2,"...추출 실패")),silent=TRUE)
  }
}


record_log2 <- function(country_code="US",language_code="EN",platform,collect_type="initial",
                        keyword,step,total_steps,
                        status="OK",msg=".",save_dir,save_file_name) {
  ans <- list(country_code=toupper(country_code),
              language_code=toupper(language_code),
              platform=platform,
              collect_type=collect_type,
              keyword=keyword,
              log_time=format(Sys.time(),format="%Y-%m-%d %H:%M:%S %z"),
              step=step,
              total_steps=total_steps,
              status=status,
              msg=msg,
              com_name=Sys.getenv()[["COMPUTERNAME"]],
              pid=Sys.getpid())
  
  save_file_name <- if(!str_ends(save_file_name,"\\.log")) paste0(save_file_name,".log") else save_file_name
  cat(jsonlite::toJSON(ans,auto_unbox=TRUE),"\n",file=file.path(save_dir,save_file_name),append=TRUE)
}

progress_table <- function(base_dir) {
  child_directories <- list.dirs(base_dir,recursive=F,full.names=T)
  child_directories <- child_directories[!str_ends(child_directories,"json_raw|err_htmls")]
  paths <- if(length(child_directories)>0) {
    list.files(child_directories,full.names=T,pattern="log$")
  } else {
    list.files(base_dir,pattern="log$",full.names=T)
  }
  
  if(length(paths)==0) {
    return(invisible())
  }
  
  res <- lapply(paths,\(x) {
    con <- file(x,open="r")
    tmp <- stream_in(con,verbose=F)
    on.exit(close(con),add=T)
    tmp
  }) %>% rbindlist
  
  res[,log_time:=suppressMessages(ymd_hms(log_time,tz="Asia/Seoul"))]
  res[,start_time:=min(log_time),by=.(country_code,language_code,platform,com_name,pid)]
  setorder(res,platform,log_time)
  res[,msg:=as.character(msg)]
  res[]
}
