scrap_ppx_py <- function(keywords,delay_time=1,remote_driver=remDr,
                         save_dir,save_file_name,save_page=FALSE,capture=TRUE,chat_id=NULL,chat_token=NULL,
                         language_code='EN',country_code='US') {
  walk(keywords, \(x) {
    tryCatch(scrap_ppx_py_in(keyword=x,delay_time=delay_time,
                             save_dir=save_dir,save_file_name=save_file_name,
                             capture=capture,chat_id=chat_id,chat_token=chat_token,
                             language_code=language_code,country_code=country_code,
                             remote_driver=remote_driver),
             error=function(e) {
               record_log(text1=x,text2=remote_driver$current_url,
                          save_dir=save_dir,save_file_name=paste0(save_file_name,'_fail_list'),
                          chat_id=chat_id,chat_token=chat_token)
             })
  },.progress=TRUE)
}

scrap_gpt_py <- function(keywords,delay_time=1.5,save_dir,save_file_name,
                         language_code='EN',country_code='US',project_name=NULL,
                         save_page=FALSE,remote_driver=remDr,capture=FALSE,chat_id=NULL,chat_token=NULL) {
  walk(keywords, \(x) {
    tryCatch(scrap_gpt_py_in(keyword=x,delay_time=delay_time,save_page=save_page,
                             save_dir=save_dir,save_file_name=save_file_name,project_name=project_name,
                             language_code=language_code,country_code=country_code,
                             remote_driver=remote_driver,capture=capture,chat_id=chat_id,chat_token=chat_token),
             CriticalError=function(ce) {
               record_log(text1=x,text2="Critial Error로 전체 종료",remote_driver$current_url,
                          save_dir=save_dir,save_file_name=paste0(save_file_name,'_fail_list'),
                          chat_id=chat_id,chat_token=chat_token)
               stop()
             },
             error=function(e) {
               record_log(text1=x,text2=remote_driver$current_url,
                          save_dir=save_dir,save_file_name=paste0(save_file_name,'_fail_list'),
                          chat_id=chat_id,chat_token=chat_token)
             })
  },.progress=TRUE)
}

scrap_ggai_py <- function(keywords,delay_time=2,save_dir,save_file_name,
                          language_code='EN',country_code='US',
                          save_page=FALSE,remote_driver=remDr,capture=FALSE,chat_id=NULL,chat_token=NULL) {
  walk(keywords, \(x) {
    tryCatch(scrap_ggai_py_in(keyword=x,delay_time=delay_time,save_dir=save_dir,
                              save_file_name=save_file_name,save_page=save_page,
                              country_code=country_code,language_code=language_code,
                              remote_driver=remote_driver,capture=capture,chat_id=chat_id,chat_token=chat_token),
             error=function(e) {
               record_log(text1=x,text2=remote_driver$current_url,
                          save_dir=save_dir,save_file_name=paste0(save_file_name,'_fail_list'),
                          chat_id=chat_id,chat_token=chat_token)
             })
  },.progress=TRUE)
}

scrap_gemini_py <- function(keywords,delay_time=1.5,save_dir,save_file_name,save_page=FALSE,
                            language_code='EN',country_code='US',
                            remote_driver=remDr,capture=FALSE,chat_id=NULL,chat_token=NULL) {
  walk(keywords, \(x) {
    tryCatch(scrap_gemini_py_in(keyword=x,delay_time=delay_time,save_page=save_page,
                                language_code=language_code,country_code=country_code,
                                save_dir=save_dir,save_file_name=save_file_name,
                                remote_driver=remote_driver,capture=capture,chat_id=chat_id,chat_token=chat_token),
             error=function(e) {
               record_log(text1=x,text2=remote_driver$current_url,
                          save_dir=save_dir,save_file_name=paste0(save_file_name,'_fail_list'),
                          chat_id=chat_id,chat_token=chat_token)
             })
  },.progress=TRUE)
}

scrap_copilot_py <- function(keywords,delay_time=1,remote_driver=remDr,
                             save_dir,save_file_name,save_page=FALSE,capture=TRUE,chat_id=NULL,chat_token=NULL,
                             language_code='EN',country_code='US') {
  walk(keywords, \(x) {
    tryCatch(scrap_copilot_py_in(keyword=x,delay_time=delay_time,
                                 save_dir=save_dir,save_file_name=save_file_name,
                                 capture=capture,chat_id=chat_id,chat_token=chat_token,
                                 language_code=language_code,country_code=country_code,
                                 remote_driver=remote_driver),
             error=function(e) {
               record_log(text1=x,text2=remote_driver$current_url,
                          save_dir=save_dir,save_file_name=paste0(save_file_name,'_fail_list'),
                          chat_id=chat_id,chat_token=chat_token)
             })
  },.progress=TRUE)
}