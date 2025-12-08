.py_cache <- new.env(parent=emptyenv())

# 파이썬 모듈 로더 (사용자에게 system.file 노출 X)
.py_module <- function() {
  if(!exists("mod", envir=.py_cache, inherits=FALSE) || is.null(.py_cache$mod)) {
    py_dir <- system.file("python", package="ask")
    if(py_dir=="") stop("ask: inst/python 경로를 찾지 못했습니다.", call.=FALSE)
    .py_cache$mod <- reticulate::import_from_path("nd_response_capture", path=py_dir, convert=FALSE)
  }
  .py_cache$mod
}

# 필요하면 외부로 모듈 객체를 노출
nd_response_capture <- function() {
  .py_module()
}

# 편의 래퍼들 ---------------------------------------------------------------

register_handlers <- function(tab) {
  .py_module()$register_handlers(tab)
  invisible(TRUE)
}

clear_captured <- function() {
  .py_module()$clear_buffers()
  invisible(TRUE)
}

wait_for_response <- function(url_pattern, timeout=10L) {
  .py_module()$wait_for_response(url_pattern, as.integer(timeout))
}

wait_for_network_idle <- function(tab, timeout=5L, idle_time=2L) {
  .py_module()$wait_for_network_idle(tab, as.integer(timeout), as.integer(idle_time))
}

trigger_and_capture_eval <- function(tab, action_script, target_url_pattern, timeout=10L) {
  .py_module()$trigger_and_capture_eval(tab, action_script, target_url_pattern, as.integer(timeout))
}

trigger_and_capture_action <- function(tab, xpath=NULL, element=NULL, action='click', timeout=5L) {
  .py_module()$trigger_and_capture_action(tab, xpath=xpath, element=element, action=action, timeout=as.integer(timeout))
}
