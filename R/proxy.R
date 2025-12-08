start_proxy <- function(
    proxy_host="127.0.0.1",proxy_port=8899L,
    upstream_host,upstream_port,upstream_user,upstream_pw,
    log_file=NULL,log_level="w"
) {
  # OS별 null device
  null_device <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  
  callr::r_bg(\(proxy_host,proxy_port,
             upstream_host,upstream_port,upstream_user,upstream_pw,
             log_file,log_level,
             null_device) {
      
      library(reticulate)
      
      use_python(miniconda_path(), required=TRUE)
      py_cfg <- py_config()
      python_exe <- py_cfg$python
      
      proxy_pool_arg <- sprintf("%s:%s@%s:%d",upstream_user,upstream_pw,upstream_host,upstream_port)
      
      args <- c(
        "-m", "proxy",
        "--hostname", proxy_host,
        "--port", as.character(proxy_port),
        "--plugins", "proxy.plugin.ProxyPoolPlugin",
        "--proxy-pool", proxy_pool_arg,
        "--log-level", log_level
      )
      
      if (!is.null(log_file)) {
        args <- c(args, "--log-file", log_file)
      }

      system2(
        command=python_exe,
        args=args,
        stdout=null_device,
        stderr=null_device
      )
    },
    args=list(
      proxy_host=proxy_host,
      proxy_port=proxy_port,
      upstream_host=upstream_host,
      upstream_port=upstream_port,
      upstream_user=upstream_user,
      upstream_pw=upstream_pw,
      log_file=log_file,
      log_level=log_level,
      null_device=null_device
    ),
    stdout=null_device,
    stderr=null_device
  )
}
