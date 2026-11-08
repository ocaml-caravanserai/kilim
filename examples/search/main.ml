let () =
  Kilim_http_browser.install () ;
  Kilim_router_browser.install () ;
  Kilim_browser.run ~mount:"app" Search.app
