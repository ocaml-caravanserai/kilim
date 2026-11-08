let () =
  Kilim_http_browser.install () ;
  Kilim_websocket_browser.install () ;
  Kilim_storage_browser.install () ;
  Kilim_browser.run ~mount:"app" Chat.app
