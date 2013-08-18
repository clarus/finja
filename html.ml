open Batteries ;;

let print_header html fia =
  Printf.fprintf html "<!DOCTYPE html>\
<html>\
  <head>\
    <title>finja report for %s</title>\
  <style type=\"text/css\">\
  * { margin:0; padding:0; font-size:1em; }\
  body { font-size:1em; color:#222; padding:0.5em; }\
  h1 { font-size:1.2em; padding:0.5em; }\
  pre { margin:0.5em; padding:0.5em; background-color:#ddd; line-height:150%%; }\
  strong, em { color:red; }\
  </style>\
  </head>\
  <body>\
    <h1>finja report for &quot;%s&quot;</h1>\
" fia fia
;;

let print_term html term =
  Printf.fprintf html "<pre>%s</pre>\n" (Computation.html_of_term term)
;;

let close_html html =
  Printf.fprintf html "</body>\n</html>"
;;
