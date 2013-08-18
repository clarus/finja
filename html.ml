open Batteries ;;

let print_header html fia =
  Printf.fprintf html "<!DOCTYPE html><html>\n";
  Printf.fprintf html "<head><title>finja report for %s</title></head>\n" fia;
  Printf.fprintf html "<body>\n"
;;

let print_term html term =
  Printf.fprintf html "<pre>%s</pre>\n" (Computation.html_of_term term)
;;

let close_html html =
  Printf.fprintf html "</body>\n</html>"
;;
