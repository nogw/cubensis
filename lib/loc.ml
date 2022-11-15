include Location

let pp fmt loc = Location.print_loc fmt loc 

let format ppf loc =
  let file, line, start = Location.get_pos_info loc.loc_start in
  
  if String.length file != 0 
    then Format.fprintf ppf "file %s, " file;

  Format.fprintf ppf "line %i, characters %i-%i"
    line start (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + start);

  Format.pp_print_flush ppf ()

let show loc =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  
  format fmt loc;
  Format.pp_print_flush fmt ();
  Buffer.contents buf