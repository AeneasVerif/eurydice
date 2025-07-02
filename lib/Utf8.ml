let ascii_of_utf8_str (str : string) =
  let get_uchar_list (str : string) =
    let rec get_uchar_list acc idx =
      try
        let uchar = Uchar.utf_decode_uchar (String.get_utf_8_uchar str idx) in
        let char_len = Uchar.utf_8_byte_length uchar in
        get_uchar_list (uchar :: acc) (idx + char_len)
      with Invalid_argument _ -> List.rev acc
    in
    get_uchar_list [] 0
  in
  let uchar_list_to_ascii lst =
    let to_str uchar =
      if Uchar.is_char uchar then
        Uchar.to_char uchar |> Char.escaped
      else
        Printf.sprintf "\\u{%x}" @@ Uchar.to_int uchar
    in
    List.map to_str lst |> String.concat ""
  in
  get_uchar_list str |> uchar_list_to_ascii
