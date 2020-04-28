open! Core_kernel

open S

let list_regex = Re2.create_exn ", "

let process_party text uri =
  begin match Re2.split list_regex (Html.text_to_string text) |> List.to_array with
  | [|last; first; role|] -> Some {
      name = Full { first_name = Html.clean (String.uppercase first); last_name = Html.clean (String.uppercase last) };
      role = Oscn.parse_role role;
      uri;
    }
  | [|other; role|] -> Some {
      name = Other_name (Html.clean (String.uppercase other));
      role = Oscn.parse_role role;
      uri;
    }
  | [| " " |] ->
    (* Html.clean will have converted all the whitespace from blank strings into a single space, *)
    (* making it safe and easy to catch them here *)
    None
  | _ -> failwithf "Invalid parties entry: '%s'" (Html.text_to_string text) ()
  end
