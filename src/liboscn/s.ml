open! Core_kernel

let yojson_of_date date = `String (Date.to_string date)
let yojson_of_date_option = function
| Some date -> `String (Date.to_string date)
| None -> `Null
let yojson_of_uri uri = `String (Uri.to_string uri)
let yojson_of_uri_option = function
| Some uri -> `String (Uri.to_string uri)
| None -> `Null

type payment = {
  amount: int;
}

type role =
| Defendant
| Plaintiff
| Arresting_agency
| Arresting_officer
[@@deriving to_yojson]

type name =
| Full of {
    first_name: Html.text;
    last_name: Html.text;
  }
| Other_name of Html.text
[@@deriving to_yojson]

let name_to_text = function
| Full { first_name; last_name } -> sprintf "%s, %s" (Html.text_to_string last_name) (Html.text_to_string first_name) |> Html.clean
| Other_name s -> s

type party = {
  name: name;
  role: role;
  uri: Uri.t option [@to_yojson yojson_of_uri_option];
} [@@deriving to_yojson]

type event = {
  datetime: Time.t;
  description: Html.text;
}

type count = {
  notes: Html.text; (* TODO: Process this further *)
}

let parse_role = function
| "Defendant" -> Defendant
| "Plaintiff" -> Plaintiff
| "ARRESTING OFFICER" -> Arresting_officer
| "ARRESTING AGENCY" -> Arresting_agency
| s -> failwithf "Invalid role: '%s'" s ()

type case = {
  uri: Uri.t [@to_yojson yojson_of_uri];
  title: Html.text;
  parties: party list;
  is_defendant: bool;
  case_number: Html.text;
  date_filed: Date.t [@to_yojson yojson_of_date];
  judge: Html.text;
  arresting_agency: Html.text option;
  (* court_dates: Date.t list; *)
  (* charges: Html.text list; *)
  (* payments: payment list; *)
} [@@deriving to_yojson]
