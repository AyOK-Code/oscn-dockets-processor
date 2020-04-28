open! Core_kernel

let yojson_of_time time = `String (Time.to_string time)
let yojson_of_time_array arr =
  `List (Array.fold_right arr ~init:[] ~f:(fun time acc -> `String (Time.to_string time)::acc))
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

type party = {
  name: name;
  role: role;
  uri: Uri.t option [@to_yojson yojson_of_uri_option];
} [@@deriving to_yojson]

type event = {
  datetime: Time.t [@to_yojson yojson_of_time];
  description: Html.text;
}
[@@deriving to_yojson]

type count = {
  notes: Html.text; (* TODO: Process this further *)
}

type case = {
  uri: Uri.t [@to_yojson yojson_of_uri];
  title: Html.text;
  parties: party array;
  is_defendant: bool;
  case_number: Html.text;
  date_filed: Date.t [@to_yojson yojson_of_date];
  judge: Html.text;
  arresting_agency: Html.text option;
  events: event array;
  court_dates: Time.t array [@to_yojson yojson_of_time_array];
  (* charges: Html.text list; *)
  (* payments: payment list; *)
} [@@deriving to_yojson]
