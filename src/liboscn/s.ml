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

type status =
| Open
| Completed
[@@deriving to_yojson]

type role =
| Defendant
| Plaintiff
| Arresting_agency
| Arresting_officer
[@@deriving to_yojson]

type name =
| Full_name of {
    first_name: Text.t;
    last_name: Text.t;
  }
| Other_name of Text.t
[@@deriving to_yojson]

type party = {
  name: name;
  role: role;
  uri: Uri.t option [@to_yojson yojson_of_uri_option];
} [@@deriving to_yojson]

type event = {
  datetime: Time.t [@to_yojson yojson_of_time];
  description: Text.t;
  judge: Text.t option;
}
[@@deriving to_yojson]

type docket = {
  date: Date.t option [@to_yojson yojson_of_date_option];
  code: Text.t option;
  description: Text.t;
  count: Text.t option;
  party: Text.t option;
  amount: Money.t option;
}
[@@deriving to_yojson]

type complete_count = {
  count_as_filed: Text.t;
  date_of_offense: Date.t [@to_yojson yojson_of_date];
  disposition: Text.t;
  count_as_disposed: Text.t;
  violation_of: Text.t;
} [@@deriving to_yojson]

type counts =
| Simple of Text.t array
| Complete of complete_count array
[@@deriving to_yojson]

type case = {
  status: status;
  uri: Uri.t [@to_yojson yojson_of_uri];
  title: Text.t;
  parties: party array;
  is_defendant: bool;
  case_number: Text.t;
  date_filed: Date.t [@to_yojson yojson_of_date];
  date_closed: Date.t option [@to_yojson yojson_of_date_option];
  judge: Text.t;
  arresting_agency: Text.t option;
  events: event array;
  court_dates: event array;
  transactions: docket array;
  counts: counts;
} [@@deriving to_yojson]
