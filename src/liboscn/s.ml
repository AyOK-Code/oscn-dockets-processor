module Date = Core_kernel.Date

let date_option_of_yojson = function
| `String s -> Ok (Some (Date.of_string s))
| _ -> Error (Format.sprintf "Invalid JSON type for Date, expected String")
let uri_of_yojson = function
| `String s -> Ok (Uri.of_string s)
| _ -> Error (Format.sprintf "Invalid JSON type for Uri, expected String")

type search_request = {
  last_name: string;
  first_name: string option [@default None];
  middle_name: string option [@default None];
  dob_before: Date.t option [@default None] [@of_yojson date_option_of_yojson];
  dob_after: Date.t option [@default None] [@of_yojson date_option_of_yojson];
} [@@deriving of_yojson]

type case_request = {
  person: search_request;
  case_uri: Uri.t [@of_yojson uri_of_yojson];
} [@@deriving of_yojson]

open! Core_kernel

let yojson_of_time time =
  let iso = Time.to_string_iso8601_basic ~zone:Time.Zone.utc time in
  `String (String.slice iso 0 (-4) |> sprintf "%sZ")
let yojson_of_date date = `String (Date.to_string date)
let yojson_of_date_option = function
| Some date -> `String (Date.to_string date)
| None -> `Null
let yojson_of_uri uri = `String (Uri.to_string uri)
let yojson_of_uri_option = function
| Some uri -> `String (Uri.to_string uri)
| None -> `Null

let except keys json : Yojson.Safe.t =
  `Assoc (Yojson.Safe.Util.to_assoc json |> List.filter ~f:(fun (k, _v) -> Array.mem keys k ~equal:String.equal |> not))

type status =
| Open
| Completed

let status_to_yojson = function
| Open -> `String "Open"
| Completed -> `String "Completed"

type role =
| Defendant
| Plaintiff
| Arresting_agency
| Arresting_officer

let role_to_yojson = function
| Defendant -> `String "Defendant"
| Plaintiff -> `String "Plaintiff"
| Arresting_agency -> `String "Arresting_agency"
| Arresting_officer -> `String "Arresting_officer"

type name =
| Full_name of {
    first_name: Text.t;
    last_name: Text.t;
  }
| Other_name of Text.t

let name_to_yojson = function
| Full_name { first_name; last_name } -> `String (sprintf "%s, %s" (Text.to_string last_name) (Text.to_string first_name))
| Other_name s -> Text.to_yojson s

type party = {
  name: name [@key "Name__c"];
  role: role [@key "Role__c"];
  uri: Uri.t option [@to_yojson yojson_of_uri_option];
} [@@deriving to_yojson]
let party_to_yojson j = party_to_yojson j |> except [|"uri"|]

type event = {
  party: Text.t option;
  datetime: Time.t [@to_yojson yojson_of_time] [@key "Datetime__c"];
  description: Text.t [@key "Description__c"];
  judge: Text.t option [@key "Judge__c"];
} [@@deriving to_yojson]
let event_to_yojson j = event_to_yojson j |> except [|"party"|]

type docket = {
  party: Text.t option;
  date: Date.t option [@to_yojson yojson_of_date_option] [@key "Date__c"];
  code: Text.t option [@key "Code__c"];
  description: Text.t [@key "Description__c"];
  links: string list;
  count: Text.t option [@key "Count__c"];
  amount: Money.t option [@key "Amount__c"];
} [@@deriving to_yojson]
let docket_to_yojson j = docket_to_yojson j |> except [|"party"; "links"|]

type open_case_count = {
  description: Text.t [@key "Description__c"];
} [@@deriving to_yojson] [@@unboxed]

type completed_case_count = {
  party: Text.t option;
  count_as_filed: Text.t [@key "CountAsFiled__c"];
  date_of_offense: Date.t [@to_yojson yojson_of_date] [@key "DateOfOffense__c"];
  disposition: Text.t option [@key "Disposition__c"];
  count_as_disposed: Text.t option [@key "CountAsDisposed__c"];
  violation_of: Text.t option [@key "ViolationOf__c"];
} [@@deriving to_yojson]
let completed_case_count_to_yojson j = completed_case_count_to_yojson j |> except [|"party"|]

type case_data = {
  status: status;
  uri: Uri.t;
  title: Text.t;
  parties: party array;
  is_defendant: bool;
  case_number: Text.t;
  date_filed: Date.t;
  date_closed: Date.t option;
  judge: Text.t;
  arresting_agency: Text.t option;
  events: event array;
  transactions: docket array;
  open_counts: open_case_count array;
  completed_counts: completed_case_count array;
}

type case = {
  status: status;
  uri: Uri.t [@to_yojson yojson_of_uri];
  title: Text.t;
  parties: party array;
  case_number: Text.t;
  date_filed: Date.t [@to_yojson yojson_of_date];
  date_closed: Date.t option [@to_yojson yojson_of_date_option];
  judge: Text.t;
  arresting_agency: Text.t option;
  events: event array;
  transactions: docket array;
  open_counts: open_case_count array [@key "basic_counts"];
  completed_counts: completed_case_count array [@key "extended_counts"];
} [@@deriving to_yojson]
