(* The type of a city and its location *)
type city = float*float*string
			  
(* returns: a concise string representing the location *)
let city_to_string (c : city) : string = 
  let (lat, long, name) = c in 
  let lat_string = "lat=" ^ string_of_float(lat) ^ ", " in 
  let long_string = "long="^string_of_float(long)^", " in 
  let name_string = "name="^name ^ "\n" in 
  lat_string ^ long_string ^ name_string
			       
let parse_line (acc : city list) (line : string) : city list = 
  match Str.split (Str.regexp ",") line with
  | [lat; long; name] -> 
     (float_of_string(lat),float_of_string(long),name)::acc
  | _ -> failwith "Incorrect formatting, please look at input file again"	
		  
let parse_whole_file (fname : string) : city list = 
  let o_file = open_in fname in
  let rec parse_all_lines acc t input = 
    if not t then acc
    else
      try 
	let n_loc = parse_line acc (input_line input) in 
	parse_all_lines (n_loc) true input
      with End_of_file -> parse_all_lines acc false input
  in
  parse_all_lines [] true o_file 		
		  
(* parses a csv file of the format: 
 *   "Latitude", "Longitude" , "Location name"
 * and returns a corresponding list  *)
let parse (fname: string) : city list = 
  begin match (Sys.file_exists fname) with 
	| false -> []
	| true -> parse_whole_file fname
  end	
    







