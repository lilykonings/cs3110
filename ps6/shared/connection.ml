type connection = (Unix.sockaddr * Unix.file_descr * in_channel * out_channel * int) ref

let init addr retries =
  let conn = ref None in
  let retried = ref 0 in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    while !conn = None && !retried <= retries do
      try
        Unix.connect socket addr;
        conn := Some(ref (addr, socket,
                          Unix.in_channel_of_descr socket,
                          Unix.out_channel_of_descr socket, retries))
      with _ -> retried := !retried + 1
    done;
    (match !conn with
       Some(c) -> let (_, _, inp, out, _) = !c in
                    set_binary_mode_out out true;
                    set_binary_mode_in inp true;
     | _ -> ());
    !conn

let server addr socket =
  let a = Unix.in_channel_of_descr socket in
  (*let _ = Unix.set_nonblock socket in *)
  let s = Unix.out_channel_of_descr socket in
  ref (addr, socket, a,
       s, 0)

let close c =
  let (_, socket, inp, out, _) = !c in
    try Unix.close socket with _ -> ();
    close_in_noerr inp;
    close_out_noerr out

let input c =
  let (_, _, inp, _, _) = !c in
  let value = ref None in
    (try value := Some(input_value inp) with _ -> close c);
    !value

let output c v =
  let (addr, _, _, _, retries) = !c in
  let numRetries = ref 0 in
  let success = ref false in ();
    while not !success && !numRetries <= retries do
      let (_, _, _, out, _) = !c in
        try output_value out v; flush out; success := true
        with _ -> numRetries := !numRetries + 1;
                  close c;
                  match init addr retries with
                    Some(c') -> c := !c'
                  | None -> (* Opening new connection failed, will raise an
                             * exception on next loop iteration *) ()
    done;
    if not !success then close c else ();
    !success

let output_string c v =
  let (addr, _, _, _, retries) = !c in
  let numRetries = ref 0 in
  let success = ref false in
    while not !success && !numRetries <= retries do
      let (_, _, _, out, _) = !c in
        try output_string out v; flush out; success := true
        with _ -> numRetries := !numRetries + 1;
                  close c;
                  match init addr retries with
                    Some(c') -> c := !c'
                  | None -> (* Opening new connection failed, will raise an
                             * exception on next loop iteration *) ()
    done;
    if not !success then close c else ();
    !success

let address c = let (addr, _, _, _, _) = !c in addr
