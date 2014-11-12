open Async.Std

let fork d f1 f2 =
  (ignore (d >>= f1));
  (ignore (d >>= f2));
  ()

let deferred_map (l : 'a list) (f : 'a -> 'b Deferred.t) =
  let helper d acc =
    d >>= (fun x ->
      acc >>= (fun a ->
        return (x::a)
      )
    ) in
  List.fold_right helper (List.map f l) (return [])


