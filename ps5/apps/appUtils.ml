
let whitespace =
  Str.regexp "[ \n\t,;.-!?()]+"

let split_words = Str.split whitespace

let (@) xs ys =
  List.rev_append (List.rev xs) ys
