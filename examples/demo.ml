f l x r = (l, x, r);

g t e1 =
  match t with
    | nil       -> e1
    | (l, x, r) -> (l, x, r);

h x y = x;

i x y = x < y;

j x =
  if x
    then nil
    else nil;

k x y z =
  let p = x < y
  in
  if p
    then nil
    else
      let l = nil in
      let r = nil
      in
      (l, z, r);

(* test for (w : var) rule *)
l x y t a =
  let p = x < y
  in
  if p
    then nil
    else
      let n = nil
      in
      (t, a, n);
