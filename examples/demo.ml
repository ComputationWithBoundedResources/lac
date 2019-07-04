f l x r = (l, x, r);

g t e1 =
  match t with
    | nil       -> e1
    | (l, x, r) -> (l, x, r);

h x y = x;

i x y = x < y;
