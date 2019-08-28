id x = x;

const x y = x;

flip f x y = f y x;

not b = if b then false else true;

null t = if t == nil then true else false;

and t =
  match t with
    | nil -> true
    | (l, x, r) ->
      if and l
        then if x then and r
                  else false
        else false;

or t =
  match t with
    | nil -> false
    | (l, x, r) ->
      if or l then true
              else if x then true
                        else or r;

any f t = or (map f t);

all f t = and (map f t);

map f t =
  match t with
    | nil       -> nil
    | (l, x, r) -> (map f l, f x, map f r);

fold f z t =
  match t with
    | nil -> z
    | (l, x, r) -> f (f (fold f z l) x) (fold f z t);

zipWith f t u =
  match t with
    | nil       -> nil
    | (l, x, r) ->
        match u with
          | nil       -> nil
          | (v, y, w) -> let l' = zipWith f l v in
                         let r' = zipWith f r w in
                         (l', f x y, r');
