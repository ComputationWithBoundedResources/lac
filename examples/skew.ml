merge h1 h2 =
  match h1 with
    | nil -> h2
    | (l1, a1, r1) ->
      match h2 with
        | nil -> h1
        | (l2, a2, r2) ->
          if a1 <= a2
            then (merge h2 r1, a1, l1)
            else (merge h1 r2, a2, l2);

empty = nil;

insert a h = merge (nil, a, nil) h;

del_min h =
  match h with
    | nil -> nil
    | (l, m, r) -> merge l r;
