iter t =
  match t with
    | nil       -> nil
    | (l, x, r) -> (iter l, x, iter r);

append t u =
  match t with
    | nil       -> u
    | (l, x, r) -> (l, x, append r u);
