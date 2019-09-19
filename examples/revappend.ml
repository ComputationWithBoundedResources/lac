revappend t1 t2 =
  match t1 with
    | nil     -> t2
    | (l,x,r) -> revappend r (nil,x,t2);

test x =
  let xs = (nil, 0, (nil, 1, (nil, 2, nil))) in
  let ys = (nil, 3, (nil, 4, (nil, 5, nil)))
  in
  revappend xs ys;
