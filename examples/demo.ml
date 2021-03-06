f : Tree(Nat) -> Nat -> Tree(Nat) -> Tree(Nat);
f l x r = (l, x, r);

g t e1 =
  match t with
    | nil       -> e1
    | (l, x, r) -> (l, x, r);

h : Tree (Nat) -> Tree (Nat) -> Tree(Nat);
h x y = x;

i : Nat -> Nat -> Bool;
i x y = x < y;

j : Bool -> Tree (Nat);
j x =
  if x
    then nil
    else nil;

k : Nat -> Nat -> Nat -> Tree (Nat);
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
l1 : Nat -> Nat -> Tree (Nat) -> Nat -> Tree (Nat);
l1 x y t a =
  let p = x < y
  in
  if p
    then nil
    else
      let n = nil
      in
      (t, a, n);

m t x y =
  let s = (t, y, t)
  in
  (t, x, s);

n1 f x y z =
  let l = f x in
  let r = (l, y, z)
  in
  (l, y, z);
