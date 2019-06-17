inorder t q =
  match t with
    | nil -> q
    | (l, x, r) -> (nil, x, inorder l (inorder r q));

preorder t q =
  match t with
    | nil -> q
    | (l, x, r) -> (nil, x, preorder l (preorder r q));

postorder t q =
  match t with
    | nil -> q
    | (l, x, r) -> postorder l (postorder r (nil, x, q));
