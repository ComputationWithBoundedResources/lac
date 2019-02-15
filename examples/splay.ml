splay a t = match t with
    | nil -> nil
    | {cl, c, cr} ->
        if a == c then {cl, c, cr}
        else if a < c then match cl with
           | nil -> {cl, c, cr}
           | {bl, b, br} ->
               if a == b then {bl, a, {br, c, cr}}
               else if a < b
                    then if bl == nil then {bl, b, {br, c, cr}}
                         else match splay a bl with
                            | {al, a', ar} -> {al, a', {ar, b, {br, c, cr}}}
                    else if br == nil then {bl, b, {br, c, cr}}
                         else match splay a br with
                            | {al, a', ar} -> {{bl, b, al}, a', {ar, c, cr}}
              else match cr with
                 | nil -> {cl, c, cr}
                 | {bl, b, br} ->
                      if a == b then {{cl, c, bl}, a, br}
                      else if a < b
                           then if bl == nil then {{cl, c, bl}, b, br}
                                else match splay a bl with
                                     | {al, a', ar} -> {{cl, c, al}, a', {ar, b, br}}
                           else if br == nil then {{cl, c, bl}, b, br}
                                else match splay a br with
                                     | {al, x, xa} -> {{{cl, c, bbl}, b, al}, x, xa};

insert a t = if t == nil then {nil, a, nil}
             else match splay a t with
                  | {l, a', r} ->
                    if a == a' then {l, a, r}
                    else if a < a' then {l, a, {nil, a', r}}
                         else {{l, a', nil}, a, r};

delete a t = if t == nil then nil
  else match splay a t with
       | {l, a', r} ->
         if a == a' then if l == nil then r
                         else match splay_max l with
                         | {l', m, r'} -> {l', m, r}
         else {l, a', r};

splay_max t = match t with
  | nil -> nil
  | {l, b, r} -> match r with
        | nil -> {l, b, nil}
        | {rl, c, rr} ->
          if rr == nil then {{l, b, rl}, c, nil}
                       else match splay_max rr with
                            | {rrl, x, xa} -> {{{l, b, rl}, c, rrl}, x, xa};
