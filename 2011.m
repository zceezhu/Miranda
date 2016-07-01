shit1 v shit i = shit (v:i)

rn x = x
rh g = hd (g [])
f [] y = y
f (x:xs) y = f xs (shit1 x y)
g [] y = y
g (x:xs) y = g xs (x:y)

pos ::= Zero | Succ pos

plus :: pos->pos->pos
plus Zero x = x
plus x Zero = x
plus x (Succ y) = plus (Succ x) y

times :: pos->pos->pos
times Zero x = x
times (Succ x) y = plus y (times x y)

one f x = f x
two f x = f (f x)
three f x = f (f (f x))
four f x = f (f (f (f x)))

fplus first second f x = first f (second f x)

mylist * ::= Empty | Cons*(mylist *)
sawp f x y = f y x
cancel x y = x
fhd anylist = anylist cancel

shuffle [] = []
shuffle list = [list!(length div 2)]++ [list!0]++ shuffle ((list--[list!0])--[list!(length div 2)]) 
	       where length = # list

rcons a f b=f (a : b)
rnil = id
rev items = foldr rcons rnil items []
