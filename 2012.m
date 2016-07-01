
rcons a f b=f (a : b)
rnil = id1
id1 x = x
rev items = foldr rcons rnil items []

example1 a b f x = ( (a f) . (b f)) x

example2 [] j acc = acc
example2 f [] acc = acc
example2 (x:xs) (y:ys) acc = example2 xs ys (x y acc)
example3 [] y z = 42.0 + z
example3 (x:xs) y z = example3 xs y ((x . y) z)

main1 =  (foldr ((:) . length . (swap (:) [])) []) 
main = (foldr (+) 0).main1
length [] = 0
length (x:xs) = 1 + length xs
swap f x y = f y x

foldiftrue p f def [] = def
foldiftrue p f def (x:xs)
= f x (foldiftrue p f def xs), if (p x)
= foldiftrue p f def xs, otherwise

amyfold p f def = (foldr f def).(filter p) 

shuffle (front:rest) = filter (modis 3 1) (front:rest)
modis a b c = True, if (c mod a=b)  
	    = False, otherwise 


||loop x = loop (x+1)

permutations [] = [[]]
permutations anylist = [front:rest | front <- anylist; rest <-permutations(anylist--[front])]


printdots n = ['.' | a<- [1..n]]


myfilter :: (* -> bool) -> [*] -> [*]
myfilter pred anylist = [x| x<- anylist; pred x]

cards  ::= Clubs num | Diamonds num | Hearts num | Spades num 
cards2 * ::= Clubs2 * | Diamonds2 * | Hearts2 * | Spades2 *

mycard == cards
shit :: cards2 num -> num
shit (Clubs2 3) = 4

interleave 0 list = list
interleave x list = interleave (x-1) (myinterleave list)

myinterleave  [] = []
myinterleave   anylist = [anylist !(length div 2)]++[anylist ! 0]++myinterleave ((anylist--[anylist !(length div 2)])--[anylist !0])
		         where length = # anylist
f a b = check ( a*b)

check x = check(adddigit x), if adddigit x > 9
        = adddigit x, otherwise

adddigit x = (x mod 10) + adddigit (x div 10), if (x div 10) >=1
	   = x, otherwise
