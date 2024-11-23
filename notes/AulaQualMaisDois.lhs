
continuiando dos Either e Functors e etc.. 

> class Functor f where 
>    fmap :: (a -> b) -> (f a -> f b)

Pair type no mundo dos tipos:
             a :: *   b :: *
           -------------------
	        (a, b) :: *  

Pair type no mundo dos valores:

             x :: a   y:: b  
          --------------------
	    (x, y) :: (a, b)  

Se roubassem nosso "(,)":
> data Pair a b where
>     Pair :: a -> b -> Pair a b


sintaxe bunitinha pra structs
> data Customer = Customer { name  :: String
>                           ,email :: String 
>                           , id   :: Nat 
>                           , date :: Date
>                           }

<diagrama comutativo do produto>

<calculo do quadrado>

 v . f = i . u 
 j . v = w . g 

(w . g) . f = (j . v) . f,
            = j . (v . f)
	    = j . (i . u).
 
<umas equacoes do diagr. prod>

h x = (f x, g x)
f   = fst . h 
g   = snd . h

ideia minha de map geral:
> -- ? gMap :: Functor f => [a -> b] -> (f a -> f b)
>-- lgMap :: [a -> b] -> ([a] -> [b])

<uns exemplos de produtos onde temos mdc e intersec como uns
  exemplos bem especiais>



