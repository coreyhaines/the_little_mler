Control.Print.printDepth := 20;
datatype seasoning =
  Salt
 | Pepper;

datatype num =
  Zero
 | One_more_than of num;

One_more_than(
  One_more_than(
    Zero));

datatype 'a open_faced_sandwich =
  Bread of 'a
 | Slice of 'a open_faced_sandwich;

Bread(One_more_than(Zero));

datatype shish_kebab =
  Skewer
 | Onion of shish_kebab
 | Lamb of shish_kebab
 | Tomato of shish_kebab;

fun only_onions(Skewer)
    = true
  | only_onions(Onion(x))
    = only_onions(x)
  | only_onions(Lamb(x))
    = false
  | only_onions(Tomato(x))
    = false;
only_onions: shish_kebab -> bool;

fun is_vegetarian(Skewer)
    = true
  | is_vegetarian(Onion(x))
    = is_vegetarian(x)
  | is_vegetarian(Lamb(x))
    = false
  | is_vegetarian(Tomato(x))
    = is_vegetarian(x);
is_vegetarian: shish_kebab -> bool;

datatype 'a shish =
  Bottom of 'a
  | Onion of 'a shish
  | Lamb of 'a shish
  | Tomato of 'a shish;

datatype rod =
  Dagger
  | Fork
  | Sword;

datatype plate =
  Gold_plate
  | Silver_plate
  | Brass_plate;

fun is_veggie(Bottom(x))
  = true
  | is_veggie(Onion(x))
    = is_veggie(x)
  | is_veggie(Lamb(x))
    = false
  | is_veggie(Tomato(x))
    = is_veggie(x);
is_veggie: 'a shish -> bool;

fun what_bottom(Bottom(x))
  = x
  | what_bottom(Onion(x))
    = what_bottom(x)
  | what_bottom(Lamb(x))
    = what_bottom(x)
  | what_bottom(Tomato(x))
    = what_bottom(x);
what_bottom: 'a shish -> 'a;

datatype pizza =
  Crust
  | Cheese of pizza
  | Onion of pizza
  | Anchovy of pizza
  | Sausage of pizza;
val my_pizza = Anchovy(Onion(Anchovy(Anchovy(Cheese(Crust)))));

fun remove_anchovy(Crust)
  = Crust
  | remove_anchovy(Cheese(x))
    = Cheese(remove_anchovy(x))
  | remove_anchovy(Onion(x))
    = Onion(remove_anchovy(x))
  | remove_anchovy(Anchovy(x))
    = remove_anchovy(x)
  | remove_anchovy(Sausage(x))
    = Sausage(remove_anchovy(x));
remove_anchovy: pizza -> pizza;

fun top_anchovy_with_cheese(Crust)
  = Crust
  | top_anchovy_with_cheese(Cheese(x))
    = Cheese(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Onion(x))
    = Onion(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Anchovy(x))
    = Cheese(Anchovy(top_anchovy_with_cheese(x)))
  | top_anchovy_with_cheese(Sausage(x))
    = Sausage(top_anchovy_with_cheese(x));
remove_anchovy: pizza -> pizza;

fun subst_anchovy_with_cheese(x)
  = remove_anchovy(top_anchovy_with_cheese(x));


datatype meza =
  Shrimp
  | Calamari
  | Escargots
  | Hummus;

datatype main =
  Steak
  | Ravioli
  | Chicken
  | Eggplant;

datatype salad =
  Green
  | Cucumber
  | Greek;

datatype dessert =
  Sundae
  | Mousse
  | Torte;

fun add_a_steak(x:meza):(meza*main)
  = (x,Steak);

fun eq_main(Steak,Steak)
    = true
  | eq_main(Ravioli,Ravioli)
    = true
  | eq_main(Chicken,Chicken)
    = true
  | eq_main(Eggplant,Eggplant)
    = true
  | eq_main(a_main,another_main)
    = false;

fun has_steak(a_meza:meza,Steak,a_dessert:dessert)
    = true
  | has_steak(a_meza:meza,a_main,a_dessert:dessert)
    = false;

datatype 'a pizza =
  Bottom
  | Topping of ('a * ('a pizza));

datatype fish =
  Anchovy
  | Lox
  | Tuna;

val pizza = Topping(Tuna,Topping(Anchovy,Topping(Tuna,Bottom)));

fun remove_anchovy(Bottom)
    = Bottom
  | remove_anchovy(Topping(Anchovy,p))
    = remove_anchovy(p)
  | remove_anchovy(Topping(t,p))
    = Topping(t, remove_anchovy(p));

remove_anchovy(pizza);

fun remove_tuna(Bottom)
    = Bottom
  | remove_tuna(Topping(Tuna,p))
    = remove_tuna(p)
  | remove_tuna(Topping(t,p))
    = Topping(t, remove_tuna(p));

remove_tuna(pizza);

fun eq_fish(Anchovy,Anchovy) = true
  | eq_fish(Lox,Lox) = true
  | eq_fish(Tuna,Tuna) = true
  | eq_fish(fish1,fish2) = false;

fun remove_fish(f, Bottom)
    = Bottom
  | remove_fish(f, Topping(g,p))
    = if eq_fish(f,g)
      then remove_fish(f, p)
      else Topping(g,remove_fish(f,p));

remove_fish(Anchovy,pizza);
remove_fish(Tuna,pizza);

fun subst_fish(f,r,Bottom) = Bottom
  | subst_fish(f,r,Topping(t,p))
    = if eq_fish(t,r)
      then Topping(f,subst_fish(f,r,p))
      else Topping(t,subst_fish(f,r,p));

subst_fish: (fish*fish*(fish pizza)) -> (fish pizza);

subst_fish(Anchovy,Tuna,pizza);

datatype fruit =
  Peach 
  | Apple
  | Pear
  | Lemon
  | Fig

datatype tree =
  Bud
  | Flat of fruit * tree
  | Split of tree * tree;

fun flat_only(Bud) = true
  | flat_only(Flat(f,t)) = flat_only(t)
  | flat_only(Split(s,t)) = false;
flat_only: tree -> bool;

fun split_only(Bud) = true
  | split_only(Flat(f,t)) = false
  | split_only(Split(s,t)) 
    = split_only(s) andalso split_only(t);
split_only: tree -> bool;

fun contains_fruit(Bud) = false
  | contains_fruit(Flat(f,t)) = contains_fruit(t)
  | contains_fruit(Split(s,t)) 
    = contains_fruit(s) orelse contains_fruit(t);
contains_fruit: tree -> bool;

fun contains_fruit(x) 
  = if split_only(x)
    then false
    else true;

fun contains_fruit(x) = not(split_only(x));

fun less_than(n:int,m:int) = (n < m);

fun larger_of(n,m)
  = if less_than(n,m)
    then m
    else n;
larger_of: (int * int) -> int;

fun height(Bud) = 0
  | height(Flat(f,t)) = 1 + height(t)
  | height(Split(s,t)) = 1 + larger_of(height(s),height(t));
height: tree -> int;

height(Bud);
height(Flat(Apple,Split(Bud,Flat(Peach,Bud))));

fun eq_fruit(Peach,Peach) = true
  | eq_fruit(Apple,Apple) = true
  | eq_fruit(Pear,Pear) = true
  | eq_fruit(Lemon,Lemon) = true
  | eq_fruit(Fig,Fig) = true
  | eq_fruit(fruit1,fruit2) = false;

fun subst_in_tree(m,n,Bud) = Bud
  | subst_in_tree(m,n,Flat(f,t))
    = if eq_fruit(m,f)
      then Flat(n,subst_in_tree(m,n,t))
      else Flat(f,subst_in_tree(m,n,t))
  | subst_in_tree(m,n,Split(s,t)) 
    = Split(subst_in_tree(m,n,s),subst_in_tree(m,n,t));

val tree = Split(
              Flat(Apple,Bud),
              Split(
                Flat(Peach,
                  Flat(Apple,Bud)),
                Flat(Apple,Bud)));
height(tree);
tree;
subst_in_tree(Apple,Fig,tree);


fun occurs(a, Bud) = 0
  | occurs(a, Flat(f,t))
    = if eq_fruit(a,f)
      then 1 + occurs(a,t)
      else occurs(a,t)
  | occurs(a, Split(s,t)) = occurs(a,s) + occurs(a,t);
occurs: (fruit*tree) -> int;

occurs(Lemon,tree);
occurs(Apple,tree);
occurs(Peach,tree);
occurs(Fig,subst_in_tree(Apple,Fig,tree));


datatype
  'a slist =
    Empty
   |Scons of (('a sexp) * ('a slist))
and
  'a sexp =
    An_atom of 'a
   |A_slist of ('a slist);
