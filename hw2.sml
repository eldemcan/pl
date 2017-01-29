fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(item:string, string_list: string list)=
  case string_list of
    [] => NONE
  | first::rest =>
      if (same_string(first, item)) then
        SOME(rest)
      else
          (case all_except_option(item, rest) of
             SOME lst => SOME(first :: lst)
          |  NONE => NONE)

fun get_substitutions1(substitutions: (string list) list, word:string)=
  case substitutions of
    [[]]  => []
  | ([]::xs') => get_substitutions1(xs',word)
  | (x::xs') =>(case all_except_option(word, x) of
                  NONE => get_substitutions1(xs', word)
                | SOME [] => get_substitutions1(xs', word)
                | SOME lst => lst @ get_substitutions1(xs', word))
  | _ => []

fun get_substitutions2(substitutions: (string list) list, word: string)=
  let
    fun helper(substitutions: (string list) list, word: string, accumulator: string list)=
        case substitutions of
          [[]]  => accumulator
        | ([]::xs') => helper(xs', word, accumulator)
        | (x::xs') =>(case all_except_option(word, x) of
                  NONE => helper(xs', word, accumulator)
                | SOME [] => helper(xs', word, accumulator)
                | SOME lst => helper(xs', word, (accumulator @ lst )))
        | []  => accumulator
  in
    helper(substitutions,word,[])
  end

fun similar_names(substitutions, full_name)=
  let
    val {first=substitue_name,middle=middle_name,last=last_name} = full_name

    fun helper(alternatives, accumulator)=
      case alternatives of
        [] => accumulator
      | x::xs' => helper(xs', { first=x, middle=middle_name, last=last_name }::accumulator)
  in
    rev(helper(get_substitutions1(substitutions,substitue_name),full_name::[]))
end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color (suit, rank) =
    case suit of
        Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red


fun card_value(suit, rank)=
    case rank of
      Ace => 11
    | Jack => 10
    | Queen => 10
    | King => 10
    | Num(x) => x

fun remove_card(card_list, card, ex)=
    case card_list of
      [] => []
    | x::[] => if x = card then
                  []
               else
                 raise ex
    | x::xs' => if  x = card
                then
                    xs'
                else
                  x::remove_card(xs', card, ex)


fun all_same_color (cards) =
    case cards of
        [] => true
      | _::[] => true
      | head::(neck::tail) => card_color(head) = card_color(neck) andalso
                              all_same_color(neck::tail)

fun sum_cards(card_list: card list)=
  let
    fun helper(cards, accumulator)=
      case cards of
        [] => accumulator
      |  x :: xs' => helper(xs', accumulator+card_value(x))
  in
    helper(card_list, 0)
  end

fun score(held_cards: card list, goal: int)=
  let
    val total_cards = sum_cards(held_cards);
    val is_same_color = all_same_color(held_cards)
    fun calculate_preliminary_score()=
      if total_cards > goal
      then
        3 * (total_cards - goal)
      else
        (goal - total_cards)
  in
    if is_same_color
    then
      calculate_preliminary_score() div 2
    else
      calculate_preliminary_score()
  end

fun officiate(card_list, move_list, goal)=
  let
    fun helper(card_list, held_cards, move_list)=
      case move_list of
         [] => score(held_cards, goal)
      |  (Draw) :: ys' => (case card_list of
                          [] => helper([], held_cards, [])
                        | x :: xs' => if ( sum_cards((x::held_cards)) > goal )
                                         then
                                           helper(xs', x::held_cards, [])
                                         else
                                           helper(xs', (x :: held_cards), ys'))
      | (Discard card) :: xs' => helper(card_list, remove_card(held_cards, card, IllegalMove), xs')
  in
   helper(card_list, [], move_list)
  end


(* tests *)
val test1 = all_except_option ("string", ["string"]) = SOME []
val test2 = all_except_option ("orange", ["apple","orange","banana"])= SOME(["apple","banana"])
val test3 = all_except_option ("Jeff",["Fred","Fredrick"]) = NONE
val test5 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test6 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test7 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test8 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test9 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =[
         {first="Fred", last="Smith", middle="W"},
         {first="Fredrick", last="Smith", middle="W"},
         {first="Freddie", last="Smith", middle="W"},
         {first="F", last="Smith", middle="W"}
        ];
val test10 = card_color (Clubs, Num 2) = Black
val test11 = card_value (Clubs, Num 2) = 2
val test12 = remove_card([(Hearts, Ace),(Diamonds, King),(Diamonds, King)],(Diamonds, King), IllegalMove);
val test13 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test14 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test15 = all_same_color [(Clubs, Ace), (Hearts, Ace)] = false
val test16 = all_same_color [(Hearts, Ace)] = true
val test17 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test18 =sum_cards [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)] = 44
val test19 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test20 = score ([(Hearts, Num 2),(Clubs, Num 4)], 15)= 9;
val test21 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15);
val test22 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42);
