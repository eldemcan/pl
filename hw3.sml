exception NoAnswer

fun only_capitals(string_list: string list)=
  List.filter(fn word => if Char.isUpper(String.sub(word, 0)) then true else false) string_list

fun longest_string1(string_list: string list)=
    List.foldl(fn (current_item, prev_item) => if (String.size current_item > String.size prev_item ) then current_item else prev_item) ""  string_list

fun longest_string2(string_list: string list)=
  List.foldl(fn (current_item, prev_item) => if (String.size current_item >= String.size prev_item ) then current_item else prev_item) ""  string_list

fun longest_string_helper f=
  List.foldl (fn (current_item, prev_item)=> if f(String.size current_item, String.size prev_item) then current_item else prev_item) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized =
  longest_string1 o only_capitals

(* infix |> *)
(* fun x |> f = f(x) *)
(* fun longest_capitalized_alternative(string_list)= *)
(*  string_list |> only_capitals |> longest_string1 *)

val rev_string = implode o rev o explode;

(* (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f item_list=
  case item_list of
    [] => raise NoAnswer
  | x::xs' => case f x of
                NONE => first_answer f xs'
              | SOME x => x
(* all_answers on: ["#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~] *)
fun all_answers f item_list=
  let
     fun helper (f, accumulator, item_list)=
       case item_list of
         [] => SOME []
       | x::xs' => case f x of
                     NONE => NONE
                   | SOME lst => helper (f, lst @ accumulator, xs')
  in
    helper (f, [], item_list)
  end

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
  let
    val r = g f1 f2
  in
    case p of
      Wildcard          => f1 ()
    | Variable x        => f2 x
    | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
    | ConstructorP(_,p) => r p
    | _                 => 0
  end

val count_wildcards = g (fn () => 1) (fn str => 0)

fun  count_wild_and_variable_lengths(ptrn)=
  g (fn () => 1) (fn (item) => String.size item) ptrn

fun count_some_var (s, ptrn) =
  g (fn () => 0) (fn x => if x = s then 1 else 0) ptrn

(* sample input *)
(* ConstructorP ("hi",TupleP[Variable "x",Variable "x"]) *)
fun check_pat(ptrn)=
  let
    fun get_variables(ptrn)=
      case ptrn of
        Variable x => [x]
      | TupleP ps => List.foldl (fn (p,i) => (get_variables p) @ i) [] ps
      | _ => []

    fun check_repeats(char_lst)=
      case char_lst of
        [] => true
      | x::xs' => not (List.exists (fn(item) => item = x ) xs') andalso check_repeats xs'
  in
    check_repeats (get_variables(ptrn))
  end

fun match(value,ptrn)=
  case ptrn of
    Wildcard => SOME[]
  | UnitP => (case value of
                Unit => SOME[])
  | ConstP pattern_value => (case value of
                               Const constant_value => if pattern_value = constant_value then SOME[] else NONE
                               |_ => NONE )
  | Variable variable => SOME[(variable, value)]
  | TupleP tuple_list => (case value of
                            Tuple value_list => if List.length tuple_list = List.length value_list
                                          then
                                            all_answers match (ListPair.zip (value_list, tuple_list))
                                          else NONE
                            | _ => NONE)
  | ConstructorP (str, pt) => (case value of
                                 Constructor (vstr, vval) => if str = vstr
                                                     then
                                                       match (vval, pt)
                                                     else
                                                       NONE
                                | _ => NONE)

fun first_match v plst =
  SOME (first_answer (fn p => match (v, p)) plst)
    handle NoAnswer => NONE

(* tests *)
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = only_capitals ["Computer", "science"] = ["Computer"]
val test3 = only_capitals ["computer", "Science", "programming", "languages"] = ["Science"]
val test4 = longest_string1 ["A","bc","C"] = "bc"
val test5 = longest_string1 ["super", "human", "a", "car"] = "super"
val test6 = longest_string1 [""] = ""
val test7 = longest_string3 ["A","bc","C"] = "bc"
val test8 = longest_string4 ["A","B","C"] = "C"
val test9  = longest_capitalized ["A","bc","C"]           = "A"
val test10 = longest_capitalized ["A"]                    = "A"
val test11 = longest_capitalized ["A", "b", "CDEFG", "b"] = "CDEFG"
val test12 = rev_string "abc" = "cba"
val test13 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test14 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test15 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test16 = count_wildcards Wildcard = 1
val test17 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9c = count_some_var ("x", Variable("x")) = 1
val test10 = check_pat (Variable("x")) = true
val test11 = first_match Unit [UnitP] = SOME []


