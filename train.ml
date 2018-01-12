module StrMap = Map.Make (String);;(*для занчений*)
module StrSet = Set.Make (String);;(*строки*)

let print_string_third (a,b,c) = 
 Printf.printf "(%s, %s, %s) " a b c ;;
 
let print_smth f l = 
 let rec prnt i = 
  if i < List.length l then (
   f (List.nth l i); prnt (i + 1))
  else Printf.printf "\n" in
prnt 0 ;;  

let print_string_list  l =
 let rec list l i =
  if i < List.length l then (
   Printf.printf "%s " (List.nth l i);
   list l (i+1))
  else Printf.printf "\n"
in list l 0;;

let print_bool a = if a then print_string "true" else print_string "false";;

(*-----------------------------------------------------------*)
type charclass = Paren | SingleQuote | DoubleQuote | HashChar |
	WordChar | SeparatorChar | MiscOpChar | Colons | Comma

type tokenclass = TextBeginT | SeparatorT | KeyWordT | OpT | NumT (*для начала файла; разделители; ключевые слова; операцииж числа*)
 | CharT | StringT (*чары ; строки*)
 | NameT | CapNameT | PunctT | CommentT | TailCommentT (*имена; имена с большой буквы; действия; двойные комментврии; одинарные*)

(*функция, список -> поиск по ключу в таблице*) 
let rec first_successful func = function
 h::t -> (try func h with |_ -> first_successful func t)
 | [] -> raise Not_found ;;

(*возвращает элемент с наибольшим значением из StrMap.t*)
let max_val_entry = 
 StrMap.fold (fun k v (maxk,maxv) -> if v > maxv then (k,v) else (maxk, maxv)) ;;

let char_class = function
 '{' | '}' | '[' | ']' | '(' | ')' -> Paren
 | '\'' -> SingleQuote
 | '"' -> DoubleQuote
 | '#' -> HashChar
 | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> WordChar
 | ' ' | '\t' | '\n' | '\r' -> SeparatorChar
 | ';' | ':' -> Colons
 | ',' -> Comma
 | _ -> MiscOpChar ;;

let isdigit = function '0'..'9' -> true | _ ->false ;;

(*ключевые слова http://caml.inria.fr/pub/docs/manual-ocaml-312/manual044.html *)
let ocaml_keywords = [
	"and"; "as"; "assert"; 
	"begin"; 
	"class"; "constraint"; 
	"do"; "done"; "downto"; 
	"else"; "end"; "exception"; "external"; 
	"false"; "for"; "fun"; "function"; "functor"; 
	"if"; "in"; "include"; "inherit"; "initializer"; 
	"lazy"; "let"; "match"; "method"; "module"; "mutable"; 
	"new"; 
	"object"; "of"; "open"; "or"; 
	"private"; "rec"; 
	"sig"; "struct"; 
	"then"; "to"; "true"; "try"; "type"; 
	"val"; "virtual"; 
	"when"; "while"; "with";
] ;;

(*дерево для всех ключевых слов*)
let keyword_set = List.fold_right StrSet.add ocaml_keywords StrSet.empty ;;

(* читает из потока символы, пока не встретится завершающий символ е (не предваряемый нечетным числом обратных слэшей).*)
let rec read_to f e esc lst =
 let c = try input_char f with _ -> '\000' in
  if c <> '\000' then
   if esc || c <> e then read_to f e ((not esc) && c = '\\') (c::lst)
   else c::lst (*лишний символ*)
  else lst ;;

(*читает из потока символы, пока не встретится завершающая комбинация символов (e1, e2).*)
let rec read_plain_to_2 f e1 e2 lst =
 let c = try input_char f with _ -> '\000' in
  if c <> '\000' then
   if c <> e1  then read_plain_to_2 f e1 e2 (c::lst)
   else if e2 <> '\000' then
	let c2 = try input_char f with _ -> '\000' in
	 if c2<>'\000' then
	  if c2 <> e2 then read_plain_to_2 f e1 e2 (c2::c::lst)
	  else c2::c::lst
	 else c::lst
   else c::lst
  else lst ;;

(*читает из c1 и далее из потока символы, пока выполняется условие.
  последний символ, для которого условие не выпонилось, или (в случае окончания данных) '\000', также входит в возвращеемый (обратный) список.*)
let rec read_to_after f cont c1 lst =
 if cont c1 then let c2 = try input_char f with _ -> '\000' in
  read_to_after f cont c2 (c1::lst)
 else c1::lst (*лишний*);;
	
(*символы, которые могут продолжать число*)
let continues_num = function	
 '0'..'9' | '.' | 'a'..'z' | 'A'..'Z' -> true
 | _ -> false ;;

let revCharList2str l =
 let s = String.make (List.length l) '\000' in
 ignore(List.fold_right (fun x i -> s.[i]<-x; i+1) l 0); s ;;

(*токен, перевернутый список, список токенов ->
  добавляет токен, строковое значение которого представлено обратным сиском символов, в цепочку, обеспечивая чередование значимых лексем
  разделителями, а также определяя принадлежность слова к ключевым словам*)
let addtok tokClass revCharLst revTokLst =
 match revCharLst with
  [] when tokClass <> TextBeginT -> revTokLst
  | _-> 
  let revTokLst' = 
   if (tokClass <> SeparatorT) && (
   match revTokLst with
	(SeparatorT,_)::_ | [] -> false
	| _ -> true )
   then (SeparatorT,"") :: revTokLst 
   else revTokLst in
	let s = revCharList2str revCharLst in
	 let tokClass' = 
	  match tokClass with
	   NameT when  StrSet.mem s keyword_set -> KeyWordT 
	   |_ -> tokClass
	 in
    (tokClass',s)::revTokLst' ;;

let rec read_caml_r f c1 c2 lst =
  let c1 = if c1 <> '\000' then c1 else try input_char f with _-> '\000' in
  let c2 = if c2 <> '\000' then c2 else try input_char f with _-> '\000' in
  if c1 = '\000' then lst
  else
  let cc1 = char_class c1 in match cc1 with
  	WordChar | MiscOpChar | SeparatorChar | Colons ->
  	let cont_func =  match cc1 with
  		WordChar when isdigit c1 -> continues_num
  		| Colons -> (=) c1
  		| _ -> fun x -> char_class x = cc1
  	in  let tok_class =  match cc1 with
  		WordChar when isdigit c1 -> NumT
  		| WordChar -> if Char.lowercase c1 = c1 then
  			NameT (* можно поменять в KeywordT*)
  			else CapNameT
  		| MiscOpChar -> OpT 
		| SeparatorChar -> SeparatorT
  		| _ -> PunctT
  	in let rd = read_to_after f cont_func c2 [c1] in
  		read_caml_r f (List.hd rd) '\000' (addtok tok_class (List.tl rd) lst) 
  	| Paren | Comma -> if c1 = '(' && c2 ='*' then (*для комментариев*)
  		read_caml_r f '\000' '\000' (addtok CommentT ( read_plain_to_2 f '*' ')' [c2; c1]) lst) else
  		read_caml_r f c2 '\000' (addtok PunctT [c1] lst)
  	| HashChar -> read_caml_r f '\000' '\000' (addtok TailCommentT (
  		if c2<>'\n' then read_plain_to_2 f '\n' '\000' [c2;c1] else [c1;c2]) lst)
  	| SingleQuote -> 
  		if c2 = '\\' then read_caml_r f '\000' '\000' (addtok CharT (read_to f '\'' true [c2;c1]) lst)
  		else if c2 = '\'' then read_caml_r f c2 '\000' (addtok PunctT [c1] lst) 
  		else let c3 = try input_char f with _-> '\000' in 
  		if c3 = '\'' (*для чаров*)
  		  then  read_caml_r f '\000' '\000' (addtok CharT [c3;c2;c1] lst)
  		  else read_caml_r f c2 c3 (addtok PunctT [c1] lst) 
	| DoubleQuote -> if c2 <> '"' then read_caml_r f '\000' '\000' (addtok StringT (read_to f '\"' false [c2;c1]) lst)
							else read_caml_r f '\000' '\000' (addtok StringT [c2;c1] lst) ;;
							
(*читает файл на языке OCaml, создавая цепочку токенов, в котором нечетные позиции занимают значащие лексемы, а четные - разделители *)
let read_caml f = 
 addtok TextBeginT [] (List.rev (read_caml_r f '\000' '\000' [])) ;;

let separators2key s =
 if s = "" then "empty" 
 else String.map (function '\t'->'T' | ' '->'S' | '\r' -> 'R' | '\n' -> 'N' | _ -> 'X') s ;;
 
let key2separators k =
 if k = "empty" then "" 
 else String.map (function 'T'->'\t' | 'R' -> '\r' | 'N' -> '\n' | _ -> ' ') k ;;

let token2key (cl,s) =
 match cl with
  TextBeginT -> "^txt"
  | SeparatorT -> separators2key s
  | NumT -> "num"
  | KeyWordT -> s
  | NameT -> "name"
  | CapNameT -> "capname"
  | TailCommentT -> "(*..*)"
  | CommentT -> "#..."
  | CharT -> "cliteral"
  | StringT -> "sliteral"
  | PunctT -> s
  | OpT -> s ;;
  
let rec fold_chain_sep proc_fun acc = function
 t1::(SeparatorT,s)::t2::tail -> fold_chain_sep proc_fun (proc_fun t1 s t2 acc) (t2::tail)
 | [_] | [_;_] -> acc
 | _ -> raise (Invalid_argument "Badly formed chain: needs to be separator interlaced") ;;

(*заменяет разделители в цепочке на возвращаемые map_fun *)
let map_separators map_fun chain = 
 let rec r out_lst = function
  t1::(SeparatorT,s)::t2::tail -> r ((SeparatorT,map_fun t1 s t2)::t1::out_lst) (t2::tail)
  | [t] -> t::out_lst 
  | [t1;t2] -> t2::t2::out_lst
  | _ -> raise (Invalid_argument "Badly formed chain: needs to be separator interlaced")
 in List.rev(r [] chain) ;;
	
(*подсчет совпадений по ключам*)
let inc_table key1 key2	table = 
 let e1 = try StrMap.find key1 table with _ -> StrMap.empty in
  let n2 = try StrMap.find key2 e1 with _ -> 0 in 
  StrMap.add key1 (StrMap.add key2 (n2 + 1) e1)  table ;;
		
(*формирует конкретные и обобщенные ключи для пары последовательных лексем*)
let pair_keys k1 k2 = 
[String.concat "_" [k1;k2]; String.concat "_" [k1;"any"];
 String.concat "_" ["any";k2]; String.concat "_" ["any";"any"] ] ;;

(*добавить в таблицу тройку лексема-разделитель-лексема*)
let inc_table_with_toks tok1 sep tok2 table =
 let k1 = token2key tok1 and k2 = token2key tok2 and ks = separators2key sep in
 List.fold_right (fun kp t -> inc_table kp ks t) (pair_keys k1 k2) table ;;

(*делает дерево с упорядоченными по убыванию разделителями*) 
let best_separator best_map tok1 _ tok2 =
 let k1 = token2key tok1 and k2 = token2key tok2 in
  key2separators (first_successful (fun k -> StrMap.find k best_map) (pair_keys k1 k2)) ;;

(*чтение цепочки токенов программы - эталона*)
let trf = open_in "train.ml" in
let tr_chain = read_caml trf in
close_in trf;

(*подсчет числа разделителей для всех пар лексем*)
let scores = fold_chain_sep inc_table_with_toks StrMap.empty tr_chain in

(*выбор наилучшего разделителя для каждой пары лексем*)
let most_popular_seps = 
 StrMap.fold (fun k e a -> StrMap.add k (let sk,_ = max_val_entry e ("",-1) in sk) a) scores StrMap.empty  in 
(*чтение цепочки токенов преобразуемой программы*)
let tsf = open_in "test.ml" in
let ts_chain = read_caml tsf in
close_in tsf;
(*вывод таблицы статистики по разделителям пар лексем*)
let otf = open_out "ml_sep_count.tab" in
StrMap.iter (
	fun k1 e -> StrMap.iter (
	  Printf.fprintf otf "%s %s %d\n" k1) e
	 ) scores;
close_out otf;
(*вывод таблицы наилучших разделителей пар лексем *)
let otb = open_out "ml_sep_best.tab" in
StrMap.iter (Printf.fprintf otb "%s -> %s\n") most_popular_seps;
close_out otb;
(*замена разделителей лексем *)
let out_chain = map_separators (best_separator most_popular_seps) ts_chain in
(*вывод отформатированного файла*)
let ofm = open_out "formatted.out.ml" in
output_string ofm (List.fold_right (fun (_,ts) a-> ts ^ a) out_chain "");
close_out ofm;;

	