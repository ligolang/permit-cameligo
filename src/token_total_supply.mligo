#import "ligo-extendable-fa2/lib/multi_asset/fa2.mligo" "FA2"

(* token_id -> total_supply *)
type t = (FA2.Ledger.token_id, nat) big_map

let get_supply (supply : t) (token_id : FA2.Ledger.token_id) =
   match Big_map.find_opt token_id supply with 
    Some (a) -> a 
    | None -> failwith FA2.Errors.undefined_token
    
let create_supply (supply : t) (new_token_id : FA2.Ledger.token_id) (amount_ : nat) = 
    Big_map.add new_token_id amount_ supply

let increase_supply (supply : t) (token_id : FA2.Ledger.token_id) (amount_ : nat) =
    let cur_amount = get_supply supply token_id in
    let new_amount = cur_amount + amount_ in
    Big_map.update token_id (Some(new_amount)) supply

let decrease_supply (supply : t) (token_id : FA2.Ledger.token_id) (amount_ : nat) =
    let cur_amount = get_supply supply token_id in
    let new_supply = abs(cur_amount - amount_) in
    Big_map.update token_id (Some(new_supply)) supply
 