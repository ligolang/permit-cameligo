#import "ligo-extendable-fa2/lib/multi_asset/fa2.mligo" "FA2"
#import "./constants.mligo" "Constants"
#import "./storage.mligo" "Storage"
#import "./extension.mligo" "Extension"
#import "./errors.mligo" "Errors"
#import "./token_total_supply.mligo" "TokenTotalSupply"

type token_total_supply = TokenTotalSupply.t
type gen_storage = Storage.t
type storage = token_total_supply gen_storage
type result = operation list * storage
type gen_extension = Extension.t
type extension = token_total_supply gen_extension

type mint_or_burn = [@layout:comb] {
   owner    : address;
   token_id : nat;
   amount_  : nat;
}

type permit_params = (key * (signature * bytes))
type expiry_params = (address * (nat * (bytes option)))

[@entry] let create_token (metadata,owner,amount : FA2.TokenMetadata.data * address * nat) (s : storage) =
    let () = Extension.assert_admin s.extension in
    let md = Storage.add_new_token s.token_metadata metadata.token_id metadata in
    let s = Storage.set_token_metadata s md in
    let ledger = FA2.Ledger.increase_token_amount_for_user s.ledger owner metadata.token_id amount in
    let s = FA2.Storage.set_ledger s ledger in
    let supply = TokenTotalSupply.create_supply s.extension.extension metadata.token_id amount in
    Constants.no_operation, {
      s with extension = Extension.set_extension s.extension supply
    }

[@entry] let mint_token (lst : mint_or_burn list) (s : storage) =
   let () = Extension.assert_admin s.extension in
   let process_one ((ledger,supply), {owner;token_id;amount_} : (FA2.Ledger.t * TokenTotalSupply.t) * mint_or_burn) =
      let () = FA2.Storage.assert_token_exist  s token_id in
      FA2.Ledger.increase_token_amount_for_user ledger owner token_id amount_,
      TokenTotalSupply.increase_supply supply token_id amount_
   in
   let (ledger, supply) = List.fold_left process_one (s.ledger, s.extension.extension) lst in
   let s = FA2.Storage.set_ledger s ledger in
   Constants.no_operation, {
      s with extension = Extension.set_extension s.extension supply
    }

[@entry] let burn_token (lst : mint_or_burn list) (s : storage) =
   let () = Extension.assert_admin s.extension in
   let process_one ((ledger,supply), {owner;token_id;amount_} : (FA2.Ledger.t * TokenTotalSupply.t) * mint_or_burn) =
      FA2.Ledger.decrease_token_amount_for_user ledger owner token_id amount_,
      TokenTotalSupply.decrease_supply supply token_id amount_
   in
   let (ledger, supply) = List.fold_left process_one (s.ledger, s.extension.extension) lst in
   let s = FA2.Storage.set_ledger s ledger in
   Constants.no_operation,{
      s with extension = Extension.set_extension s.extension supply
    }

(* TZIP-17 *)
[@entry] let permit (permits : (permit_params list)) (s : storage) =
    let process_permit (ext, permit : extension * permit_params) =
        let (pub_key, (sig_, hash_)) = permit in
        let packed = Bytes.pack (((Tezos.get_chain_id()), Tezos.get_self_address()), (ext.counter, hash_)) in
        if Crypto.check pub_key sig_ packed
        then
            let sender_ = Tezos.address (Tezos.implicit_account (Crypto.hash_key pub_key)) in
            let permit_key = sender_, hash_ in
            match Big_map.find_opt permit_key ext.permits with
            | None -> Extension.add_permit ext permit_key
            | Some submission_timestamp ->
                let () = Extension._check_not_expired s.extension submission_timestamp permit_key in
                Extension.update_permit ext permit_key
        else ([%Michelson ({| { FAILWITH } |} : string * bytes -> extension)]) (Errors.missigned, packed)
    in
    let extension = List.fold_left process_permit s.extension permits in
    Constants.no_operation, { s with extension = extension }

(* TZIP-17 *)
[@entry] let setExpiry (p : expiry_params) (s : storage) =
    let (user_address, (seconds, permit_hash_opt)) = p in
    let new_storage =
        if seconds > s.extension.max_expiry
        then (failwith Errors.max_seconds_exceeded : storage)
        else if Tezos.get_sender() <> user_address
        then (failwith Errors.forbidden_expiry_update : storage)
        else
                match permit_hash_opt with
                | None ->
                    {
                        s with extension.user_expiries = Big_map.add
                            user_address
                            (Some seconds)
                            s.extension.user_expiries
                    }
                | Some permit_hash ->
                    {
                        s with extension.permit_expiries = Big_map.add
                            (user_address, permit_hash)
                            (Some seconds)
                            s.extension.permit_expiries
                    }
    in Constants.no_operation, new_storage

(* TZIP-17 implementation of TZIP-12 Transfer *)
[@entry] let transfer (transfer:FA2.transfer) (s: storage) =
    let make_transfer (acc, transfer_from : (FA2.Ledger.t * extension) * FA2.transfer_from) =
        let (ledger, ext) = acc in
        let transfer_from_hash = Crypto.blake2b (Bytes.pack transfer_from) in
        let permit_key : Extension.permit_key = (transfer_from.from_, transfer_from_hash) in
        let (is_transfer_authorized, ext) = Extension.transfer_presigned ext permit_key in
        let {from_; txs} = transfer_from in
        let ledger = List.fold
          (fun (ledger, dst : FA2.Ledger.t * FA2.atomic_trans) ->
            let {token_id; amount; to_} = dst in
            let () = FA2.Storage.assert_token_exist s token_id in
            let () = if not is_transfer_authorized then
                FA2.Operators.assert_authorisation s.operators from_ token_id
            in
            let ledger = FA2.Ledger.decrease_token_amount_for_user ledger from_ token_id amount in
            let ledger = FA2.Ledger.increase_token_amount_for_user ledger to_ token_id amount in
            ledger
          ) txs ledger in
          (ledger, ext)
        in
    let (new_ledger, new_ext) = List.fold make_transfer transfer (s.ledger, s.extension)
    in Constants.no_operation, { s with ledger = new_ledger; extension = new_ext }

[@entry] let set_admin (addr: address) (s: storage) =
    Constants.no_operation, { s with extension = Extension.set_admin s.extension addr }

[@entry] let balance_of (p : FA2.balance_of) (s : storage) =
    FA2.balance_of p s

[@entry] let update_operators (p : FA2.update_operators) (s : storage) =
    FA2.update_ops p s

(*
    Off-chain views required by TZIP-17

    Command to run to get the micheline expressions to put in the metadata:

    ligo compile expression cameligo '_get_counter' \
        --init-file src/main.mligo \
        --project-root . \
        --michelson-format json
*)
let _get_default_expiry ((_,s):(unit * storage)) : nat =
    s.extension.default_expiry

let _get_counter ((_,s):(unit * storage)) : nat =
    s.extension.counter

