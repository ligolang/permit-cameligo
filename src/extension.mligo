#import "./errors.mligo" "Errors"
#import "./token_total_supply.mligo" "TokenTotalSupply"

type seconds = nat
type permit_key = (address * bytes)
type permits = (permit_key, timestamp) big_map
type user_expiries = (address, seconds option) big_map
type permit_expiries = (permit_key, seconds option) big_map

type t = {
    admin: address;
    counter: nat;
    default_expiry: seconds;
    max_expiry: seconds;
    permits: permits;
    user_expiries: user_expiries;
    permit_expiries: permit_expiries;
    token_total_supply : TokenTotalSupply.t;
}

let get_user_defined_expiry (from_: address) (ext: t) : seconds option =
    match Big_map.find_opt from_ ext.user_expiries with
    | None -> (Some ext.default_expiry)
    | Some exp -> exp

let get_expiry (ext: t) ((user, param_hash): permit_key) : seconds =
    match Big_map.find_opt (user, param_hash) ext.permit_expiries with
    | None ->
        begin
            match get_user_defined_expiry user ext with
            | None -> ext.default_expiry
            | Some exp -> exp
        end
    | Some (None) -> ext.default_expiry
    | Some (Some exp) -> exp

let assert_admin (ext : t) =
    assert_with_error (Tezos.get_sender() = ext.admin) Errors.requires_admin

let set_admin (ext : t) (admin:address) =
    let () = assert_admin(ext) in
    { ext with admin = admin }


let set_supply (ext : t) (supply : TokenTotalSupply.t) =
    { ext with token_total_supply = supply }


let add_permit (ext : t) (permit_key: permit_key) =
    let now = Tezos.get_now() in
    { ext with
        permits = Big_map.add permit_key now ext.permits;
        counter = ext.counter + 1n
    }

let update_permit (ext : t) (permit_key: permit_key) =
    let now = Tezos.get_now() in
    { ext with
        permits = Big_map.update permit_key (Some(now)) ext.permits;
        counter = ext.counter + 1n
    }

let _check_not_expired (ext : t) (submission_timestamp: timestamp) (permit_key: permit_key) =
    let effective_expiry: seconds = get_expiry ext permit_key in
    if abs (Tezos.get_now() - submission_timestamp) < effective_expiry
    then failwith Errors.dup_permit

let transfer_presigned (ext : t) (param_permit_key: permit_key): bool * t =
    let permit_submit_time: timestamp =
        match Big_map.find_opt param_permit_key ext.permits with
        | None -> (0: timestamp)
        | Some exp -> exp
    in
    if permit_submit_time = (0: timestamp)
    then
        (false, ext)
    else
        let effective_expiry =
            match Big_map.find_opt param_permit_key ext.permit_expiries with
            | None ->
                begin
                    match Big_map.find_opt param_permit_key.0 ext.user_expiries with
                    | None -> (Some ext.default_expiry)
                    | Some exp -> exp
                end
            | Some exp -> exp
        in
        match effective_expiry with
        | None -> (failwith Errors.missing_expiry: (bool * t))
        | Some effective_exp ->
            let permits = Big_map.remove param_permit_key ext.permits in
            let is_authorised = abs ((Tezos.get_now()) - permit_submit_time) < effective_exp in
            (is_authorised, { ext with permits = permits })
