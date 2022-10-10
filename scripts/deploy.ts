import dotenv from "dotenv";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";
import { buf2hex } from "@taquito/utils";
import code from "../compiled/taco_shop_token.json";
import metadata from "./metadata.json";

// Read environment variables from .env file
dotenv.config();

// Initialize RPC connection
const Tezos = new TezosToolkit(process.env.NODE_URL);

// Deploy to configured node with configured secret key
const deploy = async () => {
  try {
    const signer = await InMemorySigner.fromSecretKey(process.env.SECRET_KEY);

    Tezos.setProvider({ signer });

    // create a JavaScript object to be used as initial storage
    // https://tezostaquito.io/docs/originate/#a-initializing-storage-using-a-plain-old-javascript-object
    const storage = {
      metadata: MichelsonMap.fromLiteral({
        "": buf2hex(Buffer.from("tezos-storage:contents")),
        contents: buf2hex(Buffer.from(JSON.stringify(metadata))),
      }),
      // ^ contract metadata (tzip-16)
      // https://tzip.tezosagora.org/proposal/tzip-16/

      ledger: new MichelsonMap(),
      token_metadata: new MichelsonMap(),
      operators: new MichelsonMap(),
      // ^ FA2 storage

      extension: {
        admin: process.env.ADMIN,
        counter: 0,
        default_expiry: 3600,
        max_expiry: 7200,
        permits: new MichelsonMap(),
        user_expiries: new MichelsonMap(),
        permit_expiries: new MichelsonMap(),
        extension: new MichelsonMap(),
        // ^ token_total_supply storage extension
      },
      // ^ storage extension over generic FA2
    };

    const op = await Tezos.contract.originate({ code, storage });
    await op.confirmation();
    console.log(`[OK] ${op.contractAddress}`);
  } catch (e) {
    console.log(e);
  }
};

deploy();
