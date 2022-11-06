{

    // ===== Contract Description ===== //
    // Name: Token Rarity Pool
    // Description: Guards the box holding references to all IDs of the minted tokens for the corresponding rarity pool.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Token Mint Tx ===== //
    // Description: This transaction mints the user's token.
    // Data Inputs: None
    // Inputs: Token Issuer Box i, Token Rarity Pool Box j
    // Outputs: Token Issuance Box i, Token Issuer Box (i+1), Token Rarity Pool Box j

    // ===== Rarity Pool Singleton Token Mint Tx ===== //
    // Description: Mints the singleton token identifier for the rarity pool box, which will be referenced in the AVL tree of the token pool state box.
    // Data Inputs: None
    // Inputs: Token Rarity Pool Issuance Box i, Token Pool State Box, Token Rarity Pool Box i
    // Outputs: Token Rarity Pool Issuer Box (i+1), Token Pool State Box, Token Rarity Pool i

    // ===== Token Sale Tx ===== //
    // Description: This transaction randomly selects a token from the token pool to be sold to the buyer.
    //              The selection method is customizable, depending entirely on the logic of the token sale proxy box contract.
    // Data Inputs: RNG Oracle
    // Inputs: Token Pool State Box, Token Rarity Pool Box j, ... , Token Rarity Pool Box k, Token Issuance Box m, ... , Token Issuance Box n, Token Sale Proxy Box
    // Outputs: Token Pool State Box, Token Rarity Pool Box j, ... , Token Rarity Pool Box k, Token Issuance Box m, ... , Token Issuance Box n, Buyer Box, User Box, ErgoPad Box, Tx Operator Box

    // ===== Sale End: Token Burn Tx ===== //
    // Description: The sale period has expired and the tokens must be burned.
    // Data Inputs: None
    // Inputs: Token Pool State Box, Token Rarity Pool j, Token Issuance m
    // Outputs: Token Rarity Pool j, Token Pool State Box

    // ===== Sale End: Reclaim Tokens Tx ===== //
    // Description: The sale period has expired and the tokens must be reclaimed.
    // Data Inputs: None
    // Inputs: Token Pool State Box, Token Rarity Pool Box j, Token Issuance Box m
    // Outputs: Token Rarity Pool Box j, User Box, Token Pool State Box

    // ===== Box Registers ===== //
    // R4: AvlTree => Token References
    // R5: Short => Rarity Pool Size

    // ===== Compile Time Constants ===== //
    // _MintType: Byte
    // _RarityPoolSize: Long
    // _TokenIssuanceBoxValue: Long
    // _TxOperatorPK: SigmaProp
    // _MinerFee: Long

    // ===== Context Extension Variables ===== //
    val TxType: Byte = getVar[Byte](0).get

    // ===== Mint Type ===== //
    // 0: Token Mint (TokenAmount > 1)
    // 1: On-Demand NFT Mint (TokenAmount == 1)

    // ===== Tx Types ===== //
    // 0: Token Mint Tx
    // 1: Singleton Token Mint Tx
    // 2: Token Sale Tx
    // 3: Token Burn Tx
    // 4: Reclaim Tokens Tx

    // ===== Tx Sub-Types ===== //
    // 0: Initial Token Burn Tx
    // 1: Subsequent Token Burn Tx
    // 2: Final Token Burn Tx
    // 3: Initial Reclaim Tokens Tx
    // 4: Subsequent Reclaim Tokens Tx
    // 5: Final Reclaim Tokens Tx

    if (TxType == 0) {

        // ===== Context Extension Variables ===== //
        val AVLKeyVals: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](1).get
        val AVLProof: Coll[Byte] = getVar[Coll[Byte]](2).get

        val validTokenMintTx: Boolean = {



        }

        sigmaProp(validTokenMintTx)

    } else if (TxType == 1) {

        val validSingletonTokenMintTx: Boolean = {
            


        }

        sigmaProp(validSingletonTokenMintTx)

    } else if (TxType == 2) {

        val validTokenSaleTx: Boolean = {



        }

        sigmaProp(validTokenSaleTx)

    } else if (TxType == 3) {

        // ===== Context Extension Variables ===== //
        val TxSubType: Option[Byte] = getVar[Byte](1).get
        val AVLKeyVals: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](2).get
        val AVLProof: Coll[Byte] = getVar[Coll[Byte]](3).get

        if (TxSubType == 0) {



        } else if (TxSubType == 1) {



        } else if (TxSubType == 2) {



        } else {

            sigmaProp(false)
        
        }

    } else if (TxType == 4) {

        // ===== Context Extension Variables ===== //
        val TxSubType: Option[Byte] = getVar[Byte](1).get
        val AVLKeyVals: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](2).get
        val AVLProof: Coll[Byte] = getVar[Coll[Byte]](3).get

        if (TxSubType == 3) {



        } else if (TxSubType === 4) {



        } else if (TxSubType == 5) {

            

        } else {

            sigmaProp(false)

        }

    } else {

        sigmaProp(false)

    }

}