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
    // R5: Short => Rarity Pool Size Limit
    // R6: Short => Current Rarity Pool Size
    // R7: Byte => Mint Type

    // ===== Compile Time Constants ===== //
    // _ctTokenIssuanceBoxValue: Long
    // _ctTxOperatorPK: SigmaProp
    // _ctMinerFee: Long

    // ===== Context Extension Variables ===== //
    val _cvTxType: Byte = getVar[Byte](0).get
    val _rgAvlTree: AvlTree = SELF.R4[AvlTree].get
    val _rgRarityPoolSizeLimit: Short = SELF.R5[Short].get
    val _rgCurrentRarityPoolSize: Short = SELF.R6[Short].get
    val _rgMintType: Byte = SELF.R7[Byte].get

    // ===== Mint Type ===== //
    // 1: Token Mint (TokenAmount > 1)
    // 2: On-Demand NFT Mint (TokenAmount == 1)

    // ===== Tx Types ===== //
    // 1: Token Mint Tx
    // 2: Singleton Token Transfer Tx
    // 3: Token Sale Tx
    // 4: Token Burn Tx
    // 5: Reclaim Tokens Tx

    // ===== Tx Sub-Types ===== //
    // 11: Initial Token Mint Tx
    // 12: Final Token Mint Tx
    // 41: Initial Token Burn Tx
    // 42: Subsequent Token Burn Tx
    // 43: Final Token Burn Tx
    // 51: Initial Reclaim Tokens Tx
    // 52: Subsequent Reclaim Tokens Tx
    // 53: Final Reclaim Tokens Tx

    if (_cvTxType == 1) {

        // ===== Context Extension Variables ===== //
        val _cvTxSubType: Byte = getVar[Byte](1).get
        val _cvAvlKeyVals: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](2).get
        val _cvAvlProof: Coll[Byte] = getVar[Coll[Byte]](3).get

        if (_rgMintType == 1) {

            val validTokenMintTx: Boolean = {

                // ===== Inputs ===== //
                val tokenIssuerBoxIN: Box = INPUTS(0)

                // ===== Outputs ===== //
                val tokenRarityPoolBoxOUT: Box = if (_cvTxSubType == 11) OUTPUTS(2) else OUTPUTS(1)
                val minerBoxOUT: Box = OUTPUTS(OUTPUTS.size-1)

                val validTokenRarityPoolBox: Boolean = {

                    val validSelfReplication: Boolean = {

                        allOf(Coll(
                            (tokenRarityPoolBoxOUT.value == SELF.value - _ctMinerFee),
                            (tokenRarityPoolBoxOUT.propositionBytes == SELF.propositionBytes),
                            (tokenRarityPoolBoxOUT.R5[Short].get == _rgRarityPoolSizeLimit)
                            (tokenRarityPoolBoxOUT.R6[Short].get == _rgCurrentRarityPoolSize + 1),
                            (tokenRarityPoolBoxOUT.R6[Short].get <= _rgRarityPoolSizeLimit),
                            (tokenRarityPoolBoxOUT.R7[Byte] == _rgMintType)
                        ))

                    }

                    // We want to check the following:
                    // 1. That the key is valid, i.e. the avl tree is adding the token in the correct location.
                    // 2. That the value corresponds to the minted token id.
                    // 3. That an entry was actually inserted into the avl tree.
                    val validAvlTree: Boolean = {

                        val validKeyVals: Boolean = {

                            val avlKey: Int = byteArrayToLong(_cvAvlKeyVals(0)._1).toInt
                            val avlVal: Coll[Byte] = _cvAvlKeyVals(0)._2
                            
                            if (_rgCurrentRarityPoolSize < _rgRarityPoolSizeLimit) {

                                allOf(Coll(
                                    (avlKey == _rgCurrentRarityPoolSize.toInt),
                                    (avlVal == tokenIssuerBoxIN.id)
                                ))

                            } else {
                                false
                            }

                        }

                        val validInsertion: Boolean = {

                            val newAvlTree: AvlTree = _rgAvlTree.insert(_cvAvlKeyVals, _cvAvlProof).get
                            val newDigest: Coll[Byte] = newAvlTree.digest

                            (tokenRarityPoolBoxOUT.R4[AvlTree].get.digest == newDigest)

                        }

                        allOf(Coll(
                            validKeyVals,
                            validInsertion
                        ))

                    }

                    val validOutputSize: Boolean = {

                        if (_cvTxSubType == 11) {
                            (OUTPUTS.size == 4)
                        } else if (_cvTxSubType == 12) {
                            (OUTPUTS.size == 3)
                        } else {
                            false
                        }

                    }

                    allOf(Coll(
                        validSelfReplication,
                        validAvlTree,
                        validOutputSize
                    ))

                }

                val validMinerBox: Boolean = {
                    (minerBoxOUT.value == _ctMinerFee)
                }

                allOf(Coll(
                    validTokenRarityPoolBox,
                    validMinerBox
                ))

            }  

            sigmaProp(validTokenMintTx) 

        } else {

            sigmaProp(false)
        
        }

    } else if (_cvTxType == 2) {

        val validSingletonTokenTransferTx: Boolean = {
            
            if ( _rgMintType == 1 || _rgMintType == 2 ) {

                // ===== Inputs ===== //
                val tokenRarityPoolIssuanceBoxIN: Box = INPUTS(0)

                // ===== Outputs ===== //
                val tokenRarityPoolBoxOUT: Box = OUTPUTS(2)

                val validTokenRarityPoolBox: Boolean = {

                    val validSelfReplication: Boolean = {

                        allOf(Coll(
                            (tokenRarityPoolBoxOUT.value == SELF.value),
                            (tokenRarityPoolBoxOUT.propositionBytes == SELF.propositionBytes),
                            (tokenRarityPoolBoxOUT.R4[AvlTree].get == SELF.R4[AvlTree].get),
                            (tokenRarityPoolBoxOUT.R5[Short].get == _rgRarityPoolSizeLimit),
                            (tokenRarityPoolBoxOUT.R6[Short].get == _rgCurrentRarityPoolSize),
                            (tokenRarityPoolBoxOUT.R7[Byte].get == _rgMintType)
                        ))

                    }

                    val validSingletonTokenTransfer: Boolean = {
                        (tokenRarityPoolBoxOUT.tokens(0) == tokenRarityPoolIssuanceBoxIN.tokens(0))
                    }

                    allOf(Coll(
                        validSelfReplication,
                        validSingletonTokenTransfer
                    ))

                }

            } else {
                false
            }

        }

        sigmaProp(validSingletonTokenTransferTx)

    } else if (_cvTxType == 3) {

        val validTokenSaleTx: Boolean = {



        }

        sigmaProp(validTokenSaleTx)

    } else if (_cvTxType == 4) {

        // ===== Context Extension Variables ===== //
        val _cvTxSubType: Byte = getVar[Byte](1).get
        val _cvAVLKeyVals: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](2).get
        val _cvAVLProof: Coll[Byte] = getVar[Coll[Byte]](3).get

        if (_cvTxSubType == 41) {



        } else if (_cvTxSubType == 42) {



        } else if (_cvTxSubType == 43) {



        } else {

            sigmaProp(false)
        
        }

    } else if (_cvTxType == 5) {

        // ===== Context Extension Variables ===== //
        val TxSubType: Option[Byte] = getVar[Byte](1).get
        val AVLKeyVals: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](2).get
        val AVLProof: Coll[Byte] = getVar[Coll[Byte]](3).get

        if (_cvTxSubType == 51) {



        } else if (_cvTxSubType === 52) {



        } else if (_cvTxSubType == 53) { 

            

        } else {

            sigmaProp(false)

        }

    } else {

        sigmaProp(false)

    }

}