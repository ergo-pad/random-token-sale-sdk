{

    // ===== Contract Description ===== //
    // Name: Token Issuer Contract
    // Description: Issuer box contract for the user's token, following the EIP-24 artwork standard.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Token Mint Tx ===== //
    // Description: This transaction mints the user's token.
    // Data Inputs: None
    // Inputs: Token Issuer Box i, Token Rarity Pool Box j
    // Outputs: Token Issuance Box i, Token Issuer Box (i+1), Token Rarity Pool Box j

    // ===== Box Registers ===== //
    // R4: Int                                                                                                  => Royalty Percentage
    // R5: Int                                                                                                  => Artwork Standard Version
    // R6: Coll[(Coll[Byte], Int)]                                                                              => Royalty Recipient Information 
    // R7: (Coll[(Coll[Byte], Coll[Byte])], (Coll[(Coll[Byte], (Int, Int))], Coll[(Coll[Byte], (Int,Int))]))    => Traits: Properties, Levels, Stats
    // R8: Coll[Byte]                                                                                           => Collection Token Id (if applicable)
    // R9: Coll[(Coll[Byte], Coll[Byte])]                                                                       => Additional Information

    // ===== Compile Time Constants ===== //
    // _IsCollection: Boolean
    // _TokenIssuanceBoxValue: Long
    // _TokenAmount: Long
    // _TxOperatorPK: SigmaProp
    // _MinerFee: Long

    // ===== Context Extension Variables ===== //
    val TokenIssuanceContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val TokenVerboseName: Coll[Byte] = getVar[Coll[Byte]](1).get
    val TokenDescription: Coll[Byte] = getVar[Coll[Byte]](2).get
    val TokenNumberOfDecimals: Coll[Byte] = get[Coll[Byte]](3).get // Should be 0, encoded according to EIP-4
    val TokenAssetType: Coll[Byte] = getVar[Coll[Byte]](4).get // The asset type is NFT-Artwork
    val TokenPictureSHA256Hash: Coll[Byte] = getVar[Coll[Byte]](5).get
    val TokenPictureLink: Coll[Byte] = getVar[Coll[Byte]](6).get

    val isFinalTokenMintTx: Boolean = (OUTPUTS.size == 3)
    
    val validTokenMintTx: Boolean = {

        // ===== Outputs ===== //
        val tokenIssuanceBoxOUT: Box = OUTPUTS(0)
        val minerBoxOUT: Box = OUTPUTS(OUTPUTS.size-1)

        val validTokenIssuanceBox: Boolean = {

            val validValue: Boolean = {
                (tokennIssuanceBoxOUT.value == _TokenIssuanceBoxValue)
            }

            val validContract: Boolean = {
                (tokenIssuanceBoxOUT.propositionBytes == TokenIssuanceContractBytes)
            }

            val validTokens: Boolean = {
                (tokenIssuanceBoxOUT.tokens(0) == (SELF.id, _TokenAmount))
            }

            val validTokenIssuanceProperties: Boolean = {

                allOf(Coll(
                    (tokenIssuanceBoxOUT.R4[Coll[Byte]].get == TokenVerboseName),
                    (tokenIssuanceBoxOUT.R5[Coll[Byte]].get == TokenDescription),
                    (tokenIssuanceBoxOUT.R6[Coll[Byte]].get == TokenNumberOfDecimals),
                    (tokenIssuanceBoxOUT.R7[Coll[Byte]].get == TokenAssetType),
                    (tokenIssuanceBoxOUT.R8[Coll[Byte]].get == TokenPictureSHA256Hash),
                    (tokenIssuanceBoxOUT.R9[Coll[Byte]].get == TokenPictureLink)
                ))

            }

            allOf(Coll(
                validValue,
                validContract,
                validTokens,
                validTokenIssuanceProperties
            ))

        }

        val validTokenIssuerBoxReplication: Boolean = {

            if(!isFinalTokenMintTx) {

                val tokenIssuerBoxOUT: Box = OUTPUTS(1)

                val validValue: Boolean = {
                    (tokenIssuerBoxOUT.value == SELF.value)
                }

                val validContract: Boolean = {
                    (tokenIssuerBoxOUT.propositionBytes == SELF.propositionBytes)
                }

                // Transfer the collection token
                val validCollectionTokenTransfer: Boolean {

                    if (_IsCollection) {
                        (tokenIssuanceBoxOUT.tokens(0) == SELF.tokens(0))
                    } else {
                        true
                    }
                    
                }

                allOf(Coll(
                    validValue,
                    validContract,
                    validCollectionTokenTransfer
                ))

            } else {
                
                // Burn the collection token
                val validCollectionTokenBurn: Boolean = {

                    if (_IsCollection) {
                        OUTPUTS.forall({ (output: Box) => (outputs.tokens.size == 0) })
                    } else {
                        true
                    }

                }

                allOf(Coll(
                    validCollectionTokenBurn
                ))

            }

        }

        val validOutputSize: Boolean = {

            if(!isFinalTokenMintTx) {
                (OUTPUTS.size == 4)
            } else {
                (OUTPUTS.size == 3)
            }
            
        }

        allOf(Coll(
            validTokenIssuanceBox,
            validTokenIssuerBoxReplication,
            validOutputSize
        ))

    }

    sigmaProp(validTokenCollectionMintTx) && _TxOperatorPK

}