{

    // ===== Contract Description ===== //
    // Name: Token Collection Issuer Contract
    // Description: Issuer box contract for the token sale collection, following the EIP-34 collection standard.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Token Collection Mint Tx ===== //
    // Description: This transaction mints the collection tokens.
    // Data Inputs: None
    // Inputs: Token Collection Issuer Box
    // Context Variables: None
    // Outputs: Token Collection Issuance Box

    // ===== Box Registers ===== //
    // Tokens: Whitelist Tokens
    // R4: Int                              => Collection Standard Version
    // R5: Coll[Coll[Byte]]                 => Collection Information
    // R6: Coll[(Coll[Byte], Coll[Byte])]   => Social Media Information
    // R7: Long                             => Minting Expiry Timestamp
    // R8: Coll[(Coll[Byte], Coll[Byte])]   => Additional Information

    // ===== Compile Time Constants ===== //
    // _RarityPoolsAmount: Long
    // _IsWhitelist: Boolean
    // _CollectionIssuanceBoxValue: Long
    // _TxOperatorPK: SigmaProp
    // _MinerFee: Long

    // ===== Context Extension Variables ===== //
    val TokenColectionIssuanceContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val CollectionTokenVerboseName: Coll[Byte] = getVar[Coll[Byte]](1).get
    val CollectionTokenDescription: Coll[Byte] = getVar[Coll[Byte]](2).get
    val CollectionTokenNumberOfDecimals: Coll[Byte] = getVar[Coll[Byte]](3).get // Should be 0, encoded according to EIP-4
    val CollectionTokenAssetType: Coll[Byte] = getVar[Coll[Byte]](4).get // The asset type is NFT - Artwork Collection

    val validTokenCollectionMintTx: Boolean = {

        // ===== Outputs ===== //
        val tokenCollectionIssuanceBoxOUT: Box = OUTPUTS(0)
        val minerBoxOUT: Box = OUTPUTS(1)

        val validTokenCollectionIssuanceBox: Boolean = {

            val validValue: Boolean = {
                (tokenCollectionIssuanceBoxOUT.value == _CollectionIssuanceBoxValue)
            }

            val validContract: Boolean = {
                (tokenCollectionIssuanceBoxOUT.propositionBytes == TokenCollectionIssuanceContractBytes)
            }

            val validCollectionTokens: Boolean = {

                (tokenCollectionIssuanceBoxOUT.tokens(0) == (SELF.id, _RarityPoolsAmount))

            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the token collection issuance box
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (tokenCollectionIssuanceBoxOUT.tokens.slice(1, tokenCollectionIssuanceBoxOUT.tokens.size) == SELF.tokens)
                    ))

                } else {
                    true
                }

            }

            val validCollectionIssuanceProperties: Boolean = {

                allOf(Coll(
                    (tokenCollectionIssuanceBoxOUT.R4[Coll[Byte]].get == CollectionTokenVerboseName),
                    (tokenCollectionIssuanceBoxOUT.R5[Coll[Byte]].get == CollectionTokenDescription),
                    (tokenCollectionIssuanceBoxOUT.R6[Coll[Byte]].get == CollectionTokenNumberOfDecimals),
                    (tokenCollectionIssuanceBoxOUT.R7[Coll[Byte]].get == CollectionTokenAssetType)
                ))

            }

            allOf(Coll(
                validValue,
                validContract,
                validWhitelistTokens,
                validCollectionIssuanceProperties
            ))

        }

        val validMinerBox: Boolean = {
            (minerBoxOUT.value == _MinerFee)
        }

        allOf(Coll(
            validTokenCollectionIssuanceBox,
            validMinerBox,
            (OUTPUTS.size == 2)
        ))

    }

    sigmaProp(validTokenCollectionMintTx) && _TxOperatorPK

}