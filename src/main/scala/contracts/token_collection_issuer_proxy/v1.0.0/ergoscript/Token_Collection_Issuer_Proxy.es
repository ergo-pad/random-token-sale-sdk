{

    // ===== Contract Description ===== //
    // Name: Token Collection Issuer Contract
    // Description: Issuer box contract for the token sale collection, following the EIP-34 collection standard.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Token Collection Mint Tx ===== //
    // Description: This transaction mints the collection tokens.
    // Data Inputs: None
    // Inputs: Token Collection Issuer Proxy Box
    // Context Variables: None
    // Outputs: Token Collection Issuance Proxy Box

    // ===== Box Registers ===== //
    // R4: Int                              => Collection Standard Version
    // R5: Coll[Coll[Byte]]                 => Collection Information
    // R6: Coll[(Coll[Byte], Coll[Byte])]   => Social Media Information
    // R7: Long                             => Minting Expiry Timestamp
    // R8: Coll[(Coll[Byte], Coll[Byte])]   => Additional Information
    // R9: -

    // ===== Contract Compile Time Constants ===== //
    // _TokenCollectionIssuanceProxyContractBytes: Coll[Byte]
    // _RarityPoolsAmount: Long
    // _IsWhitelist: Boolean
    // _MinerFee: Long

    validTokenCollectionMintTx: Boolean = {

        // ===== Outputs ===== //
        val tokenCollectionIssuanceProxyBoxOUT: Box = OUTPUTS(0)
        val minerBoxOUT: Box = OUTPUTS(1)

        val validNFTCollectionIssuerProxyBox: Boolean = {

            val validValue: Boolean = {
                (tokenCollectionIssuanceProxyBoxOUT.value == SELF.value - minerBoxOUT.value)
            }

            val validContract: Boolean = {
                (tokenCollectionIssuanceProxyBoxOUT.propositionBytes == _TokenCollectionIssuanceProxyContractBytes)
            }

            val validCollectionTokens: Boolean = {

                (tokenCollectionIssuanceProxyBoxOUT.tokens(0) == (SELF.id, _RarityPoolsAmount))

            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the Token Collection Issuance box
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (tokenCollectionIssuanceProxyBoxOUT.tokens.slice(1, tokenCollectionIssuanceProxyBoxOUT.tokens.size) == SELF.tokens)
                    ))

                } else {
                    true
                }

            }

            val validCollectionIssuanceRegisterProperties: Boolean = {

                

            }

            allOf(Coll(
                validValue,
                validWhitelistTokens
            ))

        }

        val validMinerFee: Boolean = {
            (minerBoxOUT.value == _MinerFee)
        }

        allOf(Coll(

        ))

    }

    sigmaProp(validTokenCollectionMintTx)

}