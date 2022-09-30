{

    // ===== Contract Description ===== //
    // Name: Token Sale Setup Proxy
    // Description: Initiates the token minting process for the random token sale and holds any whitelist tokens for validation if a whitelist is needed.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Token Sale Setup Tx ===== //
    // Description: Transaction initiates the minting process and transfers any whitelist tokens used for validation.
    // Data Inputs: None
    // Inputs: Token Sale Setup Proxy
    // Context Variables: None
    // Outputs: NFT Collection Issuer Proxy Box, ErgoPad Box, Tx Operator Box

    // ===== Contract Compile Time Constants ===== //
    // _IsWhitelist: Boolean
    // _ErgoPadPK: SigmaProp
    // _TxOperatorPK: SigmaProp
    // _ErgoPadFee: Long
    // _TxOperatorFee: Long
    // _MinerFee: Long

    validTokenSaleSetupTx: Boolean = {

        // ===== Outputs ===== //
        val nftCollectionIssuerProxyBoxOUT: Box = OUTPUTS(0)
        val ergopadBoxOUT: Box = OUTPUTS(1)
        val txOperatorBoxOUT: Box = OUTPUTS(2)
        val minerBoxOUT: Box = OUTPUTS(3)

        val validNFTCollectionIssuerProxyBox: Boolean = {

            val validValue: Boolean = {
                (nftCollectionIssuerProxyBoxOUT.value == SELF.value - ergopadBoxOUT.value - txOperatorBoxOUT.value - minerBoxOUT.value)
            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the NFT collection issuer box
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (nftCollectionIssuerProxyBoxOUT.tokens == SELF.tokens)
                    ))

                } else {
                    true
                }

            }

        }

        val validErgoPadBox: Boolean = {

            allOf(Coll(
                (ergopadBoxOUT.value == _ErgoPadFee),
                (ergopadBoxOUT.propositionBytes == ErgoPadPK.propBytes)
            ))

        }

        val validTxOperatorBox: Boolean = {

            allOf(Coll(
                (txOperatorBoxOUT.value == _TxOperatorFee),
                (txOperatorBoxOUT.propositionBytes == _TxOperatorPK)
            ))

        }

        val validMinerFee: Boolean = {
            (minerBoxOUT.value == _MinerFee)
        }

        allOf(Coll(
            validNFTCollectionIssuerProxyBox,
            validErgoPadBox,
            validTxOperatorBox,
            validMinerFee,
            (OUTPUTS.size == 4)
        ))

    }

    sigmaProp(validTokenSaleSetupTx)

}