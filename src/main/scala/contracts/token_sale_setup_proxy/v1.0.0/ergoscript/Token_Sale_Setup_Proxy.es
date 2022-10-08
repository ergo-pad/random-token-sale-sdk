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
    // Outputs: Token Collection Issuer Proxy Box, ErgoPad Box, Tx Operator Box

    // ===== Box Registers ===== //
    // R4: SigmaProp => UserPK

    // ===== Contract Compile Time Constants ===== //
    // _TokenCollectionIssuerProxyContractBytes: Coll[Byte]
    // _IsWhitelist: Boolean
    // _ErgoPadPK: SigmaProp
    // _TxOperatorPK: SigmaProp
    // _ErgoPadFee: Long
    // _TxOperatorFee: Long
    // _MinerFee: Long

    validTokenSaleSetupTx: Boolean = {

        // ===== Outputs ===== //
        val tokenCollectionIssuerProxyBoxOUT: Box = OUTPUTS(0)
        val ergopadBoxOUT: Box = OUTPUTS(1)
        val txOperatorBoxOUT: Box = OUTPUTS(2)
        val minerBoxOUT: Box = OUTPUTS(3)

        val validTokenCollectionIssuerProxyBox: Boolean = {

            val validValue: Boolean = {
                (tokenCollectionIssuerProxyBoxOUT.value == SELF.value - ergopadBoxOUT.value - txOperatorBoxOUT.value - minerBoxOUT.value)
            }

            val validContract: Boolean = {
                (tokenCollectionIssuerProxyBoxOUT.propositionBytes == _TokenCollectionIssuerProxyContractBytes)
            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the Token collection issuer box
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (tokenCollectionIssuerProxyBoxOUT.tokens == SELF.tokens)
                    ))

                } else {
                    true
                }

            }

            val validCollectionRegisterIssuerProperties: Boolean = {



            }

            allOf(Coll(
                validValue,
                validContract,
                validWhitelistTokens
            ))

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
            validTokenCollectionIssuerProxyBox,
            validErgoPadBox,
            validTxOperatorBox,
            validMinerFee,
            (OUTPUTS.size == 4)
        ))

    }

    sigmaProp(validTokenSaleSetupTx)

}