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

    // ===== Contract Context Extension Variables ===== //
    val collectionStandardVersion: Int = getVar[Int](0).get
    val collectionInfo: Coll[Coll[Byte]] = getVar[Coll[Coll[Byte]]](1).get
    val collectionSocials: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](2).get
    val collectionMintingExpiry: Long = getVar[Long](3).get
    val collectionAdditionalInfo: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](4).get

    validTokenSaleSetupTx: Boolean = {

        // ===== Outputs ===== //
        val tokenCollectionIssuerBoxOUT: Box = OUTPUTS(0)
        val ergopadBoxOUT: Box = OUTPUTS(1)
        val txOperatorBoxOUT: Box = OUTPUTS(2)
        val minerBoxOUT: Box = OUTPUTS(3)

        val validTokenCollectionIssuerProxyBox: Boolean = {

            val validValue: Boolean = {
                (tokenCollectionIssuerBoxOUT.value == SELF.value - ergopadBoxOUT.value - txOperatorBoxOUT.value - minerBoxOUT.value)
            }

            val validContract: Boolean = {
                (tokenCollectionIssuerBoxOUT.propositionBytes == _TokenCollectionIssuerProxyContractBytes)
            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the token collection issuer box
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (tokenCollectionIssuerBoxOUT.tokens == SELF.tokens)
                    ))

                } else {
                    true
                }

            }

            val validCollectionIssuerProperties: Boolean = {

                allOf(Coll(
                    (tokenCollectionIssuerBoxOUT.R4[Int].get == collectionStandardVersion),
                    (tokenCollectionIssuerBoxOUT.R5[Coll[Coll[Byte]]].get == collectionInfo),
                    (tokenCollectionIssuerBoxOUT.R6[Coll[(Coll[Byte], Coll[Byte])]].get == collectionSocials),
                    (tokenCollectionIssuerBoxOUT.R7[Long].get == collectionMintingExpiry),
                    (tokenCollectionIssuerBoxOUT.R8[Coll[(Coll[Byte], Coll[Byte])]].get == collectionAdditionalInfo)
                ))

            }

            allOf(Coll(
                validValue,
                validContract,
                validWhitelistTokens,
                validCollectionIssuerProperties
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
            validTokenCollectionIssuerBox,
            validErgoPadBox,
            validTxOperatorBox,
            validMinerFee,
            (OUTPUTS.size == 4)
        ))

    }

    sigmaProp(validTokenSaleSetupTx)

}